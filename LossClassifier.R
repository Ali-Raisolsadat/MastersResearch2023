#' @author Ali Raisolsadat
#' @description
#' A short description...
#' 

#' Load Packages
pckgs = c("readxl", "tidyverse", "dplyr", "janitor", "ggpubr", "readr", "tidyr", "caret", "magick", "flexmix", "MASS")
lapply(pckgs, FUN = function(X) {
  do.call("require", list(X)) 
})

#' **********************************************************************************************************************************
#' **********************************************************************************************************************************
#' HELPER FUNCTIONS:

#' FUNCTION 1: 
#' This function make ECDF and EDF
#' @param df is the data frame that has the loss information
#' @return a data frame with loss, ECDF and EDF values
makeEcdf <- function(df) {
  df <- df[order(df$adjusted_total_loss_by_year),]
  x <- df$adjusted_total_loss_by_year
  xDataEcdf <- sort(x)
  yDataEcdf <- seq(from = 1, to = length(x)) / length(x)
  yDataEdf <- 1 - yDataEcdf
  
  newDf <- data.frame("year" = df$year, "loss" = df$adjusted_total_loss_by_year, 
                      "cum_prob" = yDataEcdf, "exc_prob" = yDataEdf)
  newDf <- newDf %>% arrange(year, decreasing = TRUE)
  return(newDf)
}

#' FUNCTION 2: 
#' This function evaluates the probability using loss vector and given loss value
#' @param x is the loss vector
#' @param x0 is the loss value
#' @return a probability value
giveEcdfProb <- function(x, x0) {
  prob <- sum(x <= x0) / length(x)
  return(prob)
}

#' FUNCTION 3: 
#' This function evaluates the second central difference given a vector and step size.
#' @param x is the vector
#' @param h is the step size
doSecondCentralDiff <- function(x, h) {
  y <- c()
  for (i in 2:(length(x) - 1)) {
    y[i] <- (x[i+1] - 2*x[i] + x[i-1]) / (h^2)
  }
  y[1] <- (x[1] - 2*x[2] + x[3]) / (h^2)
  temp <- (x[length(x) - 2] - 2*x[length(x) - 1] + x[length(x)]) / (h^2)
  y <- c(y, temp)
  
  return(y)
}

#' FUNCTION 4: 
#' This function extracts overall p-value of a regression model
#' @param myModel is the regression model object
#' @return the p-value of the significance of the regression model
overallP <- function(myModel) {
  f <- summary(myModel)$fstatistic
  p <- pf(f[1], f[2], f[3], lower.tail=F)
  attributes(p) <- NULL
  return(p)
}

#' FUNCTION 5: 
#' This function prints the results for the regression models, the r-squared, AIC
#' and BIC. 
#' @param regModel is the regression model
#' @param threshType is the type of the threshold
#' @return a print of the summary of the regression and the goodness of fit
printRegResults <- function(regModel, threshType) {
  print("*****************************************************************************")
  if (threshType == "attachment") {
    print("The attachment threshold is given by the following model: ")
  } else {
    print("The exhaustion threshold is given by the following model: ")
  }
  print(summary(regModel))
  print("")
  print(paste0("R-squared for the model: ", round(summary(regModel)$r.squared, 3)))
  print(paste0("p-value for the model: ", overallP(regModel)))
  print(paste0("AIC for the model: ", round(AIC(regModel), 3)))
  print(paste0("BIC for the model: ", round(BIC(regModel), 3)))
  print("*****************************************************************************")
}

#' **********************************************************************************************************************************
#' **********************************************************************************************************************************
#' RESEARCH FUNCTIONS:

#' FUNCTION 6: 
#' This function extracts the required information and returns a list of data frames by 
#' individual countries.
#' @param emdatCountryData is the large data frame that contains all of the country data
#' @param keyword is the type of event selected by the user
#' @param startYear is the starting year for filtering data
#' @param endYear is the ending year for filtering data
#' options are: 1. Meteorological, 2. Climatological, and 3. Hydrological
#' @return a list of cleaned data frames for all countries loss data
getEventData <- function(emdatCountryData, keyword, startYear, endYear) {
  # make a vector of study years
  studyYears <- startYear:endYear
  
  # use disaster type keyword
  if (keyword != "all") { #select only the disasters that we need
    emdatCountryData <- emdatCountryData %>% filter(disaster_subgroup == keyword) 
  } else {
    emdatCountryData <- emdatCountryData %>% filter(disaster_subgroup %in% c("Meteorological", "Climatological", "Hydrological")) 
  }
  
  # split all the data (large data frame) into smaller data for each country
  emdatCountryData <- split(emdatCountryData, emdatCountryData$country)
  emdatCountryData <- emdatCountryData[order(sapply(emdatCountryData, nrow), decreasing = TRUE)]
  
  # filter data
  cleanEmdatCountryData <- lapply(emdatCountryData, 
                                  function(x) 
                                  {x %>% 
                                      group_by(year) %>%  #group by each year
                                      summarize(frequency = dplyr::n(), #count the number of disasters (as frequency)
                                                #summarize by adjusted total loss per year and divide by 1 Billion $USD
                                                adjusted_total_loss_by_year = sum(as.numeric(total_damages_adjusted_000_us), na.rm = TRUE)/10^9*10^3,
                                                #summarize by adjusted insured loss per year and divide by 1 Billion $USD
                                                adjusted_insured_loss_by_year = sum(as.numeric(insured_damages_adjusted_000_us), na.rm = TRUE)/10^9*10^3,
                                                #summarize by total number of affected people
                                                total_pop_affected = sum(as.numeric(total_affected), na.rm = TRUE),
                                                #summarize by total number of deaths
                                                total_pop_deaths = sum(as.numeric(total_deaths), na.rm = TRUE)) %>%
                                      #filter by year
                                      filter(year > (startYear - 1) & year < (endYear + 1))} )
  
  # fill in NA for the missing values
  fillEmdatCountryData <- lapply(cleanEmdatCountryData, function(x) x %>% 
                                   complete(year = studyYears))
  
  # return the cleaned and filled list of data frames
  return(fillEmdatCountryData)
}

#' FUNCTION 7: 
#' This function sums the regional loss information. If first detects if the 
#' list passed in as an argument has more than 1 entry, and if it does then it sums 
#' the loss information for that region. Otherwise it will return a country's information.
#' @param regionLossList is a list of countries for analysis
#' @param name of a region or country
#' @return a data frame that includes loss information for a region or a country
regionalLossDataFrameMaker <- function(regionLossList, regionCountryNames) {
  
  # filter region or country 
  regionLossList <- regionLossList[regionCountryNames]
  regionLossList[sapply(regionLossList, is.null)] <- NULL
  
  # if multiple countries (region) then sum the losses 
  # if one country just return the data frame
  if(length(regionLossList) > 1) { 
    # create a temporary list
    tempList <- list()
    
    # convert each data frame in the given list into matrix (data compatibility)
    for (i in 1:length(regionLossList)) {
      tempList[[i]] <- as.matrix(regionLossList[[i]])
    }
    
    # add all the data from each country
    regionLossDf <- as.data.frame(Reduce("+", lapply(tempList, function(x) replace(x, is.na(x), 0))))
    
    # ensure the years are correct
    regionLossDf$year <- regionLossDf$year/length(regionLossList)
  } else{
    regionLossDf <- regionLossList[[1]]
  }
  
  # return the regional data
  return(regionLossDf)
}

#' FUNCTION 8: 
#' This function classifies "near min" and "near max" loss values, given the space type.
#' @df is the data-frame that contains the loss and cumulative probability information
#' @space is a string that selects the space for the classification
#' @spaceSize is the central finite difference step size
#' @return a data-frame with loss values, cumulative probabilities, second derivative 
#' and classification given the space
makeInfoDf <- function(df, space, stepSize) {
  if (space == "loss") {
    # data-frame for the loss space classification
    infoDf <- data.frame("year" = df$year, "loss" = df$loss, "prob" = df$cum_prob, 
                         "acceleration"= doSecondCentralDiff(df$loss, h = stepSize), 
                         "accel_group" = ifelse(doSecondCentralDiff(df$loss, h = stepSize) <= 0, 1, 2))
  } else {
    # data-frame for the cumulative probability space classification
    infoDf <- data.frame("year" = df$year, "loss" = df$loss, "prob" = df$cum_prob,
                         "acceleration"= doSecondCentralDiff(df$cum_prob, h = stepSize), 
                         "accel_group" = ifelse(doSecondCentralDiff(df$cum_prob, h = stepSize) <= 0, 1, 2))
  }
  
  return(infoDf)
}

#' FUNCTION 9: 
#' This function constructs the attachment and exhaustion thresholds using simple
#' linear regression with log-transformation of the target.
#' @param infoDf is the required data from data-frame
#' @param keyWord is the key word the type of classification, based on whether to use 
#' loss space or probability space. OPTIONS: "loss", "prob"
#' @return a summary of the regression models, their significance and a plot of the thresholds
doAllYearsRegressionLogTrans <- function(infoDf, keyWord) {
  #' Select color for the classification type
  if (keyWord == "loss") {
    color1 = "#339900"
    color2 = "#FF33CC"
  } else {
    color1 ="#3333FF"
    color2 = "#FF9900"
  }
  
  #' log-transform of losses
  infoDf$loss_new <- log(infoDf$loss + 1)
  
  #' Construct the attachment threshold
  lmFitDfLowLayer = data.frame("year" = infoDf$year[infoDf$accel_group == 2], 
                               "loss" = infoDf$loss[infoDf$accel_group == 2],
                               "loss_new" = infoDf$loss_new[infoDf$accel_group == 2]) 
  lm.mod <- train(loss_new  ~ year, data = lmFitDfLowLayer, method = "lm", trControl = myTimeControl, tuneLength=10)
  lmFitLowLayer <- lm.mod$finalModel
  #' Print summary of the regression
  printRegResults(lmFitLowLayer, threshType = "attachment")
  
  #' Fit all years to the attachment threshold
  testLmFitLowLayer = exp(predict(lmFitLowLayer, infoDf["year"])) %>% as.data.frame()
  testLmFitLowLayer$year = infoDf$year
  colnames(testLmFitLowLayer) <- c("fit", "year")
  
  #' Construct the exhaustion threshold
  lmFitDfHighLayer = data.frame("year" = infoDf$year[infoDf$accel_group == 1], 
                                "loss" = infoDf$loss[infoDf$accel_group == 1],
                                "loss_new" = infoDf$loss_new[infoDf$accel_group == 1]) 
  lm.mod <- train(loss_new ~ year, data = lmFitDfHighLayer, method = "lm", trControl = myTimeControl, tuneLength=10)
  lmFitHighLayer <- lm.mod$finalModel
  #' Print summary of the regression
  printRegResults(lmFitHighLayer, threshType = "exhaustion")

  #' Testing medium-high loss layer losses
  testLmFitHighLayer = exp(predict(lmFitHighLayer, infoDf["year"])) %>% as.data.frame()
  testLmFitHighLayer$year = infoDf$year
  colnames(testLmFitHighLayer) <- c("fit", "year")
  
  #' Plot the thresholds
  plot(lmFitDfLowLayer$year, lmFitDfLowLayer$loss, pch = 19, col = color1, lwd = 3,
        ylim = c(0, 260), xlim = c(1963, 2022), xlab = "Year", ylab = "Estimated Loss (Billion USD)", 
        cex.axis = 1.25, cex.lab = 1.25, family = "serif", cex = 1.25)
  grid(nx = NULL, ny = NULL, lty = 2, col = "gray", lwd = 1)
  points(lmFitDfHighLayer$year, lmFitDfHighLayer$loss, pch = 19, col = color2, lwd = 3, cex = 1.25)
  lines(testLmFitLowLayer$year, testLmFitLowLayer$fit, col = color1, lwd = 5)
  lines(testLmFitHighLayer$year, testLmFitHighLayer$fit, col = color2, lwd = 5)
}

#' **********************************************************************************************************************************
#' **********************************************************************************************************************************
#' SECTION 1: CONSTANTS AND DATA

# set seed
set.seed(123)
# file location and name
FILENAME = "emdat_complete_df_2023.csv"
# year data
START_YEAR =  1963
END_YEAR = 2022
# difference step size constant (h)
DIFF_STEP = 1
# confidence interval
LAYER_CI = 0.996

#' read appropriate data
if(file.exists(FILENAME)) {
  emdatCompleteDf <- read.csv(FILENAME)
} else {
  source("DataCleaner.R")
}

# regression model train control
myTimeControl <- trainControl(
  method = "LOOCV", number = 100,
  verboseIter = FALSE, 
  returnData = TRUE, 
  p = 0.5, 
  index = NULL)

#' **********************************************************************************************************************************
#' **********************************************************************************************************************************
#' SECTION 1: CLEAN DATA

REGIONS <- unique(emdatCompleteDf$region)
# "Western Africa"            "Southern Asia"             "Central America"           "Northern America"         
# "Eastern Africa"            "South America"             "Western Europe"            "Eastern Asia"             
# "Caribbean"                 "South-Eastern Asia"        "Northern Africa"           "Eastern Europe"           
# "Southern Europe"           "Melanesia"                 "Australia and New Zealand" "Polynesia"                
# "Northern Europe"           "Western Asia"              "Micronesia"                "Southern Africa"          
# "Middle Africa"             "Russian Federation"        "Central Asia"  

#' Select regions for study from above list
SELECTED_REGION = c("Northern America", "Central America")
REGION_COUNTRIES <- unique(emdatCompleteDf$country[which(emdatCompleteDf$region %in% SELECTED_REGION)])

#' Or select a specific country
COUNTRY =  c("United States of America (the)", "Canada", "Mexico")

#' Read the large data frame (all of the data) into a list by country (or by region)
keyWord <- "all" #options are: "all", "Meteorological", "Climatological", "Hydrological", "Geophysical"
emdatAllEventsData <- 
  regionalLossDataFrameMaker(getEventData(emdatCompleteDf, keyWord, START_YEAR, END_YEAR), COUNTRY)

#' **********************************************************************************************************************************
#' **********************************************************************************************************************************
#' SECTION 2: CONSTRUCT THE REQUIRED 

#' Construct the cumulative and exceedance probability data-frame
lossExcessProbDf <- makeEcdf(emdatAllEventsData)

#' Cumulative probability space classification data-frame
probInfoDf <- makeInfoDf(lossExcessProbDf, "cum_prob", DIFF_STEP)

#' Loss space classification data-frame
lossInfoDf <- makeInfoDf(lossExcessProbDf, "loss", DIFF_STEP)

#' **********************************************************************************************************************************
#' **********************************************************************************************************************************
#' SECTION 3: CONSTRUCT FIGURES FOR THE PAPER

#' FIGURE 1: This plot shows total aggregated loss per year.
#' The figure is saved in the project directory.
png('figure1.png', width = 7680, height = 4320, units = "px", pointsize = 125)
plot(lossExcessProbDf$year, lossExcessProbDf$loss, type = "l",  ylim = c(0, 260), xlim = c(START_YEAR, END_YEAR),
     ylab = "Estimated Loss (Billion USD)", xlab = "Year", cex.axis = 1.25, cex.lab = 1.25, 
     family = "serif", col = "#112446", lwd = 2, xaxt = "n")
points(lossExcessProbDf$year, lossExcessProbDf$loss, col = adjustcolor("red", alpha.f=1), pch = 19, cex = 1.4)
grid(nx = NULL, ny = NULL, lty = 2, col = "gray", lwd = 1)
dev.off()

#' FIGURE 2: This plot shows cumulative probability values for losses per year.
#' The figure is saved in the project directory.
png('figure2.png', width = 7680, height = 4320, units = "px", pointsize = 125)
plot(lossExcessProbDf$year, lossExcessProbDf$cum_prob, type = "l",  ylim = c(0, 1), xlim = c(START_YEAR, END_YEAR), 
     ylab = "", xlab = "Year", lwd = 2,
     cex.axis = 1.25, cex.lab = 1.25, family = "serif", col = "#112446")
points(lossExcessProbDf$year, lossExcessProbDf$cum_prob, col = adjustcolor("red", alpha.f=1), pch = 19, cex = 1.4)
grid(nx = NULL, ny = NULL, lty = 2, col = "gray", lwd = 1)
mtext(text = expression(~ hat(F)(x[t])), side=2, padj = -1.5, at = 0.5, family = "serif", cex = 1.25)
dev.off()

#' **********************************************************************************************************************************
#' FIGURE 3: This plot shows the classified losses using the loss space.
#' The figure is saved in the project directory.
png('figure3.png', width = 7680, height = 4320, units = "px", pointsize = 125)
plot(lossInfoDf$year, lossInfoDf$loss, type = "l",  ylim = c(0, 260), xlim = c(START_YEAR, END_YEAR), col = "black", 
     ylab = "Estimated Loss (Billion USD)", xlab = "Year", cex.axis = 1.5, cex.lab = 1.5, family = "serif")
points(lossInfoDf$year[which(lossInfoDf$accel_group == 2)], lossInfoDf$loss[which(lossInfoDf$accel_group == 2)], 
       col = adjustcolor("#339900", alpha.f=1), pch = 19, cex = 1.5)
points(lossInfoDf$year[which(lossInfoDf$accel_group == 1)], lossInfoDf$loss[which(lossInfoDf$accel_group == 1)], 
       col = adjustcolor("#FF33CC", alpha.f=1), pch = 19, cex = 1.5)
grid(nx = NULL, ny = NULL, lty = 2, col = "gray", lwd = 1)
legend("topleft", fill=c("#339900", "#FF33CC"), inset=.02, cex=1.5, horiz=FALSE, title="Classification of loss", text.font = 3,
       legend=c(bquote(paste("Near Minimum: ", x[t], "''") > 0), bquote(paste("Near Maximum: ", x[t], "''") < 0)))
dev.off()

#' FIGURE 4: This plot shows the classified losses using the probability space.
#' The figure is saved in the project directory.
png('figure4.png', width = 7680, height = 4320, units = "px", pointsize = 125)
plot(probInfoDf$year, probInfoDf$loss, type = "l",  ylim = c(0, 260), xlim = c(START_YEAR, END_YEAR), col = "black", 
     ylab = "Estimated Loss (Billion USD)", xlab = "Year", cex.axis = 1.5, cex.lab = 1.5, family = "serif")
points(probInfoDf$year[which(probInfoDf$accel_group == 2)], probInfoDf$loss[which(probInfoDf$accel_group == 2)], 
       col = adjustcolor("#3333FF", alpha.f=1), pch = 19, cex = 1.5)
points(probInfoDf$year[which(probInfoDf$accel_group == 1)], probInfoDf$loss[which(probInfoDf$accel_group == 1)], 
       col = adjustcolor("#FF9900", alpha.f=1), pch = 19, cex = 1.5)
grid(nx = NULL, ny = NULL, lty = 2, col = "gray", lwd = 1)
legend("topleft", fill=c("#3333FF", "#FF9900"), inset=.02, cex=1.5, horiz=FALSE, title="Classification of loss", text.font = 3,
       legend=c(bquote(paste("Near Minimum: ", p[t], "''") > 0), bquote(paste("Near Maximum: ", p[t], "''") < 0)))
dev.off()

#' **********************************************************************************************************************************
#' FIGURE 5: This figure show the log-transformed regression results using classified losses by the loss space. 
#' The thresholds are plotted here.
#' The figure is saved in the project directory.
png('figure5.png', width = 7680, height = 4320, units = "px", pointsize = 125)
doAllYearsRegressionLogTrans(lossInfoDf, keyWord = "loss")
legend("topleft", fill=c("#339900", "#FF33CC"), inset=.02, cex=1.5, horiz=FALSE, title="Classification of loss", text.font = 3,
       legend=c(bquote(paste("Near Minimum: ", x[t], "''") > 0), bquote(paste("Near Maximum: ", x[t], "''") < 0)))
dev.off()

#' FIGURE 6: This figure show the log-transformed regression results using classified losses by the probability space. 
#' The thresholds are plotted here.
#' The figure is saved in the project directory.
png('figure6.png', width = 7680, height = 4320, units = "px", pointsize = 125)
doAllYearsRegressionLogTrans(probInfoDf, keyWord = "prob")
legend("topleft", fill=c("#3333FF", "#FF9900"), inset=.02, cex=1.5, horiz=FALSE, title="Classification of loss", text.font = 3,
       legend=c(bquote(paste("Near Minimum: ", p[t], "''") > 0), bquote(paste("Near Maximum: ", p[t], "''") < 0)))
dev.off()
