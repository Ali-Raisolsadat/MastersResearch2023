#' @author Ali Raisolsadat
#' @description
#' A short description...
#' 

#' **********************************************************************************************************************************
#' **********************************************************************************************************************************
#' PLOT FUNCTIONS:

#' FUNCTION 1: 
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

#' FUNCTION 2: 
#' This function constructs the attachment and exhaustion thresholds using simple
#' linear regression without transformation.
#' @param infoDf is the required data from data-frame
#' @param keyWord is the key word the type of classification, based on whether to use 
#' loss space or probability space. OPTIONS: "loss", "prob"
#' @return a summary of the regression models, their significance and a plot of the thresholds
doAllYearsRegressionNoTrans <- function(infoDf, keyWord) {
  #' Select color for the classification type
  if (keyWord == "loss") {
    color1 = "#339900"
    color2 = "#FF33CC"
  } else {
    color1 ="#3333FF"
    color2 = "#FF9900"
  }
  
  #' Construct the attachment threshold
  lmFitDfLowLayer = data.frame("year" = infoDf$year[infoDf$accel_group == 2], 
                               "loss" = infoDf$loss[infoDf$accel_group == 2]) 
  lm.mod <- train(loss  ~ year, data = lmFitDfLowLayer, method = "lm", trControl = myTimeControl, tuneLength=10)
  lmFitLowLayer <- lm.mod$finalModel
  #' Print summary of the regression
  printRegResults(lmFitLowLayer, threshType = "attachment")
  
  #' Fit all years to the attachment threshold
  testLmFitLowLayer = predict(lmFitLowLayer, infoDf["year"]) %>% as.data.frame()
  testLmFitLowLayer$year = infoDf$year
  colnames(testLmFitLowLayer) <- c("fit", "year")
  
  #' Construct the exhaustion threshold
  lmFitDfHighLayer = data.frame("year" = infoDf$year[infoDf$accel_group == 1], 
                                "loss" = infoDf$loss[infoDf$accel_group == 1]) 
  lm.mod <- train(loss ~ year, data = lmFitDfHighLayer, method = "lm", trControl = myTimeControl, tuneLength=10)
  lmFitHighLayer <- lm.mod$finalModel
  #' Print summary of the regression
  printRegResults(lmFitHighLayer, threshType = "exhaustion")
  
  #' Fit all years to the exhaustion threshold
  testLmFitHighLayer = predict(lmFitHighLayer, infoDf["year"]) %>% as.data.frame()
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

#' FUNCTION 3: 
#' This function constructs the attachment and exhaustion thresholds using non-linear
#' quadratic regression without transformation.
#' @param infoDf is the required data from data-frame
#' @param keyWord is the key word the type of classification, based on whether to use 
#' loss space or probability space. OPTIONS: "loss", "prob"
#' @return a summary of the regression models, their significance and a plot of the thresholds
doAllYearsRegressionSquaredTrans <- function(infoDf, keyWord) {
  #' Select color for the classification type
  if (keyWord == "loss") {
    color1 = "#339900"
    color2 = "#FF33CC"
  } else {
    color1 ="#3333FF"
    color2 = "#FF9900"
  }
  
  #' Add squared years to the data-frame 
  infoDf$year_squared <- (infoDf$year)^2
  
  #' Construct the attachment threshold
  lmFitDfLowLayer = data.frame("year" = infoDf$year[infoDf$accel_group == 2], 
                               "year_squared" = infoDf$year_squared[infoDf$accel_group == 2],
                               "loss" = infoDf$loss[infoDf$accel_group == 2]) 
  lm.mod <- train(loss  ~ year + year_squared, data = lmFitDfLowLayer, method = "lm", trControl = myTimeControl, tuneLength=10)
  lmFitLowLayer <- lm.mod$finalModel
  #' Print summary of the regression
  printRegResults(lmFitLowLayer, threshType = "attachment")

  #' Fit all years to the attachment threshold
  testLmFitLowLayer = predict(lmFitLowLayer, infoDf[c("year", "year_squared")]) %>% as.data.frame()
  testLmFitLowLayer$year = infoDf$year
  colnames(testLmFitLowLayer) <- c("fit", "year")
  
  #' Construct the exhaustion threshold
  lmFitDfHighLayer = data.frame("year" = infoDf$year[infoDf$accel_group == 1], 
                                "year_squared" = infoDf$year_squared[infoDf$accel_group == 1],
                                "loss" = infoDf$loss[infoDf$accel_group == 1]) 
  lm.mod <- train(loss ~ year + year_squared, data = lmFitDfHighLayer, method = "lm", trControl = myTimeControl, tuneLength=10)
  lmFitHighLayer <- lm.mod$finalModel
  #' Print summary of the regression
  printRegResults(lmFitHighLayer, threshType = "exhaustion")
  
  #' Fit all years to the exhaustion threshold
  testLmFitHighLayer = predict(lmFitHighLayer, infoDf[c("year", "year_squared")]) %>% as.data.frame()
  testLmFitHighLayer$year = infoDf$year
  colnames(testLmFitHighLayer) <- c("fit", "year")
  
  #' Plot the thresholds
  plot(lmFitDfLowLayer$year, lmFitDfLowLayer$loss, pch = 19, col = color1, lwd = 3,
       ylim = c(0, 260), xlim = c(1963, 2022), xlab = "Year", ylab = "", yaxt = "n", 
       cex.axis = 1.25, cex.lab = 1.25, family = "serif", cex = 1.25)
  grid(nx = NULL, ny = NULL, lty = 2, col = "gray", lwd = 1)
  points(lmFitDfHighLayer$year, lmFitDfHighLayer$loss, pch = 19, col = color2, lwd = 3, cex = 1.25)
  lines(testLmFitLowLayer$year, testLmFitLowLayer$fit, col = color1, lwd = 5)
  lines(testLmFitHighLayer$year, testLmFitHighLayer$fit, col = color2, lwd = 5)
}

#' FUNCTION 4: 
#' This function constructs the attachment and exhaustion thresholds using non-linear
#' cubic regression without transformation.
#' @param infoDf is the required data from data-frame
#' @param keyWord is the key word the type of classification, based on whether to use 
#' loss space or probability space. OPTIONS: "loss", "prob"
#' @return a summary of the regression models, their significance and a plot of the thresholds
doAllYearsRegressionCubicTrans <- function(infoDf, keyWord) {
  #' Select color for the classification type
  if (keyWord == "loss") {
    color1 = "#339900"
    color2 = "#FF33CC"
  } else {
    color1 ="#3333FF"
    color2 = "#FF9900"
  }
  
  #' Add squared and cubic years to the data-frame 
  infoDf$year_squared <- (infoDf$year)^2
  infoDf$year_cubed <- (infoDf$year)^3
  
  #' Construct the attachment threshold
  lmFitDfLowLayer = data.frame("year" = infoDf$year[infoDf$accel_group == 2], 
                               "year_squared" = infoDf$year_squared[infoDf$accel_group == 2],
                               "year_cubed" = infoDf$year_cubed[infoDf$accel_group == 2],
                               "loss" = infoDf$loss[infoDf$accel_group == 2]) 
  lm.mod <- train(loss  ~ year + year_squared + year_cubed, data = lmFitDfLowLayer, 
                  method = "lm", trControl = myTimeControl, tuneLength=10)
  lmFitLowLayer <- lm.mod$finalModel
  #' Print summary of the regression
  printRegResults(lmFitLowLayer, threshType = "attachment")
  
  #' Fit all years to the attachment threshold
  testLmFitLowLayer = predict(lmFitLowLayer, infoDf[c("year", "year_squared", "year_cubed")]) %>% as.data.frame()
  testLmFitLowLayer$year = infoDf$year
  colnames(testLmFitLowLayer) <- c("fit", "year")
  
  #' Construct the exhaustion threshold
  lmFitDfHighLayer = data.frame("year" = infoDf$year[infoDf$accel_group == 1], 
                                "year_squared" = infoDf$year_squared[infoDf$accel_group == 1],
                                "year_cubed" = infoDf$year_cubed[infoDf$accel_group == 1],
                                "loss" = infoDf$loss[infoDf$accel_group == 1]) 
  lm.mod <- train(loss ~ year + year_squared + year_cubed, data = lmFitDfHighLayer,
                  method = "lm", trControl = myTimeControl, tuneLength=10)
  lmFitHighLayer <- lm.mod$finalModel
  #' Print summary of the regression
  printRegResults(lmFitHighLayer, threshType = "exhaustion")
  
  #' Fit all years to the exhaustion threshold
  testLmFitHighLayer = predict(lmFitHighLayer, infoDf[c("year", "year_squared", "year_cubed")]) %>% as.data.frame()
  testLmFitHighLayer$year = infoDf$year
  colnames(testLmFitHighLayer) <- c("fit", "year")
  
  #' Plot the thresholds
  plot(lmFitDfLowLayer$year, lmFitDfLowLayer$loss, pch = 19, col = color1, lwd = 3,
       ylim = c(0, 260), xlim = c(1963, 2022), xlab = "Year", ylab = "", 
       cex.axis = 1.25, cex.lab = 1.25, family = "serif", cex = 1.25)
  grid(nx = NULL, ny = NULL, lty = 2, col = "gray", lwd = 1)
  points(lmFitDfHighLayer$year, lmFitDfHighLayer$loss, pch = 19, col = color2, lwd = 3, cex = 1.25)
  lines(testLmFitLowLayer$year, testLmFitLowLayer$fit, col = color1, lwd = 5)
  lines(testLmFitHighLayer$year, testLmFitHighLayer$fit, col = color2, lwd = 5)
}

#' FUNCTION 5: 
#' This function constructs the attachment and exhaustion thresholds using simple
#' linear regression with specified transformation of the target.
#' @param infoDf is the required data from data-frame
#' @param keyWord is the key word the type of classification, based on whether to use 
#' loss space or probability space. OPTIONS: "loss", "prob"
#' @param transKey
#' @return a summary of the regression models, their significance and a plot of the thresholds
doAllYearsRegressionTrans <- function(infoDf, transKey, keyWord) {
  #' Select color for the classification type
  if (keyWord == "loss") {
    color1 = "#339900"
    color2 = "#FF33CC"
  } else {
    color1 ="#3333FF"
    color2 = "#FF9900"
  }
  
  #' Based on the transformation key, transform the target variable
  
  #' square-root transformation
  if (transKey == "square_root") {
    lambda = 0.5
    x <- infoDf$loss
    new_x_exact <- (x ^ lambda - 1) / lambda
    infoDf$loss_new <- new_x_exact
  }
  
  #' inverse of square root transformation
  if (transKey == "inv_square_root") {
    lambda = -0.5
    x <- infoDf$loss
    new_x_exact <- (x ^ lambda - 1) / lambda
    new_x_exact[!is.finite(new_x_exact)] <- 0
    infoDf$loss_new <- new_x_exact
  }
  
  #' Construct the attachment threshold
  lmFitDfLowLayer = data.frame("year" = infoDf$year[infoDf$accel_group == 2], 
                               "loss" = infoDf$loss[infoDf$accel_group == 2],
                               "loss_new" = infoDf$loss_new[infoDf$accel_group == 2]) 
  lm.mod <- train(loss_new  ~ year, data = lmFitDfLowLayer, method = "lm", trControl = myTimeControl, tuneLength=10)
  lmFitLowLayer <- lm.mod$finalModel
  #' Print summary of the regression
  printRegResults(lmFitLowLayer, threshType = "attachment")
  
  #' Testing low-medium loss layer losses
  testLmFitLowLayer = predict(lmFitLowLayer, infoDf["year"]) %>% as.data.frame()
  testLmFitLowLayer = ((testLmFitLowLayer*lambda) + 1) ^ (1/lambda)
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
  testLmFitHighLayer = predict(lmFitHighLayer, infoDf["year"]) %>% as.data.frame()
  testLmFitHighLayer = ((testLmFitHighLayer*lambda) + 1) ^ (1/lambda)
  testLmFitHighLayer$year = infoDf$year
  colnames(testLmFitHighLayer) <- c("fit", "year")
  
  #' Plot the thresholds
  if (transKey == "square_root") {
    plot(lmFitDfLowLayer$year, lmFitDfLowLayer$loss, pch = 19, col = color1, lwd = 3,
         ylim = c(0, 260), xlim = c(1963, 2022), xlab = "Year", ylab = "Estimated Loss (Billion USD)", 
         cex.axis = 1.25, cex.lab = 1.25, family = "serif", cex = 1.25)
  } else {
    plot(lmFitDfLowLayer$year, lmFitDfLowLayer$loss, pch = 19, col = color1, lwd = 3,
         ylim = c(0, 260), xlim = c(1963, 2022), xlab = "Year", ylab = "", 
         cex.axis = 1.25, cex.lab = 1.25, family = "serif", yaxt = "n", cex = 1.25)
  }
  points(lmFitDfHighLayer$year, lmFitDfHighLayer$loss, pch = 19, col = color2, lwd = 3, cex = 1.25)
  grid(nx = NULL, ny = NULL, lty = 2, col = "gray", lwd = 1)
  lines(testLmFitLowLayer$year, testLmFitLowLayer$fit, col = color1, lwd = 5)
  lines(testLmFitHighLayer$year, testLmFitHighLayer$fit, col = color2, lwd = 5)
}

#' **********************************************************************************************************************************
#' **********************************************************************************************************************************
#' Loss space all years regression results

#' FIGURE 7:
png('figure7.png', width = 7680, height = 4320, units = "px", pointsize = 115)
#' set up the plot window
par(mfrow = c(1,2), oma = c(5,4,0,0) + 0.1, mar = c(0,0,1,1) + 0.1)
#' simple linear regression result
mainTitle = "(A)"
doAllYearsRegressionNoTrans(lossInfoDf, keyWord = "loss")
title(main = mainTitle, adj = 0, cex.sub = 1.25, family = "serif")
#' quadratic non-linear regression result
mainTitle = "(B)"
doAllYearsRegressionSquaredTrans(lossInfoDf, keyWord = "loss")
title(main = mainTitle, adj = 0, cex.sub = 1.25, family = "serif")
#' show the threshold plots side-by-side
title(xlab = "Year",
      ylab = "Estimated Loss (Billion USD)",
      outer = TRUE, line = 3, family = "serif", cex.axis = 1.25, cex.lab = 1.25)
dev.off()

#' FIGURE 8:
png('figure8.png', width = 7680, height = 4320, units = "px", pointsize = 115)
#' set up the plot window
par(mfrow = c(1,2), oma = c(5,4,0,0) + 0.1, mar = c(0,0,1,1) + 0.1)
#' cubic non-linear regression result
mainTitle = "(C)"
doAllYearsRegressionCubicTrans(lossInfoDf, keyWord = "loss")
title(main = mainTitle, adj = 0, cex.sub = 1.25, family = "serif")
#' logarithmic transformation regression result
mainTitle = "(D)"
doAllYearsRegressionLogTrans(lossInfoDf, keyWord = "loss")
title(main = mainTitle, adj = 0, cex.sub = 1.25, family = "serif")
#' show the threshold plots side-by-side
title(xlab = "Year",
      ylab = "Estimated Loss (Billion USD)",
      outer = TRUE, line = 3, family = "serif", cex.axis = 1.25, cex.lab = 1.25)
dev.off()

#' FIGURE 9:
png('figure9.png', width = 7680, height = 4320, units = "px", pointsize = 115)
#' set up the plot window
par(mfrow = c(1,2), oma = c(5,4,0,0) + 0.1, mar = c(0,0,1,1) + 0.1)
#' square root transformation regression result
mainTitle = "(E)"
doAllYearsRegressionTrans(lossInfoDf, transKey = "square_root", keyWord = "loss")
title(main = mainTitle, adj = 0, cex.sub = 1.25, family = "serif")
#' inverse square root transformation regression result
mainTitle = "(F)"
doAllYearsRegressionTrans(lossInfoDf, transKey = "inv_square_root", keyWord = "loss")
title(main = mainTitle, adj = 0, cex.sub = 1.25, family = "serif")
#' show the threshold plots side-by-side
title(xlab = "Year",
      ylab = "Estimated Loss (Billion USD)",
      outer = TRUE, line = 3, family = "serif", cex.axis = 1.25, cex.lab = 1.25)
dev.off()


#' **********************************************************************************************************************************
#' **********************************************************************************************************************************
#' Probability space all years regression results

#' FIGURE 10:
png('figure10.png', width = 7680, height = 4320, units = "px", pointsize = 115)
#' set up the plot window
par(mfrow = c(1,2), oma = c(5,4,0,0) + 0.1, mar = c(0,0,1,1) + 0.1)
#' simple linear regression result
mainTitle = "(A)"
doAllYearsRegressionNoTrans(probInfoDf, keyWord = "prob")
title(main = mainTitle, adj = 0, cex.sub = 1.25, family = "serif")
#' quadratic non-linear regression result
mainTitle = "(B)"
doAllYearsRegressionSquaredTrans(probInfoDf, keyWord = "prob")
title(main = mainTitle, adj = 0, cex.sub = 1.25, family = "serif")
#' show the threshold plots side-by-side
title(xlab = "Year",
      ylab = "Estimated Loss (Billion USD)",
      outer = TRUE, line = 3, family = "serif", cex.axis = 1.25, cex.lab = 1.25)
dev.off()

#' FIGURE 11:
png('figure11.png', width = 7680, height = 4320, units = "px", pointsize = 115)
#' set up the plot window
par(mfrow = c(1,2), oma = c(5,4,0,0) + 0.1, mar = c(0,0,1,1) + 0.1)
#' cubic non-linear regression result
mainTitle = "(C)"
doAllYearsRegressionCubicTrans(probInfoDf, keyWord = "prob")
title(main = mainTitle, adj = 0, cex.sub = 1.25, family = "serif")
#' logarithmic transformation regression result
mainTitle = "(D)"
doAllYearsRegressionLogTrans(probInfoDf, keyWord = "prob")
title(main = mainTitle, adj = 0, cex.sub = 1.25, family = "serif")
#' show the threshold plots side-by-side
title(xlab = "Year",
      ylab = "Estimated Loss (Billion USD)",
      outer = TRUE, line = 3, family = "serif", cex.axis = 1.25, cex.lab = 1.25)
dev.off()

#' FIGURE 12:
png('figure12.png', width = 7680, height = 4320, units = "px", pointsize = 115)
#' set up the plot window
par(mfrow = c(1,2), oma = c(5,4,0,0) + 0.1, mar = c(0,0,1,1) + 0.1)
#' square root transformation regression result
mainTitle = "(E)"
doAllYearsRegressionTrans(probInfoDf, transKey = "square_root", keyWord = "prob")
title(main = mainTitle, adj = 0, cex.sub = 1.25, family = "serif")
#' inverse square root transformation regression result
mainTitle = "(F)"
doAllYearsRegressionTrans(probInfoDf, transKey = "inv_square_root", keyWord = "prob")
title(main = mainTitle, adj = 0, cex.sub = 1.25, family = "serif")
#' show the threshold plots side-by-side
title(xlab = "Year",
      ylab = "Estimated Loss (Billion USD)",
      outer = TRUE, line = 3, family = "serif", cex.axis = 1.25, cex.lab = 1.25)
dev.off()