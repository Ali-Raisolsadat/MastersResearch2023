#' @author Ali Raisolsadat
#' @description
#' This R script cleans the EM-DAT data file, given the name of the file in 
#' directory of the project.
#' @details
#' Last updated: 14, August, 2023

#' Load Packages
pckgs = c("readxl", "tidyverse", "dplyr", "janitor", "ggpubr", "readr", "tidyr", "caret", "magick")
lapply(pckgs, FUN = function(X) {
  do.call("require", list(X)) 
})

#' **********************************************************************************************************************************
#' **********************************************************************************************************************************
#' This function reads the original Excel files and cleans them. 
#' @param path is the path of the Excel files
#' @param fileName is the Excel file name we want to analyze
#' @param startYear is the starting year for filtering data
#' @param endYear is the ending year for filtering data
#' @return a large data frame that includes all of the countries EM-DAT Public data
readCleanEMdata <- function(path, fileName, startYear, endYear) {
  #' Convert Excel files into CSV
  # create a vector of Excel files to read
  files.to.read = paste0(path, "//", list.files(path = "data", pattern = "xlsx"))
  
  # read each file and write it to csv
  lapply(files.to.read, function(f) {
    df = read_excel(f, sheet=1)
    write.csv(df, gsub("xlsx", "csv", f), row.names=FALSE)
  })
  
  
  #' Clean the new csv file
  # read the new csv file 
  emdatCompleteDf <- read.csv(paste0(path, "//", fileName), header = TRUE)
  
  # remove the first 5 rows since it has the XLSX signature information
  emdatCompleteDf <- tail(emdatCompleteDf, -5)
  
  # use the first row to rename the columns
  colnames(emdatCompleteDf) <- emdatCompleteDf[1,]
  emdatCompleteDf <- tail(emdatCompleteDf, -1)
  emdatCompleteDf <- emdatCompleteDf %>% clean_names()
  
  # set the year column as numerical value
  emdatCompleteDf$year <- as.numeric(as.character(emdatCompleteDf$year))
  
  # return cleaned EM-DAT data
  return(emdatCompleteDf)
}

#' **********************************************************************************************************************************
#' **********************************************************************************************************************************
#' Constants
# file location and name
PATH = "data"
FILENAME = "emdat_public_2023_07_22.csv"
# year data
START_YEAR =  1963
END_YEAR = 2022

#' **********************************************************************************************************************************
#' **********************************************************************************************************************************
#' Convert Excel files into CSV and Clean the new csv file
emdatCompleteDf <- readCleanEMdata(PATH, FILENAME, START_YEAR, END_YEAR)
write.csv(emdatCompleteDf, file = "emdat_complete_df_2023.csv", row.names = FALSE)
