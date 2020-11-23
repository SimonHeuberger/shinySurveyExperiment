
library(here)
library(rdrop2)
library(plyr)

setwd(here::here("shiny"))
drop_auth(rdstoken = "droptoken.rds")

loadData <- function() {                     
  # Load library and set outputDir
  outputDir <- "alldata"              # loads all responses from AU Dropbox /alldata
  # Read in all .csv files
  rdrop2:::drop_is_folder(outputDir)
  filesInfo <- drop_dir(outputDir)
  filePaths <- filesInfo$path_display
  # Turn all files into a list
  data <- lapply(filePaths, drop_read_csv, stringsAsFactors = FALSE)
  # Concatenate all data together into one data frame
  data <- ldply(data, data.frame)
  # Write data frame to .csv in local folder /alldata
  write.csv(data, file = "alldata/all_data.csv", row.names = F) 
}
