
library(here)
library(rdrop2)
setwd(here::here("shiny"))
csv.dropdir <- "alldata" 
drop_auth(rdstoken = "droptoken.rds")

saveData <- function(data) {                 
  data <- t(data)
  # Create a unique file name
  fileName <- sprintf("%s_%s.csv", as.integer(Sys.time()), digest::digest(data))
  # Write the data to a temporary file locally
  filePath <- file.path(tempdir(), fileName)
  write.csv(data, filePath, row.names = FALSE, quote = TRUE)
  # Upload the file to Dropbox/alldata
  drop_upload(filePath, path = csv.dropdir)
}
