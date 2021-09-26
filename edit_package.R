
#### Create Dropbox token (only needs to be done once!) ####
library(rdrop2)
drop_auth()
token <- drop_auth()
saveRDS(token, file = "droptoken.rds")



#### Edit questions and copy them over to the app ####
# Demographics and others
library(here)
current.q <- here::here("questions")
app.q <- here::here("survey_experiment/questions")
setwd(here::here("questions"))
list.of.files.q <- tools::file_path_sans_ext(list.files(current.q, pattern = ".txt"))
for(i in 1:length(list.of.files.q)){
  temp <- read.csv(paste0(list.of.files.q[i],".csv"))
  write.table(temp, paste0(list.of.files.q[i],".txt"), sep="\t", quote = FALSE, row.names = F)
}
file.copy(paste0(list.of.files.q, ".txt"), app.q, overwrite = TRUE)

# Treatment
library(here)
current.treat <- here::here("questions/treatment")
app.treat <- here::here("survey_experiment/questions/treatment")
setwd(here::here("questions", "treatment"))
list.of.files.treat <- tools::file_path_sans_ext(list.files(current.treat, pattern = ".txt"))
for(i in 1:length(list.of.files.treat)){
  temp <- read.csv(paste0(list.of.files.treat[i],".csv"))
  write.table(temp, paste0(list.of.files.treat[i],".txt"), sep="\t", quote = FALSE, row.names = F)
}
file.copy(paste0(list.of.files.treat, ".txt"), app.treat, overwrite = TRUE)



#### Test deployed app code after I make any manual changes ####
setwd(here::here("survey_experiment"))
library(shiny)
runApp()



#### (Re-)Deploy app ####
library(rsconnect)
rsconnect::deployApp(here::here("survey_experiment"))
Y
Y



#### Data download from Dropbox ####
# laodData() for .csv files
setwd(here::here()) # shiny main folder
library(rdrop2)
library(plyr)
outputDir = "alldata.op"
outputDir = "alldata.an"
drop_auth(rdstoken = "droptoken.rds")
loadData <- function(outputDir) {         # either alldata.op or alldata.an            
  # Read in all .csv files
  rdrop2:::drop_is_folder(outputDir)
  filesInfo <- drop_dir(outputDir)
  filePaths <- filesInfo$path_display
  # Turn all files into a list
  data <- lapply(filePaths, drop_read_csv, stringsAsFactors = FALSE)
  # Concatenate all data together into one data frame
  data <- ldply(data, data.frame)
  # Write data frame to .csv in local folder /alldata
  write.csv(data, file = paste0(outputDir, "/", outputDir, ".csv"), row.names = F) 
}
loadData(outputDir = outputDir) # run the function
alldata <- read.csv(paste0(outputDir, "/", outputDir, ".csv")) # read in the collective .csv
View(alldata) # look at the data

# drop_download() for blocked .RData files
setwd(here::here()) # shiny main folder
library(rdrop2)
drop_auth(rdstoken = "droptoken.rds")
seqpath <- "seqblock.op" # these 3 lines for blocked OP data
seqfile <- "seqhc.op.RData"
seqfile <- "seqev.op.RData"
seqpath <- "seqblock.an" # these 3 lines for blocked AN data
seqfile <- "seqhc.an.RData"
seqfile <- "seqev.an.RData"
drop_download(paste0(seqpath, "/", seqfile), overwrite = TRUE) # run function
load(seqfile) # load the .RData file
View(bdata$x) # look at the data



