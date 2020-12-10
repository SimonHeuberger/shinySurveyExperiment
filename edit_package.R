#### Editing, running, testing, an deploying the shiny app ####

## Edit my questions and copy them over to the app

library(here) 
setwd(here::here("questions"))
current.q <- "/Users/simonheuberger/dissertation/shiny/questions"
app.q <- "/Users/simonheuberger/dissertation/shiny/survey_experiment/questions"
list.of.files.q <- tools::file_path_sans_ext(list.files(current.q, pattern = ".txt"))
for(i in 1:length(list.of.files.q)){
  temp <- read.csv(paste0(list.of.files.q[i],".csv"))
  write.table(temp, paste0(list.of.files.q[i],".txt"), sep="\t", quote = FALSE, row.names = F)
}
file.copy(paste0(list.of.files.q, ".txt"), app.q, overwrite = TRUE)

## Test deployed app code after I make any manual changes
setwd(here::here("survey_experiment"))
library(shiny)
runApp()

setwd(here::here("questions", "treatment"))
current.treat <- "/Users/simonheuberger/dissertation/shiny/questions/treatment"
app.treat <- "/Users/simonheuberger/dissertation/shiny/survey_experiment/questions/treatment"
list.of.files.treat <- tools::file_path_sans_ext(list.files(current.treat, pattern = ".txt"))
for(i in 1:length(list.of.files.treat)){
  temp <- read.csv(paste0(list.of.files.treat[i],".csv"))
  write.table(temp, paste0(list.of.files.treat[i],".txt"), sep="\t", quote = FALSE, row.names = F)
}
file.copy(paste0(list.of.files.treat, ".txt"), app.treat, overwrite = TRUE)


## Test deployed app code after I make any manual changes
setwd(here::here("survey_experiment"))
library(shiny)
runApp()



## (Re-)Deploy app
library(rsconnect)
rsconnect::deployApp(here::here("survey_experiment"))
Y
Y

#### Data analysis ####

## Run the loadData() function to download/list/unlist the data from Dropbox; read in the .csv; look at the data
# make sure wd is the shiny main folder before running this
setwd(here::here())
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
loadData(outputDir = outputDir)
alldata <- read.csv(paste0(outputDir, "/", outputDir, ".csv"))
View(alldata)

## Run drop_download() to download the blocked .RData files, load them, look at the data frame 

# make sure wd is the shiny main folder before running this
setwd(here::here())
library(rdrop2)
drop_auth(rdstoken = "droptoken.rds")
seqpath <- "seqblock.op"
seqfile <- "seqhc.op.RData"
seqfile <- "seqev.op.RData"
seqpath <- "seqblock.an"
seqfile <- "seqhc.an.RData"
seqfile <- "seqev.an.RData"
drop_download(paste0(seqpath, "/", seqfile), overwrite = TRUE)
load(seqfile)
View(bdata$x)


#### Comparing to the original version of shinyPsych ####

## Run original ShinyPsych code; create app
# make sure wd is the shiny main folder before running this
setwd(here::here())
source("/Library/Frameworks/R.framework/Versions/3.5/Resources/library/ShinyPsych/shiny-examples/Survey/app.R")
shinyApp(ui = ui, server = server)


## Load original questions from folder /extdata if I want to look at some lines
filee <- "Instructions_Survey"
filee <- "Demographics"
filee <- "Survey_Example"
filee <- "Goodbye"
fil <- system.file("extdata", paste0(filee,".txt"), package = "ShinyPsych")   # read the text straight from the package
inst.df <- read.table(fil, header = TRUE, sep = "\t", stringsAsFactors = FALSE)        # create a data frame named "inst.df"
write.csv(inst.df, file = paste0(filee,".csv"), row.names = F)                # write the data frame to a .csv
system(paste("open", paste0(filee,".csv")))                                   # open the .csv in Excel where I can edit and save it

