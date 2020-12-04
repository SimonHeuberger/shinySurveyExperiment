#### Editing, running and testing the shiny app ####

## Edit my questions

library(here) 
# here() does not work in the usual sense here, since it doesn't play nicely with system(). Better to keep setwd() alongside it
# I'm using here::here() because plyr masks here() - and plyr is loaded when shiny_psych_survey.R is sourced

# Demographic questions
setwd(here::here("questions"))
filed <- "instructions"
filed <- "demographics"
filed <- "education.op"
filed <- "education.an"
filed <- "code"
filed <- "pid"
filed <- "ideol"
filed <- "pid.foll.dem"
filed <- "pid.foll.rep"
filed <- "pid.foll.ind.else"
filed <- "ideol.foll.cons"
filed <- "ideol.foll.lib"
filed <- "ideol.foll.nei"
filed <- "morals"
filed <- "self-interest"
filed <- "hc.check"
filed <- "ev.check"

temp <- read.csv(paste0(filed,".csv"))
system(paste("open", paste0(filed,".csv"))) 
write.table(temp, paste0(filed,".txt"), sep="\t", quote = FALSE, row.names = F)
system(paste("open", paste0(filed,".txt")))   


# Treatment questions
setwd(here::here("questions", "treatment"))
filed <- "hc.control"
filed <- "hc.m.opp"
filed <- "hc.m.supp"
filed <- "hc.si.opp"
filed <- "hc.si.supp"
filed <- "ev.control"
filed <- "ev.m.opp"
filed <- "ev.m.supp"
filed <- "ev.si.opp"
filed <- "ev.si.supp"

temp <- read.csv(paste0(filed,".csv"))
system(paste("open", paste0(filed,".csv"))) 
write.table(temp, paste0(filed,".txt"), sep="\t", quote = FALSE, row.names = F)
system(paste("open", paste0(filed,".txt")))   


## Load original questions from folder /extdata if I want to look at some lines
filee <- "Instructions_Survey"
filee <- "Demographics"
filee <- "Survey_Example"
filee <- "Goodbye"
fil <- system.file("extdata", paste0(filee,".txt"), package = "ShinyPsych")   # read the text straight from the package
inst.df <- read.table(fil, header = TRUE, sep = "\t", stringsAsFactors = FALSE)        # create a data frame named "inst.df"
write.csv(inst.df, file = paste0(filee,".csv"), row.names = F)                # write the data frame to a .csv
system(paste("open", paste0(filee,".csv")))                                   # open the .csv in Excel where I can edit and save it

## Run modified ShinyPsych code to structure setup; create app (.csv files are saved on AU Dropbox)
source("shiny_psych_survey.R")
shinyApp(ui = ui, server = server)

### ANY CHANGES TO /survey_experiment/app.R NEED TO BE DONE MANUALLY, NOT WITH CODE -- FOR SAFETY ###


## Test deployed app code after I make any manual changes
setwd(here::here("survey_experiment"))
library(shiny)
runApp()

## (Re-)Deploy app
library(rsconnect)
rsconnect::deployApp(here::here("survey_experiment"))
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
source("/Library/Frameworks/R.framework/Versions/3.5/Resources/library/ShinyPsych/shiny-examples/Survey/app.R")
shinyApp(ui = ui, server = server)


