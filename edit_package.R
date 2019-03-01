#### Editing, running and testing the shiny app ####

## Edit my questions

setwd("/Users/simonheuberger/Google Drive/Amerika/dissertation/___ordinal_blocking/shiny/questions")
filed <- "instructions"
filed <- "demographics"
filed <- "code"
filed <- "goodbye"
filed <- "party"
filed <- "education"

setwd("/Users/simonheuberger/Google Drive/Amerika/dissertation/___ordinal_blocking/shiny/questions/treatment")
filed <- "mw.control"
filed <- "mw.m.opp"
filed <- "mw.m.supp"
filed <- "mw.p.opp"
filed <- "mw.p.supp"
filed <- "tb.control"
filed <- "tb.m.opp"
filed <- "tb.m.supp"
filed <- "tb.p.opp"
filed <- "tb.p.supp"

temp <- read.csv(paste(filed,".csv", sep = ""))
system(paste("open", paste(filed,".csv", sep = ""))) 
write.table(temp, paste(filed,".txt", sep = ""), sep="\t", quote = FALSE, row.names = F)
system(paste("open", paste(filed,".txt", sep = "")))   

## Load original questions from folder /extdata if I want to look at some lines
filee <- "Instructions_Survey"
filee <- "Demographics"
filee <- "Survey_Example"
filee <- "Goodbye"
fil <- system.file("extdata", paste(filee,".txt", sep = ""), package = "ShinyPsych")   # read the text straight from the package
inst.df <- read.table(fil, header = TRUE, sep = "\t", stringsAsFactors = FALSE)        # create a data frame named "inst.df"
write.csv(inst.df, file = paste(filee,".csv", sep = ""), row.names = F)                # write the data frame to a .csv
system(paste("open", paste(filee,".csv", sep = "")))                                   # open the .csv in Excel where I can edit and save it

## Run modified ShinyPsych code to structure setup; create app (.csv files are saved on AU Dropbox)
setwd("/Users/simonheuberger/Google Drive/Amerika/dissertation/___ordinal_blocking/shiny")
source("shiny_psych_survey.R")
shinyApp(ui = ui, server = server)


### ANY CHANGES TO /survey_experiment/app.R NEED TO BE DONE MANUALLY, NOT WITH CODE -- FOR SAFETY ###


## Test deployed app code after I make any manual changes
setwd('/Users/simonheuberger/Google Drive/Amerika/dissertation/___ordinal_blocking/shiny/survey_experiment')
library(shiny)
runApp()

## (Re-)Deploy app
library(rsconnect)
rsconnect::deployApp('/Users/simonheuberger/Google Drive/Amerika/dissertation/___ordinal_blocking/shiny/survey_experiment')
Y


#### Data analysis ####

## Run the loadData() function to download/list/unlist the data from Dropbox; read in the .csv; look at the data
loaddata <- function() {                     # loads all responses from AU Dropbox /block_data
  # Load library and set outputDir
  library(rdrop2)
  outputDir <- "block_data"
  # Read in all .csv files
  filesInfo <- drop_dir(outputDir)
  filePaths <- filesInfo$path_display
  # Turn all files into a list
  data <- lapply(filePaths, drop_read_csv, stringsAsFactors = FALSE)
  # Concatenate all data together into one data frame
  library(plyr)
  data <- ldply(data, data.frame)
  # Write data frame to .csv in local folder /block_data
  write.csv(data, file = "block_data/all_data.csv", row.names = F) 
}

setwd("/Users/simonheuberger/Google Drive/Amerika/dissertation/___ordinal_blocking/shiny")
loaddata()
# Currently not working for some reason, opened an issue: https://github.com/karthik/rdrop2/issues/166
all_data <- read.csv("block_data/all_data.csv")
View(all_data)


#### Comparing to the original version of shinyPsych ####

## Run original ShinyPsych code; create app (.csv files are saved locally in /original_data)
setwd("/Users/simonheuberger/Google Drive/Amerika/dissertation/___ordinal_blocking/shiny")
source("/Library/Frameworks/R.framework/Versions/3.5/Resources/library/ShinyPsych/shiny-examples/Survey/app.R")
shinyApp(ui = ui, server = server)


