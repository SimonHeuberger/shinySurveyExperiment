
#### Run the below on Jeff's machine, with the file "droptoken.rds" copied over as well ###

rm(list=ls())
library(rdrop2)
library(stringr)
library(blockTools)
library(plyr)

csv.dropdir <- "testing/csv" 
rdata.dropdir <- "testing/rdata"
drop_auth(rdstoken = "droptoken.rds")

savedata <- function(data) {                 
  data <- t(data)
  # Create a unique file name
  fileName <- sprintf("%s_%s.csv", as.integer(Sys.time()), digest::digest(data))
  # Write the data to a temporary file locally
  filePath <- file.path(tempdir(), fileName)
  write.csv(data, filePath, row.names = FALSE, quote = TRUE)
  # Upload the file to Dropbox/alldata
  drop_upload(filePath, path = csv.dropdir)
}

sequpload <- function(file) {
  drop_upload(file, path = rdata.dropdir, mode = "overwrite")
}

seqdownload <- function(file) {
  drop_download(paste(rdata.dropdir, file, sep = "/"), overwrite = TRUE)
}

n.tr <- 5
#mw.file <- "testing.blocking.dropbox.1000.RData"    # name for .RData with 1,000 'respondents'
mw.file <- "testing.blocking.dropbox.10000.RData"    # name for .RData with 10,000 'respondents'
mw.treat <- c("mw.control", "mw.m.opp",   "mw.m.supp",  "mw.p.opp",   "mw.p.supp")

ed.values <- as.numeric(1:5)
pid.values <- as.factor(1:3)
#sims <- 1:1000    # 1,000 'respondents'
sims <- 1:10000    # 10,000 'respondents'

pb <- txtProgressBar(min = 1, max = length(sims), style = 3)

test.block.drop <- function(i) {
            setTxtProgressBar(pb, i)
            if(drop_exists(path = paste(rdata.dropdir, mw.file, sep = "/"))){
              
              seqdownload(mw.file)
              load(mw.file)
              seqblock(query = FALSE, object = mw.file, id.vals = bdata$x[nrow(bdata$x), "ID"]+1, 
                       covar.vals = sample(ed.values, 1, replace = TRUE),
                       exact.vals = sample(pid.values, 1, replace = TRUE),
                       file.name = mw.file, n.tr = n.tr, tr.names = mw.treat)
              sequpload(mw.file)
              
            }else{
              
              seqblock(query = FALSE, id.vars = "ID", id.vals = 1, covar.vars = "education", 
                       covar.vals = sample(ed.values, 1, replace = TRUE),
                       exact.vars = "pid", exact.vals = sample(pid.values, 1, replace = TRUE),
                       file.name = mw.file, n.tr = n.tr, tr.names = mw.treat)
              sequpload(mw.file)
            }
  
            seqdownload(mw.file)
            load(mw.file)
            mw.sample <- paste(bdata$x[nrow(bdata$x), "Tr"])

            data.list <- list()

            data.list[["ID"]] <- paste(bdata$x$ID[nrow(bdata$x)])
            data.list[["pid"]] <- paste(bdata$x$pid[nrow(bdata$x)])
            data.list[["education"]] <- paste(bdata$x$education[nrow(bdata$x)])
            data.list[["Tr"]] <- mw.sample
      
            savedata(data.list)
            }

sapply(sims, test.block.drop)

loaddata <- function() {                     
  # Load library and set outputDir
  outputDir <- csv.dropdir              
  # Read in all .csv files
  rdrop2:::drop_is_folder(outputDir)
  filesInfo <- drop_dir(outputDir)
  filePaths <- filesInfo$path_display
  # Turn all files into a list
  data <- lapply(filePaths, drop_read_csv, stringsAsFactors = FALSE)
  # Concatenate all data together into one data frame
  data <- ldply(data, data.frame)
  # Write data frame to .csv in local folder /alldata
  write.csv(data, file = "testing.blocking.dropbox.10000.csv", row.names = F) 
}

loaddata()

#### After running the above on Jeff's machine ####

## For sims <- 1:1000
#df <- read.csv("testing.blocking.dropbox.1000.csv")
#tapply(df$education, df$Tr, mean)
# mw.control   mw.m.opp  mw.m.supp   mw.p.opp  mw.p.supp 
#  2.987730   3.018794   3.071438   3.052690   3.027928
#tapply(df$pid, df$Tr, mean)
# mw.control   mw.m.opp  mw.m.supp   mw.p.opp  mw.p.supp 
#  1.990196   1.947644   1.967033   1.919431   2.018868

#load("testing.blocking.dropbox.1000.RData")
#tapply(bdata$x$education, bdata$x$Tr, mean)
#mw.control   mw.m.opp  mw.m.supp   mw.p.opp  mw.p.supp 
#  2.987730   3.018794   3.071438   3.052690   3.027928 
#tapply(bdata$x$pid, bdata$x$Tr, mean)
#mw.control   mw.m.opp  mw.m.supp   mw.p.opp  mw.p.supp 
#  1.990196   1.947644   1.967033   1.919431   2.018868 

## For sims <- 1:10000
#df <- read.csv("testing.blocking.dropbox.10000.csv")
#tapply(df$education, df$Tr, mean)
#mw.control   mw.m.opp  mw.m.supp   mw.p.opp  mw.p.supp 
#  3.013629   3.011400   3.010242   3.017436   3.011126
#tapply(df$pid, df$Tr, mean)
#mw.control   mw.m.opp  mw.m.supp   mw.p.opp  mw.p.supp 
#  1.995115   2.032924   1.987799   1.957489   1.982884 

#load("testing.blocking.dropbox.10000.RData")
#tapply(bdata$x$education, bdata$x$Tr, mean)
#mw.control   mw.m.opp  mw.m.supp   mw.p.opp  mw.p.supp 
#  3.013629   3.011400   3.010242   3.017436   3.011126
#tapply(bdata$x$pid, bdata$x$Tr, mean)
#mw.control   mw.m.opp  mw.m.supp   mw.p.opp  mw.p.supp 
#  1.995115   2.032924   1.987799   1.957489   1.982884 


