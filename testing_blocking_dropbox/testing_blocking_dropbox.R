
#### Run the below on Jeff's machine, with the file "droptoken.rds" copied over as well ###
# I'm only saving the .RData because that's all I need here


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
  # Upload the file to Dropbox
  drop_upload(filePath, path = csv.dropdir)
}

sequpload <- function(file) {
  drop_upload(file, path = rdata.dropdir, mode = "overwrite")
}

seqdownload <- function(file) {
  drop_download(paste(rdata.dropdir, file, sep = "/"), overwrite = TRUE)
}

n.tr <- 5
mw.file <- "testing.blocking.dropbox.1000.RData"    # name for .RData with 1,000 'respondents'
mw.treat <- c("mw.control", "mw.m.opp",   "mw.m.supp",  "mw.p.opp",   "mw.p.supp")

ed.values <- as.numeric(1:5)
pid.values <- as.factor(1:3)
sims <- 1:1000    # 1,000 'respondents'
pb <- txtProgressBar(min = 1, max = length(sims), style = 3)

test.block.drop <- function(i) {
            setTxtProgressBar(pb, i)
            if(drop_exists(path = paste(rdata.dropdir, mw.file, sep = "/"))){
              
              seqdownload(mw.file)
              load(mw.file)
              seqblock(query = FALSE, object = mw.file, id.vals = bdata$orig[nrow(bdata$orig), "ID"]+1, 
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
            mw.sample <- paste(bdata$orig[nrow(bdata$orig), "Tr"])

            data.list <- list()

            data.list[["ID"]] <- paste(bdata$orig$ID[nrow(bdata$orig)])
            data.list[["pid"]] <- paste(bdata$orig$pid[nrow(bdata$orig)])
            data.list[["education"]] <- paste(bdata$orig$education[nrow(bdata$orig)])
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
  # Write data frame to .csv
  write.csv(data, file = "testing.blocking.dropbox.1000.csv", row.names = F) 
}

loaddata()


#### After running the above on Jeff's machine, run this below ####

df <- read.csv("testing.blocking.dropbox.1000.csv")
tapply(df$education, df$Tr, mean)
# mw.control   mw.m.opp  mw.m.supp   mw.p.opp  mw.p.supp 
# 3.028169   3.061538   3.012048   3.053140   3.013699
tapply(df$pid, df$Tr, mean)
# mw.control   mw.m.opp  mw.m.supp   mw.p.opp  mw.p.supp 
#2.037559   1.958974   1.969880   1.980676   2.045662

load("testing.blocking.dropbox.1000.RData")
tapply(bdata$orig$education, bdata$orig$Tr, mean)
#mw.control   mw.m.opp  mw.m.supp   mw.p.opp  mw.p.supp 
# 3.028169   3.061538   3.012048   3.053140   3.013699
tapply(bdata$orig$pid, bdata$orig$Tr, mean)
#mw.control   mw.m.opp  mw.m.supp   mw.p.opp  mw.p.supp 
# 2.037559   1.958974   1.969880   1.980676   2.045662 
