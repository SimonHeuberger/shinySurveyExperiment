library(here)
library(rdrop2)
setwd(here::here("shiny"))
rdata.dropdir <- "seqblock"
drop_auth(rdstoken = "droptoken.rds")

seqUpload <- function(file) {
  drop_upload(file, path = rdata.dropdir, mode = "overwrite")
}
