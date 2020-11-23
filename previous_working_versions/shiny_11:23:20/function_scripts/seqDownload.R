library(here)
library(rdrop2)
setwd(here::here("shiny"))
rdata.dropdir <- "seqblock"
drop_auth(rdstoken = "droptoken.rds")

seqDownload <- function(file) {
  drop_download(paste(rdata.dropdir, file, sep = "/"), overwrite = TRUE)
}
