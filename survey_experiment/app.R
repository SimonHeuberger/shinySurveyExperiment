
#-------------------------------------------- Preliminary stuff before shiny gets involved --------------------------------------------#


# Working directory, libraries
library(shiny)
library(shinyjs)
library(ShinyPsych)
library(rdrop2)
library(stringr)
library(blockTools)
library(rdrop2)
library(plyr)

# Javascript code to set up a function that redirects to a web page
js_code <- "
shinyjs.browseURL = function(url) {
  window.open(url,'_self');
}
"

# The survey slightly differs for the pre-test on MTurk and the experiment on Lucid.
# I make that selection here
platform <- "Lucid"  # either Mturk or Lucid

# Dropbox directory to save data. op = ordered probit; an = ANES
csv.dropdir.op <- "alldata.op"
csv.dropdir.an <- "alldata.an"
rdata.dropdir.op <- "seqblock.op"
rdata.dropdir.an <- "seqblock.an"
drop_auth(rdstoken = "droptoken.rds")

# Function that uploads a file to Dropbox/seqblock and overwrites any pre-existing file
seqUpload <- function(file, rdata.dropdir) {
  drop_upload(file, 
              path = rdata.dropdir, 
              mode = "overwrite")
}

# Function that downloads a file from Dropbox/seqblock.op and overwrites any pre-existing file
seqDownload <- function(file, rdata.dropdir) {
  drop_download(path = paste(rdata.dropdir, file, sep = "/"), 
                overwrite = TRUE)
}

# Treatment question and party ID follow-up names (they are needed for idsVec below)
if(platform == "Mturk"){
  ins <- "instructions.mturk"
  co <- "code.mturk"
  throw <- "throw.mturk" # throw-away introductory questions
}
if(platform == "Lucid"){
  ins <- "instructions"
  co <- "code"
  throw <- "throw" # throw-away introductory questions
}

dem.intro <- "demographics.intro"
issues.intro <- "issues.intro"
mor.si.intro <- "mor.si.intro" # morality, self-interest
measure1 <- "measure1" # the first framing measure the respondent sees (either morality or self-interest)
measure2 <- "measure2" # the remaining measure (which the respondent sees after the first)
dem1 <- "demographics1"
dem2 <- "demographics2"
pid <- "pid"
pid.foll.up <- "pid.follow.up"
ideol <- "ideol"
ideol.foll.up <- "ideol.follow.up"
ed.both <- "ed.both" # to distinguish between the OP and AN education categories
issue1 <- "hc"
issue1.check <- paste0(issue1, ".check") # attention check for issue 1
issue2 <- "ev"
issue2.check <- paste0(issue2, ".check") # attention check for issue 2
com <- "comments"


# All these short versions are used in the code later with paste, assign etc. That way I only have to adjust things once at the beginning.
# The only exception are the samples, because otherwise I would keep re-sampling and messing things up.
# I deleted several lines to create page lists from the original here. They show up later several times with "assign".
# There is no point in keeping them up here because I needed to copy-paste the  "assign"s anyway in order to keep using paste

# Vector with page ids used to access objects later
idsVec <- c(str_to_title(ins), str_to_title(ed.both), str_to_title(issue1), str_to_title(issue2), 
            str_to_title(co), str_to_title(dem1), str_to_title(dem2), 
            str_to_title(ideol), str_to_title(pid),
            str_to_title(pid.foll.up), str_to_title(ideol.foll.up),
            str_to_title(measure1), str_to_title(measure2),
            str_to_title(issue1.check), str_to_title(issue2.check),
            str_to_title(com),
            str_to_title(throw),
            str_to_title(mor.si.intro),
            str_to_title(dem.intro),
            str_to_title(issues.intro))

# Specifications to run seqblock() so I don't have to retype them
n.tr <- 5 # number of treatment groups
# list of healthcare treatment questions
hc.treat <- tools::file_path_sans_ext(list.files("questions/treatment", pattern="^.*hc.*.txt"))
# list of environment treatment questions
ev.treat <- tools::file_path_sans_ext(list.files("questions/treatment", pattern="^.*ev.*.txt"))
hc.file.op <- "seqhc.op.RData"
ev.file.op <- "seqev.op.RData"
hc.file.an <- "seqhc.an.RData"
ev.file.an <- "seqev.an.RData"





#-------------------------------------------- Define ui function  --------------------------------------------#

ui <- fixedPage(
  
  # App title
  title = "ShinySurvey",
  uiOutput("MainAction"),
  
  # For ShinyJS functions
  useShinyjs(),
  
  # Include appropriate CSS and JS scripts
  includeScriptFiles(),
  
  # Needed for the JS browseURL function defined above
  extendShinyjs(text = js_code, functions = 'browseURL')
  
)


#-------------------------------------------- Define server function  --------------------------------------------#


##### Define server function #####
server <- function(input, output, session) {
  
  
  
  # Pulling in Lucid RID (i.e. the query string).
  # Pulls in the query string from the current URL as a list.
  # Outside of Lucid's system, there won't be a query string, which means parseQueryString() returns "".
  # This means the query.string list has length 0. If that's the case, RID will be stored as "no.query.string".
  # If there is a query.string, which is the case when Lucid tests it, RID is stored as query.string[[1]], which is the query string as a character.
  # The idea behind this is to redirect to an external website with the RID, which is required by Lucid.
  # This is set up so that I can deploy/run it and Lucid can test it with the same code (i.e. I don't have to adjust anything between the two platforms)
  
  if(platform == "Lucid"){
    RID <- reactive({
      query.string <- parseQueryString(session$clientData$url_search)
      if(length(query.string) == 0){
        "no.query.string"
      }else{
        query.string[[1]]
      }
    })
  }
  
  
  output$MainAction <- renderUI({
    PageLayouts()
    
  })
  
  
  # CurrentValues controls page settings such as which page to display
  CurrentValues <- createCtrlList(firstPage = ins, # id of the first page
                                  globIds = idsVec,           # ids of pages for createPage
                                  complCode = TRUE,           # create a completion code
                                  complName = "survey")    # first element of completion code
  
  
  
  ############# Section A: Blocking Code #############
  
  
  # Randomize which issue measure (morality or self-interest) is shown first
  mor.si.first <- reactiveValues()
  mor.si.first$a <- sample(c("morals.txt", "self-interest.txt"), 1) # sample one of the issue measures as the first measure to be shown
  mor.si.second <- reactiveValues()
  mor.si.second$a <- ifelse(isolate(mor.si.first$a) == "morals.txt", # assign the other issue measure as the second measure to be shown
                            paste0("self-interest.txt"), 
                            paste0("morals.txt"))
  
  
  
  # I wanted to set everything for if(Mturk) and if(Lucid) rather than ifelse() to avoid potential errors.
  # You can't have two if statements within eventReactive(). You can have ifelse, but not one if after the other. 
  # That's why I needed to duplicate the eventReactives into two ifs
  
  if(platform == "Mturk"){
    ed.sample <- eventReactive(input[[paste(str_to_title(ideol.foll.up), "_next", sep = "")]], { # this is triggered when they hit "Continue" on the ideology follow up questions
      "education.op.txt" # For Mturk purposes, I only needed one set of education categories, so I chose the smaller one
    })
  }
  if(platform == "Lucid"){
    ed.sample <- eventReactive(input[[paste(str_to_title(ideol.foll.up), "_next", sep = "")]], { # same trigger as above
      sample(c("education.op.txt", "education.an.txt"), 1) # sample one of the sets of education categories
    })
  }
  
  # Upload/Download into the correct OP/AN folder, depending on the content of ed.sample.
  # eventReactive() creates a reactive object that changes based on the event. This object can be used in later functions.
  # The event here is when respondents hit "Continue" on the issue introduction page.
  # If the hc .RData file exists in the corresponding OP/AN seqblock folder on Dropbox, the current user is not the first respondent. 
  # The function then downloads and loads the file, blocks on the existing data and the current user, then uploads the new .RData file.
  # If the hc .RData file does not exist in seqblock/ on Dropbox, the current user is the first respondent. 
  # The function then blocks on only the current user, then uploads the resulting .RData file.
  # Except for the first respondent to answer the survey, the code always reads in the existing .RData file with pre-assigned respondents.
  # input[[paste(str_to_title(issues.intro), "_next", sep = "")]] is code for the "Continue" button
  # input[[paste(str_to_title(ed.both), "_educ", sep = "")]] pulls in the user-selected education category. 
  # I have to put as.numeric() around it because it's pulled as a factor, and factors don't work with covar.vals.
  # id.vals creates the user id. It takes the last current row of the .RData file and adds 1.
  # The actual content of hc.sample is a string with the ending .txt. This is to identify and display the correct treatment page below.
  # The corresponding code here is paste(bdata$orig[nrow(bdata$orig), "Tr"], ".txt", sep = ""), which extracts the assigned treatment group, adds .txt, and saves that string. 
  # The load() code immediately before that loads the uploaded file into the local environment. Otherwise the assigned group is not available locally.
  # bdata is simply the name of the object when the hc .RData file is loaded. That's part of the blockTools package code. 
  # bdata$orig is the data frame with the IDs, blocked education categories, and assigned treatment groups
  
  # From the blockTools documentation:
  # Assign first unit (assume a 25 year old member of the Republican Party) to a treatment group. ## Save the results in file "sdata.RData":
  # seqblock(query = FALSE, id.vars = "ID", id.vals = 001, exact.vars = "party", exact.vals = "Republican", covar.vars = "age", covar.vals = 25, file.name = "sdata.RData")
  # Assign next unit (age 30, Democratic Party):
  # seqblock(query = FALSE, object = "sdata.RData", id.vals = 002, exact.vals = "Democrat", covar.vars = "age", covar.vals = 30, file.name = "sdata.RData")

  hc.sample <- eventReactive(input[[paste(str_to_title(issues.intro), "_next", sep = "")]], {
    
    withProgress(message = "", value = 0, {
      incProgress(.25)
      
      if(ed.sample() == "education.op.txt"){
        
        if(drop_exists(path = paste(rdata.dropdir.op, hc.file.op, sep = "/"))){
          seqDownload(hc.file.op, rdata.dropdir = rdata.dropdir.op)
          load(hc.file.op)
          seqblock(query = FALSE, object = hc.file.op, id.vals = bdata$orig[nrow(bdata$orig), "ID"]+1, 
                   covar.vals = as.numeric(input[[paste(str_to_title(ed.both), "_educ", sep = "")]]),
                   file.name = hc.file.op, n.tr = n.tr, tr.names = hc.treat)
          seqUpload(hc.file.op, rdata.dropdir = rdata.dropdir.op)
          load(hc.file.op)
          paste(bdata$orig[nrow(bdata$orig), "Tr"], ".txt", sep = "")
        }else{
          seqblock(query = FALSE, id.vars = "ID", id.vals = 1, covar.vars = "education", 
                   covar.vals = as.numeric(input[[paste(str_to_title(ed.both), "_educ", sep = "")]]),
                   file.name = hc.file.op, n.tr = n.tr, tr.names = hc.treat)
          seqUpload(hc.file.op, rdata.dropdir = rdata.dropdir.op)
          load(hc.file.op)
          paste(bdata$orig[nrow(bdata$orig), "Tr"], ".txt", sep = "")
        }
        
      }else{
        
        if(drop_exists(path = paste(rdata.dropdir.an, hc.file.an, sep = "/"))){
          seqDownload(hc.file.an, rdata.dropdir = rdata.dropdir.an)
          load(hc.file.an)
          seqblock(query = FALSE, object = hc.file.an, id.vals = bdata$orig[nrow(bdata$orig), "ID"]+1, 
                   covar.vals = as.numeric(input[[paste(str_to_title(ed.both), "_educ", sep = "")]]),
                   file.name = hc.file.an, n.tr = n.tr, tr.names = hc.treat)
          seqUpload(hc.file.an, rdata.dropdir = rdata.dropdir.an)
          load(hc.file.an)
          paste(bdata$orig[nrow(bdata$orig), "Tr"], ".txt", sep = "")
        }else{
          seqblock(query = FALSE, id.vars = "ID", id.vals = 1, covar.vars = "education", 
                   covar.vals = as.numeric(input[[paste(str_to_title(ed.both), "_educ", sep = "")]]),
                   file.name = hc.file.an, n.tr = n.tr, tr.names = hc.treat)
          seqUpload(hc.file.an, rdata.dropdir = rdata.dropdir.an)
          load(hc.file.an)
          paste(bdata$orig[nrow(bdata$orig), "Tr"], ".txt", sep = "")
        }
      }
    })
  })
  
  
  
  # The same as above, just for ev
  ev.sample <- eventReactive(input[[paste(str_to_title(issue1.check), "_next", sep = "")]], {
    
    withProgress(message = "", value = 0, {
      incProgress(.25)
      
      if(ed.sample() == "education.op.txt"){
        
        if(drop_exists(path = paste(rdata.dropdir.op, ev.file.op, sep = "/"))){
          seqDownload(ev.file.op, rdata.dropdir = rdata.dropdir.op)
          load(ev.file.op)
          seqblock(query = FALSE, object = ev.file.op, id.vals = bdata$orig[nrow(bdata$orig), "ID"]+1, 
                   covar.vals = as.numeric(input[[paste(str_to_title(ed.both), "_educ", sep = "")]]),
                   file.name = ev.file.op, n.tr = n.tr, tr.names = ev.treat)
          seqUpload(ev.file.op, rdata.dropdir = rdata.dropdir.op)
          load(ev.file.op)
          paste(bdata$orig[nrow(bdata$orig), "Tr"], ".txt", sep = "")
        }else{
          seqblock(query = FALSE, id.vars = "ID", id.vals = 1, covar.vars = "education", 
                   covar.vals = as.numeric(input[[paste(str_to_title(ed.both), "_educ", sep = "")]]), 
                   file.name = ev.file.op, n.tr = n.tr, tr.names = ev.treat)
          seqUpload(ev.file.op, rdata.dropdir = rdata.dropdir.op)
          load(ev.file.op)
          paste(bdata$orig[nrow(bdata$orig), "Tr"], ".txt", sep = "")
        }
        
      }else{
        
        if(drop_exists(path = paste(rdata.dropdir.an, ev.file.an, sep = "/"))){
          seqDownload(ev.file.an, rdata.dropdir = rdata.dropdir.an)
          load(ev.file.an)
          seqblock(query = FALSE, object = ev.file.an, id.vals = bdata$orig[nrow(bdata$orig), "ID"]+1, 
                   covar.vals = as.numeric(input[[paste(str_to_title(ed.both), "_educ", sep = "")]]),
                   file.name = ev.file.an, n.tr = n.tr, tr.names = ev.treat)
          seqUpload(ev.file.an, rdata.dropdir = rdata.dropdir.an)
          load(ev.file.an)
          paste(bdata$orig[nrow(bdata$orig), "Tr"], ".txt", sep = "")
        }else{
          seqblock(query = FALSE, id.vars = "ID", id.vals = 1, covar.vars = "education", 
                   covar.vals = as.numeric(input[[paste(str_to_title(ed.both), "_educ", sep = "")]]), 
                   file.name = ev.file.an, n.tr = n.tr, tr.names = ev.treat)
          seqUpload(ev.file.an, rdata.dropdir = rdata.dropdir.an)
          load(ev.file.an)
          paste(bdata$orig[nrow(bdata$orig), "Tr"], ".txt", sep = "")
        }
      }
    })
  })
  
  
  
  
  
  
  
  ############# Section B: Skip Logic #############
  
  # The same technique as above, where I saved the blocked treatment group name.
  # When respondents hit "Continue" on the party ID page, this code sets in and loads the party ID follow-up question .txt file.
  # I created three party ID follow-up .txt question files: One for Dem, one for Rep, one for Ind/Something Else. The last one simply asks whether they feel nearer to Dems, Reps or Neither.
  # The order in the survey is "Dem, Rep, Ind, Something Else", so 1, 2, 3, 4. 
  # The code loads the .txt for Dem if they clicked Dem (= 1), the .txt for Rep if they clicked Rep (= 2), and the .txt for Ind/Something Else if they clicked any of the other two
  
  pid.foll.sample <- eventReactive(input[[paste(str_to_title(pid), "_next", sep = "")]], {
    if(as.numeric(input[[paste(str_to_title(pid), "_pid", sep = "")]]) == 1){
      "pid.foll.dem.txt"
    }else if(as.numeric(input[[paste(str_to_title(pid), "_pid", sep = "")]]) == 2){
      "pid.foll.rep.txt"
    }else{
      "pid.foll.ind.else.txt"
    }    
  })
  
  
  # The same as above, just for ideology
  ideol.foll.sample <- eventReactive(input[[paste(str_to_title(ideol), "_next", sep = "")]], {
    if(as.numeric(input[[paste(str_to_title(ideol), "_ideol", sep = "")]]) == 1){
      "ideol.foll.lib.txt"
    }else if(as.numeric(input[[paste(str_to_title(ideol), "_ideol", sep = "")]]) == 2){
      "ideol.foll.cons.txt"
    }else{
      "ideol.foll.nei.txt"
    }    
  })
  
  
  # Integrating the Mturk creation of a unique ID for each respondent would have taken up time, and it's easier here.
  # When respondents hit "Continue" on the page with the attention check for the second issue, unique.id saves a random 9-digit code with the word "survey" in front.
  # That code is displayed to users at the end so they can enter it into Mturk. I also save it for myself so I can identify users who failed one of the checks
  
  if(platform == "Mturk"){
    unique.id <- eventReactive(input[[paste(str_to_title(issue2.check), "_next", sep = "")]], {
      paste("survey", sample(100:999, 1), sample(100:999, 1), sample(100:999, 1), sep = "-")
    })
  }
  
  
  
  
  
  
  
  ############# Section C: Page Layouts #############
  
  PageLayouts <- reactive({
    
    if (CurrentValues$page == ins) {
      return(
        # "assign" creates the object instructions .list that reads in the instructions .txt and creates a global ID.
        # The rest creates the html logic of the instructions page
        createPage(pageList = assign(paste(ins, ".list", sep = ""), 
                                     createPageList(fileName = paste("questions/", ins, ".txt" , sep = ""),
                                                    globId = str_to_title(ins), defaulttxt = FALSE)),
                   pageNumber = CurrentValues[[paste(str_to_title(ins), ".num", sep = "")]],
                   globId = str_to_title(ins), ctrlVals = CurrentValues)
      )}
    
    if (CurrentValues$page == throw) {
      return(
        # The same as above, just for the throw-away page
        createPage(pageList = assign(paste(throw, ".list", sep = ""), 
                                     createPageList(fileName = paste("questions/", throw, ".txt" , sep = ""),
                                                    globId = str_to_title(throw), defaulttxt = FALSE)),
                   pageNumber = CurrentValues[[paste(str_to_title(throw), ".num", sep = "")]],
                   globId = str_to_title(throw), ctrlVals = CurrentValues)
      )}
    
    if (CurrentValues$page == mor.si.intro) {
      return(
        createPage(pageList = assign(paste(mor.si.intro, ".list", sep = ""), 
                                     createPageList(fileName = paste("questions/", mor.si.intro, ".txt" , sep = ""),
                                                    globId = str_to_title(mor.si.intro), defaulttxt = FALSE)),
                   pageNumber = CurrentValues[[paste(str_to_title(mor.si.intro), ".num", sep = "")]],
                   globId = str_to_title(mor.si.intro), ctrlVals = CurrentValues)
      )}
    
    if (CurrentValues$page == measure1) {
      return(
        createPage(pageList = assign(paste(measure1, ".list", sep = ""), 
                                     createPageList(fileName = paste0("questions/", mor.si.first$a),
                                                    globId = str_to_title(measure1), defaulttxt = FALSE)),
                   pageNumber = CurrentValues[[paste(str_to_title(measure1), ".num", sep = "")]],
                   globId = str_to_title(measure1), ctrlVals = CurrentValues)
      )}
    
    
    if (CurrentValues$page == measure2) {
      return(
        createPage(pageList = assign(paste(measure2, ".list", sep = ""), 
                                     createPageList(fileName = paste0("questions/", mor.si.second$a),
                                                    globId = str_to_title(measure2), defaulttxt = FALSE)),
                   pageNumber = CurrentValues[[paste(str_to_title(measure2), ".num", sep = "")]],
                   globId = str_to_title(measure2), ctrlVals = CurrentValues)
      )}
    
    
    if (CurrentValues$page == dem.intro) {
      return(
        createPage(pageList = assign(paste(dem.intro, ".list", sep = ""), 
                                     createPageList(fileName = paste("questions/", dem.intro, ".txt" , sep = ""),
                                                    globId = str_to_title(dem.intro), defaulttxt = FALSE)),
                   pageNumber = CurrentValues[[paste(str_to_title(dem.intro), ".num", sep = "")]],
                   globId = str_to_title(dem.intro), ctrlVals = CurrentValues)
      )}
    
    if (CurrentValues$page == dem1) {
      return(
        createPage(pageList = assign(paste(dem1, ".list", sep = ""), 
                                     createPageList(fileName = paste("questions/", dem1, ".txt" , sep = ""),
                                                    globId = str_to_title(dem1), defaulttxt = FALSE)),
                   pageNumber = CurrentValues[[paste(str_to_title(dem1), ".num", sep = "")]],
                   globId = str_to_title(dem1), ctrlVals = CurrentValues)
      )}
    
    if (CurrentValues$page == dem2) {
      return(
        createPage(pageList = assign(paste(dem2, ".list", sep = ""), 
                                     createPageList(fileName = paste("questions/", dem2, ".txt" , sep = ""),
                                                    globId = str_to_title(dem2), defaulttxt = FALSE)),
                   pageNumber = CurrentValues[[paste(str_to_title(dem2), ".num", sep = "")]],
                   globId = str_to_title(dem2), ctrlVals = CurrentValues)
      )}
    
    if (CurrentValues$page == pid) {
      return(
        createPage(pageList = assign(paste(pid, ".list", sep = ""), 
                                     createPageList(fileName = paste("questions/", pid, ".txt" , sep = ""),
                                                    globId = str_to_title(pid), defaulttxt = FALSE)),
                   pageNumber = CurrentValues[[paste(str_to_title(pid), ".num", sep = "")]],
                   globId = str_to_title(pid), ctrlVals = CurrentValues)
      )}
    
    if (CurrentValues$page == pid.foll.up) {
      return(
        createPage(pageList = assign(paste(pid.foll.up, ".list", sep = ""), 
                                     createPageList(fileName = paste("questions/", pid.foll.sample(), sep = ""),
                                                    globId = str_to_title(pid.foll.up), defaulttxt = FALSE)), 
                   pageNumber = CurrentValues[[paste(str_to_title(pid.foll.up), ".num", sep = "")]],
                   globId = str_to_title(pid.foll.up), ctrlVals = CurrentValues)
      )}
    
    if (CurrentValues$page == ideol) {
      return(
        createPage(pageList = assign(paste(ideol, ".list", sep = ""), 
                                     createPageList(fileName = paste("questions/", ideol, ".txt" , sep = ""),
                                                    globId = str_to_title(ideol), defaulttxt = FALSE)),
                   pageNumber = CurrentValues[[paste(str_to_title(ideol), ".num", sep = "")]],
                   globId = str_to_title(ideol), ctrlVals = CurrentValues)
      )}
    
    if (CurrentValues$page == ideol.foll.up) {
      return(
        createPage(pageList = assign(paste(ideol.foll.up, ".list", sep = ""), 
                                     createPageList(fileName = paste("questions/", ideol.foll.sample(), sep = ""),
                                                    globId = str_to_title(ideol.foll.up), defaulttxt = FALSE)), 
                   pageNumber = CurrentValues[[paste(str_to_title(ideol.foll.up), ".num", sep = "")]],
                   globId = str_to_title(ideol.foll.up), ctrlVals = CurrentValues)
      )}
    
    if (CurrentValues$page == ed.both) {
      return(
        createPage(pageList = assign(paste(ed.both, ".list", sep = ""), 
                                     createPageList(fileName = paste("questions/", ed.sample(), sep = ""),
                                                    globId = str_to_title(ed.both), defaulttxt = FALSE)), 
                   pageNumber = CurrentValues[[paste(str_to_title(ed.both), ".num", sep = "")]],
                   globId = str_to_title(ed.both), ctrlVals = CurrentValues)
      )}
    
    if (CurrentValues$page == issues.intro) {
      return(
        createPage(pageList = assign(paste(issues.intro, ".list", sep = ""), 
                                     createPageList(fileName = paste("questions/", issues.intro, ".txt" , sep = ""),
                                                    globId = str_to_title(issues.intro), defaulttxt = FALSE)),
                   pageNumber = CurrentValues[[paste(str_to_title(issues.intro), ".num", sep = "")]],
                   globId = str_to_title(issues.intro), ctrlVals = CurrentValues)
      )}
    
    if (CurrentValues$page == issue1) {
      return(
        createPage(pageList = assign(paste(issue1, ".list", sep = ""), 
                                     createPageList(fileName = paste("questions/treatment/", hc.sample(), sep = ""),
                                                    globId = str_to_title(issue1), defaulttxt = FALSE)), 
                   pageNumber = CurrentValues[[paste(str_to_title(issue1), ".num", sep = "")]],
                   globId = str_to_title(issue1), ctrlVals = CurrentValues)
      )}
    
    if (CurrentValues$page == issue1.check) {
      return(
        createPage(pageList = assign(paste(issue1.check, ".list", sep = ""), 
                                     createPageList(fileName = paste("questions/", issue1.check, ".txt" , sep = ""),
                                                    globId = str_to_title(issue1.check), defaulttxt = FALSE)),
                   pageNumber = CurrentValues[[paste(str_to_title(issue1.check), ".num", sep = "")]],
                   globId = str_to_title(issue1.check), ctrlVals = CurrentValues)
      )}
    
    if (CurrentValues$page == issue2) {
      return(
        createPage(pageList = assign(paste(issue2, ".list", sep = ""), 
                                     createPageList(fileName = paste("questions/treatment/", ev.sample(), sep = ""),
                                                    globId = str_to_title(issue2), defaulttxt = FALSE)), 
                   pageNumber = CurrentValues[[paste(str_to_title(issue2), ".num", sep = "")]],
                   globId = str_to_title(issue2), ctrlVals = CurrentValues)
      )}
    
    if (CurrentValues$page == issue2.check) {
      return(
        createPage(pageList = assign(paste(issue2.check, ".list", sep = ""), 
                                     createPageList(fileName = paste("questions/", issue2.check, ".txt" , sep = ""),
                                                    globId = str_to_title(issue2.check), defaulttxt = FALSE)),
                   pageNumber = CurrentValues[[paste(str_to_title(issue2.check), ".num", sep = "")]],
                   globId = str_to_title(issue2.check), ctrlVals = CurrentValues)
      )}
    
    if (CurrentValues$page == com) {
      return(
        createPage(pageList = assign(paste(com, ".list", sep = ""), 
                                     createPageList(fileName = paste("questions/", com, ".txt" , sep = ""),
                                                    globId = str_to_title(com), defaulttxt = FALSE)),
                   pageNumber = CurrentValues[[paste(str_to_title(com), ".num", sep = "")]],
                   globId = str_to_title(com), ctrlVals = CurrentValues))
    }
    
    # If the survey is fielded on Mturk, the code page needs to display the unique ID code created above. This is done here
    if (CurrentValues$page == co) {
      if(platform == "Mturk"){
        return(
          createPage(pageList = assign(paste(co, ".list", sep = ""),
                                       changePageVariable(pageList = assign(paste(co, ".list", sep = ""),
                                                                            createPageList(fileName = paste("questions/", co, ".txt" , sep = ""),
                                                                                           globId = str_to_title(co), defaulttxt = FALSE)),
                                                          variable = "text",
                                                          oldLabel = "completion.code",
                                                          newLabel = unique.id())),
                     pageNumber = CurrentValues[[paste(str_to_title(co), ".num", sep = "")]],
                     globId = str_to_title(co), ctrlVals = CurrentValues,
                     continueButton = FALSE))
      }
      if(platform == "Lucid"){
        return(
          createPage(pageList = assign(paste(co, ".list", sep = ""), 
                                       createPageList(fileName = paste("questions/", co, ".txt" , sep = ""),
                                                      globId = str_to_title(co), defaulttxt = FALSE)),
                     pageNumber = CurrentValues[[paste(str_to_title(co), ".num", sep = "")]],
                     globId = str_to_title(co), ctrlVals = CurrentValues))
      }
    }
    
  })
  
  
  
  ############# Section D: Page Navigation Buttons #############
  
  # All "assign"s are simply copied from above
  
  observeEvent(input[[paste(str_to_title(ins), "_next", sep = "")]],{
    nextPage(pageId = ins, ctrlVals = CurrentValues, 
             nextPageId = throw, pageList = assign(paste(ins, ".list", sep = ""), 
                                                   createPageList(fileName = paste("questions/", ins, ".txt" , sep = ""),
                                                                  globId = str_to_title(ins), defaulttxt = FALSE)), 
             globId = str_to_title(ins))
  })
  
  
  observeEvent(input[[paste(str_to_title(throw), "_next", sep = "")]],{
    nextPage(pageId = throw, ctrlVals = CurrentValues, 
             nextPageId = mor.si.intro, pageList = assign(paste(throw, ".list", sep = ""), 
                                                          createPageList(fileName = paste("questions/", throw, ".txt" , sep = ""),
                                                                         globId = str_to_title(throw), defaulttxt = FALSE)), 
             globId = str_to_title(throw))
  })
  
  
  observeEvent(input[[paste(str_to_title(mor.si.intro), "_next", sep = "")]],{
    nextPage(pageId = mor.si.intro, ctrlVals = CurrentValues, 
             nextPageId = measure1, pageList = assign(paste(mor.si.intro, ".list", sep = ""), 
                                                      createPageList(fileName = paste("questions/", mor.si.intro, ".txt" , sep = ""),
                                                                     globId = str_to_title(mor.si.intro), defaulttxt = FALSE)), 
             globId = str_to_title(mor.si.intro))
  })
  
  
  observeEvent(input[[paste(str_to_title(measure1), "_next", sep = "")]],{
    nextPage(pageId = measure1, ctrlVals = CurrentValues,
             nextPageId = measure2, pageList = assign(paste(measure1, ".list", sep = ""), 
                                                      createPageList(fileName = paste0("questions/", mor.si.first$a),
                                                                     globId = str_to_title(measure1), defaulttxt = FALSE)),
             globId = str_to_title(measure1))
  })
  
  
  observeEvent(input[[paste(str_to_title(measure2), "_next", sep = "")]],{
    nextPage(pageId = measure2, ctrlVals = CurrentValues,
             nextPageId = dem.intro, pageList = assign(paste(measure2, ".list", sep = ""), 
                                                       createPageList(fileName = paste0("questions/", mor.si.second$a),
                                                                      globId = str_to_title(measure2), defaulttxt = FALSE)),
             globId = str_to_title(measure2))
  })
  
  
  observeEvent(input[[paste(str_to_title(dem.intro), "_next", sep = "")]],{
    nextPage(pageId = dem.intro, ctrlVals = CurrentValues, 
             nextPageId = dem1, pageList = assign(paste(dem.intro, ".list", sep = ""), 
                                                  createPageList(fileName = paste("questions/", dem.intro, ".txt" , sep = ""),
                                                                 globId = str_to_title(dem.intro), defaulttxt = FALSE)), 
             globId = str_to_title(dem.intro))
  })
  
  
  observeEvent(input[[paste(str_to_title(dem1), "_next", sep = "")]],{
    nextPage(pageId = dem1, ctrlVals = CurrentValues, 
             nextPageId = dem2, pageList = assign(paste(dem1, ".list", sep = ""), 
                                                  createPageList(fileName = paste("questions/", dem1, ".txt" , sep = ""),
                                                                 globId = str_to_title(dem1), defaulttxt = FALSE)), 
             globId = str_to_title(dem1))
  })
  
  
  observeEvent(input[[paste(str_to_title(dem2), "_next", sep = "")]],{
    nextPage(pageId = dem2, ctrlVals = CurrentValues, 
             nextPageId = pid, pageList = assign(paste(dem2, ".list", sep = ""), 
                                                 createPageList(fileName = paste("questions/", dem2, ".txt" , sep = ""),
                                                                globId = str_to_title(dem2), defaulttxt = FALSE)), 
             globId = str_to_title(dem2))
  })
  
  
  # If respondents with Lucid fail the attention check on the second demographics page (i.e. choose any answer but 2, the Lucid survey terminates.
  # When I run this code locally and the RID thus is "no.query.string" (see above), they get redirected to Google. 
  # Within Lucid's system, they are informed that they won't be able to participate further.
  # There is no code for this with Mturk because there was no way to terminate the survey for respondents who failed the check. I had to do that manually afterwards
  
  observeEvent(input[[paste(str_to_title(dem2), "_next", sep = "")]],{
    
    if(platform == "Lucid"){
      if(input[[paste(str_to_title(dem2), "_att", sep = "")]] != 2){
        if(RID() == "no.query.string"){
          js$browseURL(paste("http://www.google.com/"))
        }else{
          js$browseURL(paste("https://samplicio.us/s/ClientCallBack.aspx?RIS=20&RID=", RID(), sep = ""))
        }
      }
    }
  })
  
  
  observeEvent(input[[paste(str_to_title(pid), "_next", sep = "")]],{
    nextPage(pageId = pid, ctrlVals = CurrentValues, 
             nextPageId = pid.foll.up, pageList = assign(paste(pid, ".list", sep = ""), 
                                                         createPageList(fileName = paste("questions/", pid, ".txt" , sep = ""),
                                                                        globId = str_to_title(pid), defaulttxt = FALSE)), 
             globId = str_to_title(pid))
  })
  
  
  observeEvent(input[[paste(str_to_title(pid.foll.up), "_next", sep = "")]],{
    nextPage(pageId = pid.foll.up, ctrlVals = CurrentValues,
             nextPageId = ideol, pageList = assign(paste(pid.foll.up, ".list", sep = ""), 
                                                   createPageList(fileName = paste("questions/", pid.foll.sample(), sep = ""),
                                                                  globId = str_to_title(pid.foll.up), defaulttxt = FALSE)),
             globId = str_to_title(pid.foll.up))
  })
  
  
  observeEvent(input[[paste(str_to_title(ideol), "_next", sep = "")]],{
    nextPage(pageId = ideol, ctrlVals = CurrentValues, 
             nextPageId = ideol.foll.up, pageList = assign(paste(ideol, ".list", sep = ""), 
                                                           createPageList(fileName = paste("questions/", ideol, ".txt" , sep = ""),
                                                                          globId = str_to_title(ideol), defaulttxt = FALSE)), 
             globId = str_to_title(ideol))
  })
  
  
  observeEvent(input[[paste(str_to_title(ideol.foll.up), "_next", sep = "")]],{
    nextPage(pageId = ideol.foll.up, ctrlVals = CurrentValues,
             nextPageId = ed.both, pageList = assign(paste(ideol.foll.up, ".list", sep = ""), 
                                                     createPageList(fileName = paste("questions/", ideol.foll.sample(), sep = ""),
                                                                    globId = str_to_title(ideol.foll.up), defaulttxt = FALSE)),
             globId = str_to_title(ideol.foll.up))
  })
  
  
  observeEvent(input[[paste(str_to_title(ed.both), "_next", sep = "")]],{
    nextPage(pageId = ed.both, ctrlVals = CurrentValues,
             nextPageId = issues.intro, pageList = assign(paste(ed.both, ".list", sep = ""), 
                                                          createPageList(fileName = paste("questions/", ed.sample(), sep = ""),
                                                                         globId = str_to_title(ed.both), defaulttxt = FALSE)),
             globId = str_to_title(ed.both))
  })
  
  
  observeEvent(input[[paste(str_to_title(issues.intro), "_next", sep = "")]],{
    nextPage(pageId = issues.intro, ctrlVals = CurrentValues, 
             nextPageId = issue1, pageList = assign(paste(issues.intro, ".list", sep = ""), 
                                                    createPageList(fileName = paste("questions/", issues.intro, ".txt" , sep = ""),
                                                                   globId = str_to_title(issues.intro), defaulttxt = FALSE)), 
             globId = str_to_title(issues.intro))
  })
  
  
  observeEvent(input[[paste(str_to_title(issue1), "_next", sep = "")]],{
    nextPage(pageId = issue1, ctrlVals = CurrentValues,
             nextPageId = issue1.check, pageList = assign(paste(issue1, ".list", sep = ""), 
                                                          createPageList(fileName = paste("questions/treatment/", hc.sample(), sep = ""),
                                                                         globId = str_to_title(issue1), defaulttxt = FALSE)),
             globId = str_to_title(issue1))
  })
  
  observeEvent(input[[paste(str_to_title(issue1.check), "_next", sep = "")]],{
    nextPage(pageId = issue1.check, ctrlVals = CurrentValues,
             nextPageId = issue2, pageList = assign(paste(issue1.check, ".list", sep = ""), 
                                                    createPageList(fileName = paste("questions/", issue1.check, ".txt" , sep = ""),
                                                                   globId = str_to_title(issue1.check), defaulttxt = FALSE)),
             globId = str_to_title(issue1.check))
  })
  
  
  observeEvent(input[[paste(str_to_title(issue2), "_next", sep = "")]],{
    nextPage(pageId = issue2, ctrlVals = CurrentValues,
             nextPageId = issue2.check, pageList = assign(paste(issue2, ".list", sep = ""), 
                                                          createPageList(fileName = paste("questions/treatment/", ev.sample(), sep = ""),
                                                                         globId = str_to_title(issue2), defaulttxt = FALSE)),
             globId = str_to_title(issue2))
  })
  
  observeEvent(input[[paste(str_to_title(issue2.check), "_next", sep = "")]],{
    nextPage(pageId = issue2.check, ctrlVals = CurrentValues,
             nextPageId = com, pageList = assign(paste(issue2.check, ".list", sep = ""), 
                                                 createPageList(fileName = paste("questions/", issue2.check, ".txt" , sep = ""),
                                                                globId = str_to_title(issue2.check), defaulttxt = FALSE)),
             globId = str_to_title(issue2.check))
  })
  
  observeEvent(input[[paste(str_to_title(com), "_next", sep = "")]],{
    nextPage(pageId = com, ctrlVals = CurrentValues,
             nextPageId = co, pageList = assign(paste(com, ".list", sep = ""), 
                                                createPageList(fileName = paste("questions/", com, ".txt" , sep = ""),
                                                               globId = str_to_title(com), defaulttxt = FALSE)),
             globId = str_to_title(com))
  })
  
  # After the final page, Lucid respondents need to get redirected to a website containing the RID (see above). 
  # If I run it locally, it redirects to Google. Otherwise it goes to the Lucid RID website
  observeEvent(input[[paste(str_to_title(co), "_next", sep = "")]],{
    
    if(platform == "Lucid"){
      if(RID() == "no.query.string"){
        js$browseURL(paste("http://www.google.com/"))
      }else{
        js$browseURL(paste("https://notch.insights.supply/cb?token=187df571-53eb-46e4-b03b-0425864e5361&RID=", RID(), sep = ""))
      }
    }
  })
  
  
  
  
  
  ############# Section E: Event Control #############
  
  # To make sure answers are selected and respondents don't just hit "Continue".
  # As before, all "assign"s are simply copied from above 
  
  observeEvent(reactiveValuesToList(input),{
    
    onInputEnable(pageId = ins, ctrlVals = CurrentValues,
                  pageList = assign(paste(ins, ".list", sep = ""), 
                                    createPageList(fileName = paste("questions/", ins, ".txt" , sep = ""),
                                                   globId = str_to_title(ins), defaulttxt = FALSE)), 
                  globId = str_to_title(ins),
                  inputList = input)
    
    onInputEnable(pageId = throw, ctrlVals = CurrentValues,
                  pageList = assign(paste(throw, ".list", sep = ""), 
                                    createPageList(fileName = paste("questions/", throw, ".txt" , sep = ""),
                                                   globId = str_to_title(throw), defaulttxt = FALSE)), 
                  globId = str_to_title(throw),
                  inputList = input)
    
    onInputEnable(pageId = mor.si.intro, ctrlVals = CurrentValues,
                  pageList = assign(paste(mor.si.intro, ".list", sep = ""), 
                                    createPageList(fileName = paste("questions/", mor.si.intro, ".txt" , sep = ""),
                                                   globId = str_to_title(mor.si.intro), defaulttxt = FALSE)), 
                  globId = str_to_title(mor.si.intro),
                  inputList = input)
    
    onInputEnable(pageId = measure1, ctrlVals = CurrentValues,
                  pageList = assign(paste(measure1, ".list", sep = ""), 
                                    createPageList(fileName = paste0("questions/", mor.si.first$a),
                                                   globId = str_to_title(measure1), defaulttxt = FALSE)), 
                  globId = str_to_title(measure1),
                  inputList = input)
    
    onInputEnable(pageId = measure2, ctrlVals = CurrentValues,
                  pageList = assign(paste(measure2, ".list", sep = ""), 
                                    createPageList(fileName = paste0("questions/", mor.si.second$a),
                                                   globId = str_to_title(measure2), defaulttxt = FALSE)), 
                  globId = str_to_title(measure2),
                  inputList = input)
    
    onInputEnable(pageId = dem.intro, ctrlVals = CurrentValues,
                  pageList = assign(paste(dem.intro, ".list", sep = ""), 
                                    createPageList(fileName = paste("questions/", dem.intro, ".txt" , sep = ""),
                                                   globId = str_to_title(dem.intro), defaulttxt = FALSE)), 
                  globId = str_to_title(dem.intro),
                  inputList = input)
    
    onInputEnable(pageId = dem1, ctrlVals = CurrentValues,
                  pageList = assign(paste(dem1, ".list", sep = ""), 
                                    createPageList(fileName = paste("questions/", dem1, ".txt" , sep = ""),
                                                   globId = str_to_title(dem1), defaulttxt = FALSE)), 
                  globId = str_to_title(dem1),
                  inputList = input)
    
    onInputEnable(pageId = dem2, ctrlVals = CurrentValues,
                  pageList = assign(paste(dem2, ".list", sep = ""), 
                                    createPageList(fileName = paste("questions/", dem2, ".txt" , sep = ""),
                                                   globId = str_to_title(dem2), defaulttxt = FALSE)), 
                  globId = str_to_title(dem2),
                  inputList = input)
    onInputEnable(pageId = pid, ctrlVals = CurrentValues,
                  pageList = assign(paste(pid, ".list", sep = ""), 
                                    createPageList(fileName = paste("questions/", pid, ".txt" , sep = ""),
                                                   globId = str_to_title(pid), defaulttxt = FALSE)), 
                  globId = str_to_title(pid),
                  inputList = input)
    
    onInputEnable(pageId = pid.foll.up, ctrlVals = CurrentValues,
                  pageList = assign(paste(pid.foll.up, ".list", sep = ""), 
                                    createPageList(fileName = paste("questions/", pid.foll.sample(), sep = ""),
                                                   globId = str_to_title(pid.foll.up), defaulttxt = FALSE)), 
                  globId = str_to_title(pid.foll.up),
                  inputList = input)
    
    onInputEnable(pageId = ideol, ctrlVals = CurrentValues,
                  pageList = assign(paste(ideol, ".list", sep = ""), 
                                    createPageList(fileName = paste("questions/", ideol, ".txt" , sep = ""),
                                                   globId = str_to_title(ideol), defaulttxt = FALSE)), 
                  globId = str_to_title(ideol),
                  inputList = input)
    
    onInputEnable(pageId = ideol.foll.up, ctrlVals = CurrentValues,
                  pageList = assign(paste(ideol.foll.up, ".list", sep = ""), 
                                    createPageList(fileName = paste("questions/", ideol.foll.sample(), sep = ""),
                                                   globId = str_to_title(ideol.foll.up), defaulttxt = FALSE)), 
                  globId = str_to_title(ideol.foll.up),
                  inputList = input)
    
    onInputEnable(pageId = ed.both, ctrlVals = CurrentValues,
                  pageList = assign(paste(ed.both, ".list", sep = ""), 
                                    createPageList(fileName = paste("questions/", ed.sample(), sep = ""),
                                                   globId = str_to_title(ed.both), defaulttxt = FALSE)), 
                  globId = str_to_title(ed.both),
                  inputList = input)
    
    onInputEnable(pageId = issues.intro, ctrlVals = CurrentValues,
                  pageList = assign(paste(issues.intro, ".list", sep = ""), 
                                    createPageList(fileName = paste("questions/", issues.intro, ".txt" , sep = ""),
                                                   globId = str_to_title(issues.intro), defaulttxt = FALSE)), 
                  globId = str_to_title(issues.intro),
                  inputList = input)
    
    onInputEnable(pageId = issue1, ctrlVals = CurrentValues,
                  pageList = assign(paste(issue1, ".list", sep = ""), 
                                    createPageList(fileName = paste("questions/treatment/", hc.sample(), sep = ""),
                                                   globId = str_to_title(issue1), defaulttxt = FALSE)), 
                  globId = str_to_title(issue1),
                  inputList = input)
    
    onInputEnable(pageId = issue1.check, ctrlVals = CurrentValues,
                  pageList = assign(paste(issue1.check, ".list", sep = ""), 
                                    createPageList(fileName = paste("questions/", issue1.check, ".txt" , sep = ""),
                                                   globId = str_to_title(issue1.check), defaulttxt = FALSE)), 
                  globId = str_to_title(issue1.check),
                  inputList = input)
    
    onInputEnable(pageId = issue2, ctrlVals = CurrentValues,
                  pageList = assign(paste(issue2, ".list", sep = ""), 
                                    createPageList(fileName = paste("questions/treatment/", ev.sample(), sep = ""),
                                                   globId = str_to_title(issue2), defaulttxt = FALSE)), 
                  globId = str_to_title(issue2),
                  inputList = input)
    
    onInputEnable(pageId = issue2.check, ctrlVals = CurrentValues,
                  pageList = assign(paste(issue2.check, ".list", sep = ""), 
                                    createPageList(fileName = paste("questions/", issue2.check, ".txt" , sep = ""),
                                                   globId = str_to_title(issue2.check), defaulttxt = FALSE)), 
                  globId = str_to_title(issue2.check),
                  inputList = input)
    
    onInputEnable(pageId = com, ctrlVals = CurrentValues,
                  pageList = assign(paste(com, ".list", sep = ""), 
                                    createPageList(fileName = paste("questions/", com, ".txt" , sep = ""),
                                                   globId = str_to_title(com), defaulttxt = FALSE)), 
                  globId = str_to_title(com),
                  inputList = input)
    
    # The same differentiation for Mturk and Lucid codes as for createPage() above
    
    if(platform == "Mturk"){
      onInputEnable(pageId = co, ctrlVals = CurrentValues,
                    pageList = assign(paste(co, ".list", sep = ""),
                                      changePageVariable(pageList = assign(paste(co, ".list", sep = ""),
                                                                           createPageList(fileName = paste("questions/", co, ".txt" , sep = ""),
                                                                                          globId = str_to_title(co), defaulttxt = FALSE)),
                                                         variable = "text",
                                                         oldLabel = "completion.code",
                                                         newLabel = unique.id())),
                    globId = str_to_title(co),
                    inputList = input)
    }
    if(platform == "Lucid"){
      onInputEnable(pageId = co, ctrlVals = CurrentValues,
                    pageList = assign(paste(co, ".list", sep = ""), 
                                      createPageList(fileName = paste("questions/", co, ".txt" , sep = ""),
                                                     globId = str_to_title(co), defaulttxt = FALSE)), 
                    globId = str_to_title(co),
                    inputList = input)
    }
    
  })
  
  
  
  ############# Section F: Save data #############
  
  # Save the input data on the second-to-last page (before respondent are redirected)
  
  observeEvent(input[[paste(str_to_title(com), "_next", sep = "")]], {(
    
    # Create progress message
    withProgress(message = "Saving data...", value = 0, {
      
      incProgress(.25)
      
      # Create a list to save data.
      # Pre-created empty and then filled in because this was the only way to use paste
      data.list <- list()
      
      # The names in inut[[...]] correspond to the names in the question .txt files
      data.list[["birthyear"]] <- input[[paste(str_to_title(dem1), "_birthyear", sep = "")]]
      data.list[["race"]] <- input[[paste(str_to_title(dem1), "_race", sep = "")]]
      data.list[["gender"]] <- input[[paste(str_to_title(dem1), "_gender", sep = "")]]
      data.list[["att"]] <- input[[paste(str_to_title(dem2), "_att", sep = "")]]
      data.list[["empl"]] <- input[[paste(str_to_title(dem2), "_empl", sep = "")]]
      data.list[["inc"]] <- input[[paste(str_to_title(dem2), "_inc", sep = "")]]
      data.list[["pid"]] <- input[[paste(str_to_title(pid), "_pid", sep = "")]]
      data.list[["pid.follow"]] <- input[[paste(str_to_title(pid.foll.up), "_pid.follow", sep = "")]]
      data.list[["ideol"]] <- input[[paste(str_to_title(ideol), "_ideol", sep = "")]]
      data.list[["ideol.follow"]] <- input[[paste(str_to_title(ideol.foll.up), "_ideol.follow", sep = "")]]
      data.list[["educ"]] <- input[[paste(str_to_title(ed.both), "_educ", sep = "")]]
      
      if(mor.si.first$a == "morals.txt"){
        data.list[["mor.suffer"]] <- input[[paste(str_to_title(measure1), "_mor.suffer", sep = "")]]
        data.list[["mor.care"]] <- input[[paste(str_to_title(measure1), "_mor.care", sep = "")]]
        data.list[["mor.cruel"]] <- input[[paste(str_to_title(measure1), "_mor.cruel", sep = "")]]
        data.list[["mor.comp"]] <- input[[paste(str_to_title(measure1), "_mor.comp", sep = "")]]
        data.list[["mor.anim"]] <- input[[paste(str_to_title(measure1), "_mor.anim", sep = "")]]
        data.list[["mor.kill"]] <- input[[paste(str_to_title(measure1), "_mor.kill", sep = "")]]
      }else{
        data.list[["mor.suffer"]] <- input[[paste(str_to_title(measure2), "_mor.suffer", sep = "")]]
        data.list[["mor.care"]] <- input[[paste(str_to_title(measure2), "_mor.care", sep = "")]]
        data.list[["mor.cruel"]] <- input[[paste(str_to_title(measure2), "_mor.cruel", sep = "")]]
        data.list[["mor.comp"]] <- input[[paste(str_to_title(measure2), "_mor.comp", sep = "")]]
        data.list[["mor.anim"]] <- input[[paste(str_to_title(measure2), "_mor.anim", sep = "")]]
        data.list[["mor.kill"]] <- input[[paste(str_to_title(measure2), "_mor.kill", sep = "")]]
      }
      
      if(mor.si.second$a == "self-interest.txt"){
        data.list[["si.white"]] <- input[[paste(str_to_title(measure2), "_si.white", sep = "")]]
        data.list[["si.care"]] <- input[[paste(str_to_title(measure2), "_si.care", sep = "")]]
        data.list[["si.kids"]] <- input[[paste(str_to_title(measure2), "_si.kids", sep = "")]]
        data.list[["si.kill"]] <- input[[paste(str_to_title(measure2), "_si.kill", sep = "")]]
        data.list[["si.good"]] <- input[[paste(str_to_title(measure2), "_si.good", sep = "")]]
        data.list[["si.help"]] <- input[[paste(str_to_title(measure2), "_si.help", sep = "")]]
      }else{
        data.list[["si.white"]] <- input[[paste(str_to_title(measure1), "_si.white", sep = "")]]
        data.list[["si.care"]] <- input[[paste(str_to_title(measure1), "_si.care", sep = "")]]
        data.list[["si.kids"]] <- input[[paste(str_to_title(measure1), "_si.kids", sep = "")]]
        data.list[["si.kill"]] <- input[[paste(str_to_title(measure1), "_si.kill", sep = "")]]
        data.list[["si.good"]] <- input[[paste(str_to_title(measure1), "_si.good", sep = "")]]
        data.list[["si.help"]] <- input[[paste(str_to_title(measure1), "_si.help", sep = "")]]
      }
      
      data.list[[paste(issue1, ".group", sep = "")]] <- tools::file_path_sans_ext(hc.sample()) %>% str_remove(., "hc.")
      data.list[[paste0(issue1, ".likert")]] <- input[[paste(str_to_title(issue1), "_treat", sep = "")]]
      data.list[[paste0(issue1, ".slider")]] <- input[[paste(str_to_title(issue1), "_treat.slide", sep = "")]]
      data.list[[paste(issue1.check)]] <- input[[paste(str_to_title(issue1.check), "_check", sep = "")]]
      data.list[[paste(issue2, ".group", sep = "")]] <- tools::file_path_sans_ext(ev.sample()) %>% str_remove(., "ev.")
      data.list[[paste0(issue2, ".likert")]] <- input[[paste(str_to_title(issue2), "_treat", sep = "")]]
      data.list[[paste0(issue2, ".slider")]] <- input[[paste(str_to_title(issue2), "_treat.slide", sep = "")]]
      data.list[[paste(issue2.check)]] <- input[[paste(str_to_title(issue2.check), "_check", sep = "")]]
      
      data.list[["online.length"]] <- input[[paste(str_to_title(throw), "_online.length", sep = "")]]
      data.list[["study.choice"]] <- input[[paste(str_to_title(throw), "_study.choice", sep = "")]]
      
      if(platform == "Mturk"){
        data.list[["unique.id"]] <- unique.id()
      }
      if(platform == "Lucid"){
        data.list[["RID"]] <- RID()
      }
      
      # Turn the data into a data frame.
      # I'm doing online.why and com here just because it's easier.
      # The code writes the data frame into a .csv locally and then uploads it to the corresponding OP/AN Dropbox directory
      
      df.dat <- do.call(cbind.data.frame, data.list)
      df.dat$online.why <- input[[paste(str_to_title(throw), "_online.why", sep = "")]]
      df.dat$com <- input[[paste(str_to_title(com), "_com", sep = "")]]
      fileName <- sprintf("%s_%s.csv", as.integer(Sys.time()), digest::digest(df.dat))
      localPath <- file.path(tempdir(), fileName)
      write.csv(df.dat, localPath, row.names = FALSE, quote = TRUE)
      
      if(ed.sample() == "education.op.txt"){
        drop_upload(localPath, path = csv.dropdir.op)
      }else{
        drop_upload(localPath, path = csv.dropdir.an)
      }        
      
    })
  )})
  
}


#-------------------------------------------- Create the app  --------------------------------------------#

shinyApp(ui = ui, server = server)


