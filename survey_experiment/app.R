# From blockTools documentation

## Assign first unit (assume a 25 year old member of the Republican Party) to a treatment group. ## Save the results in file "sdata.RData":
## seqblock(query = FALSE, id.vars = "ID", id.vals = 001, exact.vars = "party", exact.vals = "Republican", covar.vars = "age", covar.vals = 25, file.name = "sdata.RData")
## Assign next unit (age 30, Democratic Party):
## seqblock(query = FALSE, object = "sdata.RData", id.vals = 002, exact.vals = "Democrat", covar.vars = "age", covar.vals = 30, file.name = "sdata.RData")



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

# Dropbox directory to save data
csv.dropdir.op <- "alldata.op" 
csv.dropdir.an <- "alldata.an"
rdata.dropdir.op <- "seqblock.op"
rdata.dropdir.an <- "seqblock.an"
drop_auth(rdstoken = "droptoken.rds")

# Function that saves each response as a .csv file to the AU Dropbox/alldata folders
# Defined here; executed in the app
saveData <- function(data, csv.dropdir) {                 
  data <- t(data)
  # Create a unique file name
  fileName <- sprintf("%s_%s.csv", as.integer(Sys.time()), digest::digest(data))
  # Write the data to a temporary file locally
  localPath <- file.path(tempdir(), fileName)
  write.csv(data, localPath, row.names = FALSE, quote = TRUE)
  # Upload the file to Dropbox/alldata.op
  drop_upload(localPath, path = csv.dropdir)
}

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

# I tried saving and loading the files from a local folder (rather than the wd), but the syntax for drop_download is weird, so it's not worth it

# Treatment question and party ID follow-up names (they are needed for idsVec below)
issue1 <- "hc"
issue2 <- "ev"
pid.foll.up <- "pid.follow.up"
ideol.foll.up <- "ideol.follow.up"
ed.both <- "ed.both"
measure1 <- "measure1"
measure2 <- "measure2"

tools::file_path_sans_ext(list.files("questions", pattern = "*check.txt"))
# Load the non-treatment question files, pull their names, save short versions for later
temp <- tools::file_path_sans_ext(list.files("questions", pattern = "*.txt"))
co <- temp[1]  # these need to be in alphabetical order, otherwise they are mislabelled
dem <- temp[2]
ideol <- temp[10]
ins <- temp[11]
# mor <- temp[12]
pid <- temp[16]
#si <- temp[17]
issue1.check <- paste0(issue1, ".check")
issue2.check <- paste0(issue2, ".check")



# All these short versions are used in the code later with paste, assign etc. That way I only have to adjust things once at the beginning
# The only exception are the samples, because otherwise I would keep resampling and messing things up
# I deleted several lines to create page lists from the original here
# They show up later several times with "assign"
# There is no point in keeping them up here because I needed to copy-paste the "assign"s anyway in order to keep using paste

# Vector with page ids used to later access objects
idsVec <- c(str_to_title(ins), str_to_title(ed.both), str_to_title(issue1), str_to_title(issue2), 
            str_to_title(co), str_to_title(dem), str_to_title(ideol), str_to_title(pid),
            str_to_title(pid.foll.up), str_to_title(ideol.foll.up),
            # str_to_title(mor), 
            # str_to_title(si),
            str_to_title(measure1), str_to_title(measure2),
            str_to_title(issue1.check), str_to_title(issue2.check))

# Specifications to run seqblock(), so that I don't have to retype them
n.tr <- 5
hc.treat <- tools::file_path_sans_ext(list.files("questions/treatment", pattern="^.*hc.*.txt"))
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

  # For Shinyjs functions
  useShinyjs(),

  # Include appropriate css and js scripts
  includeScriptFiles(),
  
  # Needed for the JS browseURL function defined above
  extendShinyjs(text = js_code, functions = 'browseURL')

)


#-------------------------------------------- Define server function  --------------------------------------------#


##### Define server function #####
server <- function(input, output, session) {

  
  # Pulling in Lucid RID (i.e. the query string)
  # Pulls in the query string from the current URL as a list
  # Set up so that I can deploy/run it and Lucid can test it with the same code
  # Outside of Lucid's system, there won't be a query string, which means parseQueryString() returns ""
  # This means the query.string list has length 0. If that's the case, RID will be stored as "no.query.string"
  # If there is a query.string, which is the case when Lucid tests it, RID is stored as query.string[[1]], which is the query string as a character
  RID <- reactive({
    query.string <- parseQueryString(session$clientData$url_search)
    if(length(query.string) == 0){
      "no.query.string"
    }else{
      query.string[[1]]
    }
  })
  
  
  output$MainAction <- renderUI({
    PageLayouts()

  })


  # CurrentValues controls page setting such as which page to display
  CurrentValues <- createCtrlList(firstPage = ins, # id of the first page
                                  globIds = idsVec,           # ids of pages for createPage
                                  complCode = TRUE,           # create a completion code
                                  complName = "survey")    # first element of completion code


  
############# Section A: Blocking Code #############
  
    # observeEvent() is triggered based on event. Anything in here cannot be used in later functions
    # The 'event' here is when users hit "Continue" on the education page
    # If the hc .RData file exists in seqblock/ on Dropbox, the current user is not the first respondent. The function then downloads the file, blocks on the existing data and the current user, then uploads the new .RData file
    # If the hc .RData file does not exist in seqblock/ on Dropbox, the current user is the first respondent. The function then blocks on only the current user, then uploads the resulting .RData file
    # Except for the first respondent to answer the survey, the code always reads in the existing .RData file with pre-assigned respondents
    # input[[paste(str_to_title(ed), "_next", sep = "")]] is code for the "Continue" button
    # input[[paste(str\_to\_title(ed), "\_educ", sep = "")]] pulls in the user-selected education category. I have to put as.numeric() around it because it's pulled as a factor, and factors don't work with covar.vals
    # id.vals creates the user id. It takes the last current row of the .RData file and adds 1
    # bdata is simply the name of the object when the hc .RData file is loaded. It's something in Ryan's package code. bdata$orig is the data frame with the IDs, blocked education categories, and assigned treatment groups
  
  
    mor.si.first <- reactiveValues()
    mor.si.first$a <- sample(c("morals.txt", "self-interest.txt"), 1)
    mor.si.second <- reactiveValues()
    mor.si.second$a <- ifelse(isolate(mor.si.first$a) == "morals.txt", 
                              paste0("self-interest.txt"), 
                              paste0("morals.txt"))

    
    ed.sample <- eventReactive(input[[paste(str_to_title(ideol.foll.up), "_next", sep = "")]], {
      
            sample(c("education.op.txt", "education.an.txt"), 1)

    })


    hc.sample <- eventReactive(input[[paste(str_to_title(ed.both), "_next", sep = "")]], {
      
      withProgress(message = "", value = 0, {
        incProgress(.25)
        
        if(ed.sample() == "education.op.txt"){
          
          if(drop_exists(path = paste(rdata.dropdir.op, hc.file.op, sep = "/"))){
            seqDownload(hc.file.op, rdata.dropdir = rdata.dropdir.op)
            load(hc.file.op)
            seqblock(query = FALSE, object = hc.file.op, id.vals = bdata$orig[nrow(bdata$orig), "ID"]+1, 
                     covar.vals = as.numeric(input[[paste(str_to_title(ed.both), "_educ", sep = "")]]),
                     # exact.vals = input[[paste(str_to_title(pid), "_pid", sep = "")]],
                     file.name = hc.file.op, n.tr = n.tr, tr.names = hc.treat)
            seqUpload(hc.file.op, rdata.dropdir = rdata.dropdir.op)
            load(hc.file.op)
            paste(bdata$orig[nrow(bdata$orig), "Tr"], ".txt", sep = "")
          }else{
            seqblock(query = FALSE, id.vars = "ID", id.vals = 1, covar.vars = "education", 
                     covar.vals = as.numeric(input[[paste(str_to_title(ed.both), "_educ", sep = "")]]),
                     # exact.vars = "pid", exact.vals = input[[paste(str_to_title(pid), "_pid", sep = "")]],
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
                     # exact.vals = input[[paste(str_to_title(pid), "_pid", sep = "")]],
                     file.name = hc.file.an, n.tr = n.tr, tr.names = hc.treat)
            seqUpload(hc.file.an, rdata.dropdir = rdata.dropdir.an)
            load(hc.file.an)
            paste(bdata$orig[nrow(bdata$orig), "Tr"], ".txt", sep = "")
          }else{
            seqblock(query = FALSE, id.vars = "ID", id.vals = 1, covar.vars = "education", 
                     covar.vals = as.numeric(input[[paste(str_to_title(ed.both), "_educ", sep = "")]]),
                     # exact.vars = "pid", exact.vals = input[[paste(str_to_title(pid), "_pid", sep = "")]],
                     file.name = hc.file.an, n.tr = n.tr, tr.names = hc.treat)
            seqUpload(hc.file.an, rdata.dropdir = rdata.dropdir.an)
            load(hc.file.an)
            paste(bdata$orig[nrow(bdata$orig), "Tr"], ".txt", sep = "")
          }
        }
      })
    })
  
  
    # eventReactive() creates a reactive object that changes based on the event. This object can be used in later functions
    # When users hit "Continue" on the education page, it downloads the hc .RData file, extracts the assigned treatment group for the current user, and saves it for later use to display the correct treatment page
    # I have to download the hc .RData file again because it can't be used outside of observeEvent() above (and I don't know if they can be combined)
    # paste(bdata$orig[nrow(bdata$orig), "Tr"], ".txt", sep = "") extracts the assigned treatment group, adds .txt, and saves that string. This is to identify and display the correct treatment page below

    
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
                     # exact.vals = input[[paste(str_to_title(pid), "_pid", sep = "")]],
                     file.name = ev.file.op, n.tr = n.tr, tr.names = ev.treat)
            seqUpload(ev.file.op, rdata.dropdir = rdata.dropdir.op)
            load(ev.file.op)
            paste(bdata$orig[nrow(bdata$orig), "Tr"], ".txt", sep = "")
          }else{
            seqblock(query = FALSE, id.vars = "ID", id.vals = 1, covar.vars = "education", 
                     covar.vals = as.numeric(input[[paste(str_to_title(ed.both), "_educ", sep = "")]]), 
                     # exact.vars = "pid", exact.vals = input[[paste(str_to_title(pid), "_pid", sep = "")]],
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
                     # exact.vals = input[[paste(str_to_title(pid), "_pid", sep = "")]],
                     file.name = ev.file.an, n.tr = n.tr, tr.names = ev.treat)
            seqUpload(ev.file.an, rdata.dropdir = rdata.dropdir.an)
            load(ev.file.an)
            paste(bdata$orig[nrow(bdata$orig), "Tr"], ".txt", sep = "")
          }else{
            seqblock(query = FALSE, id.vars = "ID", id.vals = 1, covar.vars = "education", 
                     covar.vals = as.numeric(input[[paste(str_to_title(ed.both), "_educ", sep = "")]]), 
                     # exact.vars = "pid", exact.vals = input[[paste(str_to_title(pid), "_pid", sep = "")]],
                     file.name = ev.file.an, n.tr = n.tr, tr.names = ev.treat)
            seqUpload(ev.file.an, rdata.dropdir = rdata.dropdir.an)
            load(ev.file.an)
            paste(bdata$orig[nrow(bdata$orig), "Tr"], ".txt", sep = "")
          }
        }
      })
    })
    
    
    
    
    
    
    
############# Section B: Skip Logic #############
    
    # The same technique as above, where I saved the blocked treatment group name
    # When users hit "Continue" on the last page of the demographics page, this code sets in and loads a new question .txt file
    # I created three party ID follow-up .txt question files: One for Dem, one for Rep, one for Ind/Something Else. The last one simply asks whether they feel nearer to the Dems, Reps or Neither
    # The order in the survey is "Dem, Rep, Ind, Something Else", so 1, 2, 3, 4. The code loads the .txt for Dem if they clicked Dem (= 1), the .txt for Rep if they clicked Rep (= 2), and the .txt for Ind/Something Else if they clicked any of the other two
    pid.foll.sample <- eventReactive(input[[paste(str_to_title(pid), "_next", sep = "")]], {
          
            if(as.numeric(input[[paste(str_to_title(pid), "_pid", sep = "")]]) == 1){
              
              "pid.foll.dem.txt"
              
              }else if(as.numeric(input[[paste(str_to_title(pid), "_pid", sep = "")]]) == 2){
                
                "pid.foll.rep.txt"
                
                }else{
                  
                  "pid.foll.ind.else.txt"
            }    
    })



    ideol.foll.sample <- eventReactive(input[[paste(str_to_title(ideol), "_next", sep = "")]], {
          
            if(as.numeric(input[[paste(str_to_title(ideol), "_ideol", sep = "")]]) == 1){
              
              "ideol.foll.lib.txt"
              
              }else if(as.numeric(input[[paste(str_to_title(ideol), "_ideol", sep = "")]]) == 2){
                
                "ideol.foll.cons.txt"
                
                }else{
                  
                  "ideol.foll.nei.txt"
            }    
    })

        
        
    
    

############# Section C: Page Layouts #############

  PageLayouts <- reactive({

    # The inner "assign" creates the object code.list that reads in "code.txt" and creates a global ID
    # The outer "assign" overwrites the inner code.list in order to include the completion.code
    # It is necessary this way because paste() can otherwise not be used to create object names
    # assign(paste(co, ".list", sep = ""), 
    #        changePageVariable(pageList = assign(paste(co, ".list", sep = ""), 
    #                                             createPageList(fileName = paste("questions/", co, ".txt" , sep = ""),
    #                                             globId = str_to_title(co), defaulttxt = FALSE)),
    #                           variable = "text",
    #                           oldLabel = "completion.code",
    #                           newLabel = CurrentValues$completion.code))
        

      if (CurrentValues$page == ins) {
      return(
        # "assign" creates the object instructions.list that reads in "instructions.txt" and creates a global ID
        # The rest creates the html logic of the instructions page
        createPage(pageList = assign(paste(ins, ".list", sep = ""), 
                                     createPageList(fileName = paste("questions/", ins, ".txt" , sep = ""),
                                     globId = str_to_title(ins), defaulttxt = FALSE)),
                   pageNumber = CurrentValues[[paste(str_to_title(ins), ".num", sep = "")]],
                   globId = str_to_title(ins), ctrlVals = CurrentValues)
      )}

      if (CurrentValues$page == measure1) {
      return(
        # "assign" creates the object demographics.list that reads in "demographics.txt" and creates a global ID
        # The rest creates the html logic of the demographics page
        createPage(pageList = assign(paste(measure1, ".list", sep = ""), 
                                     createPageList(fileName = paste0("questions/", mor.si.first$a),
                                     globId = str_to_title(measure1), defaulttxt = FALSE)),
                   pageNumber = CurrentValues[[paste(str_to_title(measure1), ".num", sep = "")]],
                   globId = str_to_title(measure1), ctrlVals = CurrentValues)
      )}


      if (CurrentValues$page == measure2) {
      return(
        # "assign" creates the object demographics.list that reads in "demographics.txt" and creates a global ID
        # The rest creates the html logic of the demographics page
        createPage(pageList = assign(paste(measure2, ".list", sep = ""), 
                                     createPageList(fileName = paste0("questions/", mor.si.second$a),
                                     globId = str_to_title(measure2), defaulttxt = FALSE)),
                   pageNumber = CurrentValues[[paste(str_to_title(measure2), ".num", sep = "")]],
                   globId = str_to_title(measure2), ctrlVals = CurrentValues)
      )}


      if (CurrentValues$page == dem) {
      return(
        # "assign" creates the object demographics.list that reads in "demographics.txt" and creates a global ID
        # The rest creates the html logic of the demographics page
        createPage(pageList = assign(paste(dem, ".list", sep = ""), 
                                     createPageList(fileName = paste("questions/", dem, ".txt" , sep = ""),
                                     globId = str_to_title(dem), defaulttxt = FALSE)),
                   pageNumber = CurrentValues[[paste(str_to_title(dem), ".num", sep = "")]],
                   globId = str_to_title(dem), ctrlVals = CurrentValues)
      )}
    
      if (CurrentValues$page == pid) {
      return(
        # "assign" creates the object demographics.list that reads in "demographics.txt" and creates a global ID
        # The rest creates the html logic of the demographics page
        createPage(pageList = assign(paste(pid, ".list", sep = ""), 
                                     createPageList(fileName = paste("questions/", pid, ".txt" , sep = ""),
                                     globId = str_to_title(pid), defaulttxt = FALSE)),
                   pageNumber = CurrentValues[[paste(str_to_title(pid), ".num", sep = "")]],
                   globId = str_to_title(pid), ctrlVals = CurrentValues)
      )}
    
      if (CurrentValues$page == pid.foll.up) {
      return(
        # "assign" reates the object pid.foll.up.list that reads in the appropriate pid follow-up
        # The rest creates the html logic of the hc page
        createPage(pageList = assign(paste(pid.foll.up, ".list", sep = ""), 
                                     createPageList(fileName = paste("questions/", pid.foll.sample(), sep = ""),
                                     globId = str_to_title(pid.foll.up), defaulttxt = FALSE)), 
                   pageNumber = CurrentValues[[paste(str_to_title(pid.foll.up), ".num", sep = "")]],
                   globId = str_to_title(pid.foll.up), ctrlVals = CurrentValues)
      )}
        
      if (CurrentValues$page == ideol) {
      return(
        # "assign" creates the object demographics.list that reads in "demographics.txt" and creates a global ID
        # The rest creates the html logic of the demographics page
        createPage(pageList = assign(paste(ideol, ".list", sep = ""), 
                                     createPageList(fileName = paste("questions/", ideol, ".txt" , sep = ""),
                                     globId = str_to_title(ideol), defaulttxt = FALSE)),
                   pageNumber = CurrentValues[[paste(str_to_title(ideol), ".num", sep = "")]],
                   globId = str_to_title(ideol), ctrlVals = CurrentValues)
      )}
    
      if (CurrentValues$page == ideol.foll.up) {
      return(
        # "assign" reates the object pid.foll.up.list that reads in the appropriate pid follow-up
        # The rest creates the html logic of the hc page
        createPage(pageList = assign(paste(ideol.foll.up, ".list", sep = ""), 
                                     createPageList(fileName = paste("questions/", ideol.foll.sample(), sep = ""),
                                     globId = str_to_title(ideol.foll.up), defaulttxt = FALSE)), 
                   pageNumber = CurrentValues[[paste(str_to_title(ideol.foll.up), ".num", sep = "")]],
                   globId = str_to_title(ideol.foll.up), ctrlVals = CurrentValues)
      )}
        
      if (CurrentValues$page == ed.both) {
      return(
        # "assign" reates the object pid.foll.up.list that reads in the appropriate pid follow-up
        # The rest creates the html logic of the hc page
        createPage(pageList = assign(paste(ed.both, ".list", sep = ""), 
                                     createPageList(fileName = paste("questions/", ed.sample(), sep = ""),
                                     globId = str_to_title(ed.both), defaulttxt = FALSE)), 
                   pageNumber = CurrentValues[[paste(str_to_title(ed.both), ".num", sep = "")]],
                   globId = str_to_title(ed.both), ctrlVals = CurrentValues)
      )}


      # if (CurrentValues$page == ed) {
      # return(
      #   # "assign" creates the object education.list that reads in "education.txt" and creates a global ID
      #   # The rest creates the html logic of the education page
      #   createPage(pageList = assign(paste(ed, ".list", sep = ""), 
      #                                createPageList(fileName = paste("questions/", ed, ".txt" , sep = ""),
      #                                globId = str_to_title(ed), defaulttxt = FALSE)),
      #              pageNumber = CurrentValues[[paste(str_to_title(ed), ".num", sep = "")]],
      #              globId = str_to_title(ed), ctrlVals = CurrentValues)
      # )}
    
      if (CurrentValues$page == issue1) {
      return(
        # "assign" reates the object hc.list that reads in whatever hc question file was sampled
        # The rest creates the html logic of the hc page
        createPage(pageList = assign(paste(issue1, ".list", sep = ""), 
                                     createPageList(fileName = paste("questions/treatment/", hc.sample(), sep = ""),
                                     globId = str_to_title(issue1), defaulttxt = FALSE)), 
                   pageNumber = CurrentValues[[paste(str_to_title(issue1), ".num", sep = "")]],
                   globId = str_to_title(issue1), ctrlVals = CurrentValues)
      )}

      if (CurrentValues$page == issue1.check) {
      return(
        # "assign" creates the object demographics.list that reads in "demographics.txt" and creates a global ID
        # The rest creates the html logic of the demographics page
        createPage(pageList = assign(paste(issue1.check, ".list", sep = ""), 
                                     createPageList(fileName = paste("questions/", issue1.check, ".txt" , sep = ""),
                                     globId = str_to_title(issue1.check), defaulttxt = FALSE)),
                   pageNumber = CurrentValues[[paste(str_to_title(issue1.check), ".num", sep = "")]],
                   globId = str_to_title(issue1.check), ctrlVals = CurrentValues)
      )}
    
      if (CurrentValues$page == issue2) {
      return(
        # "assign" reates the object ev.list that reads in whatever ev question file was sampled
        # The rest creates the html logic of the ev page
        createPage(pageList = assign(paste(issue2, ".list", sep = ""), 
                                     createPageList(fileName = paste("questions/treatment/", ev.sample(), sep = ""),
                                     globId = str_to_title(issue2), defaulttxt = FALSE)), 
                   pageNumber = CurrentValues[[paste(str_to_title(issue2), ".num", sep = "")]],
                   globId = str_to_title(issue2), ctrlVals = CurrentValues)
      )}

      if (CurrentValues$page == issue2.check) {
      return(
        # "assign" creates the object demographics.list that reads in "demographics.txt" and creates a global ID
        # The rest creates the html logic of the demographics page
        createPage(pageList = assign(paste(issue2.check, ".list", sep = ""), 
                                     createPageList(fileName = paste("questions/", issue2.check, ".txt" , sep = ""),
                                     globId = str_to_title(issue2.check), defaulttxt = FALSE)),
                   pageNumber = CurrentValues[[paste(str_to_title(issue2.check), ".num", sep = "")]],
                   globId = str_to_title(issue2.check), ctrlVals = CurrentValues)
      )}
    
      if (CurrentValues$page == co) {
      return(
        # "assign" creates the object education.list that reads in "education.txt" and creates a global ID
        # The rest creates the html logic of the education page
        createPage(pageList = assign(paste(co, ".list", sep = ""), 
                                     createPageList(fileName = paste("questions/", co, ".txt" , sep = ""),
                                     globId = str_to_title(co), defaulttxt = FALSE)),
                   pageNumber = CurrentValues[[paste(str_to_title(co), ".num", sep = "")]],
                   globId = str_to_title(co), ctrlVals = CurrentValues)
      )}

      # if (CurrentValues$page == co){
      # return(
      #   # Both "assign"s are needed here in order to carry forward the code.completion code
      #   # Both "assign"s are simply copiy-pasted from above
      #   # The rest creates the html logic of the code page
      #   createPage(pageList = assign(paste(co, ".list", sep = ""),
      #                                changePageVariable(pageList = assign(paste(co, ".list", sep = ""), 
      #                                                       createPageList(fileName = paste("questions/", co, ".txt" , sep = ""),
      #                                                       globId = str_to_title(co), defaulttxt = FALSE)),
      #                                                       variable = "text",
      #                                                       oldLabel = "completion.code",
      #                                                       newLabel = CurrentValues$completion.code)), 
      #              pageNumber = CurrentValues[[paste(str_to_title(co), ".num", sep = "")]],
      #              globId = str_to_title(co), ctrlVals = CurrentValues)
      # )}
    
  })
   
  

############# Section D: Page Navigation Buttons #############

    # All "assign"s are simply copied from above  
    observeEvent(input[[paste(str_to_title(ins), "_next", sep = "")]],{
    nextPage(pageId = ins, ctrlVals = CurrentValues, 
             nextPageId = measure1, pageList = assign(paste(ins, ".list", sep = ""), 
                                                createPageList(fileName = paste("questions/", ins, ".txt" , sep = ""),
                                                globId = str_to_title(ins), defaulttxt = FALSE)), 
             globId = str_to_title(ins))
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
             nextPageId = dem, pageList = assign(paste(measure2, ".list", sep = ""), 
                                                            createPageList(fileName = paste0("questions/", mor.si.second$a),
                                                            globId = str_to_title(measure2), defaulttxt = FALSE)),
             globId = str_to_title(measure2))
  })


    observeEvent(input[[paste(str_to_title(dem), "_next", sep = "")]],{
    nextPage(pageId = dem, ctrlVals = CurrentValues, 
             nextPageId = pid, pageList = assign(paste(dem, ".list", sep = ""), 
                                     createPageList(fileName = paste("questions/", dem, ".txt" , sep = ""),
                                     globId = str_to_title(dem), defaulttxt = FALSE)), 
             globId = str_to_title(dem))
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
             nextPageId = issue1, pageList = assign(paste(ed.both, ".list", sep = ""), 
                                                            createPageList(fileName = paste("questions/", ed.sample(), sep = ""),
                                                            globId = str_to_title(ed.both), defaulttxt = FALSE)),
             globId = str_to_title(ed.both))
  })
    
  #   observeEvent(input[[paste(str_to_title(ed), "_next", sep = "")]],{
  #   nextPage(pageId = ed, ctrlVals = CurrentValues,
  #            nextPageId = issue1, pageList = assign(paste(ed, ".list", sep = ""), 
  #                                                           createPageList(fileName = paste("questions/", ed, ".txt" , sep = ""),
  #                                                           globId = str_to_title(ed), defaulttxt = FALSE)),
  #            globId = str_to_title(ed))
  # })

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
             nextPageId = co, pageList = assign(paste(issue2.check, ".list", sep = ""), 
                                                            createPageList(fileName = paste("questions/", issue2.check, ".txt" , sep = ""),
                                                            globId = str_to_title(issue2.check), defaulttxt = FALSE)),
             globId = str_to_title(issue2.check))
  })


  
  
############# Section E: Event Control #############

  # Make sure answers are selected
  # As before, all "assign"s are simply copied from above  
  observeEvent(reactiveValuesToList(input),{

    onInputEnable(pageId = ins, ctrlVals = CurrentValues,
                  pageList = assign(paste(ins, ".list", sep = ""), 
                                    createPageList(fileName = paste("questions/", ins, ".txt" , sep = ""),
                                    globId = str_to_title(ins), defaulttxt = FALSE)), 
                  globId = str_to_title(ins),
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

    onInputEnable(pageId = dem, ctrlVals = CurrentValues,
                  pageList = assign(paste(dem, ".list", sep = ""), 
                                    createPageList(fileName = paste("questions/", dem, ".txt" , sep = ""),
                                    globId = str_to_title(dem), defaulttxt = FALSE)), 
                  globId = str_to_title(dem),
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
    
    # onInputEnable(pageId = ed, ctrlVals = CurrentValues,
    #               pageList = assign(paste(ed, ".list", sep = ""), 
    #                                 createPageList(fileName = paste("questions/", ed, ".txt" , sep = ""),
    #                                 globId = str_to_title(ed), defaulttxt = FALSE)), 
    #               globId = str_to_title(ed),
    #               inputList = input)

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

    onInputEnable(pageId = co, ctrlVals = CurrentValues,
                  pageList = assign(paste(co, ".list", sep = ""), 
                                    createPageList(fileName = paste("questions/", co, ".txt" , sep = ""),
                                    globId = str_to_title(co), defaulttxt = FALSE)), 
                  globId = str_to_title(co),
                  inputList = input)

        # onInputEnable(pageId = co, ctrlVals = CurrentValues,
        #           pageList = assign(paste(co, ".list", sep = ""), 
        #                             changePageVariable(pageList = assign(paste(co, ".list", sep = ""), 
        #                                                           createPageList(fileName = paste("questions/", co, ".txt" , sep = ""),
        #                                                           globId = str_to_title(co), defaulttxt = FALSE)),
        #                                                           variable = "text",
        #                                                           oldLabel = "completion.code",
        #                                                           newLabel = CurrentValues$completion.code)), 
        #           globId = str_to_title(co),
        #           inputList = input)

  })

  
  
############# Section F: Save data #############

  observeEvent(input[[paste(str_to_title(co), "_next", sep = "")]], {(

    # Create progress message
    withProgress(message = "Saving data...", value = 0, {

      incProgress(.25)
    
      # Create a list to save data
      # Pre-created empty and then filled in because this was the only way to use paste
      data.list <- list()
      # data.list[["id"]] <- input[[paste(str_to_title(co), "_workerid", sep = "")]]
      data.list[["age"]] <- input[[paste(str_to_title(dem), "_age", sep = "")]]
      data.list[["race"]] <- input[[paste(str_to_title(dem), "_race", sep = "")]]
      data.list[["gender"]] <- input[[paste(str_to_title(dem), "_gender", sep = "")]]
      data.list[["att"]] <- input[[paste(str_to_title(dem), "_att", sep = "")]]
      data.list[["empl"]] <- input[[paste(str_to_title(dem), "_empl", sep = "")]]
      data.list[["inc"]] <- input[[paste(str_to_title(dem), "_inc", sep = "")]]
      data.list[["pid"]] <- input[[paste(str_to_title(pid), "_pid", sep = "")]]
      data.list[["pid.follow"]] <- input[[paste(str_to_title(pid.foll.up), "_pid.follow", sep = "")]]
      data.list[["ideol"]] <- input[[paste(str_to_title(ideol), "_ideol", sep = "")]]
      data.list[["ideol.follow"]] <- input[[paste(str_to_title(ideol.foll.up), "_ideol.follow", sep = "")]]
      
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
        data.list[["si.hon"]] <- input[[paste(str_to_title(measure2), "_si.hon", sep = "")]]
        data.list[["si.boat"]] <- input[[paste(str_to_title(measure2), "_si.boat", sep = "")]]
        data.list[["si.kill"]] <- input[[paste(str_to_title(measure2), "_si.kill", sep = "")]]
        data.list[["si.good"]] <- input[[paste(str_to_title(measure2), "_si.good", sep = "")]]
        data.list[["si.help"]] <- input[[paste(str_to_title(measure2), "_si.help", sep = "")]]
      }else{
        data.list[["si.white"]] <- input[[paste(str_to_title(measure1), "_si.white", sep = "")]]
        data.list[["si.care"]] <- input[[paste(str_to_title(measure1), "_si.care", sep = "")]]
        data.list[["si.kids"]] <- input[[paste(str_to_title(measure1), "_si.kids", sep = "")]]
        data.list[["si.hon"]] <- input[[paste(str_to_title(measure1), "_si.hon", sep = "")]]
        data.list[["si.boat"]] <- input[[paste(str_to_title(measure1), "_si.boat", sep = "")]]
        data.list[["si.kill"]] <- input[[paste(str_to_title(measure1), "_si.kill", sep = "")]]
        data.list[["si.good"]] <- input[[paste(str_to_title(measure1), "_si.good", sep = "")]]
        data.list[["si.help"]] <- input[[paste(str_to_title(measure1), "_si.help", sep = "")]]
      }
      
      # data.list[["interest"]] <- input[[paste(str_to_title(dem), "_interest", sep = "")]]
      data.list[[paste(ed.both)]] <- input[[paste(str_to_title(ed.both), "_educ", sep = "")]]
      data.list[[paste(issue1)]] <- input[[paste(str_to_title(issue1), "_treat", sep = "")]]
      data.list[[paste(issue1, ".group", sep = "")]] <- tools::file_path_sans_ext(hc.sample())
      data.list[[paste(issue1.check)]] <- input[[paste(str_to_title(issue1.check), "_check", sep = "")]]
      data.list[[paste(issue2)]] <- input[[paste(str_to_title(issue2), "_treat", sep = "")]]
      data.list[[paste(issue2, ".group", sep = "")]] <- tools::file_path_sans_ext(ev.sample())
      data.list[[paste(issue2.check)]] <- input[[paste(str_to_title(issue2.check), "_check", sep = "")]]
      data.list[["RID"]] <- RID()
      if(ed.sample() == "education.an.txt"){
        data.list[["ANES"]] <- "Yes"
      }else{
        data.list[["ANES"]] <- "No"
      }
      
      if(ed.sample() == "education.op.txt"){
        saveData(data.list, csv.dropdir.op)                     # this is where the created function saveData.op() is executed
      }else{
        saveData(data.list, csv.dropdir.an)                     # this is where the created function saveData.an() is executed
      }        
      
      # Redirect to external website, which is required by Lucid
      # Set up so that I can deploy/run it and Lucid can test it with the same code
      # If RID is "no.query.string", i.e. if I'm running it, it redirects to Google, with "no.query.string" added
      # If RID is anything else, i.e. if Lucid is testing it, it redirects to their completion side, with the RID woven in
      if(RID() == "no.query.string"){
        js$browseURL(paste("http://www.google.com/"))
      }else{
        js$browseURL(paste("https://notch.insights.supply/cb?token=187df571-53eb-46e4-b03b-0425864e5361&RID=", RID(), sep = ""))
      }
    })
    
  )})
  
}


#-------------------------------------------- Create the app  --------------------------------------------#

shinyApp(ui = ui, server = server)


