
###### Very minimal example, set up so that I have to change as little as possible if things change ######



#-------------------------------------------- Preliminary stuff before shiny gets involved --------------------------------------------#


# Working directory, libraries
library(shiny)
library(shinyjs)
library(ShinyPsych)
library(rdrop2)
library(stringr)

# Dropbox directory to save data
outputDir <- "block_data" 

token<-drop_auth()
saveRDS(token, "droptoken.rds")

# Function that saves each response as a .csv file to AU Dropbox /block_data
# Defined here; executed in the app
savedata <- function(data) {                 
  data <- t(data)
  # Create a unique file name
  fileName <- sprintf("%s_%s.csv", as.integer(Sys.time()), digest::digest(data))
  # Write the data to a temporary file locally
  filePath <- file.path(tempdir(), fileName)
  write.csv(data, filePath, row.names = FALSE, quote = TRUE)
  # Upload the file to Dropbox outputDir
  drop_upload(filePath, path = outputDir)
}

# Load the non-treatment question files, pull their names, save short versions for later
temp <- tools::file_path_sans_ext(list.files("questions", pattern = "*.txt"))
co <- temp[1]  # these need to be in alphabetical order, otherwise they are mislabelled
ed <- temp[2]
gb <- temp[3]
ins <- temp[4]

# Load the treatment question files, sample one per issue, save "mw" and "tb" for later
# Note that these comes from the /treatment subfolder
mw.sample <- sample(list.files("questions/treatment", pattern="^.*mw.*.txt"), 1, replace = TRUE)
tb.sample <- sample(list.files("questions/treatment", pattern="^.*tb.*.txt"), 1, replace = TRUE)
issue1 <- substr(mw.sample, 1, 2)  # deletes all characters except the last two, resulting in mw
issue2 <- substr(tb.sample, 1, 2)  # same for tb

# All these short versions are used in the code later with paste, assign etc. That way I only have to adjust things once at the beginning
# The only exception are the samples, because otherwise I would keep resampling and messing things up
# I deleted several lines to create page lists from the original here
# They show up later several times with "assign"
# There is no point in keeping them up here because I needed to copy-paste the "assign"s anyway in order to keep using paste


# Vector with page ids used to later access objects
idsVec <- c(str_to_title(ins), str_to_title(ed), str_to_title(issue1), str_to_title(issue2), str_to_title(co), str_to_title(gb))




#-------------------------------------------- Define ui function  --------------------------------------------#

ui <- fixedPage(

  # App title
  title = "ShinySurvey",
  uiOutput("MainAction"),

  # For Shinyjs functions
  useShinyjs(),

  # Include appropriate css and js scripts
  includeScriptFiles()

)


#-------------------------------------------- Define server function  --------------------------------------------#


##### Define server function #####
server <- function(input, output, session) {

  output$MainAction <- renderUI( {
    PageLayouts()

  })


  # CurrentValues controls page setting such as which page to display
  CurrentValues <- createCtrlList(firstPage = ins, # id of the first page
                                  globIds = idsVec,           # ids of pages for createPage
                                  complCode = TRUE,           # create a completion code
                                  complName = "survey")    # first element of completion code

  
############# Section A: Page Layouts #############

  PageLayouts <- reactive({

    # The inner "assign" creates the object code.list that reads in "code.txt" and creates a global ID
    # The outer "assign" overwrites the inner code.list in order to include the completion.code
    # It is necessary this way because paste() can otherwise not be used to create object names
    assign(paste(co, ".list", sep = ""), 
           changePageVariable(pageList = assign(paste(co, ".list", sep = ""), 
                                                createPageList(fileName = paste("questions/", co, ".txt" , sep = ""),
                                                globId = str_to_title(co), defaulttxt = FALSE)),
                              variable = "text",
                              oldLabel = "completion.code",
                              newLabel = CurrentValues$completion.code))
        

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

    
      if (CurrentValues$page == ed) {
      return(
        # "assign" creates the object education.list that reads in "education.txt" and creates a global ID
        # The rest creates the html logic of the education page
        createPage(pageList = assign(paste(ed, ".list", sep = ""), 
                                     createPageList(fileName = paste("questions/", ed, ".txt" , sep = ""),
                                     globId = str_to_title(ed), defaulttxt = FALSE)),
                   pageNumber = CurrentValues[[paste(str_to_title(ed), ".num", sep = "")]],
                   globId = str_to_title(ed), ctrlVals = CurrentValues)
      )}
    
      if (CurrentValues$page == issue1) {
      return(
        # "assign" reates the object mw.list that reads in whatever mw question file was sampled
        # The rest creates the html logic of the mw page
        createPage(pageList = assign(paste(issue1, ".list", sep = ""), 
                                     createPageList(fileName = paste("questions/treatment/", mw.sample, sep = ""),
                                     globId = str_to_title(issue1), defaulttxt = FALSE)), 
                   pageNumber = CurrentValues[[paste(str_to_title(issue1), ".num", sep = "")]],
                   globId = str_to_title(issue1), ctrlVals = CurrentValues)
      )}

      if (CurrentValues$page == issue2) {
      return(
        # "assign" reates the object tb.list that reads in whatever tb question file was sampled
        # The rest creates the html logic of the tb page
        createPage(pageList = assign(paste(issue2, ".list", sep = ""), 
                                     createPageList(fileName = paste("questions/treatment/", tb.sample, sep = ""),
                                     globId = str_to_title(issue2), defaulttxt = FALSE)), 
                   pageNumber = CurrentValues[[paste(str_to_title(issue2), ".num", sep = "")]],
                   globId = str_to_title(issue2), ctrlVals = CurrentValues)
      )}

      if (CurrentValues$page == co){
      return(
        # Both "assign"s are needed here in order to carry forward the code.completion code
        # Both "assign"s are simply copiy-pasted from above
        # The rest creates the html logic of the code page
        createPage(pageList = assign(paste(co, ".list", sep = ""),
                                     changePageVariable(pageList = assign(paste(co, ".list", sep = ""), 
                                                            createPageList(fileName = paste("questions/", co, ".txt" , sep = ""),
                                                            globId = str_to_title(co), defaulttxt = FALSE)),
                                                            variable = "text",
                                                            oldLabel = "completion.code",
                                                            newLabel = CurrentValues$completion.code)), 
                   pageNumber = CurrentValues[[paste(str_to_title(co), ".num", sep = "")]],
                   globId = str_to_title(co), ctrlVals = CurrentValues)
      )}

      if (CurrentValues$page == gb) {
      return(
        # "assign" creates the object goodbye.list that reads in "goodbye.txt" and creates a global ID
        # The rest creates the html logic of the goodbye page. Not that continueButton is turned off, as this is the last page
        createPage(pageList = assign(paste(gb, ".list", sep = ""), 
                                     createPageList(fileName = paste("questions/", gb, ".txt" , sep = ""), defaulttxt = FALSE)), 
                   pageNumber = CurrentValues[[paste(str_to_title(gb), ".num", sep = "")]],
                   globId = str_to_title(gb), ctrlVals = CurrentValues, 
                   continueButton = FALSE)
      )}

    
  })
   
  

############# Section B: Page Navigation Buttons #############

    # All "assign"s are simply copied from above  
    observeEvent(input[[paste(str_to_title(ins), "_next", sep = "")]],{
    nextPage(pageId = ins, ctrlVals = CurrentValues, 
             nextPageId = ed, pageList = assign(paste(ins, ".list", sep = ""), 
                                                createPageList(fileName = paste("questions/", ins, ".txt" , sep = ""),
                                                globId = str_to_title(ins), defaulttxt = FALSE)), 
             globId = str_to_title(ins))
  })

  
    observeEvent(input[[paste(str_to_title(ed), "_next", sep = "")]],{
    nextPage(pageId = ed, ctrlVals = CurrentValues,
             nextPageId = issue1, pageList = assign(paste(ed, ".list", sep = ""), 
                                                            createPageList(fileName = paste("questions/", ed, ".txt" , sep = ""),
                                                            globId = str_to_title(ed), defaulttxt = FALSE)),
             globId = str_to_title(ed))
  })
  
    
  observeEvent(input[[paste(str_to_title(issue1), "_next", sep = "")]],{
    nextPage(pageId = issue1, ctrlVals = CurrentValues,
             nextPageId = issue2, pageList = assign(paste(issue1, ".list", sep = ""), 
                                                            createPageList(fileName = paste("questions/treatment/", mw.sample, sep = ""),
                                                            globId = str_to_title(issue1), defaulttxt = FALSE)),
             globId = str_to_title(issue1))
  })

  observeEvent(input[[paste(str_to_title(issue2), "_next", sep = "")]],{
    nextPage(pageId = issue2, ctrlVals = CurrentValues,
             nextPageId = co, pageList = assign(paste(issue2, ".list", sep = ""), 
                                                    createPageList(fileName = paste("questions/treatment/", tb.sample, sep = ""),
                                                    globId = str_to_title(issue2), defaulttxt = FALSE)),
             globId = str_to_title(issue2))
  })


  
  
############# Section C: Event Control #############

  # Make sure answers are selected
  # As before, all "assign"s are simply copied from above  
  observeEvent(reactiveValuesToList(input),{

    onInputEnable(pageId = ins, ctrlVals = CurrentValues,
                  pageList = assign(paste(ins, ".list", sep = ""), 
                                    createPageList(fileName = paste("questions/", ins, ".txt" , sep = ""),
                                    globId = str_to_title(ins), defaulttxt = FALSE)), 
                  globId = str_to_title(ins),
                  inputList = input)

    onInputEnable(pageId = ed, ctrlVals = CurrentValues,
                  pageList = assign(paste(ed, ".list", sep = ""), 
                                    createPageList(fileName = paste("questions/", ed, ".txt" , sep = ""),
                                    globId = str_to_title(ed), defaulttxt = FALSE)), 
                  globId = str_to_title(ed),
                  inputList = input)

    onInputEnable(pageId = issue1, ctrlVals = CurrentValues,
                  pageList = assign(paste(issue1, ".list", sep = ""), 
                                    createPageList(fileName = paste("questions/treatment/", mw.sample, sep = ""),
                                    globId = str_to_title(issue1), defaulttxt = FALSE)), 
                  globId = str_to_title(issue1),
                  inputList = input)
    
    onInputEnable(pageId = issue2, ctrlVals = CurrentValues,
                  pageList = assign(paste(issue2, ".list", sep = ""), 
                                    createPageList(fileName = paste("questions/treatment/", tb.sample, sep = ""),
                                    globId = str_to_title(issue2), defaulttxt = FALSE)), 
                  globId = str_to_title(issue2),
                  inputList = input)
    
    onInputEnable(pageId = co, ctrlVals = CurrentValues,
                  pageList = assign(paste(co, ".list", sep = ""), 
                                    changePageVariable(pageList = assign(paste(co, ".list", sep = ""), 
                                                                  createPageList(fileName = paste("questions/", co, ".txt" , sep = ""),
                                                                  globId = str_to_title(co), defaulttxt = FALSE)),
                                                                  variable = "text",
                                                                  oldLabel = "completion.code",
                                                                  newLabel = CurrentValues$completion.code)), 
                  globId = str_to_title(co),
                  inputList = input)

  })

  
  
############# Section D: Save data #############

  observeEvent(input[[paste(str_to_title(co), "_next", sep = "")]], {(

    # Create progress message
    withProgress(message = "Saving data...", value = 0, {

      incProgress(.25)

      # Create a list to save data
      # Pre-created empty and then filled in because this was the only way to use paste
      data.list <- list()
      data.list[["id"]] <- input[[paste(str_to_title(co), "_workerid", sep = "")]]
      data.list[[paste(ed)]] <- input[[paste(str_to_title(ed), "_educ", sep = "")]]
      data.list[[paste(issue1)]] <- input[[paste(str_to_title(issue1), "_treat", sep = "")]]
      data.list[[paste(issue1, ".group", sep = "")]] <- tools::file_path_sans_ext(mw.sample)
      data.list[[paste(issue2)]] <- input[[paste(str_to_title(issue2), "_treat", sep = "")]]
      data.list[[paste(issue2, ".group", sep = "")]] <- tools::file_path_sans_ext(tb.sample)

      
      savedata(data.list)                     # this is where the created function savedata() is executed

      CurrentValues$page <- gb  # last page

    })

    
  )})

}


#-------------------------------------------- Create the app  --------------------------------------------#

shinyApp(ui = ui, server = server)

