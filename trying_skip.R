
#### ShinyPsych code that I am gradually changing/extending to fit my needs ####

setwd("/Users/simonheuberger/Google Drive/Amerika/dissertation/___ordinal_blocking/shiny")
library(shiny)
library(shinyjs)
library(ShinyPsych)
library(rdrop2)

# Dropbox directory to save data
outputDir <- "block_data"

savedata <- function(data) {                 # saves each response as a .csv file to AU Dropbox /block_data; defined here; executed in the app
  data <- t(data)
  # Create a unique file name
  fileName <- sprintf("%s_%s.csv", as.integer(Sys.time()), digest::digest(data))
  # Write the data to a temporary file locally
  filePath <- file.path(tempdir(), fileName)
  write.csv(data, filePath, row.names = FALSE, quote = TRUE)
  # Upload the file to Dropbox outputDir
  drop_upload(filePath, path = outputDir)
}


# Section A: assign external values ============================================


# Vector with page ids used to later access objects

# create page lists for the instructions and the last page
party.list <- createPageList(fileName = "questions/party.txt",
                                  globId = "Party", defaulttxt = FALSE)
goodbye.list <- createPageList(fileName = "questions/goodbye.txt", defaulttxt = FALSE)

idsVec <- c("Party", "Goodbye")


# Section B: Define overall layout =============================================

ui <- fixedPage(

  # App title
  title = "ShinySurvey",
  uiOutput("MainAction"),

  # For Shinyjs functions
  useShinyjs(),

  # include appropriate css and js scripts
  includeScriptFiles()

)

server <- function(input, output, session) {

  output$MainAction <- renderUI( {
    PageLayouts()

  })


# Section C: Define Reactive Values ==========================================

  # CurrentValues controls page setting such as which page to display
  CurrentValues <- createCtrlList(firstPage = "party", # id of the first page
                                  globIds = idsVec,           # ids of pages for createPage
                                  complCode = TRUE,           # create a completion code
                                  complName = "survey")    # first element of completion code



# Section D: Page Layouts ====================================================

  PageLayouts <- reactive({

    # display instructions page
    if (CurrentValues$page == "party") {

      return(
        # create html logic of instructions page
        createPage(pageList = party.list,
                   pageNumber = CurrentValues$Party.num,
                   globId = "Party", ctrlVals = CurrentValues)
      )}


# Section E: Goodbye
    if (CurrentValues$page == "goodbye") {

      return(
        createPage(pageList = goodbye.list, pageNumber = CurrentValues$Goodbye.num,
                   globId = "Goodbye", ctrlVals = CurrentValues,
                   continueButton = FALSE)
      )}

  })


# Section F: Event (e.g.; button) actions ======================================

# Section F1: Page Navigation Buttons ----------------------


# Section F2: Event Control ----------------------


  # Make sure answers are selected
  observeEvent(reactiveValuesToList(input),{



    onInputEnable(pageId = "party", ctrlVals = CurrentValues,
                  pageList = party.list, globId = "Party",
                  inputList = input)
    
  })

# Section G: Save data =========================================================

  observeEvent(input[["Party_next"]], {(

    # Create progress message
    withProgress(message = "Saving data...", value = 0, {

      incProgress(.25)

      # Create a list to save data
      data.list <- list(  "id" = input$Party_pid)

      # save Data                                                            
        savedata(data.list)                     # this is where the created function savedata() is executed

      CurrentValues$page <- "goodbye"

    })

  )})

}

shinyApp(ui = ui, server = server)

