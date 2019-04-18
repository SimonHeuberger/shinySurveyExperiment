
#### ShinyPsych code that I am gradually changing/extending to fit my needs ####


library(shiny)
library(shinyjs)
library(ShinyPsych)
library(rdrop2)


# Dropbox directory to save data
outputDir <- "block_data"

token<-drop_auth()
saveRDS(token, "droptoken.rds")

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

MW.treatments <- c("MW.Control", "MW.MOpp", "MW.Popp", "MW.MSupp", "MW.PSupp")
MW.randomized <- sample(MW.treatments, 1, replace = TRUE)

TB.treatments <- c("TB.Control", "TB.MOpp", "TB.MSupp", "TB.POpp", "TB.PSupp")
TB.randomized <- sample(TB.treatments, 1, replace = TRUE)

# create page lists for the instructions and the last page
instructions.list <- createPageList(fileName = "questions/Instructions.txt",
                                    globId = "Instructions", defaulttxt = FALSE)
education.list <- createPageList(fileName = "questions/Education.txt",
                                      globId = "Education", defaulttxt = FALSE)
MW.treatment.list <- createPageList(fileName = paste("questions/", MW.randomized, ".txt" , sep = ""),
                                   globId = "MW.Treatment", defaulttxt = FALSE)
TB.treatment.list <- createPageList(fileName = paste("questions/", TB.randomized, ".txt" , sep = ""),
                                   globId = "TB.Treatment", defaulttxt = FALSE)
code.list <- createPageList(fileName = "questions/Code.txt",
                                 globId = "Code", defaulttxt = FALSE)
goodbye.list <- createPageList(fileName = "questions/Goodbye.txt", defaulttxt = FALSE)


# Vector with page ids used to later access objects
idsVec <- c("Instructions", "Education", "MW.Treatment", "TB.Treatment", "Code", "Goodbye")


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
  CurrentValues <- createCtrlList(firstPage = "instructions", # id of the first page
                                  globIds = idsVec,           # ids of pages for createPage
                                  complCode = TRUE,           # create a completion code
                                  complName = "survey")    # first element of completion code

# Section D: Page Layouts ====================================================

  PageLayouts <- reactive({

        # insert created completion code that it can later be displayed
    code.list <- changePageVariable(pageList = code.list, variable = "text",
                                       oldLabel = "completion.code",
                                       newLabel = CurrentValues$completion.code)

      if (CurrentValues$page == "instructions") {

      return(
        # create html logic of instructions page
        createPage(pageList = instructions.list,
                   pageNumber = CurrentValues$Instructions.num,
                   globId = "Instructions", ctrlVals = CurrentValues)
      )}


      if (CurrentValues$page == "education") {

      return(
        # create html logic of instructions page
        createPage(pageList = education.list,
                   pageNumber = CurrentValues$Education.num,
                   globId = "Education", ctrlVals = CurrentValues)
      )}
    
      if (CurrentValues$page == "MW.treatment") {

      return(
        createPage(pageList = MW.treatment.list, 
                   pageNumber = CurrentValues$MW.Treatment.num,
                   globId = "MW.Treatment", ctrlVals = CurrentValues)
      )}

      if (CurrentValues$page == "TB.treatment") {

      return(
        createPage(pageList = TB.treatment.list, 
                   pageNumber = CurrentValues$TB.Treatment.num,
                   globId = "TB.Treatment", ctrlVals = CurrentValues)
      )}

        if (CurrentValues$page == "code"){

      return(
        createPage(pageList = code.list, pageNumber = CurrentValues$Code.num,
                   globId = "Code", ctrlVals = CurrentValues)
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

    observeEvent(input[["Instructions_next"]],{
    nextPage(pageId = "instructions", ctrlVals = CurrentValues, 
             nextPageId = "education", pageList = instructions.list, 
             globId = "Instructions")
  })


    observeEvent(input[["Education_next"]],{
    nextPage(pageId = "education", ctrlVals = CurrentValues,
             nextPageId = "MW.treatment", pageList = education.list,
             globId = "Education")
  })
  
    
  observeEvent(input[["MW.Treatment_next"]],{
    nextPage(pageId = "MW.treatment", ctrlVals = CurrentValues,
             nextPageId = "TB.treatment", pageList = MW.treatment.list,
             globId = "MW.Treatment")
  })

  observeEvent(input[["TB.Treatment_next"]],{
    nextPage(pageId = "TB.treatment", ctrlVals = CurrentValues,
             nextPageId = "code", pageList = TB.treatment.list,
             globId = "TB.Treatment")
  })


# Section F2: Event Control ----------------------


  # Make sure answers are selected
  observeEvent(reactiveValuesToList(input),{

    onInputEnable(pageId = "instructions", ctrlVals = CurrentValues,
                  pageList = instructions.list, globId = "Instructions",
                  inputList = input)

    onInputEnable(pageId = "education", ctrlVals = CurrentValues,
                  pageList = education.list, globId = "Education",
                  inputList = input)

    onInputEnable(pageId = "MW.treatment", ctrlVals = CurrentValues,
                  pageList = MW.treatment.list, globId = "MW.Treatment",
                  inputList = input)
    
    onInputEnable(pageId = "TB.treatment", ctrlVals = CurrentValues,
                  pageList = TB.treatment.list, globId = "TB.Treatment",
                  inputList = input)
    
    onInputEnable(pageId = "code", ctrlVals = CurrentValues,
                  pageList = code.list, globId = "Code",
                  inputList = input)

  })

  
# Section G: Save data =========================================================

  observeEvent(input[["Code_next"]], {(

    # Create progress message
    withProgress(message = "Saving data...", value = 0, {

      incProgress(.25)

      # Create a list to save data
      data.list <- list(    "id" = input$Code_workerid,
                            "educ" = input$Education_educ,
                            "mw.treat" = input$MW.Treatment_treat,
                            "mw.group" = MW.randomized,
                            "tb.treat" = input$TB.Treatment_treat,
                            "tb.group" = TB.randomized)

      # save Data                                                            
        savedata(data.list)                     # this is where the created function savedata() is executed

      CurrentValues$page <- "goodbye"

    })

  )})

}

shinyApp(ui = ui, server = server)

