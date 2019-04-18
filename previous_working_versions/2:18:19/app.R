
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


# Vector with page ids used to later access objects
idsVec <- c("BlockInstructions", "BlockDemographics", "BlockTreatments", "BlockCode", "BlockGoodbye")

# create page lists for the instructions and the last page
blockInstructions.list <- createPageList(fileName = "questions/BlockInstructions.txt",
                                    globId = "BlockInstructions", defaulttxt = FALSE)
blockDemographics.list <- createPageList(fileName = "questions/BlockDemographics.txt",
                              globId = "BlockDemographics", defaulttxt = FALSE)
blockTreatments.list <- createPageList(fileName = "questions/BlockTreatments.txt",
                              globId = "BlockTreatments", defaulttxt = FALSE)
blockCode.list <- createPageList(fileName = "questions/BlockCode.txt",
                                 globId = "BlockCode", defaulttxt = FALSE)
blockGoodbye.list <- createPageList(fileName = "questions/BlockGoodbye.txt", defaulttxt = FALSE)



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
  CurrentValues <- createCtrlList(firstPage = "blockInstructions", # id of the first page
                                  globIds = idsVec,           # ids of pages for createPage
                                  complCode = TRUE,           # create a completion code
                                  complName = "survey")    # first element of completion code


# Section D: Page Layouts ====================================================

  PageLayouts <- reactive({

    # insert created completion code that it can later be displayed
    blockCode.list <- changePageVariable(pageList = blockCode.list, variable = "text",
                                       oldLabel = "completion.code",
                                       newLabel = CurrentValues$completion.code)

    # display instructions page
    if (CurrentValues$page == "blockInstructions") {

      return(
        # create html logic of instructions page
        createPage(pageList = blockInstructions.list,
                   pageNumber = CurrentValues$BlockInstructions.num,
                   globId = "BlockInstructions", ctrlVals = CurrentValues)
      )}

    # display blockDemographics page
    if (CurrentValues$page == "blockDemographics") {

      return(
        # create html logic of instructions page
        createPage(pageList = blockDemographics.list,
                   pageNumber = CurrentValues$BlockDemographics.num,
                   globId = "BlockDemographics", ctrlVals = CurrentValues)
      )}


    if (CurrentValues$page == "blockTreatments"){

      return(
        createPage(pageList = blockTreatments.list, pageNumber = CurrentValues$BlockTreatments.num,
                   globId = "BlockTreatments", ctrlVals = CurrentValues)
      )}

    if (CurrentValues$page == "blockCode"){

      return(
        createPage(pageList = blockCode.list, pageNumber = CurrentValues$BlockCode.num,
                   globId = "BlockCode", ctrlVals = CurrentValues)
      )}

    
# Section E: Goodbye
    if (CurrentValues$page == "blockGoodbye") {

      return(
        createPage(pageList = blockGoodbye.list, pageNumber = CurrentValues$BlockGoodbye.num,
                   globId = "BlockGoodbye", ctrlVals = CurrentValues, continueButton = FALSE)
      )}

  })


# Section F: Event (e.g.; button) actions ======================================

# Section F1: Page Navigation Buttons ----------------------


  observeEvent(input[["BlockInstructions_next"]],{
    nextPage(pageId = "blockInstructions", ctrlVals = CurrentValues, 
             nextPageId = "blockDemographics", pageList = blockInstructions.list, 
             globId = "BlockInstructions")
  })

  observeEvent(input[["BlockDemographics_next"]],{
    nextPage(pageId = "blockDemographics", ctrlVals = CurrentValues,
             nextPageId = "blockTreatments", pageList = blockDemographics.list,
             globId = "BlockDemographics")
  })

  observeEvent(input[["BlockTreatments_next"]],{
    nextPage(pageId = "blockTreatments", ctrlVals = CurrentValues,
             nextPageId = "blockCode", pageList = blockTreatments.list,
             globId = "BlockTreatments")
  })

# Section F2: Event Control ----------------------


  # Make sure answers are selected
  observeEvent(reactiveValuesToList(input),{

    onInputEnable(pageId = "blockInstructions", ctrlVals = CurrentValues,
                  pageList = blockInstructions.list, globId = "BlockInstructions",
                  inputList = input)

    onInputEnable(pageId = "blockDemographics", ctrlVals = CurrentValues,
                  pageList = blockDemographics.list, globId = "BlockDemographics",
                  inputList = input)

    onInputEnable(pageId = "blockTreatments", ctrlVals = CurrentValues,
                  pageList = blockTreatments.list, globId = "BlockTreatments",
                  inputList = input)

    onInputEnable(pageId = "blockCode", ctrlVals = CurrentValues,
                  pageList = blockCode.list, globId = "BlockCode",
                  inputList = input)
    
  })

# Section G: Save data =========================================================

  observeEvent(input[["BlockCode_next"]], {(

    # Create progress message
    withProgress(message = "Saving data...", value = 0, {

      incProgress(.25)

      # Create a list to save data
      data.list <- list(  "id" = input$BlockCode_workerid,                     # the labels (e.g. "qu1") have to match what's in the .txt
                          "educ" = input$BlockDemographics_educ,               # if I add a question to any of the .txts, I need to add it here too
                          "treat" = input$BlockTreatments_treat,
                          "input.order" = blockDemographics.list$id.order)

      # save Data                                                            
        savedata(data.list)                     # this is where the created function savedata() is executed

      CurrentValues$page <- "blockGoodbye"

    })

  )})

}


shinyApp(ui = ui, server = server)





