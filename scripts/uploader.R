# Expression Data ####
uploaderExpressionUI<-function(){
  box(
    "Please upload expression data file here",
    tags$hr(),
    fileInput("expressionDataFile", "Choose Expression Data File",
              multiple = FALSE,
              accept = c("text/csv",
                         "text/comma-separated-values,text/plain",
                         ".csv")),
    # Horizontal line ----
    tags$hr(),
    
    # Checkbox if file has header ----
    checkboxInput("expressionHeader", "Header", TRUE),
    
    # Select separator ----
    radioButtons("expressionSep", "Separator",
                 choices = c(Comma = ",",
                             Semicolon = ";",
                             Tab = "\t"),
                 selected = ","),
    
    # Select quotes ----
    radioButtons("expressionQuote", "Quote",
                 choices = c(None = "",
                             "Double Quote" = '"',
                             "Single Quote" = "'"),
                 selected = ""),
    # How many columns to display ---
    numericInput("expressionNumber", "Columns to Display", value = 2),
    actionButton("uploadExpression","Upload")
  )
}

# Coldata ####
uploaderColDataUI<-function(input=input,output=output){
  box(
    "Please upload colData table here",
    tags$hr(),
    fileInput("colDataFile", "Choose ColData File",
              multiple = FALSE,
              accept = c("text/csv",
                         "text/comma-separated-values,text/plain",
                         ".csv")),
    # Horizontal line ----
    tags$hr(),
    
    # Checkbox if file has header ----
    checkboxInput("colDataHeader", "Header", TRUE),
    
    # Select separator ----
    radioButtons("colDataSep", "Separator",
                 choices = c(Comma = ",",
                             Semicolon = ";",
                             Tab = "\t"),
                 selected = ","),
    
    # Select quotes ----
    radioButtons("colDataQuote", "Quote",
                 choices = c(None = "",
                             "Double Quote" = '"',
                             "Single Quote" = "'"),
                 selected = ""),
    # How many columns to display ---
    numericInput("colDataNumber", "Columns to Display", value = 2),
    actionButton("uploadColData","Upload")
  )
}

# uploaderServer<-function(input=input,output=output){
#   uploaderServerOutput<-list()
#   # Expression Data Reader ####
#   expressionData<-eventReactive(
#     input$uploadExpression,
#     {
#       if (is.null(input$expressionDataFile)) {
#         # User has not uploaded a file yet
#         return(NULL)
#       }
#       inFileExpression<-input$expressionDataFile
#       read.csv(
#         file =inFileExpression$datapath,
#         header=input$expressionHeader,
#         sep=input$expressionSep,
#         quote=input$expressionQuote,
#         stringsAsFactors = F,
#         row.names = 1
#       )
#     })
#   
#   
#   # Expression Data Summary ####
#   expressionSummary<-DT::renderDataTable(
#     expressionData[,c(1:input$expressionNumber)],
#     options=list(scrollX=T,scroller=T)
#   )
#   
#   # Column Data Reader ####
#   colData<-eventReactive(
#     input$uploadColData,
#     {
#       if (is.null(input$colDataFile)) {
#         # User has not uploaded a file yet
#         return(NULL)
#       }
#       inFileColData<-input$colDataFile
#       read.csv(
#         file = inFileColData$datapath,
#         header = input$colDataHeader,
#         sep = input$colDataSep,
#         quote = input$colDataQuote,
#         row.names = 1
#       )
#     })
#   
#   
#   # Col Data Summary ####
#   colDataSummary<-DT::renderDataTable(
#     colData()[,c(1:input$colDataNumber)],
#     options=list(scrollX=T,scroller=T)
#   )
#   uploaderServerOutput$expressionData<-expressionData
#   uploaderServerOutput$expressionSummary<-expressionSummary
#   uploaderServerOutput$colData<-colData
#   uploaderServerOutput$colDataSummary<-colDataSummary
#   return(uploaderServerOutput)
# }



