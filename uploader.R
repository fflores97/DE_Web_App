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
    actionButton("uploadExpression","Process Expression Data")
  )
}

# Coldata ####
uploaderColDataUI<-function(){
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
    numericInput("colDataNumber", "Columns to Display", value = 2)
  )
}

# Server ####
uploaderExpressionServer<-function(input){
  uploaderExpressionServerOutput<-list()
  # Expression Data Reader ####
  expressionData<-reactive(
    {
      if (is.null(input$expressionDataFile)) {
        # User has not uploaded a file yet
        return(NULL)
      }
      inFileExpression<-input$expressionDataFile
      read.csv(
        file =inFileExpression$datapath,
        header=input$expressionHeader,
        sep=input$expressionSep,
        quote=input$expressionQuote,
        stringsAsFactors = F,
        row.names = 1
      )
    })
  
  # Expression Data Summary ####
  expressionSummary<-DT::renderDataTable(
    expressionData()[,c(1:input$expressionNumber)],
    options=list(scrollX=T,scroller=T)
  )
  uploaderExpressionServerOutput$expressionData<-expressionData
  uploaderExpressionServerOutput$expressionSummary<-expressionSummary
  return(uploaderExpressionServerOutput)
}
