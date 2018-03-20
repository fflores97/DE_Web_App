library(shinydashboard)
library(DT)
library(DESeq2)
source("uploader.R")
options(shiny.trace=T)
shinyServer(function(input, output) {
  # Expression Data Reader ####
  expressionData<-reactive({
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
      stringsAsFactors = F
    )
  })
  
  # Column Data Reader ####
  colData<-reactive({
    if (is.null(input$colDataFile)) {
      # User has not uploaded a file yet
      return(NULL)
    }
    inFileColData<-input$colDataFile
    read.csv(
      file = inFileColData$datapath,
      header = input$colDataHeader,
      sep = input$colDataSep,
      quote = input$colDataQuote
    )
  })
  
  # Expression Data Summary ####
  output$expressionSummary<-DT::renderDataTable(
    expressionData()[,c(1:input$expressionNumber)],
    options=list(scrollX=T,scroller=T)
  )
  # Col Data Summary ####
  output$colDataSummary<-DT::renderDataTable(
    colData()[,c(1:input$colDataNumber)],
    options=list(scrollX=T,scroller=T)
  )
  
  output$designChoices <- renderUI({
    # If missing input, return to avoid error later in function
    if(is.null(input$colDataFile))
      return()
    
    # Get the data set with the appropriate name
    designChoices <- colnames(colData())
    
    # Create the checkboxes and select them all by default
    checkboxGroupInput("userDesignChoice", "Choose columns", 
                       choices  = designChoices,
                       selected = NULL)
  })
  
  
  # DESeq ####
  dds<-reactive({
    if (is.null(input$expressionDataFile)) {
      # User has not uploaded a file yet
      return(NULL)
    }
  })
  reactive({
    if(input$beginDE!=0){
      output$action<-renderPrint("hello")
      expressionData <- expressionData[rowSums(expressionData) > 10,]
      expressionData <- expressionData[,order(colnames(expressionData))]
      dds <- DESeq2::DESeqDataSetFromMatrix(countData = expressionData,colData=colData,design = ~ input$userDesignChoice)
      dds <- DESeq2::estimateSizeFactors(dds)
      dds <- DESeq2::DESeq(dds)
      output$res <- DT::renderDataTable(
        results(dds)
      )
    }
  })
})








