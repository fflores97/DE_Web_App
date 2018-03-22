# library(shinydashboard)
# library(DT)
library(DESeq2)
# source("scripts/DESeq.R")
options(shiny.trace=T)
shinyServer(function(input, output,session) {
  session$onSessionEnded(stopApp)
  runcodeServer()
  # Expression Data Reader
  expressionData<-eventReactive(
    input$uploadExpression,
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
  output$expressionSummary<-DT::renderDataTable(
    expressionData()[,c(1:input$expressionNumber)],
    options=list(scrollX=T,scroller=T)
  )
  
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
        quote = input$colDataQuote,
        row.names = 1
      )
    })
  
  
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
  DESeqFinished <- F
  observeEvent( 
    input$beginDE,#Button beginDE triggers this
    {
      observe({
        withProgress(message = "DESeq in Progress",value=0,
        {
          
          expressionData <- expressionData()[rowSums(expressionData()) > 10,]
          expressionData <- expressionData()[,order(colnames(expressionData()))]
          designFormula <- as.formula(paste("", paste(input$userDesignChoice, collapse=" + "), sep="~ "))
          dds <- DESeq2::DESeqDataSetFromMatrix(countData = expressionData(),colData=colData(),design = designFormula)
          dds <- DESeq2::estimateSizeFactors(dds)
          dds <- DESeq2::DESeq(dds)
          res <- DESeq2::results(dds)
          output$DESeqFinishedMessage<-renderText("DESeq Finished!")
          output$downloadDESeqHandler<-downloadHandler(
            filename = "res.csv",
            content = function(file) {
              write.csv(res, file, row.names = T)
            }
          )
          output$downloadDESeqResults<-renderUI({
            downloadButton("downloadDESeqHandler","Download Results")
          })
              
        
          
          
          
          
          
        })
        
      })
      
    })
  
  
})








