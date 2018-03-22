# library(shinydashboard)
# library(DT)
library(DESeq2)
# source("scripts/DESeq.R")
options(shiny.trace=T)
shinyServer(function(input, output,session) {
  session$onSessionEnded(stopApp) # So shiny closes on closing window
  runcodeServer() #Only for testing
  # Expression Data Reader
  expressionData<-eventReactive(
    input$uploadExpression, #File is only processed once user clicks a button
    {
      if (is.null(input$expressionDataFile)) {
        # User has not uploaded a file yet
        return(NULL)
      }
      inFileExpression<-input$expressionDataFile #just how the import system works
      read.csv(
        file =inFileExpression$datapath, #access to file
        header=input$expressionHeader, #asking user if header should be included
        sep=input$expressionSep, #asking user for separator
        quote=input$expressionQuote, #asking user for quote
        stringsAsFactors = F,
        row.names = 1
      )
    })
  
  
  # Expression Data Summary ####
  output$expressionSummary<-DT::renderDataTable(
    expressionData()[1:100,c(1:input$expressionNumber)], # makes a js table that is nicer to explore
    options=list(scrollX=T,scroller=T)
  )
  
  # Column Data Reader ####
  colData<-reactive({ # Same as before
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
  output$colDataSummary<-DT::renderDataTable( # Same as before
    colData()[,c(1:input$colDataNumber)],
    options=list(scrollX=T,scroller=T)
  )
  
  designChoices <- colnames(colData()) #presents user the choice for DE design
  groupChoices<- unique(designChoices)
  
  output$designChoicesDESeq <- renderUI({  #selects 
    # If missing input, return to avoid error later in function
    if(is.null(input$colDataFile))
      return()
    # Create the checkboxes and select none by default
    checkboxGroupInput("userDesignChoiceDESeq", "Choose columns", 
                       choices  = designChoices,
                       selected = NULL)
    numericInput("pValueFilterDESeq","P Value")
    numericInput("absFCMinDESeq","Minimum Absolute Fold Change")
    radioButtons("userGroup1DESeq", "Choose Group 1", 
                       choices  = groupChoices,
                       selected = NULL)
    radioButtons("userGroup2DESeq", "Choose Group 2", 
                 choices  = groupChoices,
                 selected = NULL)
    textInput(filePrefixDESeq,"File Prefix")
  })
  
  # DESeq ####
  observeEvent( 
    input$beginDE,# Button beginDE triggers this
    {
      observe({
        withProgress(message = "DESeq in Progress",value=0,
        {
          expressionData <- expressionData()[rowSums(expressionData()) > 10,]
          expressionData <- expressionData()[,order(colnames(expressionData()))]
          designFormula <- as.formula(paste("", paste(input$userDesignChoiceDESeq, collapse=" + "), sep="~ "))
          dds <- DESeq2::DESeqDataSetFromMatrix(countData = expressionData(),colData=colData(),design = designFormula)
          dds <- DESeq2::estimateSizeFactors(dds)
          dds <- DESeq2::DESeq(dds)
          res <- DESeq2::results(dds)
          
          for (choices in input$userDesignChoiceDESeq){
            differentialRes[[choices]] <- DESeq2WriteDiff(
              deseqData = dds, 
              diffColumn = choices, 
              group1 = as.character(input$userGroup1DESeq), 
              group2 = as.character(input$userGroup2DESeq), 
              outputFolder = tempdir(), 
              outputFilePrefix = input$filePrefixDESeq, 
              absLog2FCMin = log2(input$absFCMinDESeq), 
              geneDescCSV = NA, 
              padjFilter = input$pValueFilterDESeq
            )
          }
          
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








