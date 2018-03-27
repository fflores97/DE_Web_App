# library(shinydashboard)
# library(DT)
library(DESeq2)
# source("scripts/DESeq.R")
options(shiny.trace=T)
shinyServer(function(input, output,session) {
  session$onSessionEnded(stopApp) # So shiny closes on closing window
  runcodeServer() #Only for testing

  # Expression Data Reader ####
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
        row.names = 1,
        check.names = F
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

# TODO: Fix reactive behavior here
  output$numberOfPCs <-renderUI({
    if(is.null(input$expressionDataFile))
      return()
    numericInput("numberOfPCs","Number of PCs to display",2)
  })

  observeEvent(
    input$beginPCA, {
    if (is.null(input$numberOfPCs))
      return()

    withProgress(message="Plotting PCA",value=1/2,{
        pcaPlots <- samplePCA(expressionData(),input$numberOfPCs)
        output$pcaImportancePlot<-renderPlot({pcaPlots$variancePlot})
        incProgress(3/4)
        output$pcaGridPlot<-renderPlot({pcaPlots$pcaGrid})
        incProgress(1)
    })



  })



  # Design choice checkboxes ####
  output$designChoicesDESeq <- renderUI({ #selects
    # If missing input, return to avoid error later in function
    if(is.null(input$colDataFile))
      return()

    designChoices <- colnames(colData()) #presents user the choice for DE design

    # Create the checkboxes and select none by default
    checkboxGroupInput("userDesignChoiceDESeq", "Choose columns",
                       choices  = designChoices,
                       selected = NULL)
  })



# TODO: Correlation plots
  # DESeq ####
  # User input for pValue, LFC, design of the experiment, and file Prefix
  output$pValueFilterDESeq <-renderUI({numericInput("pValueFilterDESeq","P Value",0.1)})
  output$absFCMinDESeq <-renderUI({numericInput("absFCMinDESeq","Minimum Absolute Fold Change",1)})
  output$userGroup1DESeq <-renderUI({
    chosenDesign1<-input$userDesignChoiceDESeq
    groupChoices1<- unique(colData()[chosenDesign1][[1]])
    radioButtons("userGroup1DESeq", "Choose Group 1",
                 choices  = groupChoices1,
                 selected = NULL)})
  output$userGroup2DESeq <-renderUI({
    chosenDesign2<-input$userDesignChoiceDESeq
    groupChoices2<- unique(colData()[chosenDesign2][[1]])
    radioButtons("userGroup2DESeq", "Choose Group 2 (control)",
                 choices  = groupChoices2,
                 selected = NULL)})
  output$filePrefixDESeq <-renderUI({textInput("filePrefixDESeq","File Prefix",value = "")})




  observeEvent(
    input$beginDE,# Button beginDE triggers this
    {
        if (is.null(input$userDesignChoiceDESeq)){
            return(NULL) # So app doesn't crash without user input
        }
        observe({
        withProgress(message = "DESeq in Progress",value=0,
        {
        # Filter data for counts that are too low
          expressionData <- expressionData()[rowSums(expressionData()) > 10,]
          expressionData <- expressionData()[,order(colnames(expressionData()))]
          # Generate design formula
          designFormula <- as.formula(paste("", paste(input$userDesignChoiceDESeq, collapse=" + "), sep="~ "))
          # Begin DESeq
          incProgress(1/4) # Progress indicators
          dds <- DESeq2::DESeqDataSetFromMatrix(countData = expressionData(),colData=colData(),design = designFormula)
          incProgress(1/2)
          dds <- DESeq2::estimateSizeFactors(dds)
          incProgress(3/4)
          dds <- DESeq2::DESeq(dds)
          # Message to the user
          output$DESeqFinishedMessage<-renderText("DESeq Finished!")
          # Download a zip file with the summarized results
          output$downloadDESeqHandler<-downloadDESeq(deseqData=dds,diffColumn=input$userDesignChoiceDESeq,group1=as.character(input$userGroup1DESeq),group2=as.character(input$userGroup2DESeq),outputFilePrefix=input$filePrefixDESeq,absLog2FCMin=log2(input$absFCMinDESeq),padjFilter=input$pValueFilterDESeq)

          # Download Button
          output$downloadDESeqResults<-renderUI({
            downloadButton("downloadDESeqHandler","Download Results")
          })
        })
      })
    })
})
