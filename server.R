# TODO: Add 3d PCA
# TODO: Eliminate Duplicate DESeq behavior for plots and then DESeq (find out how to use reactive variables from one observe in another one)
# TODO: EdgeR
# TODO: Heatmap for a selection of genes
# TODO: Documentation for introduction
# TODO: Check results with code
# TODO: Test with multiple data inputs
# TODO: Deploy and present to lab
# TODO: Write up for SCRB 91r

options(shiny.trace=T)
shinyServer(function(input, output,session) {
  
  # Setup -------------------------------------------------------------------
  
  
  session$onSessionEnded(stopApp) # So shiny closes on closing window
  runcodeServer() #Only for testing
  sessionDir <- tempdir()
  
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
        file = inFileExpression$datapath, #access to file
        header = input$expressionHeader, #asking user if header should be included
        sep = input$expressionSep, #asking user for separator
        quote = input$expressionQuote, #asking user for quote
        stringsAsFactors = FALSE,
        row.names = 1,
        check.names = FALSE
      )
    })
  
  # Expression Data Reader --------------------------------------------------
  
  
  # Expression Data Summary --------------------------------------------------
  output$expressionSummary<-DT::renderDataTable(
    expressionData()[1:100,c(1:input$expressionNumber)], # makes a js table that is nicer to explore
    options = list(scrollX = TRUE, scroller = TRUE)
  )
  
  # Column Data Reader --------------------------------------------------
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
  
  
  # Col Data Summary --------------------------------------------------
  output$colDataSummary<-DT::renderDataTable( # Same as before
    colData()[,c(1:input$colDataNumber)],
    options=list(scrollX=T,scroller=T)
  )
  
  # Design choice checkboxes --------------------------------------------------
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
  
  # PCA parameters ----------------------------------------------------------
  
  output$numberOfPCs <-renderUI({
    if(is.null(input$expressionDataFile))
      return()
    numericInput("numberOfPCs","Number of PCs to display",2)
  })
  
  output$automaticClusteringPCA <- renderUI({
    checkboxInput("automaticClusteringPCA", "Automatic k-means?")
  })
  
  output$clustersPCA <- renderUI({
    if (input$automaticClusteringPCA == FALSE)
      numericInput("clustersPCA", "Number of k-means clusters (1 is no clustering)", 1)
    else{
      clusters <- 1
    }
  })
  
  output$colorBy <- renderUI({
    # checkboxInput("colorByCluster", "Color by cluster?")
    radioButtons(
      inputId = "colorBy", 
      label = "Color by:", 
      choiceNames = c("Sample", "Cluster"), 
      choiceValues = c("sample", "cluster")
    )
  })
  
  # Correlation and PCA Plots --------------------------------------------------
  observeEvent(
    input$beginPCA, {
      if (is.null(input$numberOfPCs))
        return()
      
      observe({
        withProgress(message = "Plotting PCA", value = 0, {
          # DE Seq Stuff needed for PCA and correlation --------------------------------------------------
          expressionData <- expressionData()[rowSums(expressionData()) > 10,]
          expressionData <- expressionData()[,order(colnames(expressionData()))]
          
          # Generate design formula
          designFormula <- as.formula(paste("", paste(input$userDesignChoiceDESeq, collapse=" + "), sep="~ "))
          
          # Begin DESeq
          dds <- DESeq2::DESeqDataSetFromMatrix(countData = expressionData(), colData = colData(), design = designFormula)
          dds <- DESeq2::estimateSizeFactors(dds)
          normalizedCountsTable <- counts(dds, normalized = TRUE)
          
          # PCA plots --------------------------------------------------
          pcaPlots <- samplePCA(
            expressionMatrix = normalizedCountsTable,
            numberOfPCs = input$numberOfPCs,
            autoClustering = input$automaticClusteringPCA,
            clusters = input$clustersPCA,
            colorBy = input$colorBy
          )
          
          output$pcaImportancePlot <- renderPlot({pcaPlots$importancePlot})
          
          incProgress(1 / 4)
          
          output$pcaGridPlot <- renderPlot({pcaPlots$pcaPlot})
          
          output$pca3dPlot <- plotly::renderPlotly({pcaPlots$pca3dPlot})
          incProgress(1/2)
          
          # Correlation plot --------------------------------------------------
          correlationPlot <- sampleCorrelation(normalizedCountsTable)
          
          output$correlationPlot <- renderPlot({correlationPlot})
          
          incProgress(1)
          
          # Save correlation plot to temporary directory
          pdf(paste(sessionDir,"/correlationPlot.pdf", sep = ""), width = 12, height=5)
          print(sampleCorrelation)
          dev.off()
          
          # Similarly, save the other plots with ggsave
          pcaFileNames <- paste(sessionDir,"/",c("pcaImportancePlot.pdf", "pcaPlot.pdf"), sep = "")
          mapply(
            function(plots, i) { ggsave(filename = pcaFileNames[i], device = "pdf", plot = plots) }, 
            plots = pca[-4], 
            i = 1:length(pcaFileNames)
          )
          
          # Download handler will put all pdf files into a zip
          output$downloadPlotHandler <- downloadHandler(
            filename = "summaryPlots.zip",
            content = function(file){
              zip(zipfile = file, files = paste(sessionDir,"/",list.files(path = sessionDir, pattern = "*.pdf"), sep = ""))
            }
          )
          
          # Renders download button
          output$downloadPlotsButton <- renderUI(downloadButton("downloadPlotHandler", "Download Plots"))
        })
      })
    })
  
  # Design choice checkboxes ####
  
  # output$designChoicesDESeq <- renderUI({ #selects
  #   # If missing input, return to avoid error later in function
  #   if(is.null(input$colDataFile))
  #     return()
  # 
  #   designChoices <- colnames(colData()) #presents user the choice for DE design
  # 
  #   # Create the checkboxes and select none by default
  #   checkboxGroupInput("userDesignChoiceDESeq", "Choose columns",
  #                      choices  = designChoices,
  #                      selected = NULL)
  # })
  
  
  
  # DESeq --------------------------------------------------
  # User input for pValue, LFC, design of the experiment, and file Prefix
  output$pValueFilterDESeq <-renderUI({numericInput("pValueFilterDESeq","P Value",0.05)})
  
  output$absFCMinDESeq <-renderUI({numericInput("absFCMinDESeq","Minimum Absolute Fold Change",2)})
  
  output$userGroup1DESeq <-renderUI({
    chosenDesign1<-input$userDesignChoiceDESeq
    groupChoices1<- unique(colData()[chosenDesign1][[1]])
    radioButtons(
      inputId = "userGroup1DESeq", 
      label = "Choose Group 1",
      choices  = groupChoices1,
      selected = NULL
    )
  })
  
  output$userGroup2DESeq <-renderUI({
    chosenDesign2<-input$userDesignChoiceDESeq
    groupChoices2<- unique(colData()[chosenDesign2][[1]])
    radioButtons(
      inputId = "userGroup2DESeq", 
      label = "Choose Group 2 (control)",
      choices  = groupChoices2,
      selected = NULL
    )
  })
  
  output$filePrefixDESeq <-renderUI({textInput("filePrefixDESeq","File Prefix",value = "")})

  observeEvent(
    input$beginDE,# Button beginDE triggers analysis
    {
      if (is.null(input$userDesignChoiceDESeq)){
        return(NULL) # So app doesn't crash without user input
      }
      observe({
        withProgress(
          message = "DESeq in Progress",
          value=0, {
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
            output$downloadDESeqHandler <-
              downloadDESeq(
                deseqData = dds,
                diffColumn = input$userDesignChoiceDESeq,
                group1 = as.character(input$userGroup1DESeq),
                group2 = as.character(input$userGroup2DESeq),
                outputFilePrefix = input$filePrefixDESeq,
                log2FCMin = log2(input$absFCMinDESeq),
                padjFilter = input$pValueFilterDESeq
              )
            
            # Download Button
            output$downloadDESeqResults<-renderUI({downloadButton("downloadDESeqHandler","Download Results")})
          })
      })
    })
})
