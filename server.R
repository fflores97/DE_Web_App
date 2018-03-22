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
    radioButtons("userGroup2DESeq", "Choose Group 2",
                 choices  = groupChoices2,
                 selected = NULL)})
  output$filePrefixDESeq <-renderUI({textInput("filePrefixDESeq","File Prefix",value = "")})
  

  
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
          # for (choices in input$userDesignChoiceDESeq){
          #  DESeq2WriteDiff( #Credits to Ekram here
              # deseqData = dds
              # diffColumn = input$userDesignChoiceDESeq
              # group1 = as.character(input$userGroup1DESeq)
              # group2 = as.character(input$userGroup2DESeq)
              # outputFolder = tempdir()
              # outputFilePrefix = input$filePrefixDESeq
              # absLog2FCMin = log2(input$absFCMinDESeq)
              # geneDescCSV = NA
              # padjFilter = input$pValueFilterDESeq
          #   )
          # }
          
          output$DESeqFinishedMessage<-renderText("DESeq Finished!")
          output$downloadDESeqHandler<-downloadHandler(
            # filename = "res.csv",
            filename = "res.zip",
            content = function(file) {
              # Original - keep
              # write.csv(res, file, row.names = T)
              
              deseqData = dds
              diffColumn = input$userDesignChoiceDESeq
              group1 = as.character(input$userGroup1DESeq)
              group2 = as.character(input$userGroup2DESeq)
              outputFolder = tempdir()
              outputFilePrefix = input$filePrefixDESeq
              absLog2FCMin = log2(input$absFCMinDESeq)
              geneDescCSV = NA
              padjFilter = input$pValueFilterDESeq
              diffDF <- as.data.frame(results(deseqData, contrast = c(diffColumn, group1, group2)))
              diffDF$Gene.Symbol <- rownames(diffDF)
              diffDF <- diffDF[,c(7,1:6)]
              diffDF <- diffDF[!is.na(diffDF$log2FoldChange),]
              diffDF <- diffDF[!is.na(diffDF$padj),]
              diffDF <- diffDF[order(diffDF$padj, -abs(diffDF$log2FoldChange)),]
              diffDF2 <- diffDF[abs(diffDF$log2FoldChange) >= absLog2FCMin,]
              diffDF2 <- diffDF2[diffDF2$padj <= padjFilter,]
              # if (!is.na(geneDescCSV))
              # {
              #   geneDesc <- read.csv(geneDescCSV, stringsAsFactors = FALSE)
              #   diffDF2$Gene.Name <- geneDesc$Gene.Name[match(rownames(diffDF2), geneDesc$Gene.Symbol)]
              #   diffDF2$Gene.Desc <- geneDesc$Gene.Desc[match(rownames(diffDF2), geneDesc$Gene.Symbol)]
              # }
              
              temp1 <- paste(gsub('/$','',outputFolder),'/',outputFilePrefix,'.all.tsv', sep = '')
              temp2 <- paste(gsub('/$','',outputFolder),'/',outputFilePrefix,'.all.csv', sep = '')
              temp3 <- paste(gsub('/$','',outputFolder),'/',outputFilePrefix,'.filtered.tsv', sep = '')
              temp4 <- paste(gsub('/$','',outputFolder),'/',outputFilePrefix,'.filtered.up.tsv', sep = '')
              temp5 <- paste(gsub('/$','',outputFolder),'/',outputFilePrefix,'.filtered.down.tsv', sep = '')
              temp6 <- paste(gsub('/$','',outputFolder),'/',outputFilePrefix,'.filtered.csv', sep = '')
              
              write.table(diffDF[,c(1,7)], file = temp1, sep = '\t', quote = FALSE, row.names = FALSE)
              write.csv(diffDF, file = temp2, row.names = FALSE)
              write.table(diffDF2[,c(1,7)], file = temp3, sep = '\t', quote = FALSE, row.names = FALSE)
              write.table(diffDF2[diffDF2$log2FoldChange >= 0,c(1,7)], file = temp4, sep = '\t', quote = FALSE, row.names = FALSE)
              write.table(diffDF2[diffDF2$log2FoldChange < 0,c(1,7)], file = temp5, sep = '\t', quote = FALSE, row.names = FALSE)
              write.csv(diffDF2, file = temp6, row.names = FALSE)
              
              
              zip(file,c(temp1,temp2,temp3,temp4,temp5,temp6))
            }
          )
          output$downloadDESeqResults<-renderUI({
            downloadButton("downloadDESeqHandler","Download Results")
          })
        })
      })
    })
})








