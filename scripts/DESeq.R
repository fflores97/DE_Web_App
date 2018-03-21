webAppDESeq<-function(input,output){
  output$designChoices <- renderUI({
    # If missing input, return to avoid error later in function
    # if(is.null(input$colDataFile))
      # return()
    
    # Get the data set with the appropriate name
    designChoices <- list("lol","lol2") #colnames(colDataFrame)
    
    # Create the checkboxes and select them all by default
    checkboxGroupInput("userDesignChoice", "Choose columns", 
                       choices  = designChoices,
                       selected = NULL)
    return(output)
  })
  
  
  # observeEvent( 
  #   input$beginDE,#Button beginDE triggers this
  #   {
  #     expressionData <- expressionData()[rowSums(expressionData()) > 10,]
  #     expressionData <- expressionData[,order(colnames(expressionData()))]
  #     output$beginningMessage<-renderText({"Beginning DESeq Transformations"})
  #     designFormula <- as.formula(paste("", paste(input$userDesignChoice, collapse=" + "), sep="~ "))
  #     dds <- DESeq2::DESeqDataSetFromMatrix(countData = expressionData(),colData=colData(),design = designFormula)
  #     output$sizeFactorMessage <- renderPrint({"Estimating Size Factors"})
  #     dds <- DESeq2::estimateSizeFactors(dds)
  #     output$DESeqMessage<-renderPrint({"Beginning DESeq"})
  #     dds <- DESeq2::DESeq(dds)
  #     res <- DESeq2::results(dds)
  #     output$DESeqFinishedMessage<-renderPrint({"Finished DESeq"})
  #     output$downloadData <- downloadHandler(
  #       filename = "res.csv",
  #       content = function(file) {
  #         write.csv(res, file, row.names = T)
  #       }
  #     )
  #   })
  # return(output)
}