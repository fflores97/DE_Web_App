#' Title
#'
#' @param deseqData DESeq object 'dds' in most examples
#' @param diffColumn Column from the infoTable to be used to find groups
#' @param group1 Group 1 for comparison (must be a valid value in the infoTable)
#' @param group2 Group 2 for comparison (must be a valid value in the infoTable)
#' @param outputFolder Can choose an arbitrary folder (temps by default) 
#' @param outputFilePrefix All files exported will have this as a prefix
#' @param log2FCMin Minimum LFC to filter
#' @param padjFilter Max pvalue to filter genes
#'
#' @return A download handler to be used in shiny

downloadDESeq<-function(deseqData,diffColumn,group1,group2,outputFilePrefix="res",log2FCMin,padjFilter){
    
  downloadHandler(
      # filename = "res.csv",
      filename = "res.zip",
      content = function(file) {
        # Original - keep
        # write.csv(res, file, row.names = T)

        # deseqData = dds
        # diffColumn = input$userDesignChoiceDESeq
        # group1 = as.character(input$userGroup1DESeq)
        # group2 = as.character(input$userGroup2DESeq)
        # outputFolder = tempdir()
        # outputFilePrefix = input$filePrefixDESeq
        # log2FCMin = log2(input$absFCMinDESeq)
        # geneDescCSV = NA
        # padjFilter = input$pValueFilterDESeq
        outputFolder=tempdir()
        
        diffDF <- as.data.frame(results(deseqData, contrast = c(diffColumn, group1, group2)))
        diffDF$Gene.Symbol <- rownames(diffDF)
        diffDF <- diffDF[,c(7,1:6)]
        diffDF <- diffDF[!is.na(diffDF$log2FoldChange),]
        diffDF <- diffDF[!is.na(diffDF$padj),]
        diffDF <- diffDF[order(diffDF$padj, -abs(diffDF$log2FoldChange)),]
        diffDF2 <- diffDF[abs(diffDF$log2FoldChange) >= log2FCMin,]
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
}
