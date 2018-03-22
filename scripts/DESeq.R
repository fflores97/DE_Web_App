### DESeq2WriteDiff
DESeq2WriteDiff <- function (deseqData, diffColumn, group1, group2, outputFolder='./', outputFilePrefix, absLog2FCMin=1, padjFilter=0.1, geneDescCSV=NA)
{
  diffDF <- as.data.frame(results(deseqData, contrast = c(diffColumn, group1, group2)))
  diffDF$Gene.Symbol <- rownames(diffDF)
  diffDF <- diffDF[,c(7,1:6)]
  diffDF <- diffDF[!is.na(diffDF$log2FoldChange),]
  diffDF <- diffDF[!is.na(diffDF$padj),]
  diffDF <- diffDF[order(diffDF$padj, -abs(diffDF$log2FoldChange)),]

  
  diffDF2 <- diffDF[abs(diffDF$log2FoldChange) >= absLog2FCMin,]
  diffDF2 <- diffDF2[diffDF2$padj <= padjFilter,]
  if (!is.na(geneDescCSV))
  {
    geneDesc <- read.csv(geneDescCSV, stringsAsFactors = FALSE)
    diffDF2$Gene.Name <- geneDesc$Gene.Name[match(rownames(diffDF2), geneDesc$Gene.Symbol)]
    diffDF2$Gene.Desc <- geneDesc$Gene.Desc[match(rownames(diffDF2), geneDesc$Gene.Symbol)]
  }
  
 
  # temp1 <- tempfile(fileext=".tsv")
  # temp2 <- tempfile(fileext=".csv")
  # temp3 <- tempfile(fileext=".tsv")
  # temp4 <- tempfile(fileext=".tsv")
  # temp5 <- tempfile(fileext=".tsv")
  # temp6 <- tempfile(fileext=".csv")
  # 
  # 
  # write.table(diffDF[,c(1,7)], file = temp1, sep = '\t', quote = FALSE, row.names = FALSE)
  # write.csv(diffDF, file = temp2, row.names = FALSE)
  # write.table(diffDF2[,c(1,7)], file = temp3, sep = '\t', quote = FALSE, row.names = FALSE)
  # write.table(diffDF2[diffDF2$log2FoldChange >= 0,c(1,7)], file = temp4, sep = '\t', quote = FALSE, row.names = FALSE)
  # write.table(diffDF2[diffDF2$log2FoldChange < 0,c(1,7)], file = temp5, sep = '\t', quote = FALSE, row.names = FALSE)
  # write.csv(diffDF2, file = temp6, row.names = FALSE)
  # 
  # 
  # zip("file.zip",c(temp1,temp2,temp3,temp4,temp5,temp6))
  write.table(diffDF[,c(1,7)], file = paste(gsub('/$','',outputFolder),'/',outputFilePrefix,'.all.tsv', sep = ''), sep = '\t', quote = FALSE, row.names = FALSE)
  write.csv(diffDF, file = paste(gsub('/$','',outputFolder),'/',outputFilePrefix,'.all.csv', sep = ''), row.names = FALSE)
  write.table(diffDF2[,c(1,7)], file = paste(gsub('/$','',outputFolder),'/',outputFilePrefix,'.filtered.tsv', sep = ''), sep = '\t', quote = FALSE, row.names = FALSE)
  write.table(diffDF2[diffDF2$log2FoldChange >= 0,c(1,7)], file = paste(gsub('/$','',outputFolder),'/',outputFilePrefix,'.filtered.up.tsv', sep = ''), sep = '\t', quote = FALSE, row.names = FALSE)
  write.table(diffDF2[diffDF2$log2FoldChange < 0,c(1,7)], file = paste(gsub('/$','',outputFolder),'/',outputFilePrefix,'.filtered.down.tsv', sep = ''), sep = '\t', quote = FALSE, row.names = FALSE)
  write.csv(diffDF2, file = paste(gsub('/$','',outputFolder),'/',outputFilePrefix,'.filtered.csv', sep = ''), row.names = FALSE)
  return(diffDF2)
}