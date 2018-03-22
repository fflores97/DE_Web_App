### DESeq2WriteDiff
DESeq2WriteDiff <- function (deseqData, diffColumn, group1, group2, outputFolder='./', outputFilePrefix="", absLog2FCMin=1, padjFilter=0.1, geneDescCSV=NA)
{
  diffDF <- as.data.frame(results(deseqData, contrast = c(diffColumn, group1, group2)))
  diffDF$Gene.Symbol <- rownames(diffDF)
  diffDF <- diffDF[,c(7,1:6)]
  diffDF <- diffDF[!is.na(diffDF$log2FoldChange),]
  diffDF <- diffDF[!is.na(diffDF$padj),]
  diffDF <- diffDF[order(diffDF$padj, -abs(diffDF$log2FoldChange)),]
  write.table(diffDF[,c(1,7)], file = paste(gsub('/$','',outputFolder),'/',outputFilePrefix,'.all.tsv', sep = ''), sep = '\t', quote = FALSE, row.names = FALSE)
  write.csv(diffDF, file = paste(gsub('/$','',outputFolder),'/',outputFilePrefix,'.all.csv', sep = ''), row.names = FALSE)
  diffDF <- diffDF[abs(diffDF$log2FoldChange) >= absLog2FCMin,]
  diffDF <- diffDF[diffDF$padj <= padjFilter,]
  write.table(diffDF[,c(1,7)], file = paste(gsub('/$','',outputFolder),'/',outputFilePrefix,'.filtered.tsv', sep = ''), sep = '\t', quote = FALSE, row.names = FALSE)
  write.table(diffDF[diffDF$log2FoldChange >= 0,c(1,7)], file = paste(gsub('/$','',outputFolder),'/',outputFilePrefix,'.filtered.up.tsv', sep = ''), sep = '\t', quote = FALSE, row.names = FALSE)
  write.table(diffDF[diffDF$log2FoldChange < 0,c(1,7)], file = paste(gsub('/$','',outputFolder),'/',outputFilePrefix,'.filtered.down.tsv', sep = ''), sep = '\t', quote = FALSE, row.names = FALSE)
  if (!is.na(geneDescCSV))
  {
    geneDesc <- read.csv(geneDescCSV, stringsAsFactors = FALSE)
    diffDF$Gene.Name <- geneDesc$Gene.Name[match(rownames(diffDF), geneDesc$Gene.Symbol)]
    diffDF$Gene.Desc <- geneDesc$Gene.Desc[match(rownames(diffDF), geneDesc$Gene.Symbol)]
  }
  write.csv(diffDF, file = paste(gsub('/$','',outputFolder),'/',outputFilePrefix,'.filtered.csv', sep = ''), row.names = FALSE)
  return(diffDF)
}