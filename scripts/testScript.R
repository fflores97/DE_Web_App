expressionMatrix <- read.csv("~/Dropbox (Partners HealthCare)/DFCI_Rotation_Students-Bojana-Polyak-Lab/2017_Felipe_Flores_and_Nick_Harper/SUM159_NLGN4_RNAseq_Analysis_Felipe/shNLGN4X-combined-gene-level-counts.csv",header = TRUE,row.names = 1,stringsAsFactors = FALSE)
infoTable <- read.csv("~/Downloads/infoTable.csv",header = TRUE)
pca <- samplePCA(expressionMatrix = expressionMatrix,numberOfPCs = 3)
sampleCorrelation <- print(sampleCorrelation(normalizedCountsTable = expressionMatrix))

pdf(paste(sessionDir,"/correlationTest.pdf", sep = ""), width = 12, height=5)
print(sampleCorrelation)
dev.off()

autoClustering <- FALSE
clusters <- 2
numberOfPCs <- 3


# pca <- samplePCA(
#   expressionMatrix,
#   numberOfPCs = numberOfPCs,
#   autoClustering = autoClustering,
#   clusters = clusters
# )

expressionMatrix <- expressionMatrix[which(apply(expressionMatrix, 1, var)!=0), ]
pca <- stats::prcomp(t(expressionMatrix),center=TRUE,scale=TRUE)
pcaDF <- as_data_frame(pca$x,rownames = "Sample")
pcaDF <- pcaDF %>%
  dplyr::right_join(dplyr::data_frame(
    Sample=names(eclustering$cluster),
    Cluster=LETTERS[eclustering$cluster]
  ))

pca3dPlot <- pcaDF %>%
  plotly::plot_ly(x = ~PC1, y = ~PC2, z = ~PC3, text = ~Sample, mode="marker+text") 

pca3dPlot <- pca3dPlot %>%
  plotly::add_trace(data = pcaDF, color = ~Cluster, showlegend = FALSE, colors = "Paired")

colData <- infoTable

expressionMatrix <- expressionMatrix[rowSums(expressionMatrix) > 10,]
expressionMatrix <- expressionMatrix[,order(colnames(expressionMatrix))]

# Generate design formula
designFormula <- as.formula(paste("", paste("Treatment", collapse=" + "), sep="~ "))

# Begin DESeq
dds <- DESeq2::DESeqDataSetFromMatrix(countData = expressionMatrix, colData = colData, design = designFormula)
dds <- DESeq2::estimateSizeFactors(dds)
ddsSUM159NLGN4X.DF <- DESeq2WriteDiff(dds, 'Treatment', as.character(colData$Treatment[2]), as.character(colData$Treatment[1]), outputFolder = 'differential-gene-lists/', outputFilePrefix = 'diff_SUM159_shNLGN4X_Dox_vs_Control', absLog2FCMin = log2(absFCMin), geneDescCSV = NA, padjFilter = padjFilter)


normalizedCountsTable <- counts(dds, normalized = TRUE)



dds <- DESeq2::DESeq(dds)
res <- as.data.frame(DESeq2::results(dds, contrast = c("Treatment","dox_minus","dox_plus")))

absFCMin <- 2
padjFilter <- 0.05

  res = res 
  mainLabel = 'SUM159 shNLGN4X-1-2 Dox vs Control' 
  minAdjPvalLabel = Inf
  minAdjPvalLabelByTop = 15 
  sigCol = c('#cccccc', '#d3424e', '#e6939a') 
  colorFCFilter=absFCMin
  colorpAdjFilter=padjFilter


### DESeq2PlotVolcano
DESeq2PlotVolcano <- function(
  res, 
  mainLabel, 
  minAdjPvalLabel=Inf, 
  minAdjPvalLabelByTop=Inf, 
  sigCol, 
  adjPvalFilter=Inf, 
  colorFCFilter=2, 
  colorpAdjFilter=0.1, 
  relativeTo=''
)
{
  df <- res[,1:6]
  df$minusLog10Padj <- -log10(df$padj)
  df <- df[df$minusLog10Padj < adjPvalFilter,]
  df$gene <- NA
  df$significance <- 'Not significant'
  df[which(abs(df$log2FoldChange) >= log2(colorFCFilter) & df$minusLog10Padj < -log10(colorpAdjFilter)),'significance'] <- 'padj > 0.1 & fc >= 2X'
  df[which(abs(df$log2FoldChange) >= log2(colorFCFilter) & df$minusLog10Padj >= -log10(colorpAdjFilter)),'significance'] <- 'padj < 0.1 & fc >= 2X'
  for (i in 1:nrow(df))
  {
    df[i,'gene'] <- rownames(df)[i]
  }
  df <- df[order(df$padj),]
  
  mainPlot <- ggplot(df, aes(log2FoldChange, minusLog10Padj)) + 
    theme_bw() + 
    geom_point(size = 4, aes(col = significance)) + 
    # scale_color_manual(values = sigCol) + 
    theme(plot.margin = margin(0,0,10,10), axis.text=element_text(size=20), axis.title = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_rect(colour = 'black', size = 1.25), legend.position = 'none') + 
    geom_hline(yintercept = -log10(colorpAdjFilter), color = 'red', linetype = 'dashed') + 
    geom_vline(xintercept = log2(colorFCFilter), color = 'red', linetype = 'dashed') + 
    geom_vline(xintercept = -log2(colorFCFilter), color = 'red', linetype = 'dashed')
  
  if (minAdjPvalLabel <= max(df$minusLog10Padj))
  {
    mainPlot <- mainPlot + geom_text_repel(data = filter(df, minusLog10Padj >= minAdjPvalLabel), aes(label = gene), size = 6, fontface = 'bold', point.padding = unit(0.75, 'lines'), force = 2)
  }else
  {
    if (minAdjPvalLabelByTop != Inf)
    {
      mainPlot <- mainPlot + geom_text_repel(data = df[1:minAdjPvalLabelByTop,], aes(label = gene), size = 6, fontface = 'bold', point.padding = unit(0.75, 'lines'), force = 2)
    }
  }
  xTitle <- expression('Log'[2]*' of fold change')
  if (relativeTo != '') { xTitle <- paste('Log2 of fold change\n', '(in ', relativeTo, ')', sep = '')}
  marginalPlot(mainPlot = mainPlot, mainPlotDF = df, mainPlotXCol = 'log2FoldChange', mainPlotYCol = 'minusLog10Padj', mainTitle = mainLabel, mainXTitle = xTitle, mainYTitle = expression('Negative of log'[10]*' of adjusted p-value'))
}

DESeq2PlotVolcano <- function(res, mainLabel, minAdjPvalLabel=Inf, minAdjPvalLabelByTop=Inf, sigCol, adjPvalFilter=Inf, colorFCFilter=2, colorpAdjFilter=0.1, relativeTo='')
{
  df <- ggplot2::remove_missing(res)
  df <- df[,1:6]
  df$minusLog10Padj <- -log10(df$padj)
  df <- df[df$minusLog10Padj < adjPvalFilter,]
  df$gene <- NA
  df$significance <- 'Not significant'
  df[which(abs(df$log2FoldChange) >= log2(colorFCFilter) & df$minusLog10Padj < -log10(colorpAdjFilter)),'significance'] <- 'padj > 0.1 & fc >= 2X'
  df[which(abs(df$log2FoldChange) >= log2(colorFCFilter) & df$minusLog10Padj >= -log10(colorpAdjFilter)),'significance'] <- 'padj < 0.1 & fc >= 2X'
  for (i in 1:nrow(df))
  {
    df[i,'gene'] <- rownames(df)[i]
  }
  df <- df[order(df$padj),]
  
  mainPlot <- ggplot(df, aes(log2FoldChange, minusLog10Padj)) + theme_bw() + geom_point(size = 4, aes(col = significance)) + scale_color_manual(values = sigCol) + theme(plot.margin = margin(0,0,10,10), axis.text=element_text(size=20), axis.title = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_rect(colour = 'black', size = 1.25), legend.position = 'none') + geom_hline(yintercept = -log10(colorpAdjFilter), color = 'red', linetype = 'dashed') + geom_vline(xintercept = log2(colorFCFilter), color = 'red', linetype = 'dashed') + geom_vline(xintercept = -log2(colorFCFilter), color = 'red', linetype = 'dashed')
  if (minAdjPvalLabel <= max(df$minusLog10Padj))
  {
    mainPlot <- mainPlot + geom_text_repel(data = filter(df, minusLog10Padj >= minAdjPvalLabel), aes(label = gene), size = 6, fontface = 'bold', point.padding = unit(0.75, 'lines'), force = 2)
  }else
  {
    if (minAdjPvalLabelByTop != Inf)
    {
      mainPlot <- mainPlot + geom_text_repel(data = df[1:minAdjPvalLabelByTop,], aes(label = gene), size = 6, fontface = 'bold', point.padding = unit(0.75, 'lines'), force = 2)
    }
  }
  xTitle <- expression('Log'[2]*' of fold change')
  if (relativeTo != '') { xTitle <- paste('Log2 of fold change\n', '(in ', relativeTo, ')', sep = '')}
  marginalPlot(mainPlot = mainPlot, mainPlotDF = df, mainPlotXCol = 'log2FoldChange', mainPlotYCol = 'minusLog10Padj', mainTitle = mainLabel, mainXTitle = xTitle, mainYTitle = expression('Negative of log'[10]*' of adjusted p-value'))
}





