expressionMatrix <- read.csv("~/Downloads/shNLGN4X-combined-gene-level-counts.csv",header = TRUE,row.names = 1,stringsAsFactors = FALSE)
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
