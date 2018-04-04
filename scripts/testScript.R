expressionMatrix <- read.csv("~/Downloads/shNLGN4X-combined-gene-level-counts.csv",header = TRUE,row.names = 1,stringsAsFactors = FALSE)
infoTable <- read.csv("~/Downloads/infoTable.csv",header = TRUE)
pca <- samplePCA(expressionMatrix = expressionMatrix,numberOfPCs = 3)
sampleCorrelation <- print(sampleCorrelation(normalizedCountsTable = expressionMatrix))

pdf(paste(sessionDir,"/correlationTest.pdf", sep = ""), width = 12, height=5)
print(sampleCorrelation)
dev.off()

autoClustering <- FALSE
clusters <- 3
numberOfPCs <- 3


pcaPlots <- samplePCA(
  expressionMatrix,
  numberOfPCs = numberOfPCs,
  autoClustering = autoClustering,
  clusters = clusters
)