expressionMatrix <- read.csv("~/Downloads/shNLGN4X-combined-gene-level-counts.csv",header = TRUE,row.names = 1,stringsAsFactors = FALSE)
infoTable <- read.csv("~/Downloads/infoTable.csv",header = TRUE)
pca <- samplePCA(expressionMatrix = expressionMatrix,numberOfPCs = 3)
sampleCorrelation <- print(sampleCorrelation(normalizedCountsTable = expressionMatrix))
