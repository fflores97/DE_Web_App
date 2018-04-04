
gridPlot <- pca$pcaPlot
importancePlot <- pca$importancePlot
outputFolder <- tempdir()
plotNames <- c("pcaPlot", "importancePlot")

downloadPlots <- function(..., plotNames){

  
  # fileNames <- sapply(
  #   X = plots, 
  #   function(x) {
  #     paste("~/Desktop/testingR/", names(x), ".pdf", sep = "")
  #     # paste(gsub('/$','',outputFolder),'/',names(x),'.pdf', sep = '')
  # })
  # 
  # fileNames <- plots %>% 
  #   imap(.x = .,
  #        .f =  function(.x){
  #     paste("~/Desktop/testingR/", LETTERS[.x], ".pdf", sep = "")
  #   })
  
  downloadHandler(
    filename = "RNASeq-plots.zip",
    content = function(file){
      # sapply(seq_along(plots),function(i){ggsave(filename = fileNames[i], device = "pdf",plot = x)})
      plots <- list(...)
      outputFolder=tempdir()
      
      # plots <- list(gridPlot,importancePlot)
      names(plots) <- plotNames
      # fileNames <- paste("~/Desktop/testingR/", names(plots),".pdf")
      fileNames <- paste(gsub('/$','',outputFolder),'/', plotNames, sep = '')
      mapply(function(plots, i) {ggsave(filename = fileNames[i], device = "pdf", plot = plots)}, plots = plots, i = 1:length(fileNames))
      
      zip(zipfile = file, files = fileNames)
    }
    
  )
}



