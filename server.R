library(shinydashboard)
library(DT)
library(DESeq2)
source("scripts/uploader.R")
source("scripts/DESeq.R")
options(shiny.trace=T)
shinyServer(function(input, output) {
  
  reactive({
    uploadServer<-uploaderServer()
    output$expressionSummary<-uploadServer$expressionSummary
    output$colDataSummary<-upload$colDataSummary
  })
  
  #webAppDESeq()
  
})








