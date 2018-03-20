# Expression Data ####
uploaderExpression<-function(){
  box(
    fileInput("expressionDataFile", "Choose Expression Data File",
              multiple = FALSE,
              accept = c("text/csv",
                         "text/comma-separated-values,text/plain",
                         ".csv")),
    # Horizontal line ----
    tags$hr(),
    
    # Checkbox if file has header ----
    checkboxInput("expressionHeader", "Header", TRUE),
    
    # Select separator ----
    radioButtons("expressionSep", "Separator",
                 choices = c(Comma = ",",
                             Semicolon = ";",
                             Tab = "\t"),
                 selected = ","),
    
    # Select quotes ----
    radioButtons("expressionQuote", "Quote",
                 choices = c(None = "",
                             "Double Quote" = '"',
                             "Single Quote" = "'"),
                 selected = '"'),
    # How many columns to display ---
    numericInput("expressionNumber", "Columns to Display", value = 2),
    footer="Please upload expression data file here"
  )
}

# Coldata ####
uploaderColData<-function(){
  box(
    fileInput("colDataFile", "Choose ColData File",
              multiple = FALSE,
              accept = c("text/csv",
                         "text/comma-separated-values,text/plain",
                         ".csv")),
    # Horizontal line ----
    tags$hr(),
    
    # Checkbox if file has header ----
    checkboxInput("colDataHeader", "Header", TRUE),
    
    # Select separator ----
    radioButtons("colDataSep", "Separator",
                 choices = c(Comma = ",",
                             Semicolon = ";",
                             Tab = "\t"),
                 selected = ","),
    
    # Select quotes ----
    radioButtons("colDataQuote", "Quote",
                 choices = c(None = "",
                             "Double Quote" = '"',
                             "Single Quote" = "'"),
                 selected = '"'),
    # How many columns to display ---
    numericInput("colDataNumber", "Columns to Display", value = 2),
    footer="Please upload column data file here"
  )
}