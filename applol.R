# library(shinydashboard)
# source("uploader.R")
# ui <- dashboardPage(
#   dashboardHeader(title = "DE Analysis"),
#   dashboardSidebar(
#     sidebarMenu(
#       menuItem("Uploader", tabName = "uploader", icon = icon("dashboard")),
#       menuItem("Widgets", tabName = "widgets", icon = icon("th"))
#     )
#   ),
#   dashboardBody(
#     tabItems(
#       # First tab content
#       tabItem(tabName = "uploader",
#               fluidRow(
#                 box(
#                   uploaderExpression(),
#                   "Please upload expression data (csv file) here. Genes should be row names and sample names should be column names"
#                     ),
#                 
#                 box(
#                   uploaderColData(),
#                   "Please upload column data here"
#                 )
#               )
#       ),
#       
#       # Second tab content
#       tabItem(tabName = "widgets",
#               h2("Widgets tab content")
#       )
#     )
#   )
# )
# 
# server <- function(input, output) {
#   expressionData<-readoutput$expressionDataFile
# }
# 
# shinyApp(ui, server)