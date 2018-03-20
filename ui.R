shinyUI(dashboardPage(
  skin="purple",
  dashboardHeader(title = "DE Analysis"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Introduction",tabName = "intro"),
      menuItem("Uploader", tabName = "uploader", icon = icon("dashboard")),
      menuItem("Summary", tabName = "summary", icon = icon("th")),
      menuItem("Differential Expression",tabName="differentialExpression",
               menuSubItem("DESeq2",tabName = "DESeq2"),
               menuSubItem("EdgeR",tabName = "edgeR")
      )
    )
  ),
  dashboardBody(
    tabItems(
      # Introduction ####
      tabItem(
        tabName = "intro"
      ),
      
      # Uploader #### 
      tabItem(
        tabName = "uploader",
        fluidRow(
          box(
            title="Upload Data",
            solidHeader= TRUE,
            status="info",
            width=12,
            uploaderExpression(),
            uploaderColData()
          )
        ),
        fluidRow(
          box(
            title="Summary of Expression Data",
            solidHeader = TRUE,
            status="info",
            width = 12,  
            DT::dataTableOutput("expressionSummary",width = "auto"))
        ),
        fluidRow(
          box(
            title="Summary of Col Data",
            solidHeader = TRUE,
            status="info",
            width = 12,  
            DT::dataTableOutput("colDataSummary",width = "auto"))
        )
      ),
      # Summary ####
      tabItem(
        tabName = "summary"
        
      ),
      
      # DESeq2 #####
      tabItem(
        tabName="DESeq2",
        "Hello",
        uiOutput("designChoices"),
        actionButton("beginDE", label = "Action"),
        textOutput("action"),
        box(DT::dataTableOutput("res",width = "auto"))
      ),
      
      tabItem(
       tabName = "edgeR" 
      )
      
    )
  )
))