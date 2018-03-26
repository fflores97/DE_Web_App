shinyUI(
  dashboardPage(
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
        tabName = "intro",
        runcodeUI(code = "shinyjs::alert()",includeShinyjs = T)
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
            uploaderExpressionUI(),
            uploaderColDataUI()
          )
        ),
        fluidRow(
          box(
            title="Preview of Expression Data",
            solidHeader = TRUE,
            status="info",
            width = 12,
            DT::dataTableOutput("expressionSummary",width = "auto"))
        ),
        fluidRow(
          box(
            title="Preview of Col Data",
            solidHeader = TRUE,
            status="info",
            width = 12,
            DT::dataTableOutput("colDataSummary",width = "auto"))
        )
      ),
      # Summary ####
      tabItem(
        tabName = "summary",
        fluidRow(
          uiOutput("numberOfPCs"),
          plotOutput("pcaImportancePlot"),
          plotOutput("pcaGridPlot")
        )
      ),

      # DESeq2 #####
      tabItem(
        tabName="DESeq2",
        fluidRow(
          box(
            title="Select Design",
            solidHeader= TRUE,
            status="info",
            width=6,
            uiOutput("designChoicesDESeq"),
            uiOutput("pValueFilterDESeq"),
            uiOutput("absFCMinDESeq"),
            uiOutput("userGroup1DESeq"),
            uiOutput("userGroup2DESeq"),
            uiOutput("filePrefixDESeq"),
            actionButton("beginDE", label = "Begin DESeq")
          ),
          box(
            title="Download Results",
            solidHeader = TRUE,
            status="info",
            width=6,
            textOutput("DESeqFinishedMessage"),
            uiOutput("downloadDESeqResults")
          )
        )
      ),
      tabItem(
       tabName = "edgeR"
      )

    )
  )
))
