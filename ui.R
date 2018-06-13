shinyUI(
  dashboardPage(
    skin="purple",
    dashboardHeader(title = "DE Analysis"),
    
    # Sidebar -----------------------------------------------------------------
    dashboardSidebar(sidebarMenu(
      menuItem("Introduction", tabName = "intro"),
      menuItem("Uploader", tabName = "uploader", icon = icon("dashboard")),
      menuItem("Summary", tabName = "summary", icon = icon("th")),
      menuItem(
        text = "Differential Expression",
        tabName = "differentialExpression",
        menuSubItem("DESeq2", tabName = "DESeq2"),
        menuSubItem("EdgeR", tabName = "edgeR")
      )
    )), 
    
    # Body --------------------------------------------------------------------
    dashboardBody(
      tabItems(
        # Introduction ------------------------------------------------------------
        tabItem(
          tabName = "intro",
          runcodeUI(code = "shinyjs::alert()", includeShinyjs = T)
        ),
        # Uploader ----------------------------------------------------------------
        tabItem(
          tabName = "uploader",
          fluidRow(
            box(
              title="Upload Data",
              solidHeader= TRUE,
              status="info",
              width=12,
                uploaderExpressionUI(),
                uploaderColDataUI(),
              box(
                title="Experiment Design",
                solidHeader= TRUE,
                status="info",
                width=12,
                uiOutput("designChoicesDESeq")
              )
            )
          ),
          fluidRow(
            box(
              title="Preview of Expression Data",
              solidHeader = TRUE,
              status="info",
              width = 12,
              DT::dataTableOutput("expressionSummary", width = "auto"))
          ),
          fluidRow(
            box(
              title="Preview of Col Data",
              solidHeader = TRUE,
              status="info",
              width = 12,
              DT::dataTableOutput("colDataSummary", width = "auto"))
          )
        ),
        # Summary -----------------------------------------------------------------
        tabItem(
          tabName = "summary",
          fluidRow(
            box(
              title = "Parameters",
              solidHeader = TRUE,
              status ="info",
              width = 12,
              column(
                width = 6,
                # uiOutput("designChoicesDESeq"),
                # uiOutput("userGroup1DESeq"),
                # uiOutput("userGroup2DESeq"),
                uiOutput("numberOfPCs"),
                uiOutput("automaticClusteringPCA"),
                actionButton("beginPCA", label = "PCA & Correlation Plots")
              ),
              column(
                width = 6,
                uiOutput("clustersPCA"),
                uiOutput("colorBy"),
                uiOutput("downloadPlotsButton")
              )
            )
          ),
          fluidRow(
            box(
              title = "PCA Plot",
              solidHeader = TRUE,
              status ="info",
              width = 12,
              plotOutput("pcaImportancePlot"),
              plotOutput("pcaGridPlot"),
              plotlyOutput("pca3dPlot"),
              plotOutput("pcaClusteringPlot")
            ),
            box(
              title="Sample Correlation Plot",
              solidHeader = TRUE,
              width = 12,
              status = "info",
              plotOutput("correlationPlot")
            )
          )
        ),
        
        # DESeq -------------------------------------------------------------------
        tabItem(
          tabName="DESeq2",
          fluidRow(
            box(
              title="Select Design",
              solidHeader = TRUE,
              status = "info",
              width = 6,
              # uiOutput("designChoicesDESeq"),
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
              status = "info",
              width = 6,
              textOutput("DESeqFinishedMessage"),
              uiOutput("downloadDESeqResults")
            )
          )
        ),
        tabItem(
          tabName = "edgeR"
        )
        # EdgeR -------------------------------------------------------------------
        ,tabItem(
          tabName = "EdgeR",
          fluidRow(
            # EdgeR goes here
          )
        )
        
        
        
      )
    )
  ))
