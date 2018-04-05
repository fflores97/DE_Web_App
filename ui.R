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
          runcodeUI(code = "shinyjs::alert()",includeShinyjs = T)
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
        # Summary -----------------------------------------------------------------
        tabItem(
          tabName = "summary",
          fluidRow(
            column(
              width=4,
              box(
                title="Parameters",
                solidHeader= TRUE,
                status="info",
                width=12,
                actionButton("beginPCA",label="PCA & Correlation Plots"),
                uiOutput("designChoicesDESeq"),
                # uiOutput("userGroup1DESeq"),
                # uiOutput("userGroup2DESeq"),
                uiOutput("numberOfPCs"),
                uiOutput("automaticClusteringPCA"),
                uiOutput("clustersPCA"),
                uiOutput("colorBy")
              )
            ),
            column(
              width=8,
              fluidRow(
                box(
                  title="PCA Plot",
                  solidHeader= TRUE,
                  status="info",
                  width=12,
                  plotOutput("pcaImportancePlot"),
                  plotOutput("pcaGridPlot"),
                  plotlyOutput("pca3dPlot")
                ),
                box(
                  title="Sample Correlation Plot",
                  solidHeader= TRUE,
                  width=12,
                  status="info",
                  plotOutput("correlationPlot"),
                  uiOutput("downloadPlotsButton")
                )
              )
              
              
              
            )
          )
        ),
        
        # DESeq -------------------------------------------------------------------
        tabItem(
          tabName="DESeq2",
          fluidRow(
            box(
              title="Select Design",
              solidHeader= TRUE,
              status="info",
              width=6,
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
