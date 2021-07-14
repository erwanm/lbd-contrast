#
# EM Oct 20
#  new MI data: show most associated concepts given a target concept
#
# Update Nov 20 : re-implemented using brainmend-v2-PMI.R modules
# Update July 21: update with KD and PTC 

library(shiny)

source('../brainmend-v2-PMI.R',chdir=TRUE)
source('../../contrast-experiments.R')

linkUserGuide <- "../2010-concept-associations.user-guide"
defaultMinJointFreq <- 10


ui <- fluidPage(
  
  # Application title
  titlePanel("Top Associated Concepts with a Target Concept"),
  
  sidebarLayout(
    sidebarPanel(
      helpText(tags$a(href=linkUserGuide,target="_blank", "User Guide")),
      datasetSelectionInput("dataSelection","Select dataset"),
      targetSelectionInput("targetSelection"),
      fluidRow(
        column(6,
               numericInput("minJointFreq", label = "min joint freq:", value = defaultMinJointFreq)
        ),
        column(6,
               associationMeasuresInput("associationMeasure")
        )
      ),
      semanticTypesInput("semanticTypes")
#      viewOptionsInput("viewOptions")
    ),
    
    mainPanel(
      DT::dataTableOutput("associatedCuisTable")
    )
  )
)


server <- function(input, output) {

  datasetList <- datasetSelectionServer("dataSelection")  

  datasetPath <- reactive({
    l <- datasetList()
    paste(l$source, l$level, l$view,sep='/')  
  })

  dataSourceIsKD <- reactive({
    l <- datasetList()
    l$source == 'KD'
  })
    
  targetCUI <- targetSelectionServer("targetSelection", datasetPath)
  
#  semGranularity <- semGranularityServer("semGranularity")
  
  associationMeasure <- associationMeasuresServer("associationMeasure")
  
  targetCuiRelations <- reactive({
    if (!is.null(targetCUI()) & !is.null(datasetPath()) ) {
      loadFullTargetCuiRelations(targetCUI(), datasetPath(),TRUE)
    }
  })

  statsTargetAndView <- reactive({
    if (!is.null(datasetPath()) ) {
      targets <- getAllTargets(datasetPath())
      f <- targets[targets$targetId==targetCUI(),'targetFreq']
      list(viewTotal=getViewTotal(datasetPath()),targetFreq=f)
    }
  })
    
  orderedByAssociation <- reactive({
    df <- targetCuiRelations()
    if (!is.null(df) & !is.null(associationMeasure())) {
      df <- df[df$jointFreq>=input$minJointFreq,]
      statsTV <- statsTargetAndView()
      df$nbDocs <- rep(statsTV$viewTotal,nrow(df))
      df$targetFreq <- rep(statsTV$targetFreq, nrow(df))
      df <- calculateAssociation(df, filterMeasures=associationMeasure(),docFreq1Col='targetFreq',docFreq2Col='freq')
#      print(head(df))
#      print(associationMeasure())
      df <- df[order(-df[,associationMeasure()]),] 
#      df$rank <- rank(df[,associationMeasure()])
#      df$relRank <- df$rank / nrow(df)
    }
    df    
  })
  
    
  existingSemTypes <- reactive({
    d <- orderedByAssociation()
    if (!is.null(d)) {
      as.character(unique(d$group))
    }
  })
  
#  semTypes <- semanticTypesServer("semanticTypes", reactive({existingSemTypes()}))
  semTypes <- semanticTypesServer("semanticTypes", existingSemTypes)
  
  semTypesFilteredData <- reactive({
    d <- orderedByAssociation()
    d[d$group %in% semTypes(),]
#    granularityValue <- semGranularity()
#    if (granularityValue != 'unused') {
#      d <- d[d[,'typeId'] %in% semTypes(),]
#    }
#    d
  })

  selectColumns <- reactive({
    df<-semTypesFilteredData()
    # if (!is.null(df)) {
    #   #    df$firstTerm <- firstTermCol(df)
    #   keepColumns <- c("CUI" ,
    #                    "terms", 
    #                    "coarseCatId",
    #                    #                   "freqCui",
    #                    #                   "probCui",           
    #                    "jointFreq",
    #                    #                   "probJoint",
    #                    "probCuiGivenTarget",
    #                    "probTargetGivenCui",
    #                    #                   "terms",
    #                    "typeId",
    #                    #                   "rank",
    #                    #                   "relRank",
    #                    "PMI",
    #                    "NPMI",
    #                    "PMI^2",
    #                    "PMI^3",
    #                    "MI"
    #                    #    input$associationMeasure
    #   )
    #   df <- df[, keepColumns]
      df
#    }
  })
  
  
  finalData <- viewOptionsServer("viewOptions", reactive({selectColumns()}), reactive({associationMeasure() }), reactive({ dataSourceIsKD() }))
  
  output$associatedCuisTable <- DT::renderDataTable({
    d <- finalData()
    validate(need(d, 'Data not ready yet, please wait...'))
    d
    #    datatable(df) %>% formatPercentage(c("probCuiGivenTarget.high", "probTargetGivenCui.high","probCuiGivenTarget.low", "probTargetGivenCui.low"), 3)
    #    df[,c('CUI','firstTerm','probCuiGivenTarget.high','probTargetGivenCui.high','probCuiGivenTarget.low','probTargetGivenCui.low','MI')]
  },escape=FALSE,rownames= FALSE,options = list(  # escape=FALSE for displaying links as html
    # columnDefs = list(list(className = 'dt-center', targets = 5)),
    pageLength = 25
    #lengthMenu = c(5, 10, 15, 20)
  ))
  
}



# Run the application 
shinyApp(ui = ui, server = server)

