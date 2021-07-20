
#
# EM July 21 - shiny code for baseline view of concepts related to a target concept ordered by strength of association
#       
# Code based on previous versions but simplified:
#   - not using shiny modules anymore for the sake of simplicity
#   - modified so that both KD and PTC data sources are possible
#   - removed the option to select fine-grained UMLS categories
#   - removed filter by ND group for target
#

library(shiny)
library(shinyWidgets)
library(DT)
source('../../contrast-experiments.R')
source('../util.R')

linkUserGuide <- "../baseline-view.user-guide"
defaultMinJointFreq <- 10

sourcesList <- list('KD','PTC')
viewsList <- list("abstracts+articles"="abstracts+articles",
                  "abstracts only"="unfiltered-medline",
                  "articles only"="pmc-articles")
levelsList <- list("by paper"="by-doc",
                   "by sentence"="by-sent")
associationMeasureList <- c('SCP'='scp', 
                            'PMI'='pmi', 
                            'NPMI'='npmi',
                            'MI'='mi',
                            'NMI'='nmi', 
                            'PMI^2'='pmi2',
                            'PMI^3'='pmi3')

defaultSource <- 'KD'
defaultView <- 'unfiltered-medline'
defaultLevel <- 'by-doc'
defaultMeasure <- 'pmi'

ui <- fluidPage(
  
  # Application title
  titlePanel("Top Associated Concepts with a Target Concept"),
  
  sidebarLayout(
    sidebarPanel(
      helpText(tags$a(href=linkUserGuide,target="_blank", "User Guide")),
    
      # dataset selection
      radioButtons("sourceSelectionInput", "Source:",
                   sourcesList, 
                   selected = defaultSource),
      radioButtons("viewSelectionInput", "View:",
                   viewsList, 
                   selected = defaultView),
      radioButtons("coocLevelInput", "Coocurrences level:",
                   levelsList,
                   selected = defaultLevel),

      # target selection
      uiOutput("targetSelection"),
      
      # min joint freq and association measure
      fluidRow(
        column(6,
               numericInput("minJointFreq", label = "min joint freq:", value = defaultMinJointFreq)
        ),
        column(6,
               radioButtons("associationMeasure","Association measure",associationMeasureList,inline = TRUE, selected = defaultMeasure)
        )
      ),

      # semantic groups selection
      uiOutput("semanticTypesSelection")
    ),
    
    mainPanel(
      DT::dataTableOutput("associatedCuisTable")
    )
  )
)


server <- function(input, output) {

  datasetPath <- reactive({
    paste(as.character(input$sourceSelectionInput), 
          as.character(input$coocLevelInput),
          as.character(input$viewSelectionInput), 
          sep='/')  
  })
  
  dataSourceIsKD <- reactive({
    as.character(input$sourceSelectionInput) == 'KD'
  })
  
  targetsData <- reactive({
    if (!is.null(datasetPath())) {
      getAllTargets(datasetPath())
    }
  })
  
  output$targetSelection <- renderUI({
      cuis <- targetsData()
      if (is.null(cuis)) {
        print('info: undefined list of possible targets')
      } else {
        choices<-as.list(cuis$targetId)
        names(choices)<-paste(cuis$targetId, cuis$targetTerm,sep=' - ')
        selectInput("targetInput", 
                    label = "Select target concept",
                    choices = choices,
                    selected = choices[[1]])
      }
  })
  
  targetCuiRelations <- reactive({
    target <- input$targetInput
    if (!is.null(target)) {
      loadFullTargetCuiRelations(target, datasetPath(),TRUE)
    }
  })

  statsTargetAndView <- reactive({
    targetsDF <- targetsData()
    if (!is.null(targetsDF) ) {
      f <- targetsDF[as.character(targetsDF$targetId)==as.character(input$targetInput),'targetFreq']
      list(viewTotal=getViewTotal(datasetPath()),targetFreq=f)
    }
  })
  
  orderedByAssociation <- reactive({
    orderConceptsByAssociation(targetCuiRelations(), statsTargetAndView(), input$associationMeasure)
  })
  
  
  availableSemanticGroups <- reactive({
    df <- orderedByAssociation()
    if (!is.null(df)) {
      as.character(unique(df$group))
    }
  })
  
  output$semanticTypesSelection <- renderUI({
    g <- availableSemanticGroups()
    choicesAsList <- as.list(g)
    names(choicesAsList) <- g
    pickerInput(
      inputId = "semTypes",
      label = "Filter by semantic types",
      choices = choicesAsList,
      selected = choicesAsList,
      options = list(
        `actions-box` = TRUE,
        size = 10,
        `selected-text-format` = "count > 3"
      ),
      multiple = TRUE
    )
  })
  
  finalData <- reactive({
    df <- orderedByAssociation()
    if (!is.null(df)) {
      df$concept <- createCuiLink(df$concept,dataSourceIsKD())
      selectedGroups <- as.character(input$semTypes)
      df <- df[as.character(df$group) %in% selectedGroups,]
      df
    }
    
  })
  
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

