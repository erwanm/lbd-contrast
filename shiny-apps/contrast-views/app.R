
#
# EM July 21 - shiny code for contrast method applied to two views.
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

linkUserGuide <- "../2010-hidden-association-contrast-datasets.user-guide"

defaultMinJointFreq1 <- 10
defaultMaxJointFreq2 <- 0

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
showOrderedByList <- list("Basic contrast" = "basicContrast",
                          "Relative diff. rank 1 vs 2" = "diffRelRank",
                          "Absolute diff. rank 1 vs 2" = "diffAbsRank")

defaultSource <- 'KD'
defaultView1 <- 'abstracts+articles'
defaultLevel1 <- 'by-doc'
defaultView2 <- 'unfiltered-medline'
defaultLevel2 <- 'by-doc'
defaultMeasure <- 'pmi'
defaultMethod <- 'basicContrast'
  
ui <- fluidPage(
  # Application title
  titlePanel("Contrast Study with Associated Concepts"),
  
  fluidRow(
    column(4,
           wellPanel(
             helpText(tags$a(href=linkUserGuide,target="_blank", "User Guide")),
             fluidRow(
               radioButtons("sourceSelectionInput", "Source:",
                            sourcesList, 
                            selected = defaultSource),
               column(6,
                      helpText("Retrieve concepts with HIGH association in dataset 1:"),
                      radioButtons("viewSelectionInputHigh", "View:",
                                   viewsList, 
                                   selected = defaultView1),
                      radioButtons("coocLevelInputHigh", "Coocurrences level:",
                                   levelsList,
                                   selected = defaultLevel1)
               ),
               column(6,
                      helpText("Retrieve concepts with LOW association in dataset 2:"),
                      radioButtons("viewSelectionInputLow", "View:",
                                   viewsList, 
                                   selected = defaultView2),
                      radioButtons("coocLevelInputLow", "Coocurrences level:",
                                   levelsList,
                                   selected = defaultLevel2)
               )
             ),
             # target selection
             uiOutput("targetSelection"),
             fluidRow(
               column(6,
                      radioButtons("associationMeasure","Association measure",associationMeasureList,inline = TRUE, selected = defaultMeasure)
               ),
               column(6,
                      radioButtons("showOrderedBy", "method to rank CUIs:",
                                   choices = showOrderedByList, selected = "basicContrast")
               )
             ),
             fluidRow(
               column(6,
                      numericInput("minJointFreq1", label = "min joint freq in dataset 1:", value = defaultMinJointFreq1)
               ),
               column(6,
                      numericInput("maxJointFreq2", label = "max joint freq in dataset 2:", value = defaultMaxJointFreq2)
               )
             ),

             # semantic groups selection
             uiOutput("semanticTypesSelection")
           )
    ),
    column(8,
           DT::dataTableOutput("associatedCuisTable")
    )
  )
  
)


server <- function(input, output) {

  datasetPathHigh <- reactive({
    paste(as.character(input$sourceSelectionInput), 
          as.character(input$coocLevelInputHigh),
          as.character(input$viewSelectionInputHigh), 
          sep='/')  
  })
  
  datasetPathLow <- reactive({
    paste(as.character(input$sourceSelectionInput), 
          as.character(input$coocLevelInputLow),
          as.character(input$viewSelectionInputLow), 
          sep='/')  
  })

  dataSourceIsKD <- reactive({
    as.character(input$sourceSelectionInput) == 'KD'
  })
  
  targetsData <- reactive({
    if (!is.null(datasetPathHigh()) & !is.null(datasetPathLow())) {
      th <- getAllTargets(datasetPathHigh())
      tl <- getAllTargets(datasetPathLow())
      # select targets in common
      merge(th,tl,by=c('targetId','targetTerm','targetGroup'),all=TRUE,suffixes=c('.high','.low'))
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
  
  targetCuiRelationsHigh <- reactive({
    target <- input$targetInput
    if (!is.null(target)) {
      loadFullTargetCuiRelations(target, datasetPathHigh(),TRUE)
    }
  })

  targetCuiRelationsLow <- reactive({
    target <- input$targetInput
    if (!is.null(target)) {
      loadFullTargetCuiRelations(target, datasetPathLow(),TRUE)
    }
  })

  statsTargetAndViewHigh <- reactive({
    targetsDF <- targetsData()
    if (!is.null(targetsDF) ) {
      f <- targetsDF[as.character(targetsDF$targetId)==as.character(input$targetInput),'targetFreq.high']
      list(viewTotal=getViewTotal(datasetPathHigh()),targetFreq=f)
    }
  })

  statsTargetAndViewLow <- reactive({
    targetsDF <- targetsData()
    if (!is.null(targetsDF) ) {
      f <- targetsDF[as.character(targetsDF$targetId)==as.character(input$targetInput),'targetFreq.low']
      list(viewTotal=getViewTotal(datasetPathLow()),targetFreq=f)
    }
  })

  orderedByAssociationHigh <- reactive({
    orderConceptsByAssociation(targetCuiRelationsHigh(), statsTargetAndViewHigh(), input$associationMeasure)
  })
  orderedByAssociationLow <- reactive({
    orderConceptsByAssociation(targetCuiRelationsLow(), statsTargetAndViewLow(), input$associationMeasure)
  })
  

  contrastData <- reactive({
    #    print('contrastData')
    dh <- orderedByAssociationHigh()
    dl <- orderedByAssociationLow()
#    print(paste('contrastData: nrow(dh)=',nrow(dh),'; nrow(dl)=',nrow(dl)))
    print(colnames(dh))
    if (!is.null(dh) & !is.null(dl)) {
      method = input$showOrderedBy
      print(method)
      print(input$minJointFreq1)
      print(input$maxJointFreq2)
      d <- contrastViews(dh, dl, minJointFreqRef=input$minJointFreq1, maxJointFreqMask=input$maxJointFreq2, discardRowNotInMaskView=FALSE,methodId=method,mergeByCols=c('concept','term','group'), rankCol='rank', relRankCol='relRank', jointFreqCol='jointFreq')
#      print(paste('contrastData: nrow(d)=',nrow(d),'colnames(d)='))
#      print(colnames(d))
      d[,c('concept','term','group','rank','relRank','diffRelRank','diffRank','pmi.ref','pmi.mask','jointFreq.ref','jointFreq.mask')]
    }
  })
  

  availableSemanticGroups <- reactive({
    df <- contrastData()
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
    df <- contrastData()
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

