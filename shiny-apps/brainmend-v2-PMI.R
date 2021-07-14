
#
# EM Nov 2020 - Custom Shiny module for BrainMend V.2 "PMI/MI visualization"
#
#    UPDATE July 2021: 
#     - modified so that both KD and PTC data sources are possible
#     - removed the option to select fine-grained UMLS categories
#     - removed filter by ND group
#
#  Note: the last update lead to a poor design of the module due to inconsistencies between the old and new
#        version. Ideally the whole module should be re-designed from scratch.
#
# * DATASET SELECTION 
#   - datasetSelectionInput:    UI radio buttons for abstracts/articles/both + by paper/sentence
#       arg defaultSource:        
#       arg defaultData:          Dataset selected by default (default: 'abstracts+article')
#       arg defaultLevel:         Level selected by default (default: 'by-sentence')
#
#   - datasetStringServer:      return reactive list (source,view,level)  
#
# * TARGET CONCEPT SELECTION
#   - targetSelectionInput:     UI selection for selecting target CUI
#   - targetSelectionServer:    returns the selected target CUI (character)
#
# * ASSOCIATION MEASURES
#   - associationMeasuresInput: UI for selecting the association measure
#      arg radioButtons:          boolean whether UI as radio buttons (default True)
#      arg radioButtonsInline:    boolean whether radio buttons one the same line (default True)
#      arg selected:              character for the default measure (default 'PMI')
# - associationMeasuresServer:  return the selected measure as character
#
# * SEMANTIC GRANULARITY 
#   - semGranularityInput:      UI selection for 'unused', 'coarse', 'detailed'
#   - semGranularityServer:     returns the selected granularity value
#
# * SEMANTIC TYPES SELECTION
#   - semanticTypesInput:       UI selection of a group of semantic types, where the list is based on
#                               the granularity value given as input (granularity UI not included)
#   - semanticTypesSever:       returns a vector of selected semantic types (always detailed 'typeId')
#     arg semGranularityValue:    value returned by 'semGranularityServer', which must be called 
#                                 independently.
#     arg allowedTypes:           typically obtained from an input dataframe as:
#                                     allowedTypes <- as.character(unique(d[,'typeId']))
#                              IMPORTANT: both arguments should be transmitted as reactive values.
#
# * VIEW OPTIONS
#   - viewOptionsInput:        UI checkboxes for visuaization options: all term, concepts groups, 
#                              show all association values, show conditional probs.
#   - viewOptionsServer:       given the input data, returns a dataframe after view options have been 
#                              applied. Should be called after selecting the columns to keep, because the
#                              caller does not know which columns are left after the process.
#                              The data must contain 'typeId' as input, it is removed in the output.
#                              The data should contain 'terms', not 'firstTerm'.
#     arg data:                  input dataframe provided as reactive value.
#     arg associationMeasure:    selected measure provided as reactive value.
#     arg associationCols:       a list in which every possible association measure is the name of an element and this element
#                                consists of a character vector of columns names for this measure. Default: each association
#                                measure has only one column name, same name as the measure.
#     arg columnsCondi:          names of the conditional prob columns 
#     arg nbDigitsFloat:         nb digits for float values (cond probs, PMI and/or MI)
#
#
# * INDEPENDENT HELPER FUNCTIONS
#
#   - getAllTargets:            Returns the list of ND concepts with terms, firstTerm, types and ND group
#                               after reading all the files <ND>-details.list. The "group concepts" are
#                               also added.
#
#   - computeSemTypesDF:        Returns a dataframe containing the ids and names of the semantic types
#                               depending on (1) allowed types in 'allowedTypes' and (2) the granularity
#                               user choice 'semTypeGranularitySelect'.
#       arg semTypeGranularitySelect: 'coarse' or 'detailed' (cannot be 'unusued')
#       arg semDF:                    list of UMLS semantic types obtained by:  readUMLSSemanticTypes(umlsSemTypesFile)
#       arg allowedTypes:             list of proposed types, obtained with:  as.character(unique(d[,'typeId']))
#
#   - loadFullTargetCuiRelations: loads a full relations file 
#       arg cui:                    the CUI target (can be NULL)
#       arg dataName:               a string <data id>.<level id> as returned by 'datasetStringServer'
#       arg semGranularityValue:    value returned by 'semGranularityServer', default 'unused'
#       arg orderByCol:             column name for ordering dataframe, default 'PMI'
#       arg minJointFreq:           minimum joint frequency, default 0
#
#   - createCuiLink:            Given a CUI id, returns an HTML string with this id link to the
#                               corresponding UMLS.
#
#
#
#

#
#

library(shiny)
library(shinyWidgets)
library(DT)
library(data.table)


dataDir <- '../../../ND-hierarchy-data-for-rankings'
#pathNDConcepts <- 'ND-concepts'
#pathExtractedRelationsByConcept <- 'extracted-relations-by-cui'


##### DATASET SELECTION

sourcesList <- list('KD','PTC')
viewsList <- list("abstracts+articles"="abstracts+articles",
                     "abstracts only"="unfiltered-medline",
                     "articles only"="pmc-articles")
levelsList <- list("by paper"="by-doc",
                   "by sentence"="by-sent")


datasetSelectionInput <- function(id, label = "Data Selection", defaultSource='KD',defaultView="abstracts+articles", defaultLevel='by-sent') {
  
  ns <- NS(id)
  tagList(
    radioButtons(ns("sourceSelectionInput"), "Source:",
                 sourcesList, 
                 selected = defaultSource),
    radioButtons(ns("viewSelectionInput"), "View:",
             viewsList, 
             selected = defaultView),
    radioButtons(ns("coocLevelInput"), "Coocurrences level:",
             levelsList,
             selected = defaultLevel)
  )
  
}


datasetSelectionServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    return(
      reactive({ 
        list(source=input$sourceSelectionInput,view=input$viewSelectionInput,level=input$coocLevelInput) 
      })
    )
  })
}



##### ND GROUPS SELECTION

# groupChoices <-c('AD','ALS','Autism', 'CBD','FTD','MSA','Other','PD','PSP','SCZ')
# groupChoicesNoOther <-c('AD','ALS','Autism', 'CBD','FTD','MSA','PD','PSP','SCZ')

#groupsSelectionInput <- function(id, label = "Filter by ND group", availableGroups=groupChoices) {
#  ns <- NS(id)
#  checkboxGroupInput(ns("groupsCheckboxInput"), label, 
#                     choiceNames = availableGroups,
#                     choiceValues = availableGroups,
#                     selected = availableGroups,inline=TRUE)
#}

#selectedGroupsServer <- function(id) {
#  moduleServer(id, function(input, output, session) {
#    return(
#      reactive({
#        input$groupsCheckboxInput
#      })
#    )
#  })
#}

##### TARGET CONCEPT SELECTION


targetSelectionInput <- function(id) {
  ns <- NS(id)
  tagList(
#    groupsSelectionInput(ns('groupsSelection')),
    uiOutput(ns("targetCuiSelection"))
  )
}


targetSelectionServer <- function(id, viewDir) {
  moduleServer(id, function(input, output, session) {
    
    ns <- session$ns
#    selectedGroups <- selectedGroupsServer('groupsSelection')
    
    getPossibleTargets <- reactive({
#      groups <- selectedGroups()
#      if (is.null(groups)) {
#        print('info: undefined list of selected groups')
#      } else {
#        targets <- getAllTargets()
#        targets[targets$group %in% selectedGroups() | targets$group == 'ALL',]
#      }
      getAllTargets(viewDir())
    })
    
    output$targetCuiSelection <- renderUI({
      cuis <- getPossibleTargets()
      if (is.null(cuis)) {
        print('info: undefined list of possible targets')
      } else {
        choices<-as.list(cuis$targetId)
        names(choices)<-paste(cuis$targetId, cuis$targetTerm,sep=' - ')
        selectInput(ns("targetCuiInput"), 
                  label = "Select target concept",
                  choices = choices,
                  selected = choices[[1]])
      }
    })
    
    
    return(
      reactive({
        input$targetCuiInput
      })
    )
  })
}


##### ASSOCIATION MEASURE 

associationMeasureList <- c('SCP'='scp', 
                 'PMI'='pmi', 
                 'NPMI'='npmi',
                 'MI'='mi',
                 'NMI'='nmi', 
                 'PMI^2'='pmi2',
                 'PMI^3'='pmi3')


associationMeasuresInput <- function(id, radioButtons=TRUE, radioButtonsInline=TRUE, selected='pmi') {
  ns <- NS(id)
  if (radioButtons) {
    radioButtons(ns("associationMeasure"),"Association measure",associationMeasureList,inline = radioButtonsInline, selected = selected)
  } else {
    selectInput(ns("associationMeasure"),
                label = "Association measure",
                choices = associationMeasureList,
                selected = selected
    )
  }
}


associationMeasuresServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    return(
      reactive({
        input$associationMeasure
      })
    )
  })
}


##### SEMANTIC GRANULARITY

#granularityChoices = c('unused', 'coarse', 'detailed')

#semGranularityInput <- function(id) {
#  ns <- NS(id)
#  selectInput(ns("semTypeGranularitySelect"),
#                      label = "Granularity of the semantic categories",
#                      choices = granularityChoices,
#                      selected = granularityChoices[1]
#             )
#}

#semGranularityServer <- function(id) {
#  moduleServer(id, function(input, output, session) {
#    return(
#      reactive({
#        input$semTypeGranularitySelect
#      })
#    )
#  })
#}



##### SEMANTIC TYPES SELECTION

#umlsSemTypesFile <- paste0(dataDir, '/','SemGroups.txt')

semanticTypesInput <- function(id) {
  ns <- NS(id)
  uiOutput(ns("semanticTypesSelection"))
}

#semanticTypesServer <- function(id, semGranularityValue, allowedTypes) {
semanticTypesServer <- function(id, allowedTypes) {
    moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # this is needed to avoid that the selection is reset every time the data changes
#    lastSemTypeSelection <- reactiveValues()
    
    
    # raw UMLS sem types 
 #   getSemTypesData <- reactive({
#      if (semGranularityValue() != 'unused') {
#        print(paste('Info: reading semantic types from ',umlsSemTypesFile))
#        df <- readUMLSSemanticTypes(umlsSemTypesFile)
 #       print(paste("Info: nrow UMLS sem types:",nrow(df)))
#        df
#      }
#    })
    
    # obtain list of sem types proposed to the user in the UI
#    getSemTypesDF <- reactive({
 #     if (semGranularityValue() != 'unused') {
#        s <- getSemTypesData()
        #    print(unique(s[,c('coarseCatId','coarseCatName')]))
#        df <- computeSemTypesDF(semGranularityValue(), s, allowedTypes())
#        print(paste('Info: getSemTypesDF; nrow =',nrow(df)))
#        df
#      }
#    })
    
    currentlySelectedTypes <- reactive({
#      if (semGranularityValue() != 'unused') {
#        lastSelec <- lastSemTypeSelection[['default']]
        typesChoices <- allowedTypes()
#        typesChoices <- getSemTypesDF()
#        if (!is.null(lastSelec)) {
#          print('info: reusing previously selected types:')
#          lastSelec[lastSelec %in% as.character(typesChoices)]
#        } else {
#          print('info: default all selected types')
          as.character(typesChoices)
#        }
#      }
    })
    
    # UI element for selection a group of sem types, either coarse or detailed
    output$semanticTypesSelection <- renderUI({
#      if (semGranularityValue() == 'unused') {
#        helpText("Select a 'granularity' value to enable semantic type selection")
#      } else {
#        print('info: recalculating sem types selection box')
        typesChoices <- allowedTypes()
#              checkboxGroupInput(ns("semTypes"), label = "Filter by semantic types",
#                                 choiceNames = as.character(typesChoices[,'name']),
#                                 choiceValues = as.character(typesChoices[,'id']),
#                                 selected = as.character(typesChoices[,'id']))
        choicesAsList <- as.list(typesChoices)
        names(choicesAsList) <- typesChoices
        pickerInput(
          inputId = ns("semTypes"),
          label = "Filter by semantic types",
          choices = choicesAsList,
          selected = currentlySelectedTypes(),
          options = list(
            `actions-box` = TRUE,
            size = 10,
            `selected-text-format` = "count > 3"
          ),
          multiple = TRUE
        )
        
        
#      }
    })
    
#    observeEvent(input$semTypes, {
#      if (!is.null(input$semTypes) ) {
#        lastSemTypeSelection[['default']] <- input$semTypes
#      }
#    })
    
    # obtain the list of selected sem types as a list of 'detailed' sem types
    getSelectedSemTypes <- reactive({
#      if (semGranularityValue() != 'unused') {
#        s <- getSemTypesData()
#        r <- if (semGranularityValue() == 'coarse') {
#          coarseSemTypes <- input$semTypes
#          s[s[,'coarseCatId'] %in% coarseSemTypes, 'typeId']
#        } else {
          input$semTypes
#        }
#        print(paste('Info: getSelectedSemTypes; nrow =',nrow(r)))
#        print(r)
#        r
#      }
    })
    
    
    return(
      reactive({
#        print(getSelectedSemTypes())
        as.character(getSelectedSemTypes())
      })
    )
  })
}


#### VIEW OPTIONS

viewOptionsChoices <- list("Show all terms" = "all-terms", 
                           "Show concepts groups" = "concepts-groups",
                           "Show all association values" = "show-all-association-values",
                           "Show conditional probabilities" = "show-condi")
viewOptionsDefault <- c("concepts-groups", "show-condi")

viewOptionsInput <- function(id) {
  ns <- NS(id)
  checkboxGroupInput(ns("viewOptionsInput"), "View options", 
                     choices = viewOptionsChoices,
                     selected = viewOptionsDefault)
}

viewOptionsServer <- function(id, data, associationMeasure,  umlsForLinks) {
  moduleServer(id, function(input, output, session) {
    
    applyViewFilters <- reactive({
      df<-data()
      if (!is.null(df) & (!is.null(associationMeasure()))) {
#        print('apply view filters start')
#        print(nrow(df))
        df$concept <- createCuiLink(df$concept,umlsForLinks())
#        if (! "all-terms" %in% input$viewOptionsInput) {
#          df$terms <- firstTermCol(df)
#          colnames(df)[which(colnames(df)=='terms')] <- 'firstTerm'
#        }
#        if (! "concepts-groups" %in% input$viewOptionsInput) {
#          df <-df[!is.na(df$typeId),]
#        }
#        allAssociationCols <- unlist(associationCols)
#        print(associationCols)
#        print(associationCols[[associationMeasure()]])
#         if (! "show-all-association-values" %in% input$viewOptionsInput) {
#           # restrict columns to the ones for the selected association measure
#           selectedAssocCols <- associationCols[[associationMeasure()]]
#           removeAssocCols <- allAssociationCols[!(allAssociationCols %in% selectedAssocCols)]
#           df <- df[,colnames(df)[!colnames(df) %in% removeAssocCols]]
#           df[,selectedAssocCols] <- round(df[,selectedAssocCols],digits=nbDigitsFloat)
#         } else {
#           df[,allAssociationCols] <- round(df[,allAssociationCols],digits=nbDigitsFloat)
#         }
#         if (! "show-condi" %in% input$viewOptionsInput) {
#           df <- df[,colnames(df)[!colnames(df) %in% columnsCondi]]
#         } else {
#           df[,columnsCondi] <- round(df[,columnsCondi],digits=nbDigitsFloat)
#         }
# #        print('apply view filters end')
# #        print(nrow(df))
#         df[,colnames(df)[colnames(df) != 'typeId']]
      }
      df
    })
    
    return(
      reactive({
        applyViewFilters()
      })
    )
    
  })
}


##### INDEPENDENT HELPER FUNCTIONS

#
# returns targets df: 'targetId','targetFreq','targetTerm','targetGroup'
#
getAllTargets <- function(viewDir, targetsFilename='targets.tsv') {
  path <- paste(dataDir,viewDir, targetsFilename,sep='/')
  # print(path)
  df <- read.table(path, sep='\t', quote="", comment.char = "", row.names=NULL,stringsAsFactors = FALSE)
  colnames(df) <- c('targetId','targetFreq','targetTerm','targetGroup')
  df
}

#
# returns single value = total nb documents
#
getViewTotal <- function(viewDir, totalFilename='total.tsv') {
  path <- paste(dataDir,viewDir, totalFilename,sep='/')
  df <- read.table(path, sep='\t', quote="", comment.char = "", row.names=NULL,stringsAsFactors = FALSE)
  # nb docs is the second column:
  df[,2]
}

# OLD VERSION
#   - getAllTargets:            Returns the list of ND concepts with terms, firstTerm, types and ND group
#                               after reading all the files <ND>-details.list. The "group concepts" are
#                               also added.
# getAllTargets <- function(pathDetailsConceptFiles=pathNDConcepts) {
#   pattern <- paste(dataDir,pathDetailsConceptFiles,'*-details.list',sep='/')
#   files <- Sys.glob(pattern)
#   if (length(files) == 0) {
#     stop(paste('Error: no files found with pattern',pattern))
#   }
#   df <- ldply(files, function(f) {
#     #      print(paste('info: reading',f))
#     fbase <- last(strsplit(f,'/',fixed = TRUE)[[1]])
#     thisGroup <- sub(fbase,pattern='-details.list',replacement='',fixed=TRUE)
#     df <- read.table(f,header=TRUE, sep='\t', quote="", comment.char = "", row.names=NULL,stringsAsFactors = FALSE)
#     df$group <- rep(thisGroup,nrow(df))
#     rbind(df,data.frame( # adding group concept
#       CUI=paste0('group.',thisGroup),
#       terms=paste(thisGroup,'(all concepts)'),
#       meshIds='NA',
#       meshTerms='NA',
#       typeId='NA',
#       typeTreeId='NA',
#       typeName='NA',
#       group=thisGroup
#     )) 
#   })
#   df <- rbind(df, data.frame(CUI='group.allNDs',
#                              terms="all NDs target concepts",        
#                              meshIds='NA',
#                              meshTerms='NA',
#                              typeId='NA',
#                              typeTreeId='NA',
#                              typeName='NA',
#                              group='ALL'
#   ))
#   df <- rbind(df, data.frame(CUI='group.allNoOther',
#                              terms="all NDs target concepts except group 'Other'",        
#                              meshIds='NA',
#                              meshTerms='NA',
#                              typeId='NA',
#                              typeTreeId='NA',
#                              typeName='NA',
#                              group='ALL'
#   ))
# #  print(df[df$group=='ALL',])
#   df$firstTerm <- firstTermCol(df)
#   df
# }


# UNUSED
#   - computeSemTypesDF:        Returns a dataframe containing the ids and names of the semantic types
#                               depending on (1) allowed types in 'allowedTypes' and (2) the granularity
#                               user choice 'semTypeGranularitySelect'.
#       arg semTypeGranularitySelect: 'coarse' or 'detailed' (cannot be 'unusued')
#       arg semDF:                    list of UMLS semantic types obtained by:  readUMLSSemanticTypes(umlsSemTypesFile)
#       arg allowedTypes:             list of proposed types, obtained with:  as.character(unique(d[,'typeId']))
#
#computeSemTypesDF <- function(semTypeGranularitySelect, semDF, allowedTypes) {
#  print('computeSemTypesDF: ')
#  print(paste('nrow semDF=',nrow(semDF),'; length(allowedTypes)=',length(allowedTypes)))
#  semDF <- semDF[semDF[,'typeId'] %in% allowedTypes,]
#  if (semTypeGranularitySelect == 'coarse') {
#    u <- unique(semDF[,c('coarseCatId','coarseCatName')])
#  } else {
#    if (semTypeGranularitySelect == 'detailed') {
#      u <- unique(semDF[,c('typeId','typeName')])
#    } else { # unused
#      u <- NULL
#    }
#  }
#  if (!is.null(u)) {
#    colnames(u) <- c('id', 'name')
#  }
#  u
#}



#   - loadFullTargetCuiRelations: loads a full relations file 
#       arg cui:                  the CUI target (can be NULL)
#       arg dataName:             a string <data id>.<level id> as returned by 'datasetStringServer'
#       arg semGranularityValue:  value returned by 'semGranularityServer', default 'unused'
#       arg orderByCol:           column name for ordering dataframe, default 'PMI'
#       arg minJointFreq:         minimum joint frequency, default 0
#
loadFullTargetCuiRelations <- function(cui, viewDir, singleGroupByRow=FALSE) {
  if (!is.null(cui)) {
    filename <- cui
    if (singleGroupByRow) {
      filename <- paste0(filename,'.key-id-group')
    }
    path <- paste(dataDir,viewDir, filename,sep='/')
    df <- read.table(path, sep='\t', quote="", comment.char = "", row.names=NULL,stringsAsFactors = FALSE)
    colnames(df) <- c('concept','freq','term','group','jointFreq')
    df
  }
}


#   - createCuiLink              : given a CUI id, returns an HTML string with this id link to the
#                                  corresponding UMLS.
createCuiLink <- function(cui, umls=TRUE) {
  # below version to display link as a button
  if (umls) {
    # sprintf('<a href="https://uts.nlm.nih.gov/uts/umls/concept/%s" target="_blank" class="btn btn-primary">Info</a>',cui)
    sprintf('<a href="https://uts.nlm.nih.gov/uts/umls/concept/%s" target="_blank">%s</a>',cui,cui)
  } else {
    # assuming that every MESH concept starts with C or D (probably catches a few others by mistake)
    bools <- substr(cui,1,1)=='D' | substr(cui,1,1)=='C'
    cui[bools] <- sprintf('<a href="https://meshb.nlm.nih.gov/record/ui?ui=%s" target="_blank">%s</a>',cui[bools],cui[bools])
    cui
  }
}
