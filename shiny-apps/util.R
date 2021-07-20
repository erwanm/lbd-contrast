

# helper functions used by the shiny apps

#
# returns targets df: 'targetId','targetFreq','targetTerm','targetGroup'
#
getAllTargets <- function(viewDir, dataDir='../../../ND-hierarchy-data-for-rankings', targetsFilename='targets.tsv') {
  path <- paste(dataDir,viewDir, targetsFilename,sep='/')
#  print(path)
  df <- read.table(path, sep='\t', quote="", comment.char = "", row.names=NULL,stringsAsFactors = FALSE)
  colnames(df) <- c('targetId','targetFreq','targetTerm','targetGroup')
  df
}

#
# returns single value = total nb documents
#
getViewTotal <- function(viewDir, dataDir='../../../ND-hierarchy-data-for-rankings', totalFilename='total.tsv') {
  path <- paste(dataDir,viewDir, totalFilename,sep='/')
  df <- read.table(path, sep='\t', quote="", comment.char = "", row.names=NULL,stringsAsFactors = FALSE)
  # nb docs is the second column:
  df[,2]
}

loadFullTargetCuiRelations <- function(cui, viewDir, dataDir='../../../ND-hierarchy-data-for-rankings', singleGroupByRow=FALSE) {
  if (!is.null(cui)) {
    filename <- cui
    if (singleGroupByRow) {
      filename <- paste0(filename,'.key-id-group')
    }
    path <- paste(dataDir,viewDir, filename,sep='/')
    df <- read.table(path, sep='\t', quote="", comment.char = "", row.names=NULL,stringsAsFactors = FALSE)
    colnames(df) <- c('concept','freq','term','group','jointFreq')
#    print(nrow(df))
    df
  }
}


orderConceptsByAssociation <- function(df, statsTV, measure) {
  if (!is.null(df) & !is.null(measure)) {
    df$nbDocs <- rep(statsTV$viewTotal,nrow(df))
    df$targetFreq <- rep(statsTV$targetFreq, nrow(df))
    df <- calculateAssociation(df, filterMeasures=measure,docFreq1Col='targetFreq',docFreq2Col='freq')
    df <- df[order(-df[,measure]),] 
    df$rank <- rank(-df[,measure])
    df$relRank <- df$rank / nrow(df)
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


