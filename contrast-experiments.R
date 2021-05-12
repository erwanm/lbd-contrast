library(reshape2)
library(ggplot2)
library(plyr)



levels = c('by-doc','by-sent')
views = c('abstracts+articles',  'pmc-articles',  'unfiltered-medline')
measures = c('pmi', 'npmi', 'mi', 'pmi2', 'pmi3')
methodParams = c('dataset', 'methodId', 'refView', 'refLevel', 'maskView', 'maskLevel', 'measure', 'minFreq', 'maxFreq','discardRowsNotInMaskView')

loadDiscoveries <- function(f='data/discoveries.tsv') {
  d<-read.table(f,sep='\t')
  colnames(d) <- c('concept1','concept2')
  d
}

loadTargetsList <- function(f='data/targets.list') {
  d<-read.table(f,sep='\t', header=TRUE)
  d
}


getIdInTargets <- function(df, conceptName,col) {
  selected <- df[as.character(df$short_name) == as.character(conceptName),]
  if (nrow(selected) == 0) {
    stop(paste("Error: cannot find concept ",conceptName,"in data"))
  }
  if (nrow(selected) > 1) {
    stop(paste("Error: multiple rows with concept ",conceptName,"in data"))
  }
  selected[, col]
}


preprocessDiscoveries <-function(discoveriesDF, targetsListDF) {
  kd1 <- ddply(discoveriesDF, c('concept1','concept2'), function(row) {
    id1 <- getIdInTargets(targetsListDF, row$concept1, 'cui')
    id2 <- getIdInTargets(targetsListDF, row$concept2, 'cui')
    data.frame(dataset="KD", id1=id1, id2=id2)
  })
  ptc1 <- ddply(discoveriesDF, c('concept1','concept2'), function(row) {
    id1 <- getIdInTargets(targetsListDF, row$concept1, 'mesh')
    id2 <- getIdInTargets(targetsListDF, row$concept2, 'mesh')
    data.frame(dataset="PTC", id1=paste0("MESH:",id1), id2=paste0("MESH:",id2))
  })
  rbind(kd1,ptc1)
}


#
# expected input dir struct: <data dir>/<dataset>/<indiv|joint>/<level>/<view>/<year>[.total]
#

loadData <- function(dataDir='data', datasets=c('KD','PTC'),probTypes=c('indiv','joint','total'),views=c('abstracts+articles','pmc-articles','unfiltered-medline'),levels=c('by-doc','by-sent'), minYear=0, maxYear=9999,removePTCTypes=TRUE,verbose=TRUE) {
  l<-llply(probTypes, function(probType) {
    ldply(datasets, function(dataset) {
#    ldply(c('indiv', 'joint'), function(probType) {
      ldply(levels, function(level) {
        ldply(views, function(view) {
          d<-loadDataDir(dataDir, dataset, probType, level, view, minYear, maxYear, verbose=verbose)
          if (verbose) {
            print(paste('loaded', nrow(d),'rows'))
          }
          if (dataset == 'PTC' & removePTCTypes) {
            if (probType == 'indiv') {
              d<-removeTypePTC(d, 'concept')
            } else {
              if (probType == 'joint') {
                d<-removeTypePTC(d)
              }
            }
          }
          d
        })
      })
#    })
    })
  })
  names(l) <- probTypes
  l
}


#
# input: <dir>/year
#
loadDataDir <- function(dataDir, dataset, probType, level, view, minYear=0, maxYear=9999,verbose=TRUE) {
  probTypeDir <- probType
  globPattern='????'
  if (probType == 'total') {
    probTypeDir <- 'indiv' 
    globPattern='????.total'
  }
  dir = paste(dataDir,dataset,probTypeDir,level,view,sep='/')
  if (dir.exists(dir)) {
    if (verbose) {
      print(paste("loading from",dir))
    }
    files = Sys.glob(paste(dir,globPattern,sep='/'))
    ldply(files, function(filename) {
      if (file.size(filename) > 0) {
        year = basename(filename)
        if (year >= minYear & year <= maxYear) {
          d<-read.table(filename,header=FALSE,sep='\t',quote = "",comment.char = "")
          if (probType == 'joint') {
            colnames(d) <- c('year', 'concept1', 'concept2', 'jointFreq')
          } else {
            if (probType == 'indiv') {
              colnames(d) <- c('year', 'concept', 'docFreq', 'tokenFreq')
            } else {
              if (probType == 'total') {
                colnames(d) <- c('year', 'nbUniqueConcepts', 'nbDocs', 'nbTokens')
              } else {
                stop("Error: invalid 'probType' argument")
              }
            }
          }
          cols <- data.frame(dataset = rep(dataset,nrow(d)),
                             level = rep(level, nrow(d)),
                             view = rep(view, nrow(d)))
          cbind(cols,d)
        }
      }
    })
  } else {
    stop(paste("Error: cannot find directory",dir))
  }
}


subsetFullData <- function(fullDataAsList,datasets=c('KD','PTC'),views=c('abstracts+articles','pmc-articles','unfiltered-medline'),levels=c('by-doc','by-sent')) {
  dindiv <- fullDataAsList$indiv
  i<-dindiv[dindiv$dataset %in% datasets & dindiv$view %in% views & dindiv$level %in% levels,]
  djoint <- fullDataAsList$joint
  j<-djoint[djoint$dataset %in% datasets & djoint$view %in% views & djoint$level %in% levels,]
  dtotal <- fullDataAsList$total
  t<-djoint[dtotal$dataset %in% datasets & dtotal$view %in% views & dtotal$level %in% levels,]
  list(indiv=i,joint=j, total=t)
}


# CAUTION tends to cause out of memory error
#findLastCharInString <- function(s, c) {
#  unlist(lapply(gregexpr(pattern = c, s,fixed=TRUE), max))
#}


# it's long (40 seconds for 8m joint rows) but at least it doesn't crash
removeTypePTC <- function(d,cols=c('concept1','concept2')) {
  res<-llply(cols, function(col) {
    l<-strsplit(as.character(d[,col]),'@', fixed=TRUE)
    res <- unlist(llply(l, function(row) {
      ll <- length(row)
      if (ll>2) {
        # remove last element then join the others
        paste(row[-c(ll)],collapse='@')
      } else {
        row[[1]]
      }
    }))
  })
  d[,cols] <- res
  d
}


filterSingleDirectedPair <- function(df, id1, id2, concept1, concept2) {
  r <- df[as.character(df$concept1)==id1 & as.character(df$concept2)==id2,]
  colnames(r)[colnames(r)=='concept1'] <- 'id1'
  colnames(r)[colnames(r)=='concept2'] <- 'id2'
  cbind(concept1=rep(concept1,nrow(r)),
        concept2=rep(concept2,nrow(r)),
        r)
}


filterJointPairs <- function(targetPairsDF, jointDF,yearMin=1950, yearMax=2019) {
  ddply(jointDF[jointDF$year>=yearMin & jointDF$year<=yearMax,], 'dataset', function(datasetDF) {
    dataset <- datasetDF[1,'dataset']
    print(dataset)
    datasetPairsDF <- targetPairsDF[as.character(targetPairsDF$dataset)==as.character(dataset),]
    print(nrow(datasetPairsDF))
    z<-ddply(datasetPairsDF, c('concept1','concept2'),function(pairRow) {
      print(paste(dataset,": ",as.character(pairRow$id1),as.character(pairRow$id2)))
      r1<-filterSingleDirectedPair(datasetDF, as.character(pairRow$id1),as.character(pairRow$id2), as.character(pairRow$concept1), as.character(pairRow$concept2))
      r2<-filterSingleDirectedPair(datasetDF, as.character(pairRow$id2),as.character(pairRow$id1), as.character(pairRow$concept2), as.character(pairRow$concept1))
      rbind(r1,r2)
    })
    print(nrow(z))
    z
  })
}



plotTargetDiscoveriesJointFreq <- function(filteredJointPairsDF,yearMin=1980, layout1=FALSE, discardParkinson=TRUE, onlyMedline=FALSE,casesYGrid=FALSE) {
  d <- filteredJointPairsDF[filteredJointPairsDF$year>=yearMin,]
  if (onlyMedline) {
    d <- d[d$view=='unfiltered-medline',]
  }
  first <- firstYearConsecutiveSequence(d)
  if (discardParkinson) {
    # The case Parkinson-ALS is removed because the frequency is much higher than other cases, making other graphs unreadable
    d <- d[d$concept1 != 'Parkinson' & d$concept2 != 'Parkinson',]
    first <- first[first$concept1 != 'Parkinson' & first$concept2 != 'Parkinson',]
  }
  g <- if (layout1) {
    ggplot(d,aes(year,jointFreq,fill=level)) + geom_col(position='dodge') + geom_vline(data = first,aes(xintercept = startYear,colour=level),linetype='dashed')
  } else {
    d <- d[d$view != 'pmc-articles',]
    first <- first[first$view != 'pmc-articles',]
    ggplot(d,aes(year,jointFreq,fill=level)) + geom_col(position='identity',alpha=.5) + geom_vline(data = first,aes(xintercept = startYear,colour=level),linetype='dashed') 
  }
  if (casesYGrid) {
    g + facet_grid(concept1+concept2 ~ dataset+view,scales = "free_y")
  } else {
    g + facet_grid(dataset+view ~ concept1+concept2, scales = "free_y")
  }
}


# obtain the first year from which there is at least one cooccurrence every year afterwards, for every data view and every pair of concepts.
#
firstYearConsecutiveSequence <- function(filteredJointPairsDF, groupBy=c('concept1','concept2','dataset','view','level', 'id1', 'id2')) {
  ddply(filteredJointPairsDF, groupBy, function(df) {
    minYear <- min(df$year)
    year <- max(df$year)
    freqYear <- 0
    if (dim(df[df$year==year,])[1]==1) {
      freqYear <- df[df$year==year,'jointFreq']
    }
    while (year>minYear & freqYear>0) {
      year <- year -1
      freqYear <- 0
      if (dim(df[df$year==year,])[1]==1) {
        freqYear <- df[df$year==year,'jointFreq']
      }
    }
    startYear <- year +1
    sumAfter <- sum(df[df$year>=startYear,'jointFreq'])
    propAfter <- sumAfter / sum(df$jointFreq)
    data.frame(startYear=startYear, totalAfter=sumAfter, propAfter=propAfter)
  })
}


#
# select the range of years for future aggregation with 
# - yearMin in the start year
# - the end year is obtained as the first of the consecutive years: it is defined as variable for the dataset (KD or PMC) but fixed for the view and the level.
#
selectYearRange <- function(filteredJointPairsDF, selectView='unfiltered-medline', selectLevel='by-doc', yearMin=1950, groupBy=c('concept1','concept2','dataset','view','level', 'id1', 'id2')) {
  consDF0 <- firstYearConsecutiveSequence(filteredJointPairsDF)
  consDF <- consDF0[consDF0$view==selectView & consDF0$level==selectLevel,]
  ddply(consDF, groupBy, function(row) {
    r <- row[,c('concept1','concept2','dataset','view','level','propAfter')]
#    print(r)
    r$start <- yearMin
    # take the year before the start of the consecutive sequence
    r$end <-  consDF[consDF$concept1 == row$concept1 & consDF$concept2 == row$concept2 & consDF$dataset==row$dataset & consDF$view == selectView & consDF$level == selectLevel,'startYear']-1
    r
  })
}


#
# Reads a DF targetPairsRange obtained from selectYearRange which look like this:
#
#       id1      id2 concept1 concept2 dataset               view  level   propAfter start  end
#  C0002736 C0004352      ALS   Autism      KD unfiltered-medline by-doc   0.8490566  1950 2010
#
# to a DF with 'target' and 'goldConcept' columns which looks like this:
#
#   dataset targetName     targetId goldConceptName goldConceptId start  end
#        KD        ALS     C0002736          Autism      C0004352  1950 2010
#
# If arg 'bothDirections' is true then every pair (A,B) is converted to two target/gold pairs (A,B) and (B,A)
#
convertTargetPairsToTargetGoldPairs <- function(targetPairsRangeDF, bothDirections=FALSE, selectTargetNames=NULL) {
  targetPairsDF <- data.frame(dataset = as.character(targetPairsRangeDF[,'dataset']),
                                  targetName = as.character(targetPairsRangeDF[,'concept1']),
                                  targetId = as.character(targetPairsRangeDF[,'id1']),
                                  goldConceptName = as.character(targetPairsRangeDF[,'concept2']),
                                  goldConceptId = as.character(targetPairsRangeDF[,'id2']),
                                  start =  targetPairsRangeDF[,'start'],
                                  end =   targetPairsRangeDF[,'end']
                                  )
  if (bothDirections) {
    targetPairsDF <- rbind(targetPairsDF, data.frame(dataset =  as.character(targetPairsRangeDF[,'dataset']) ,
                                  targetName = as.character(targetPairsRangeDF[,'concept2']) ,
                                  targetId =   as.character(targetPairsRangeDF[,'id2']) ,
                                  goldConceptName = as.character(targetPairsRangeDF[,'concept1']) ,
                                  goldConceptId = as.character(targetPairsRangeDF[,'id1']) ,
                                  start =  targetPairsRangeDF[,'start'] ,
                                  end =    targetPairsRangeDF[,'end'] 
                                  ))
  }
  if (!is.null(selectTargetNames)) {
    targetPairsDF <- targetPairsDF[targetPairsDF$targetName %in% selectTargetNames,]
  }
  targetPairsDF
}








imputeMissingValuesWithZerosAllVariants <- function(df, xCol='year',yCol='jointFreq',colsVariants=c('concept1','concept2','dataset','view','level','id1','id2'), yearMin=1950, yearMax=2019) {
  s <- seq(yearMin,yearMax)
  ddply(df, colsVariants, function(subDF) {
     row <- subDF[1,]
#    imputeMissingValuesWithZeros(subDF, xCol, yCol)
     missingYears <- s[! s %in% subDF[,xCol]]
     r <- ldply(missingYears, function(year) {
       row[,xCol] <- year
       row[,yCol] <- 0
       row
     })
#     print(colnames(r))
#     print(colnames(subDF))
     rbind(r,subDF)
  })
}

imputeMissingValuesWithZeros <- function(df, xCol='year',yCol='jointFreq') {
  ldply(seq(min(df[,xCol]),max(df[,xCol])),function(x) {
    xDF <- df[df[,xCol]==x,]
    r <- if (dim(xDF)[1] == 0) {
      data.frame(x=x,y=0)
    } else {
      data.frame(x=x,y=xDF[,yCol])
    }
    colnames(r) <- c(xCol,yCol)
    r
  })
}

getSlope <- function(df,xCol='year',yCol='jointFreq') {
  r<-lm(df[,yCol] ~ df[,xCol])
  r$coefficients[[2]]
}

propBeforeAfter <- function(filteredJointPairsDF) {
  ddply(filteredJointPairsDF, c('concept1','concept2','dataset','view','level'), function(df0) {
    df <- imputeMissingValuesWithZeros(df0)
    total <- sum(df[,'jointFreq'])
    ldply(seq(from=min(df$year),to=max(df$year)), function(year) {
      if (year > min(df$year)) {
        datBefore <- df[df$year<year,]
        countBefore  <- sum(datBefore$jointFreq)/total
        slopeBefore <- getSlope(datBefore)
      } else {
        countBefore <- 0
        slopeBefore = NA
      }
      datAfter <- df[df$year>=year,]
      countAfter  = sum(datAfter$jointFreq)/total
      slopeAfter <- getSlope(datAfter)
      data.frame(year=year,countBefore=countBefore,countAfter=countAfter, slopeBefore=slopeBefore,slopeAfter=slopeAfter)
    })
  })
}

plotCumulativeJointFreq <- function(filteredJointPairsDF,withSmoothed=TRUE) {
  d <- propBeforeAfter(filteredJointPairsDF)
  g<- ggplot(d,aes(year,countBefore,colour=level))+geom_line()
  if (withSmoothed) {
    g <- g+geom_smooth()
  }
  g +facet_grid( dataset+view~concept1+concept2)
}

plotSlopeBeforeAfter <- function(filteredJointPairsDF) {
  d0 <- propBeforeAfter(filteredJointPairsDF)
  d<-melt(r[,c('concept1','concept2','dataset','view','level','year','slopeBefore','slopeAfter')],
            measure.vars = c('slopeBefore','slopeAfter'), variable.name = 'direction',value.name = 'slope')
  ggplot(d,aes(year,slope,colour=direction))+geom_line()+facet_grid( dataset+view+level~concept1+concept2)

}


#
# unsure if this is useful?
#
integratePairsDataByYear <- function(filteredJointPairsDF, fullDataAsList, yearMin=1950, yearMax=2019) {
  # 1) impute missing years with zeros
  d0 <- imputeMissingValuesWithZerosAllVariants(filteredJointPairsDF, yearMin=yearMin, yearMax=yearMax)
  # 2) adding indiv freq for both concept1 and concept2
  # caution: the concept id is in id1/id2, not concept1/concept2
  d1 <- merge(d0, fullDataAsList$indiv, by.x=c('dataset','view','level','year','id1'),by.y=c('dataset','view','level','year','concept'),all.x=TRUE)
  d2 <- merge(d1, fullDataAsList$indiv, by.x=c('dataset','view','level','year','id2'),by.y=c('dataset','view','level','year','concept'),suffixes=c('1','2'),all.x=TRUE)
  # 3) assigning zero to indiv freq (missing rows from the indiv df) 
  d2$docFreq1[is.na(d2$docFreq1)] <- 0
  d2$docFreq2[is.na(d2$docFreq2)] <- 0
  #  step below optional: remove tokenFreqX columns (irrelevant) 
  #  d3 <- d2[,c('dataset','view','level','year','concept1','concept2','id1','id2','docFreq1','docFreq2','jointFreq')]
  # 4) adding total doc/tokens by year
  d4 <- merge(d2, fullDataAsList$total, by=c('dataset','view','level','year'),all.x=TRUE)
  d4
}



#
# sums across selected years for a specific column (only one) in a dataframe 'data' which can be any of 'indiv', 'joint' or 'total'; use columns groupBy and sumCol to apply the appropriate process.
#
# there are many options to sum by group in R, following most upvoted answer from https://stackoverflow.com/questions/1660124/how-to-sum-a-variable-by-group#1661144
#
aggregateAcrossYears <- function(data, yearsRangeDF, groupBy, sumCol) {
  d <- ddply(unique(yearsRangeDF[,c('start','end')]), c('start','end'), function(rangeRow) {
    print(paste('  processing range',rangeRow$start,'-',rangeRow$end,'...'))
    selectedYearsDF <- data[data$year>=rangeRow$start & data$year<=rangeRow$end,]
    aggregate(selectedYearsDF[,sumCol],by=selectedYearsDF[,groupBy],FUN=sum)
  })
  colnames(d)[colnames(d)=='x'] <- sumCol
  d
}


aggregateFullDataAcrossYears <- function(fullDataAsList, yearsRangeDF, groupBy=c('dataset', 'level', 'view')) {
    print('aggregating indiv...')
#    indiv <- aggregateAcrossYears(fullDataAsList$indiv, yearsRangeDF, c(groupBy, 'concept'), c('docFreq', 'tokenFreq'))
    indiv <- aggregateAcrossYears(fullDataAsList$indiv, yearsRangeDF, c(groupBy, 'concept'), 'docFreq')
    print('aggregating joint...')
    joint <- aggregateAcrossYears(fullDataAsList$joint, yearsRangeDF, c(groupBy, 'concept1', 'concept2'), 'jointFreq')
    # note: the column 'nbUniqueConcepts' is dropped since it doesn't make sense to aggregate it across years
    print('aggregating total...')
#    total <- aggregateAcrossYears(fullDataAsList$total, yearsRangeDF, groupBy , c('nbDocs', 'nbTokens'))
    total <- aggregateAcrossYears(fullDataAsList$total, yearsRangeDF, groupBy , 'nbDocs')
    list(indiv=indiv, joint=joint, total=total)
}


integratePairsDataByYearRange <- function(fullAggregatedDataByRange, commonCols=c('start', 'end', 'dataset','view','level')) {
  d1 <- merge(fullAggregatedDataByRange$joint, fullAggregatedDataByRange$indiv, all.x=TRUE, by.x=c(commonCols,'concept1'),by.y=c(commonCols,'concept'))
  d2 <- merge(d1, fullAggregatedDataByRange$indiv, all.x=TRUE, by.x=c(commonCols,'concept2'), by.y=c(commonCols,'concept'), suffixes=c('1','2'))
  d3 <- merge(d2, fullAggregatedDataByRange$total, by=commonCols,all.x=TRUE)
  d3

}


#
# not really useful afaik
#
aggregatePairsDataOverYears <- function(pairsDataDF, yearRangeDF, sumCols=c('jointFreq', 'docFreq1', 'tokenFreq1','docFreq2', 'tokenFreq2', 'nbUniqueConcepts', 'nbDocs', 'nbTokens'), ignoreWhenNoRangeData=TRUE) {
  ddply(pairsDataDF, c('concept1','concept2','dataset','view','level'), function(df0) {
    yearRangeRows <- yearRangeDF[yearRangeDF$concept1 == df0[1,'concept1'] & 
                                 yearRangeDF$concept2 == df0[1,'concept2'] & 
                                 yearRangeDF$dataset == df0[1,'dataset'] & 
                                 yearRangeDF$view == df0[1,'view'] &
                                 yearRangeDF$level == df0[1,'level'], ]
    if (dim(yearRangeRows)[1] == 0) {
      if (!ignoreWhenNoRangeData) {
        stop(paste("Error: no range data for ",paste(df0[,byCols], collapse=" ")))
      }
    } else {
      if (nrow(yearRangeRows) > 1) {
        print(yearRangeRows)
        stop(paste("Error: multiple ranges for ",paste(df0[,byCols], collapse=" ")))
      } else {
	df1 <- df0[df0$year>=yearRangeRows$start & df0$year<=yearRangeRows$end,]
	do.call(cbind,
         llply(sumCols, function(col) {
            r <- data.frame(x=sum(df1[,col]))
	    print('HERE')
	    print(r)
	    colnames(r) <- col
	    print(r)
	    r
          })
        )
      }
    }
  })
  
}


binaryMI <- function(pA, pB, pA_B) {
  pNA_B <- pB - pA_B
  pA_NB <- pA - pA_B
  pNA_NB <- 1 - ( pA_B + pNA_B + pA_NB)
  mi <- rep(0, length(pA_B))
  mi[pNA_NB>0] <- mi[pNA_NB>0] + pNA_NB[pNA_NB>0] * log2( pNA_NB[pNA_NB>0] / ((1-pA[pNA_NB>0]) * (1-pB[pNA_NB>0])) )
  mi[pNA_B>0] <- mi[pNA_B>0] + pNA_B[pNA_B>0] * log2( pNA_B[pNA_B>0] / ((1-pA[pNA_B>0]) * pB[pNA_B>0]) )
  mi[pA_NB>0] <- mi[pA_NB>0] + pA_NB[pA_NB>0] * log2( pA_NB[pA_NB>0] / (pA[pA_NB>0] * (1-pB[pA_NB>0])) )
  mi[pA_B>0] <- mi[pA_B>0] + pA_B[pA_B>0] * log2( pA_B[pA_B>0] / (pA[pA_B>0] * pB[pA_B>0]) )
  mi
}

calculateAssociation <- function(integratedRelationsDF, filterMeasures=measures,docFreq1Col='targetFreq',docFreq2Col='conceptFreq') { 
  jointProb <- integratedRelationsDF$jointFreq / integratedRelationsDF$nbDocs
  pA <- integratedRelationsDF[,docFreq1Col] / integratedRelationsDF$nbDocs
  pB <- integratedRelationsDF[,docFreq2Col] / integratedRelationsDF$nbDocs
  pmi <- log2( jointProb / (pA * pB) )
  if ('pmi' %in% filterMeasures) {
    integratedRelationsDF$pmi <- pmi
  }
   if ('npmi' %in% filterMeasures) {
    integratedRelationsDF$npmi <- - pmi / log2(jointProb)
  }
  if ('mi' %in% filterMeasures) {
    integratedRelationsDF$mi <- binaryMI(pA, pB, jointProb)
  }
  if ('pmi2' %in% filterMeasures) {
    integratedRelationsDF$pmi2 <- log2( (jointProb^2) / (pA * pB) )
  }
  if ('pmi3' %in% filterMeasures) {
    integratedRelationsDF$pmi3 <- log2( (jointProb^3) / (pA * pB) )
  }
  integratedRelationsDF
}

relationsSingleTargetSingleDataset <- function(conceptId, datasetDF) {
  d1 <- datasetDF[as.character(datasetDF$concept1)==conceptId,]
  d1 <- d1[,colnames(d1) != 'concept1' ]
  colnames(d1)[colnames(d1)=='concept2'] <- 'concept'
  d2 <- datasetDF[as.character(datasetDF$concept2)==conceptId,]
  d2 <- d2[,colnames(d2) != 'concept2' ]
  colnames(d2)[colnames(d2)=='concept1'] <- 'concept'
  # swap docFreq1 and docFreq2 in d2:
  tmp <- d2$docFreq1
  d2$docFreq1 <- d2$docFreq2
  d2$docFreq2 <- tmp
  r<-rbind(d1,d2)
  colnames(r)[colnames(r)=='docFreq1'] <- 'targetFreq'
  colnames(r)[colnames(r)=='docFreq2'] <- 'conceptFreq'
  r
}


#
# for any relation (A,B) in measuredDF, puts the target (A or B) in a specific column. If both A and B are targets, rows are added to represent the two cases
# This way from the output dataframe a full list of relations can be selected for any specific target.
# 
reformatRelationsByTarget <- function(measuredDF, targetPairsDF) {
  ddply(measuredDF, 'dataset', function(datasetDF) {
    print(as.character(datasetDF[1,'dataset']))
    targetPairsDataset <- targetPairsDF[as.character(targetPairsDF$dataset)==as.character(datasetDF[1,'dataset']),]
    indivTargets <- unique(data.frame(targetName=c(as.character(targetPairsDataset[,'concept1']),as.character(targetPairsDataset[,'concept2'])),
                                      targetId=c(as.character(targetPairsDataset[,'id1']),as.character(targetPairsDataset[,'id2']))))
    ddply(indivTargets, c('targetName', 'targetId'), function(targetConceptId) {
 #     print(as.character(targetConceptId$targetName))
      relationsSingleTargetSingleDataset(targetConceptId$targetId, datasetDF)
    })
  })
}




# IMPORTANT: in all the functions below lower rank means high association, i.e. the "top" rank is 1, the second top is 2, etc.
#            the measures provided with orderBy are assumed to be order by association strength, i.e. high value = high association
#            the absolute rank is from 1 (highest association) to N (lowest association), the normalized "relative rank" is from 1/N to 1 (lower is better).
#


#
# return the baseline gold rank for every target and every view in the input 'associated by target' DF.
# Note: this function is not used in the main process across methods and targets
#
#
baselineGoldRank <- function(relByTargetDF, targetGoldPairsDF, groupBy=c('start', 'end', 'dataset', 'level', 'view'), orderBy=c('pmi', 'npmi', 'mi', 'pmi2', 'pmi3'),relRankMaxIfNotFound=FALSE) {
  ddply(targetGoldPairsDF, c('dataset', 'targetName','goldConceptName'), function(goldPair) {
#    print(as.character(goldPair$targetName))
#    print(goldPair$start)
#    print(goldPair$end)
    relSelectedTarget <- relByTargetDF[   as.character(relByTargetDF$dataset)==as.character(goldPair$dataset) 
                                        & as.character(relByTargetDF$targetName) == as.character(goldPair$targetName)
                                        & relByTargetDF$start == goldPair$start
                                        & relByTargetDF$end == goldPair$end
                                      ,]
    if (nrow(relSelectedTarget)>0) {
#      print(goldPair)
      ddply(relSelectedTarget, groupBy, function(viewDF) {
        ldply(orderBy, function(orderCol) {
          tmpDF <- viewDF
          # ranked by ascending order: high rank = high association
          tmpDF$rank <- rank(-tmpDF[,orderCol])
          goldResDF <- tmpDF[as.character(tmpDF[,'concept']) == as.character(goldPair$goldConceptId),]
          if (nrow(goldResDF) > 1) {
            stop("BUG: there shouldn't be several rows with the same gold concept in a single view")
          }
          if (nrow(goldResDF) == 0) { # not found at all
            relRank <- if (relRankMaxIfNotFound) 1 else NA
	    data.frame(measure=orderCol, rank=NA, relRank=relRank,viewSize=nrow(viewDF))
          } else {  # ok, found (exactly 1 row)
            relRank <- goldResDF$rank / nrow(viewDF)
	    data.frame(measure=orderCol, rank=goldResDF$rank, relRank=relRank, viewSize=nrow(viewDF))
	  }
        })
      })
    }
  })
}


#### COPIED FROM study.R and adapted. Unsure required?
#
filterFreqOrderAndAddRank <- function(df, minJointFreq, associationMeasure) {
  if (!is.null(df) & !is.null(minJointFreq) & !is.null(associationMeasure)) {
    df <- df[df$jointFreq>=minJointFreq,]
    df <- df[order(-df[,associationMeasure]),] 
    df$rank <- rank(-df[,associationMeasure])
    df$relRank <- df$rank / nrow(df)
    df
  }
}


#
# Contrast two views 'ref' and 'mask': the goal is to assign a top rank (low rank value) to concepts which have a top rank in refViewDF and a bottom 
# rank (or are absent) in maskViewDF, i.e. 'masking' the concepts from refViewDF which also have a top rank in maskViewDF.
# 
# The process relies on column 'concept' for merging the two DFs, 'jointFreq' for filtering frequency and column 'relRank' for
# calculating the new 'contrast rank'. Any other column in the inputDFs is kept with a suffix: this can be useful to observe
# how/which rows are combined, but keeping too many columns can be confusing and/or inconvenient. If needed, it is recommended
# to filter columns before calling the function.
#
# Reminder: the rank/relRank as input and output always follows low value = high association = better 
#
# methodId indicates the ranking method used to "contrast" the views: if 'diffRelRank' the difference in relative rank is used (default), 'diffAbsRank' for the difference in absolute rank,
# otherwise if 'associationRefData' the rank follows the association measure in the ref dataset (note: with this option one must use the 'maxJointFreqMask' threshold
# to tune the ranking, otherwise it's simply the same as the ref dataset).
# Note: the output DF is ordered by the main criterion (defined by the method 'orderByDiffRank') and as second criterion
#       by the diff in joint freq between the two views. This is implemented in the order of the output DF but not in the output rank/relRank column.
# Note: in the output the final "contrast rank" column is called 'rank', in order to standardize comparisons later.
#
contrastViews <- function(refViewDF, maskViewDF, minJointFreqRef=NA, maxJointFreqMask=NA, discardRowNotInMaskView=FALSE,methodId='diffRelRank',mergeByCols=c('concept'), rankCol='rank', relRankCol='relRank', jointFreqCol='jointFreq') {
  # init columns names for later
  relRankMaskCol <- paste0(relRankCol,'.mask')
  relRankRefCol <- paste0(relRankCol,'.ref')
  rankMaskCol <- paste0(rankCol,'.mask')
  rankRefCol <- paste0(rankCol,'.ref')
  jointFreqMaskCol <- paste0(jointFreqCol,'.mask')
  jointFreqRefCol <- paste0(jointFreqCol,'.ref')

  # apply min freq to ref view (note: cannot apply max freq to mask view before merging, otherwise the removed concepts are re-added as not present)
  if (!is.na(minJointFreqRef)) {
    refViewDF <- refViewDF[refViewDF$jointFreq>=minJointFreqRef,]
  }

  # merge by concept, preserving all the rows in ref view
  if (discardRowNotInMaskView) {
    contrasted <- merge(refViewDF, maskViewDF, by=mergeByCols,suffixes=c('.ref','.mask'))
  } else {
    contrasted <- merge(refViewDF, maskViewDF, by=mergeByCols,all.x=TRUE,suffixes=c('.ref','.mask'))
  }

  # assign worst possible rank for concepts absent in mask view
  contrasted[is.na(contrasted[,rankMaskCol]), rankMaskCol] <- nrow(maskViewDF)
  contrasted[is.na(contrasted[,relRankMaskCol]), relRankMaskCol] <- 1
  contrasted[is.na(contrasted[,jointFreqMaskCol]), jointFreqMaskCol] <- 0

  # now filter on mask view max freq 
  if (!is.na(maxJointFreqMask)) {
    contrasted <- contrasted[contrasted[,jointFreqMaskCol] <= maxJointFreqMask,] 
  }

  # diffRelRank: the lowest the better, i.e. a low ref relrank and a high mask relrank is the best result
  contrasted$diffRelRank <- contrasted[,relRankRefCol] - contrasted[,relRankMaskCol]
  contrasted$diffRank <- contrasted[,rankRefCol] - contrasted[,rankMaskCol]
  contrasted$diffJointFreq <- contrasted[,jointFreqRefCol] - contrasted[,jointFreqMaskCol]
  if (methodId == 'associationRefData') {
    contrasted$rank <- rank(contrasted[,relRankRefCol])
  } else {
    if (methodId == 'diffRelRank') {
      contrasted$rank <- rank(contrasted$diffRelRank)
    } else {
      if (methodId == 'diffAbsRank') {
        contrasted$rank <- rank(contrasted$diffRank)
      } else {
        stop("Error: invalid value for orderByDiffRank")
      }
    }
  }
  contrasted$relRank <- contrasted$rank / nrow(contrasted)
  contrasted[order(contrasted$relRank, -contrasted$diffJointFreq),]

}


#
# runs a batch of methods for one target and returns the full ranking for every method as a tidy DF.
#
# - relationsDF = associatedDF after filtering one target+gold pair with its specific range of years
# - unused parameters should be NA
# - viewKeepCols = columns transmitted for each view to the contrastView function. Add columns names 'x' here in order to have 'x.ref' and 'x.mask' in the output. 
#                  If viewKeepCols is NULL, then all the columns are kept in the output (useful for debugging/analyzing)
#
#  note: methodsDF may contain the optional column 'dataset' in case the min and max are different by dataset, but this must have been filtered before this function.
#
#
applyMethodsSingleTarget <- function(relationsDF, methodsDF, methodCols=methodParams,
                                    viewKeepCols=c('concept', 'rank', 'relRank', 'jointFreq')) {
  ddply(unique(methodsDF), methodCols, function(methodRow) {
   refView <- relationsDF[ as.character(relationsDF$view) == as.character(methodRow$refView) & as.character(relationsDF$level) == as.character(methodRow$refLevel), ]
#   print(as.character(methodRow$measure))
#   print(head(refView[,as.character(methodRow$measure)], 15))
   refView$rank <- rank(-refView[,as.character(methodRow$measure)])
   refView$viewSize <- nrow(refView)
   refView$relRank <- refView$rank / nrow(refView)
#   print('TEST REF')
#   print(refView[refView$concept=='C0004352',])
   if (methodRow$methodId == 'baseline') {
#     print("BASELINE")
     if (!is.na(methodRow$minFreq)) {
       refView <- refView[refView$jointFreq>=methodRow$minFreq,]
     }
     if (!is.na(methodRow$maxFreq)) {
       refView <- refView[refView$jointFreq<=methodRow$maxFreq,]
     }
     refView
   } else {  # contrast method
#    print("CONTRAST")
     maskView <- relationsDF[as.character(relationsDF$view) == as.character(methodRow$maskView) & as.character(relationsDF$level) == as.character(methodRow$maskLevel), ]
     maskView$rank <- rank(-maskView[,as.character(methodRow$measure)])
     maskView$relRank <- maskView$rank / nrow(maskView)
      maskView$viewSize <- nrow(maskView)
#     print('TEST MASK')
#     print(maskView[maskView$concept=='C0004352',])
     if (!is.null(viewKeepCols)) {
       refView <- refView[,viewKeepCols]
       maskView <- maskView[,viewKeepCols]
     }
     contrastViews(refView, maskView, minJointFreqRef=methodRow$minFreq, maxJointFreqMask=methodRow$maxFreq, methodId=methodRow$methodId, mergeByCols=c('concept'), relRankCol='relRank', jointFreqCol='jointFreq',discardRowNotInMaskView=methodRow$discardRowsNotInMaskView)
   }
  })
}



#
# runs a batch of methods from methodsDF for all the targets in yearRangeDF, then evaluates every case against the gold and returns
# the gold rank for ever method and every target
#
# methodsDF may contain optional column 'dataset'
#
# use debugOutput=TRUE and viewKeepCols = NULL for maximum detail in the output. Important: in case of debug output there's no row if the concept is not found.
#
applyMethodsMultiTargets <- function(relByTargetDF, targetGoldPairsDF, methodsDF, methodCols=methodParams, viewKeepCols=c('concept', 'rank','relRank', 'jointFreq'), debugOutput=FALSE) {
  ddply(targetGoldPairsDF, c('dataset', 'targetName','goldConceptName'), function(goldPairRow) {
#   print(paste("GOLD CONCEPT=",as.character(goldPairRow$goldConceptId)))
    relSelectedTarget <- filterSelectedGoldPairRow(goldPairRow,relByTargetDF)
    methodsDatasetDF <- methodsDF
    if ('dataset' %in% colnames(methodsDF)) {
      methodsDatasetDF <- methodsDF[as.character(methodsDF$dataset)==as.character(goldPairRow$dataset),]
    }
    resMethods <- if (nrow(relSelectedTarget)>0) {
      # update: originally all the methods were passed to applyMethodsSingleTarget but this requires too much memory
      #   when there are many methods, since all the resulting rankings need to be stored at the same time.
      #   Thus we now run applyMethodsSingleTarget for one method at a time, collect the result for the gold and discard
      #   the ranking before going to the next method.
      ddply(methodsDatasetDF, methodCols, function(methodRow) {
#          print(paste("METHOD:"))
# 	  print(methodRow)
          ranking <- applyMethodsSingleTarget(relSelectedTarget, methodRow, viewKeepCols=viewKeepCols)
          goldResDF <- ranking[as.character(ranking[,'concept']) == as.character(goldPairRow$goldConceptId),]
#	  print("GOLD RES")
#          print(goldResDF)
          if (nrow(goldResDF) > 1) {
            stop("BUG: there shouldn't be several rows with the same gold concept in a single view")
          }
          if (debugOutput) {
            goldResDF
          } else {
            if (nrow(goldResDF) == 0) { # not found at all
#              print("NOT found")
              data.frame(rank=NA, relRank=NA,viewSize=nrow(ranking))
            } else {  # ok, found (exactly 1 row)
#              print("found")
	      data.frame(rank=goldResDF$rank, relRank=goldResDF$relRank, viewSize=nrow(ranking))
	    }
         }
      })
    } else {
      print("Warning: cannot find target-gold pair in relByTargetDF. goldPairRow = ")
      print(goldPairRow)
      ddply(methodsDF, methodCols, function(methodRow) {
        if (!debugOutput) {
          data.frame(rank=NA, relRank=NA,viewSize=NA)
        }
      })
    }
    if (nrow(resMethods) != nrow(methodsDatasetDF)) {
      stop(paste("Error: applied",nrow(methodsDatasetDF)," methods but found only",nrow(resMethods),"results"))
    }
    resMethods
  })
}



assignThresholdsToMethods <- function(methodsDF, thresholdsByView,methodCols=methodParams) {
  m1 <- merge(methodsDF, thresholdsByView, all.x=TRUE, by.x=c('refView','refLevel'),by.y=c('view','level'))
  m1$minFreq <- m1$min
#  print(nrow(m1))
  # baseline: assign max to ref view as well
  mB <- m1[m1$methodId=='baseline',methodCols]
#  print(colnames(mB))
#  print(colnames(thresholdsByView))
  mB2 <- merge(mB, thresholdsByView, all.x=TRUE, by.x=c('dataset', 'refView','refLevel'),by.y=c('dataset','view','level'))
  mB2$maxFreq <- mB2$max
#  print(nrow(mB2))
  # contrast: assign max to mask view
  mC <- m1[m1$methodId!='baseline',methodCols]
  mC2 <- merge(mC, thresholdsByView, all.x=TRUE, by.x=c('dataset','maskView','maskLevel'),by.y=c('dataset','view','level'))
  mC2$maxFreq <- mC2$max
#  print(nrow(mC2))
#  print(colnames(mB2))
#  print(colnames(mC2))
  m3 <- rbind(mB2, mC2)
  if (! 'maxFreq' %in% colnames(m3)) {
    stop("BUG: no maxFreq col??")
  }
#  print(colnames(m3))
  m3[,methodCols]
}




# - methodsDF columns = 'methodId' ('baseline', 'diffRelRank', 'associationRefData'), refView, refLevel, maskView, maskLevel, measure, minFreq, maxFreq,contrastDiscardRowsNotInMaskView
# assign NULL to refMaskPairs in order to generate all the possible combinations (many!!)
generateMethods <- function(measures = c('pmi', 'npmi', 'mi', 'pmi2', 'pmi3'),
                   refMaskPairs=data.frame(refView=c('abstracts+articles',        'abstracts+articles' ,       'abstracts+articles'),
                                          refLevel=c('by-doc'           ,         'by-sent',                   'by-doc' ),
                                          maskView=c('unfiltered-medline',        'unfiltered-medline',        'abstracts+articles'),
                                          maskLevel=c('by-doc'          ,         'by-sent' ,                  'by-sent' )),
                   contrastMethodsIds = c('diffRelRank','diffAbsRank', 'associationRefData'),
                   contrastDiscardRowsNotInMaskView=FALSE,
                   minFreqThresholds=NA,
                   withBaselines = TRUE
                            ) {
  # baseline methods
  baselinesDF <- ldply(views, function(v) {
    ldply(levels, function(l) {
      ldply(measures, function(m) {
        ldply(minFreqThresholds, function(minF) {
          data.frame(methodId='baseline', refView=v, refLevel=l, maskView=NA, maskLevel=NA,measure=m, minFreq=minF, maxFreq=NA,discardRowsNotInMaskView=FALSE)
        })
      })
    })
  })
  print(paste("Info:",nrow(baselinesDF)," baseline methods"))
  # contrast methods
  if (is.null(refMaskPairs)) {
    refMaskPairs <- ldply(views, function(refView) {
      ldply(views, function(maskView) {
         ldply(levels, function(refLevel) {
           ldply(levels, function(maskLevel) {
             if ((refView != maskView) | (refLevel != maskLevel)) {
               data.frame(refView=refView, refLevel=refLevel, maskView=maskView, maskLevel=maskLevel)
             }
           })
         })
      })
    })
  }
  print(paste("Info: refMaskPairs contains",nrow(refMaskPairs),"rows"))
  contrastDF<-ddply(refMaskPairs, c('refView', 'refLevel', 'maskView', 'maskLevel'), function(refMaskPairRow) {
    ldply(measures, function(m) {
      ldply(contrastMethodsIds, function(id) {
        ldply(contrastDiscardRowsNotInMaskView, function(discardOpt) {
          ldply(minFreqThresholds, function(minF) {
            data.frame(measure=m,methodId=id, minFreq=minF, maxFreq=NA,discardRowsNotInMaskView=discardOpt)
          })
        })
      })
    })
  })  
  print(paste("Info:",nrow(contrastDF)," contrast methods"))
  if (withBaselines) {
    rbind(baselinesDF, contrastDF)
  } else {
    contrastDF
  }
}


#
# for every target pair and view, returns the value of column 'valCol' for the pair in this view
#
extractColValueByGoldPairAndView <- function(relByTargetDF, targetPairsDF, viewCols=c('dataset', 'view', 'level'), targetGoldCols=c('dataset', 'targetName','goldConceptName', 'start', 'end'), valCol='jointFreq') {
#  print("extractColValueByGoldPairAndView")
#  print(targetPairsDF)
  ddply(targetPairsDF, targetGoldCols, function(goldPairRow) {
    relSelectedTarget <- filterSelectedGoldPairRow(goldPairRow,relByTargetDF)
#    print(paste(as.character(goldPairRow$targetName),as.character(goldPairRow$goldConceptId)))
#    print(nrow(relSelectedTarget))
    ddply(relSelectedTarget, viewCols, function(viewDF) {
      goldDF <- viewDF[as.character(viewDF[,'concept']) == as.character(goldPairRow$goldConceptId),]
      if (nrow(goldDF) > 1) {
        stop("BUG: there shouldn't be several rows with the same gold concept in a single view")
      }
      if (nrow(goldDF) == 1) {     # found
        data.frame(freq=goldDF[,valCol])
      } else {                     # not found
        data.frame(freq=0)
      }
    })

  })
}



#
# for every view returns the min and max joint freq across target pairs, with the number of pairs found
#
calculateMinMaxTargetJointFreqByView <- function(relByTargetDF, targetPairsDF, targetGoldCols=c('dataset', 'targetName','goldConceptName', 'start', 'end'), viewCols=c('dataset', 'view', 'level')) {
#  print("calculateMinMaxTargetJointFreqByView")
#  print(nrow(relByTargetDF))
  d <- extractColValueByGoldPairAndView(relByTargetDF, targetPairsDF, viewCols=viewCols, targetGoldCols=targetGoldCols, valCol='jointFreq')
  ddply(d[d$freq>0,], viewCols, function(viewFreqDF) {
    data.frame(min=min(viewFreqDF$freq), max=max(viewFreqDF$freq), nbFound=nrow(viewFreqDF))
  })
}



#
# print statistics including quantiles of the measure or rank by view+target
#
# this is meant for analyzing/debugging
#
statsByViewAndTarget <- function(relByTargetDF, targetGoldPairsDF, viewCols=c('dataset', 'view', 'level'), targetGoldCols=c('dataset', 'targetName','goldConceptName', 'start', 'end'), quantStep=0.1, measures=c('pmi'), useRank=FALSE) {
  ddply(targetGoldPairsDF, targetGoldCols, function(goldPairRow) {
    print(goldPairRow)
    relSelectedTarget <- filterSelectedGoldPairRow(goldPairRow,relByTargetDF)
    ddply(relSelectedTarget, viewCols, function(viewDF) {
  #    print(nrow(viewDF))
      if (length(unique(viewDF$concept)) != nrow(viewDF)) {
        stop(paste('BUG: the number of unique concepts',length(unique(viewDF$concept),' differs from the number of rows in the view',nrow(viewDF))))
      }
      ldply(measures, function(m) {
        values <- viewDF[,m]
#        print(length(values))
        if (useRank) {
          values <- rank(values) / length(values)
        }
        l<-quantile(values,probs=seq(0,1,quantStep))
        df <- do.call(cbind,as.list(l))
        cbind(measure=m,df, uniq=length(unique(values)),mean=mean(values), sd=sd(values),n=length(values))
      })
    })   
 })
}


#
# convenience function for selecting from any DF the rows which have the same columns dataset, targetName, start, end as goldPairRow
#
filterSelectedGoldPairRow <- function(goldPairRow, df, reverse=FALSE) {
#  print(nrow(df))
  boolSelection <- as.character(df$dataset)==as.character(goldPairRow$dataset) &
            as.character(df$targetName) == as.character(goldPairRow$targetName) &
            df$start == goldPairRow$start &
            df$end == goldPairRow$end
  if (reverse) {
    boolSelection <- !boolSelection
  }
#  print("debug")
#  print(nrow(df[boolSelection,]))
  df[boolSelection,]
}



#
# resultsGroupBy = results columns, i.e. columns on which no optimization is done. For example add 'measure' in order not to optimize on the measure. The result for
#                  each combination of values for these columns corresponds to the max method found on the training set.
#
# if returnDetailByTargetGoldPairs is true, a list containing the two dataframes is returned: the regular aggregated perf results and the detailed 
# dataframe containing the result for every target-gold pair, including the method parameters.
#
crossValidateMethodsLOO <- function(relByTargetDF, targetPairsDF, methodsDF,
                                    selectBestMeasure = 'MNTR.1000',
                                    recallAtValues=c(10,100,1000),
                                    optimMinMaxThresholds=FALSE, 
				                            returnDetailByTargetGoldPairs=FALSE,
				                            nbTargetGoldPairs=NA,
                                    targetGoldCols=c('dataset', 'targetName','goldConceptName', 'start', 'end'), 
                                    resultsGroupBy=c('dataset','methodId'), 
                                    methodCols=methodParams
                                   ) {
   
   detailedResults <- ddply(targetPairsDF, targetGoldCols, function(targetGoldTest) {

    print("TEST instance:")
    print(targetGoldTest)
    targetsGoldTrain <- targetPairsDF[targetPairsDF[,'dataset'] == targetGoldTest[,'dataset'] 
                             & ( targetPairsDF[,'targetName'] != targetGoldTest[,'targetName'] | 
      			         targetPairsDF[,'goldConceptName'] != targetGoldTest[,'goldConceptName'] | 
      			         targetPairsDF[,'start'] != targetGoldTest[,'start'] | 
      			         targetPairsDF[,'end'] != targetGoldTest[,'end'] 
                               ),]
    print(paste("number training cases: ",nrow(targetsGoldTrain)))
#    print("filtering TEST")
    relTest <- filterSelectedGoldPairRow(targetGoldTest,relByTargetDF)
#    print("filtering TRAIN")
    relTrain <- filterSelectedGoldPairRow(targetGoldTest,relByTargetDF, reverse=TRUE)

    if (optimMinMaxThresholds) {
        print("Selecting optimal min/max thresholds")
        thresholdsByView <- calculateMinMaxTargetJointFreqByView(relTrain, targetsGoldTrain)
        # this will add the optional 'dataset' column:
        methodsDF <- assignThresholdsToMethods(methodsDF, thresholdsByView)
    }

    # train
    print("    Training")
    resultsTrain <- applyMethodsMultiTargets(relTrain, targetsGoldTrain, methodsDF)
    if (nrow(methodsDF)*nrow(targetsGoldTrain) != nrow(resultsTrain)) {
      stop(paste("Expecting results for",nrow(methodsDF),"x",nrow(targetsGoldTrain),"; nrow(resultsTrain) =",nrow(resultsTrain)))
    }

    print("    Selecting best method and applying for every group")
    bestMethodByGroup <- ddply(resultsTrain, resultsGroupBy, function(resultsTrainGroup) {
 
#      print("    Select best for group:")
#      print(resultsTrainGroup[1,resultsGroupBy])

      # obtain best perf method
      optimAcrossCols <- methodCols[! methodCols %in% resultsGroupBy]
      perfDF <- evalByGroup(resultsTrainGroup, groupBy=optimAcrossCols,nbTargetGoldPairs=nbTargetGoldPairs, recallAtValues)
#      print(paste("DEBUG selectBestMeasure = ",selectBestMeasure))
#      print(perfDF)
      topPerf <- min(perfDF[,selectBestMeasure])
      # note: unlikely to have two identical scores with NMTR.1000 but using head anyway to avoid technical problems
      head(perfDF[perfDF[,selectBestMeasure] == topPerf,], 1)
    })
#    print("DEBUG bestMethodByGroup = ")
#    print(bestMethodByGroup)
      
    ddply(bestMethodByGroup, resultsGroupBy, function(bestMethodGroup) {
      # apply on test instance
#      print("    Apply on test instance for group")
#      print(bestMethodGroup[1,resultsGroupBy])
      applyMethodsMultiTargets(relTest, targetGoldTest, bestMethodGroup)
    })
  })
  print("Final aggregation across all test instances results")
  aggregatedPerf <- ddply(detailedResults, 'dataset', function(datasetRes) {
     print(paste("dataset=",as.character(datasetRes[1,'dataset'])))
#     print(paste("nrow(datasetRes)=",nrow(datasetRes)))
     targetPairsThisDataset <- targetPairsDF[as.character(targetPairsDF$dataset)==as.character(datasetRes[1,'dataset']),]
#     print(paste("nrow(targetPairsThisDataset)=",nrow(targetPairsThisDataset)))
#     print(targetPairsThisDataset)
     evalByGroup(datasetRes, groupBy=resultsGroupBy,nbTargetGoldPairs=nbTargetGoldPairs, recallAtValues)
  })
  if (returnDetailByTargetGoldPairs) {
    list(aggregatedPerf=aggregatedPerf,detailedResults=detailedResults)
  } else {
    aggregatedPerf
  }
}



#
# receives a DF from applyMethodsMultiTargets which contains several target-gold pairs and a column 'rank'.
# - 'groupBy' indicates how to group the rows so that every group contains one set of unique target-gold pairs
# - 'nbTargetGoldPairs': it is recommended to provide the number of pairs expected for a group of target-gold pairs.
#
# returns a df with one row for every 'groupBy' containing meanRank, nbFound, nbTotal + recall.N columns + MNTR.N columns
#
#  MNTR.N =  Mean Normalized Truncated Rank at N = mean of the ranks considering any rank higher or equal N as N normalized in [0,1] (lower better)
#
evalByGroup <- function(resultsByTarget, groupBy=methodParams, nbTargetGoldPairs=NA, recallAtValues=c(10,100,1000)) {
#  print("evalByGroup receives results:")
#  print(head(resultsByTarget))
  differenceNbPairs <- c()
  evalRes <- ddply(resultsByTarget, groupBy, function(resultsAcrossTargets) {
    if (is.na(nbTargetGoldPairs)) {
      nbTargetGoldPairs <- nrow(resultsAcrossTargets)
    } else {
      if (!is.na(nbTargetGoldPairs) & nrow(resultsAcrossTargets) != nbTargetGoldPairs) {
#        print("Current group:")
#        print(resultsAcrossTargets[1,groupBy])
	 differenceNbPairs <<- c(differenceNbPairs, nrow(resultsAcrossTargets))
#	 print(differenceNbPairs)
      }
    }
    rowsNotNA <- !is.na(resultsAcrossTargets$rank)
    r<-data.frame(meanRankNoNA=mean(resultsAcrossTargets$rank[rowsNotNA]),
                 nbFound=length(rowsNotNA[rowsNotNA]),
                 nbTotal=nbTargetGoldPairs
              )
    for (i in 1:length(recallAtValues)) {
      r[,paste('R',recallAtValues[i],sep='.')] <- nrow(resultsAcrossTargets[rowsNotNA & resultsAcrossTargets$rank <= recallAtValues[i],]) / nbTargetGoldPairs
      truncatedRank <- resultsAcrossTargets$rank
      truncatedRank[is.na(truncatedRank) | truncatedRank>recallAtValues[i]] <- recallAtValues[i]
#      print(paste("DEBUG truncatedRank at",recallAtValues[i],' (1):'))
#      print(truncatedRank)
      truncatedRank <- truncatedRank / recallAtValues[i]
#      print(paste("DEBUG truncatedRank at",recallAtValues[i],' (2):'))
#      print(truncatedRank)
      r[,paste('MNTR',recallAtValues[i],sep='.')] <- sum(truncatedRank) / nbTargetGoldPairs
    }
    r
  })
  if (length(differenceNbPairs)>0) {
     print("Collected number of differences between expected and actual number of target-gold pairs:")
     print(ddply(data.frame(actual=differenceNbPairs),'actual',function(sub) { data.frame(expected=nbTargetGoldPairs,actual=sub$actual[1],nb.times=nrow(sub)) }))
     warning(paste("Error with sanity check: expected",nbTargetGoldPairs,"target-gold pairs but found different number of rows (see details above)"))
  }
  evalRes
}


#
# select best method based on the mean of the Recall.N columns
# not used anymore, using NMTR now
#
selectBestMethod <- function(perfDF, maxRank=10000) {

#  print("DEBUG selectBestMethod")
#  print(nrow(perfDF))
#  print(perfDF)
  meansByRow <- rowMeans(perfDF[,meanOfCols])
#  print("DEBUG meansByRow")
#  print(meansByRow)
  maxRowsBool <- meansByRow == max(meansByRow)
#  print("DEBUG maxRowsBool")
#  print(maxRowsBool)
  selected <- perfDF[maxRowsBool,]
  if (nrow(selected)>1) {
    print("Warning: multiple maximum perf rows:")
    print(selected)
    print("Arbitrarily selecting first max row")
    selected <- selected[1,]
  }
  selected
} 


# reformats a dataframe of results to show the parmaeter of insterest as columns (allows parallel comparison)
compareMethodParam <- function(resultsByTarget,param='methodId', methodCols=methodParams) {
  d<-resultsByTarget[,!colnames(resultsByTarget) %in% c('relRank','viewSize')]
  methodColsMinusParam <- methodCols[! methodCols %in% param]
  f1 <- paste("dataset","targetName","goldConceptName",paste(methodColsMinusParam,collapse="+"),sep="+")
  dcast(d, paste(f1,param,sep="~") ,value.var = 'rank'  )
}


#
# this graph shows boxplots for param value1 vs param value 2 for every variant of the other parameters
# note that it's not very "clean" because each boxplot contains the points from the different targets + from dataset (and possibly other columns)
#
graphMethodParam <- function(resultsByTarget,param='methodId', methodCols=methodParams) {
  d<-resultsByTarget[,!colnames(resultsByTarget) %in% c('relRank','viewSize')]
  methodColsMinusParam <- methodCols[! methodCols %in% param]
  u <- unique(d[,methodColsMinusParam])
  u$id <- as.factor(1:nrow(u))
  d <- merge(d,u)
  ggplot(d,aes_string('id','rank',fill=param))+geom_boxplot()
}

#
# perfDF <- evalByGroup(resultsByTarget)
#
basicGraphMethodParamComparison <- function(perfDF,param='methodId',perfCol='MNTR.1000',byCol=NA,facetDataset=TRUE) {
  print(ddply(perfDF, param, function(s) { mean(s[,perfCol])}))
  perfDF[,param] <- as.factor(perfDF[,param])
  if (is.na(byCol)) {
    g<-ggplot(perfDF,aes_string(param,perfCol)) + geom_boxplot()
  } else {
    perfDF[,byCol] <- as.factor(perfDF[,byCol])
    g<-ggplot(perfDF,aes_string(param,perfCol,colour=byCol)) + geom_boxplot()+ylab(perfCol)
  }
  if (facetDataset) {
    g <- g + facet_grid(dataset ~ .)
  }
  g
}
