# Copyright 2020 Observational Health Data Sciences and Informatics
#
# This file is part of CancerTxPathway
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.
#' Heatmap
#' Trends in regimen heatmap
#' @param standardData
#' @param targetId
#' @param connectionDetails
#' @param resultDatabaseSchema
#' @param cohortTable
#' @param targetCohortIds
#' @param cohortName
#' @param identicalSeriesCriteria
#' @param heatmapPlotData
#' @param maximumCycleNumber
#' @param colorSeed
#' @keywords heatmap
#' @return repitition trend heatmap
#' @examples
#' @import dplyr
#' @import data.table
#' @import superheat
#' @import ggplot2
#' @import tidyr
#' @import RColorBrewer
#' @export cohortCycle
cohortCycle<- function(connectionDetails,
                       resultDatabaseSchema,
                       cohortTable,
                       targetCohortIds,
                       identicalSeriesCriteria,
                       conditionCohortIds){
  ##Condition cohort##
  if(!is.null(conditionCohortIds)){
    conditionCohort<-cohortRecords(connectionDetails,
                                   resultDatabaseSchema,
                                   cohortTable,
                                   conditionCohortIds)}

  ##Treatment cohort##
  cohortDescript <- cohortDescription()
  cycleCohort<-cohortRecords(connectionDetails,
                             resultDatabaseSchema,
                             cohortTable,
                             targetCohortIds)
  if(!is.null(conditionCohortIds)){cycleCohort<-cycleCohort %>% subset(subjectId %in% conditionCohort$subjectId)}
  cycleCohort$cohortStartDate<-as.Date(cycleCohort$cohortStartDate)
  cycleCohort$cohortEndDate<-as.Date(cycleCohort$cohortEndDate)
  cycleCohort<-dplyr::left_join(cycleCohort,cohortDescript, by= c("cohortDefinitionId"="cohortDefinitionId"))
  cohortWtDiff <- cycleCohort %>% group_by(subjectId,cohortDefinitionId) %>% arrange(subjectId,cohortStartDate) %>% mutate(dateDiff = (cohortStartDate-lag(cohortStartDate)))
  cohortWtDiff$dateDiff<-as.numeric(cohortWtDiff$dateDiff)
  cohortWtDiff$flagSeq <- NA
  cohortWtDiff$flagSeq[is.na(cohortWtDiff$dateDiff)|cohortWtDiff$dateDiff>=identicalSeriesCriteria] <- 1
  standardCycle<-data.table::as.data.table(cohortWtDiff)
  standardCycle[, cycle := seq_len(.N), by=.(cumsum(!is.na(flagSeq)))]
  standardCycle<-standardCycle %>% select(cohortDefinitionId,subjectId,cohortStartDate,cohortEndDate,cohortName,cycle)
  standardCycle<-data.frame(standardCycle)
  return(standardCycle)}

#' @export distributionTable
distributionTable <- function(standardData,
                              targetId){
  targetStandardData <- standardData %>% subset(cohortDefinitionId == targetId)
  maxCycle<-aggregate(targetStandardData$cycle,by = list(targetStandardData$subjectId), max)
  colnames(maxCycle) <- c('personId','CycleNum')

  # Total count
  totalCount<-length(unique(maxCycle$personId))

  # Count the number of patients in the value of each cycle number
  distribution<-as.data.frame(maxCycle %>% group_by(CycleNum) %>% summarise(n = n()))
  distribution$'%'<-round(prop.table(table(maxCycle$CycleNum))*100, digits = 1)
  sum<- sum(distribution$n)
  sumName<- paste0('N','(','total=',sum,')')
  distribution$cohortName <- unique(targetStandardData$cohortName)
  names(distribution) <- c('Treatment cycle',sumName,'%','cohortName')
  return(distribution)}

#' @export heatmapData
heatmapData<-function(connectionDetails,
                      resultDatabaseSchema,
                      cohortTable,
                      targetCohortIds,
                      identicalSeriesCriteria = 60,
                      conditionCohortIds = NULL){

  standardCycleData<-cohortCycle(connectionDetails,
                                 resultDatabaseSchema,
                                 cohortTable,
                                 targetCohortIds,
                                 identicalSeriesCriteria,
                                 conditionCohortIds)

  heatmapPlotData <-data.table::rbindlist(
    lapply(targetCohortIds,function(targetId){
      result<-distributionTable(standardData=standardCycleData,
                                targetId=targetId)
      names(result) <- c('cycle','n','ratio','cohortName')
      return(result)})
  )

  return(heatmapPlotData)
}

#' @export iterationHeatmap
iterationHeatmap<-function(heatmapPlotData,
                           maximumCycleNumber = 20,
                           colorSeed=1){
  #label
  total<-heatmapPlotData %>%group_by(cohortName) %>% mutate(sum = sum(n)) %>% select (cohortName,sum)
  total<-unique(total)
  total$label<-paste0(total$cohortName,' \n','(n = ',total$sum,')')

  heatmapPlotDataN <- as_tibble(heatmapPlotData) %>% mutate(ratioLabel = paste0(ratio,'\n','(n = ',n,')')) %>%  select(cycle, cohortName, ratioLabel)%>% subset(cycle <=maximumCycleNumber)
  plotDataN <- tidyr::spread(heatmapPlotDataN, cycle, ratioLabel)
  plotDataN[is.na(plotDataN)] <- 0
  plotDataN$cohortName <- NULL
  #data pre-processing
  heatmapPlotData <- as_tibble(heatmapPlotData) %>% select(cycle, cohortName, ratio) %>% subset(cycle <=maximumCycleNumber)
  class(heatmapPlotData$ratio) = "dbl"
  plotData <- tidyr::spread(heatmapPlotData, cycle, ratio)
  plotDataNMatrix<-as.matrix(plotDataN)
  sort.order <- order(plotDataNMatrix[,1])

  plotData <- left_join(plotData,total,by = c("cohortName"="cohortName"))
  plotData <- as.data.frame(plotData)
  plotData[is.na(plotData)] <- 0

  row.names(plotData) <- plotData$label
  plotData$cohortName <- NULL
  plotData$sum <- NULL
  plotData$label <- NULL
  sort.order <- order(plotData[,1])
  label<-as.matrix(plotDataN)
  #Color
  colorList<- c("Reds",'Blues','Greens')
  set.seed(colorSeed)
  randomColorNum<-sample(1:length(colorList),1)
  selectedColor<-unlist(lapply(randomColorNum,function(x){colorList[x]}))

  heatmap<-superheat::superheat(plotData,
                                X.text = label,
                                X.text.size = 4,
                                scale = FALSE,
                                left.label.text.size=4,
                                left.label.size = 0.3,
                                bottom.label.text.size=4,
                                bottom.label.size = .05,
                                heat.pal = RColorBrewer::brewer.pal(9, selectedColor),
                                heat.pal.values = c(seq(0,0.3,length.out = 8),1),
                                order.rows = sort.order,
                                title = "Trends of the Repetition")
  return(heatmap)
}
