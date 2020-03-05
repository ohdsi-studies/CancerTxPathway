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
#' @param cohortDatabaseSchema
#' @param cohortTable
#' @param targetCohortIds
#' @param cohortName
#' @param identicalSeriesCriteria
#' @param heatmapPlotData
#' @param outputFolder
#' @param outputFileTitle
#' @param maximumCycleNumber
#' @param minSubject
#' @keywords heatmap
#' @return repitition trend heatmap
#' @examples
#' @import dplyr
#' @import data.table
#' @import highcharter
#' @import ggplot2
#' @import tidyr
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
                      cohortDatabaseSchema,
                      cohortTable,
                      targetCohortIds,
                      outputFolder = NULL,
                      outputFileTitle = NULL,
                      identicalSeriesCriteria = 60,
                      conditionCohortIds = NULL){

  standardCycleData<-cohortCycle(connectionDetails,
                                 cohortDatabaseSchema,
                                 cohortTable,
                                 targetCohortIds,
                                 identicalSeriesCriteria,
                                 conditionCohortIds)
  targetCohortIds<-targetCohortIds[targetCohortIds %in% unique(standardCycleData$cohortDefinitionId)]
  heatmapPlotData <-data.table::rbindlist(
    lapply(targetCohortIds,function(targetId){
      plotData<-distributionTable(standardData=standardCycleData,
                                  targetId=targetId)
      names(plotData) <- c('cycle','n','ratio','cohortName')
      return(plotData)})
  )
  if(!is.null(outputFolder)){
    fileName <- paste0(outputFileTitle,'_','treatmentIterationHeatmap.csv')
    write.csv(heatmapPlotData, file.path(outputFolder, fileName),row.names = F)}
  return(heatmapPlotData)
}

#' @export treatmentIterationHeatmap
treatmentIterationHeatmap<-function(heatmapPlotData,
                                         maximumCycleNumber = 20,
                                         minSubject){
  #label
  total <- heatmapPlotData %>%group_by(cohortName) %>% mutate(sum = sum(n)) %>% select (cohortName,sum)
  total <- unique(total)
  total$label<-paste0(total$cohortName,' \n','(n = ',total$sum,')')
  heatmapPlotData <- heatmapPlotData %>% subset(n >= minSubject)
  h <- heatmapPlotData %>% highcharter::hchart(.,type="heatmap",hcaes(x = cycle,y=cohortName,value = ratio),dataLabels = list(allowOverlap = TRUE, enabled = TRUE,format = '{point.n}<br>{point.value}%'),align ='center') %>% hc_xAxis(max = maximumCycleNumber, tickInterval = 1) %>% hc_yAxis(title = list(text = 'Regimen')) %>% hc_colorAxis(stops = color_stops(ceiling(max(heatmapPlotData$ratio)),c("white","blue"))) %>% hc_tooltip(pointFormat = "Regimen: {point.y} <br> Cycle: {point.x} <br> Proportion: {point.value}%")
  return(h)
}
