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
#' @param heatmapColor
#' @keywords heatmap
#' @return repitition trend heatmap
#' @examples
#' @import dplyr
#' @import data.table
#' @import superheat
#' @import ggplot2
#' @import tidyr
#' @import RColorBrewer
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

  heatmapPlotData <-data.table::rbindlist(
    lapply(targetCohortIds,function(targetId){
      plotData<-distributionTable(standardData=standardCycleData,
                                  targetId=targetId)
      names(plotData) <- c('cycle','n','ratio','cohortName')
      return(plotData)})
  )
  if(!is.null(outputFolder)){
    fileName <- paste0(outputFileTitle,'_','ChemotherapyIterationDistribution.csv')
    write.csv(heatmapPlotData, file.path(outputFolder, fileName))}
  return(heatmapPlotData)
}

#' @export ChemotherapyIterationDistribution
ChemotherapyIterationDistribution<-function(heatmapPlotData,
                                       maximumCycleNumber = 20,
                                       heatmapColor="Reds"){
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
  heatmap<-superheat::superheat(plotData,
                                X.text = label,
                                X.text.size = 4,
                                scale = FALSE,
                                left.label.text.size=4,
                                left.label.size = 0.3,
                                bottom.label.text.size=4,
                                bottom.label.size = .05,
                                heat.pal = RColorBrewer::brewer.pal(9, heatmapColor),
                                heat.pal.values = c(seq(0,0.3,length.out = 8),1),
                                order.rows = sort.order,
                                title = "Trends of the Repetition")
  return(heatmap)
}
