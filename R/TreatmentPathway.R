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
#' sankey for regimen and surgery
#' Visualization tool for sankey for regimen and surgery
#' @param connectionDetails
#' @param cohortDatabaseSchema
#' @param cohortTable
#' @param conditionCohortIds
#' @param targetCohortIds
#' @param eventCohortIds
#' @param minimumRegimenChange
#' @param treatmentLine
#' @param collapseDates
#' @param nodeMinSubject
#' @param outputFolder
#' @param outputFileTitle
#' @keywords sankey
#' @return sankey for regimen with other evnets
#' @examples
#' @import dplyr
#' @import networkD3
#' @export treatmentPathway
treatmentPathway<-function(connectionDetails,
                        cohortDatabaseSchema,
                        cohortTable,
                        outputFolder = NULL,
                        outputFileTitle = NULL,
                        conditionCohortIds=NULL,
                        targetCohortIds,
                        eventCohortIds=NULL,
                        minimumRegimenChange = 0,
                        treatmentLine = 3,
                        collapseDates = 0,
                        nodeMinSubject = 0
){
  ##Treatment cohort##
  cohortDescript <- cohortDescription()
  cohortForGraph<-cohortCycle(connectionDetails,
                              cohortDatabaseSchema,
                              cohortTable,
                              targetCohortIds,
                              identicalSeriesCriteria = 60,
                              conditionCohortIds = NULL)
  cohortForGraph <- cohortForGraph %>% subset(cycle == 1)
  cohortData <- cohortForGraph %>% select(-cohortName,-cycle)
  cohortData$cohortStartDate<-as.Date(cohortData$cohortStartDate)
  cohortData$cohortEndDate<-as.Date(cohortData$cohortEndDate)
  cohortData<-dplyr::left_join(cohortData,cohortDescript, by= c("cohortDefinitionId"="cohortDefinitionId"))
  ##event cohort##
  if(!is.null(eventCohortIds)){
    eventCohort<-cohortRecords(connectionDetails,
                               cohortDatabaseSchema,
                               cohortTable,
                               eventCohortIds)
    eventCohort<-dplyr::left_join(eventCohort,cohortDescript, by= c("cohortDefinitionId"="cohortDefinitionId"))
    if(!is.null(conditionCohortIds)){eventCohort<-eventCohort %>% subset(subjectId %in% conditionCohort$subjectId)}
    colnames(eventCohort) <- colnames(cohortData)
    eventCohort$cohortStartDate<-as.Date(eventCohort$cohortStartDate)
    eventCohort$cohortEndDate<-as.Date(eventCohort$cohortEndDate)}

  ##Ignore the change to same regimen##
  cohortData <- cohortData %>% arrange(subjectId,cohortStartDate) %>% group_by(subjectId)%>% mutate(lagCDI = lag(cohortName)) %>% subset(is.na(lagCDI)|lagCDI != cohortName) %>% select(-lagCDI)
  cohortData <- as.data.frame(cohortData)
  ##Bind event and target cohort, Ignore duplicated event records##
  if(!is.null(eventCohortIds)){
    eventAndTarget<-rbind(cohortData,eventCohort) %>% arrange(subjectId,cohortStartDate) %>% group_by(subjectId)%>% mutate(lagCDI = lag(cohortName)) %>% subset(is.na(lagCDI)|lagCDI != cohortName) %>% select(-lagCDI) %>% ungroup()
    eventAndTarget$cohortName <- as.character(eventAndTarget$cohortName)
    eventAndTarget <- as.data.frame(eventAndTarget)}else{
      eventAndTarget<-cohortData %>% arrange(subjectId,cohortStartDate) %>% group_by(subjectId)%>% mutate(lagCDI = lag(cohortName)) %>% subset(is.na(lagCDI)|lagCDI != cohortName) %>% select(-lagCDI) %>% ungroup()
      eventAndTarget$cohortName <- as.character(eventAndTarget$cohortName)
      eventAndTarget <- as.data.frame(eventAndTarget)}
  ##If regimens apart from each other less than collapseDates, collapse using '/'##
  collapsedRecords<-data.table::rbindlist(lapply(unique(eventAndTarget$subjectId),function(targetSubjectId){
    reconstructedRecords <-data.frame()
    targeteventAndTarget<-eventAndTarget %>% subset(subjectId == targetSubjectId)
    reconstructedRecords<-rbind(reconstructedRecords,targeteventAndTarget[1,])

    if(nrow(targeteventAndTarget)>=2){
      for(x in 2:nrow(targeteventAndTarget)){
        if(as.integer(targeteventAndTarget[x,3]-reconstructedRecords[nrow(reconstructedRecords),3])>collapseDates){
          reconstructedRecords <-rbind(reconstructedRecords,targeteventAndTarget[x,])}else{sortNames<-sort(c(targeteventAndTarget[x,5],reconstructedRecords[nrow(reconstructedRecords),5]))
          reconstructedRecords[nrow(reconstructedRecords),5]<-paste0(sortNames,collapse = '/')
          }}}
    return(reconstructedRecords)}))
  ##Set minimum regimen change count##
  eventAndTarget<-collapsedRecords
  minimunIndexId<-unique(eventAndTarget %>% arrange(subjectId,cohortStartDate) %>% group_by(subjectId) %>% mutate(line = row_number()) %>% subset(line >= minimumRegimenChange) %>% select(subjectId) %>% ungroup())
  eventAndTarget<-eventAndTarget %>% subset(subjectId %in% minimunIndexId$subjectId) %>% arrange(subjectId,cohortStartDate)
  ##Maximum treatment line in graph##
  eventAndTarget <- eventAndTarget %>% group_by(subjectId) %>% arrange(subjectId,cohortStartDate) %>% mutate(rowNumber = row_number()) %>% subset(rowNumber <= treatmentLine) %>% select(subjectId,cohortName,rowNumber) %>% mutate(nameOfConcept = paste0(rowNumber,'_',cohortName)) %>% ungroup()
  ##Label##
  label <-unique(eventAndTarget %>% select(cohortName,nameOfConcept) %>% arrange(nameOfConcept))
  label <-label %>% mutate(num = seq(from = 0,length.out = nrow(label)))
  ##Nodes##
  treatmentRatio<-data.table::rbindlist(lapply(1:treatmentLine,function(x){eventAndTarget %>% subset(rowNumber==x) %>% group_by(nameOfConcept) %>% summarise(n=n()) %>% mutate(ratio=round(n/sum(n)*100,1))}))
  treatmentRatio<-treatmentRatio %>% subset(n>=nodeMinSubject)
  label<-dplyr::left_join(treatmentRatio,label,by=c("nameOfConcept"="nameOfConcept")) %>% mutate(name = paste0(cohortName,' (n=',n,', ',ratio,'%)'))
  label<-label %>% mutate(num = seq(from = 0, length.out = nrow(label)))
  nodes<- label %>% select(name)
  nodes<-data.frame(nodes)
  ##Pivot table##
  pivotRecords<-reshape2::dcast(eventAndTarget,subjectId ~ rowNumber, value.var="nameOfConcept")
  ##Link##
  link<-data.table::rbindlist(lapply(2:max(eventAndTarget$rowNumber),function(x){
    source <- pivotRecords[,x]
    target <- pivotRecords[,x+1]
    link <-data.frame(source,target)
    link$source<-as.character(link$source)
    link$target<-as.character(link$target)
    link<-na.omit(link)
    return(link)}))
  link$source<-as.character(link$source)
  link$target<-as.character(link$target)
  link<-link %>% select(source,target)%>% group_by(source,target)%>% summarise(n=n()) %>% ungroup()
  source<-dplyr::left_join(link,label,by = c("source" = "nameOfConcept")) %>% select(num)
  target<-dplyr::left_join(link,label,by = c("target" = "nameOfConcept")) %>% select(num)
  freq<-link %>% select(n)
  links<-data.frame(source,target,freq)
  links<-na.omit(links)
  colnames(links) <-c('source','target','value')
  links$source<-as.integer(links$source)
  links$target<-as.integer(links$target)
  links$value<-as.numeric(links$value)
  ##Sankey data##
  treatment <-list(nodes=nodes,links=links)
  if(!is.null(outputFolder)){
    fileNameNodes <- paste0(outputFileTitle,'_','SankeyNodes.csv')
    write.csv(nodes, file.path(outputFolder, fileNameNodes))
    fileNameLinks <- paste0(outputFileTitle,'_','SankeyLinks.csv')
    write.csv(links, file.path(outputFolder, fileNameLinks))}
  treatmentPathway <- networkD3::sankeyNetwork(Links = treatment$links, Nodes = treatment$nodes, Source = "source",Target = "target", Value = "value", NodeID = "name", fontSize = 12, nodeWidth = 30,sinksRight = FALSE)
  return(treatmentPathway)
}
