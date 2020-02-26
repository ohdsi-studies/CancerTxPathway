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
#' Visualization
#' Visualization tool for episode table.
#' @param connectionDetails
#' @param vocaDatabaseSchema
#' @param oncologyDatabaseSchema
#' @param episodeTable
#' @param targetRegimen
#' @keywords graph,visualization
#' @return Graph or table for episode table
#' @examples
#' @import dplyr
#' @import superheat
#' @import networkD3
#' @import ggplot2
#' @import tidyr
#' @import RColorBrewer
#' @import collapsibleTree
#' @export
readJson <- function(jsonName = "RegimenParameters.json"){
  pathToRjson <-system.file("Json", jsonName, package = "treatmentCycleExtraction")
  regimenLists <-rjson::fromJSON(file = pathToRjson)

  class(regimenLists) = "regimenLists"
  return(regimenLists)

}
#' @export parameterTree
parameterTree<-function(targetRegimenConceptIds,collapsed){
  regimenJson<-readJson()
  ##
  Regimens<-lapply(targetRegimenConceptIds,function(targetRegimen){
    targetRegimenParameter<-regimenJson[unlist(lapply(regimenJson,`[`,'conceptId')) %in% targetRegimen][[1]]
  targetRegimenParameterBasic<-targetRegimenParameter[names(targetRegimenParameter)%in%c('conceptId','regimenName','validStartDate','validEndDate','invalidReason','includeDescendant','gapDateBetweenCycle','gapDateBefore','gapDateAfter','drugInspectionDate','outofCohortPeriod')]

  targetRegimenParameterDrug<-targetRegimenParameter[!names(targetRegimenParameter)%in%c('conceptId','regimenName','validStartDate','validEndDate','invalidReason','includeDescendant','gapDateBetweenCycle','gapDateBefore','gapDateAfter','drugInspectionDate','outofCohortPeriod')]

  if(length(targetRegimenParameterBasic$invalidReason)==0){targetRegimenParameterBasic$invalidReason <-NA}
  contents<-names(targetRegimenParameterBasic)
  role<-unlist(targetRegimenParameterBasic)
  parameters <- data.frame(contents,role) %>% subset(contents != 'regimenName')
  regimenName<- data.frame(contents,role) %>% subset(contents == 'regimenName')
  regimenName<-as.character(regimenName$role)
  rownames(parameters) <- NULL
  index <- !(names(targetRegimenParameter) %in% c('conceptId','regimenName','validStartDate','validEndDate','invalidReason','includeDescendant','gapDateBetweenCycle','gapDateBefore','gapDateAfter','drugInspectionDate','outofCohortPeriod'))
  roleIndex<-lapply(targetRegimenParameter[index],`[`,"role")
  primaryDrugList<-targetRegimenParameterDrug[unlist(roleIndex) == "primary"]
  primaryDrugDataframe<-do.call(rbind.data.frame, primaryDrugList)
  secondaryDrugList<-targetRegimenParameterDrug[unlist(roleIndex) == "secondary"]
  secondaryDrugDataframe<-do.call(rbind.data.frame, secondaryDrugList)
  excludedDrugList<-targetRegimenParameterDrug[unlist(roleIndex) == "excluded"]
  excludedDrugDataframe<-do.call(rbind.data.frame, excludedDrugList)

  drug<-rbind(primaryDrugDataframe,secondaryDrugDataframe,excludedDrugDataframe)
  drug$contents <- 'drug'
  Regimen<-merge(drug,parameters,all=TRUE)
  Regimen$regimenName <- regimenName
  return(Regimen)}
  )
  ##

  colorList1<-unlist(lapply(c(1:length(Regimens)),function(x){depth1 <- "seashell"}))
  colorList2<-unlist(lapply(c(1:length(Regimens)),function(x){depth2 <- rep("#00798c",length(unique(Regimens[[x]]$contents)))}))
  colorList3<-unlist(lapply(c(1:length(Regimens)),function(x){depth3 <- rep("#d1495b",length(unique(na.omit(Regimens[[x]]$role))))}))
  colorList4<-unlist(lapply(c(1:length(Regimens)),function(x){depth4 <- rep("#edae49",length(unique(na.omit(Regimens[[x]]$conceptName))))}))
  colorList5<-unlist(lapply(c(1:length(Regimens)),function(x){depth5 <- rep("#66a182",length(unique(na.omit(Regimens[[x]]$conceptName)))+length(unique(na.omit(Regimens[[x]]$conceptName))))
  }))
  colorList <- c(colorList1,colorList2,colorList3,colorList4,colorList5)

  Regimens<-data.table::rbindlist(Regimens)

  tree<-collapsibleTree::collapsibleTree(
    Regimens,
    hierarchy = c("regimenName","contents","role","conceptName","conceptId","table"),
    fill = c("Black",colorList),
    collapsed = collapsed
  )
  return(tree)}
