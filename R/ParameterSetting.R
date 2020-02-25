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
#' ParameterSetting
#' Parameter setting for algorithm.
#' @param jsonName
#' @param regimenLists
#' @param targetRegimenConceptId
#' @param targetRegimenConceptIds
#' @keywords regimen Setting
#' @return parameters
#' @import SqlRender
#' @import DatabaseConnector
#' @examples
#' parameterSetting(targetRegimenConceptIds=targetRegimenConceptIds)

#' @export
readJson <- function(jsonName = "RegimenParameters.json"){
  pathToRjson <-system.file("Json", jsonName, package = "CancerTxPathway")
  regimenLists <-rjson::fromJSON(file = pathToRjson)

  class(regimenLists) = "regimenLists"
  return(regimenLists)

}

#' @export
regimenListParameterSetting <- function(regimenLists, targetRegimenConceptId){

  if (class(regimenLists)!="regimenLists") stop ("regimenLists should be of type regimenLists")
  filteredRegimenLists<- regimenLists[unlist(lapply(regimenLists,`[`,"conceptId"))  %in% targetRegimenConceptId][[1]]

  index <- !(names(filteredRegimenLists) %in% c('conceptId','regimenName','validStartDate','validEndDate','invalidReason','includeDescendant','gapDateBetweenCycle','gapDateBefore','gapDateAfter','drugInspectionDate','outofCohortPeriod','origin'))

  roleIndex<-lapply(filteredRegimenLists[index],`[`,"role")

  primaryConceptIdList <- sapply(filteredRegimenLists[index][unlist(roleIndex) == "primary"],`[`,"conceptId")
  secondaryConceptIdList <- sapply(filteredRegimenLists[index][unlist(roleIndex) == "secondary"],`[`,"conceptId")
  excludingConceptIdList <- sapply(filteredRegimenLists[index][unlist(roleIndex) == "excluded"],`[`,"conceptId")

  regimenConceptId <- filteredRegimenLists$conceptId
  regimenName <- filteredRegimenLists$regimenName
  includeDescendant <- filteredRegimenLists$includeDescendant
  outofCohortPeriod <- filteredRegimenLists$outofCohortPeriod
  drugInspectionDate <- filteredRegimenLists$drugInspectionDate
  gapDateBetweenCycle <- filteredRegimenLists$gapDateBetweenCycle
  gapDateAfter <- filteredRegimenLists$gapDateAfter
  gapDateBefore <- filteredRegimenLists$gapDateBefore
  if(filteredRegimenLists$invalidReason != "")stop("Target concept id has an invalid reason")
  parameters <- list(regimenConceptId,regimenName,includeDescendant,outofCohortPeriod,drugInspectionDate,gapDateBetweenCycle,gapDateAfter,gapDateBefore,primaryConceptIdList,secondaryConceptIdList,excludingConceptIdList)

  names(parameters) <- c('regimenConceptId','regimenName','includeDescendant','outofCohortPeriod','drugInspectionDate','gapDateBetweenCycle','gapDateAfter','gapDateBefore','primaryConceptIdList','secondaryConceptIdList','excludingConceptIdList')
  class(parameters)= "regimenLists"
  return(parameters)
}

#' @export parameterSetting
parameterSetting <- function(jsonName= "RegimenParameters.json",targetRegimenConceptIds){
  regimenLists <-readJson(jsonName = jsonName)
  if(is.null(targetRegimenConceptIds)){targetRegimenConceptIds <- sapply(regimenLists,`[`,"conceptId")}
  listFormRegimen<-lapply(targetRegimenConceptIds,regimenListParameterSetting,regimenLists=regimenLists)
  class(listFormRegimen)= "regimenLists"
  return(listFormRegimen)
}
