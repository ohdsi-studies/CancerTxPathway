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
#' RuleEditor
#' Edit JSON for algorithm.
#' @param targetRegimenIds
#' @param newJson
#' @keywords JSON,edit
#' @return new JSON file
#' @import dplyr
#' @import listviewer
#' @import data.table
#' @examples
#' @export ruleEditor
## Rule Edit ##
ruleEditor<-function(new=FALSE,
                     targetRegimenIds=NULL){
  if(new == TRUE){
  newJson<-ruleAdder()}else{
  jsonName = "RegimenParameters.json"
  filePath <-system.file("Json", jsonName, package = "CancerTxPathway")
  regimenJson <- readJson()
  if(!is.null(targetRegimenIds)){targetRegimenParameter<-regimenJson[unlist(lapply(regimenJson,`[`,'conceptId')) %in% targetRegimenIds]}else{targetRegimenParameter <- regimenJson}
  names(targetRegimenParameter)<-data.table::rbindlist(lapply(targetRegimenParameter,`[`,"regimenName"))$regimenName
  newJson<-listviewer::jsonedit_gadget(targetRegimenParameter)
  }
  return(newJson)
}
#' @export ruleSave
##Rule Save##
ruleSave<-function(newJson= NULL,
                   targetRegimenIds=NULL){
  if(is.null(newJson))stop("Run after the ruleEditor")
  jsonName = "RegimenParameters.json"
  filePath <-system.file("Json", jsonName, package = "CancerTxPathway")
  regimenJson <- readJson()
  if(!is.null(targetRegimenIds)){otherParameters<-regimenJson[!(unlist(lapply(regimenJson,`[`,'conceptId')) %in% targetRegimenIds)]}else{otherParameters <-regimenJson}
  names(newJson) <- NULL
  newList<-c(newJson,otherParameters)
  newJSON<-jsonlite::toJSON(newList, pretty = TRUE)
  write(newJSON,filePath)}
#' @export
## Rule Add ##
ruleAdder<-function(){
  jsonName = "RegimenTemplate.json"
  regimenJson <- readJson(jsonName)
  addJson<-listviewer::jsonedit_gadget(regimenJson)
}
