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
#' CallRawData
#' Bring the chemotherapy raw data table.
#' @param connectionDetails
#' @param cohortDatabaseSchema
#' @param cohortTable
#' @param targetCohortIds
#' @param newCohortDescription
#' @keywords raw data
#' @return raw data for visualization
#' @examples
#' @export
cohortRecords <- function(connectionDetails,
                          cohortDatabaseSchema,
                          cohortTable,
                          targetCohortIds){
  connection <- DatabaseConnector::connect(connectionDetails)
  sql <- 'SELECT * FROM @result_database_schema.@cohort_table WHERE cohort_definition_id IN (@target_cohort_ids)'
  sql <- SqlRender::render(sql,
                           result_database_schema = cohortDatabaseSchema,
                           cohort_table = cohortTable,
                           target_cohort_ids= targetCohortIds)
  sql <- SqlRender::translate(sql, targetDialect = connectionDetails$dbms)
  Cohort <- DatabaseConnector::querySql(connection, sql)
  colnames(Cohort) <- SqlRender::snakeCaseToCamelCase(colnames(Cohort))
  DatabaseConnector::disconnect(connection)
  return(Cohort)}
#' @export cohortDescription
cohortDescription <- function(){
  cohortDescriptionPath <- system.file("csv", "CohortDescription.csv", package = "CancerTxPathway")
  cohortDescription<-read.csv(cohortDescriptionPath,stringsAsFactors = F)
  return(cohortDescription)
}

#' @export
cohortCycle<- function(connectionDetails,
                       cohortDatabaseSchema,
                       cohortTable,
                       targetCohortIds,
                       identicalSeriesCriteria,
                       conditionCohortIds){
  ##Condition cohort##
  if(!is.null(conditionCohortIds)){
    conditionCohort<-cohortRecords(connectionDetails,
                                   cohortDatabaseSchema,
                                   cohortTable,
                                   conditionCohortIds)}

  ##Treatment cohort##
  cohortDescript <- cohortDescription()
  cycleCohort<-cohortRecords(connectionDetails,
                             cohortDatabaseSchema,
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

