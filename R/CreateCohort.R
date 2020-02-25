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
#' CreateCohort
#' Create cohort of interest by using condition concept Ids.
#' @param createCohortTable
#' @param connectionDetails
#' @param oracleTempSchema
#' @param cdmDatabaseSchema
#' @param cohortDatabaseSchema
#' @param vocabularyDatabaseSchema
#' @param cohortTable
#' @param conceptIdSet
#' @param includeConceptIdSetDescendant
#' @param targetCohortId
#' @export
createCohort <- function(createCohortTable = F,
                         connectionDetails,
                         oracleTempSchema = NULL,
                         cdmDatabaseSchema,
                         cohortDatabaseSchema,
                         vocabularyDatabaseSchema = cdmDatabaseSchema,
                         cohortTable,
                         conceptIdSet = c(),
                         includeConceptIdSetDescendant = F,
                         targetCohortId
                         ){
  if(length(targetCohortId) != 1) stop ("Specify 'targetCohortId' as one integer. It cannot be multiple.")
  if(length(as.numeric(conceptIdSet)) <1 ) stop ("Please specify concept Id Set as a numeric vector")

  connection <- DatabaseConnector::connect(connectionDetails = connectionDetails)

  if(createCohortTable){
    ParallelLogger::logInfo("Creating table for the cohorts")
    sql <- SqlRender::loadRenderTranslateSql(sqlFilename= "CreateCohortTable.sql",
                                             packageName = "CancerTxPathway",
                                             dbms = attr(connection,"dbms"),
                                             oracleTempSchema = oracleTempSchema,
                                             cohort_database_schema = cohortDatabaseSchema,
                                             cohort_table = cohortTable)
    DatabaseConnector::executeSql(connection, sql, progressBar = TRUE, reportOverallTime = TRUE)
  }

  ParallelLogger::logInfo("Insert cohort of interest into the cohort table")
  sql <- SqlRender::loadRenderTranslateSql(sqlFilename= "CohortGeneration.sql",
                                           packageName = "CancerTxPathway",
                                           dbms = attr(connection,"dbms"),
                                           oracleTempSchema = oracleTempSchema,
                                           cdm_database_schema = cdmDatabaseSchema,
                                           vocabulary_database_schema = vocabularyDatabaseSchema,
                                           target_database_schema = cohortDatabaseSchema,
                                           target_cohort_table = cohortTable,
                                           include_descendant = includeConceptIdSetDescendant,
                                           condition_concept_ids = paste(conceptIdSet,collapse=","),
                                           target_cohort_id = targetCohortId)
  DatabaseConnector::executeSql(connection, sql, progressBar = TRUE, reportOverallTime = TRUE)

  DatabaseConnector::disconnect(connection)
}


