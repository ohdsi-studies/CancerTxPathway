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
#' @param oncologyDatabaseSchema
#' @param vocabularyDatabaseSchema
#' @param cohortTable
#' @param episodeTable
#' @param conceptIdSet
#' @param includeConceptIdSetDescendant
#' @param targetCohortId
#' @param cycle
#' @export createCohort
createCohort <- function(createCohortTable = F,
                         connectionDetails,
                         oracleTempSchema = NULL,
                         cdmDatabaseSchema,
                         cohortDatabaseSchema,
                         oncologyDatabaseSchema,
                         vocabularyDatabaseSchema = cdmDatabaseSchema,
                         cohortTable,
                         episodeTable,
                         conceptIdSet = c(),
                         includeConceptIdSetDescendant = F,
                         collapseGapSize=0,
                         targetCohortId,
                         cycle = TRUE){
  if(length(targetCohortId) != 1) stop ("specify targetCohortId as one integer. It cannot be multiple.")
  if(length(as.numeric(conceptIdSet)) <1 ) stop ("please specify concept Id Set as a numeric vector")

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
  if(cycle == TRUE){sql <- SqlRender::loadRenderTranslateSql(sqlFilename= "CreateTreatmentCycleCohort.sql",
                                                             packageName = "CancerTxPathway",
                                                             dbms = attr(connection,"dbms"),
                                                             oracleTempSchema = oracleTempSchema,
                                                             cdm_database_schema = cdmDatabaseSchema,
                                                             oncology_database_schema = oncologyDatabaseSchema,
                                                             vocabulary_database_schema = vocabularyDatabaseSchema,
                                                             target_database_schema = cohortDatabaseSchema,
                                                             target_cohort_table = cohortTable,
                                                             episode_table = episodeTable,
                                                             include_descendant = includeConceptIdSetDescendant,
                                                             collapse_gap_size = collapseGapSize,
                                                             episode_source_concept_ids = paste(conceptIdSet,collapse=","),
                                                             target_cohort_id = targetCohortId)}else{
                                                               sql <- SqlRender::loadRenderTranslateSql(sqlFilename= "CreateTreatmentLineCohort.sql",
                                                                                                        packageName = "CancerTxPathway",
                                                                                                        dbms = attr(connection,"dbms"),
                                                                                                        oracleTempSchema = oracleTempSchema,
                                                                                                        cdm_database_schema = cdmDatabaseSchema,
                                                                                                        oncology_database_schema = oncologyDatabaseSchema,
                                                                                                        vocabulary_database_schema = vocabularyDatabaseSchema,
                                                                                                        target_database_schema = cohortDatabaseSchema,
                                                                                                        target_cohort_table = cohortTable,
                                                                                                        episode_table = episodeTable,
                                                                                                        include_descendant = includeConceptIdSetDescendant,
                                                                                                        episode_source_concept_ids = paste(conceptIdSet,collapse=","),
                                                                                                        target_cohort_id = targetCohortId)}
  DatabaseConnector::executeSql(connection, sql, progressBar = TRUE, reportOverallTime = TRUE)

  DatabaseConnector::disconnect(connection)
}




