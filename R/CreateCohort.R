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
#' @param vocaDatabaseSchema
#' @param cohortTable
#' @param includeConceptIdSetDescendant
#' @export
createCohort <- function(createCohortTable = F,
                         connectionDetails,
                         oracleTempSchema = NULL,
                         cdmDatabaseSchema,
                         cohortDatabaseSchema,
                         vocaDatabaseSchema = cdmDatabaseSchema,
                         cohortTable,
                         includeConceptIdSetDescendant = F
                         ){
  pathToCsv <- system.file("csv", "CancerConceptIdSet.csv", package = "CancerTxPathway")
  cohortsToCreate <- read.csv(pathToCsv, stringsAsFactors = F)
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
  for (i in 1:nrow(cohortsToCreate)) {
  targetConceptIdSet <- paste(strsplit(as.character(cohortsToCreate$conceptIds),';')[[i]],collapse = ',')
  targetCohortId <- cohortsToCreate$cohortId[i]
  sql <- SqlRender::loadRenderTranslateSql(sqlFilename= "CohortGeneration.sql",
                                           packageName = "CancerTxPathway",
                                           dbms = attr(connection,"dbms"),
                                           oracleTempSchema = oracleTempSchema,
                                           cdm_database_schema = cdmDatabaseSchema,
                                           vocabulary_database_schema = vocaDatabaseSchema,
                                           target_database_schema = cohortDatabaseSchema,
                                           target_cohort_table = cohortTable,
                                           include_descendant = includeConceptIdSetDescendant,
                                           condition_concept_ids = targetConceptIdSet,
                                           target_cohort_id = targetCohortId)
  DatabaseConnector::executeSql(connection, sql, progressBar = TRUE, reportOverallTime = TRUE)
  }
  DatabaseConnector::disconnect(connection)
}


