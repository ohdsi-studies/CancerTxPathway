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
#' Main
#' @param connectionDetails
#' @param oracleTempSchema
#' @param cdmDatabaseSchema
#' @param cohortDatabaseSchema
#' @param vocaDatabaseSchema
#' @param oncologyDatabaseSchema
#' @param createCohortTable
#' @param createEpisodeAndEventTable
#' @param episodeTable
#' @param episodeEventTable
#' @param cohortTable
#' @param maxCores
#' @param createTargetCohort
#' @keywords
#' @return Episode table, Episode Event table, cancer cohort
#' @examples
#' @export executeExtraction
#Main
executeExtraction <- function(connectionDetails,
                              oracleTempSchema = NULL,
                              cdmDatabaseSchema,
                              cohortDatabaseSchema,
                              vocaDatabaseSchema = cdmDatabaseSchema,
                              oncologyDatabaseSchema = cdmDatabaseSchema,
                              createCohortTable = FALSE,
                              createEpisodeAndEventTable = FALSE,
                              createTargetCohort = FALSE,
                              episodeTable,
                              episodeEventTable,
                              cohortTable,
                              maxCores = 4
){
  ## create target cohorts
  if(createTargetCohort){
  createCohort(createCohortTable = createCohortTable,
               connectionDetails = connectionDetails,
               oracleTempSchema = oracleTempSchema,
               cdmDatabaseSchema = cdmDatabaseSchema,
               cohortDatabaseSchema = cohortDatabaseSchema,
               vocaDatabaseSchema = vocaDatabaseSchema,
               cohortTable = cohortTable,
               includeConceptIdSetDescendant = TRUE
  )}

  pathToCsv <- system.file("csv", "RegimenConceptId.csv", package = "CancerTxPathway")
  regimenConceptId <- read.csv(pathToCsv)

  ## create the episode table and episode event table
  if(createEpisodeAndEventTable == TRUE){

    connection <- DatabaseConnector::connect(connectionDetails = connectionDetails)
    ParallelLogger::logInfo("Creating table for the episode")
    sql <- SqlRender::loadRenderTranslateSql(sqlFilename= "CreateEpisodeTable.sql",
                                             packageName = "CancerTxPathway",
                                             dbms = attr(connection,"dbms"),
                                             oracleTempSchema = oracleTempSchema,
                                             oncology_database_schema = oncologyDatabaseSchema,
                                             episode_table = episodeTable)
    DatabaseConnector::executeSql(connection, sql, progressBar = TRUE, reportOverallTime = TRUE)
    ParallelLogger::logInfo("Creating table for the episode_event")
    sql <- SqlRender::loadRenderTranslateSql(sqlFilename= "CreateEpisodeEventTable.sql",
                                             packageName = "CancerTxPathway",
                                             dbms = attr(connection,"dbms"),
                                             oracleTempSchema = oracleTempSchema,
                                             oncology_database_schema = oncologyDatabaseSchema,
                                             episode_event_table = episodeEventTable)
    DatabaseConnector::executeSql(connection, sql, progressBar = TRUE, reportOverallTime = TRUE)
    DatabaseConnector::disconnect(connection)
  }

  ## episode table and episode event generation:
  for(i in 1:nrow(regimenConceptId)){
    targetCohortId <- regimenConceptId$targetCohortId[i]
    targetRegimenConceptIds <- strsplit(as.character(regimenConceptId$regimenConceptIds),';')[[i]]
    episodeAndEpisodeEvent <- generateEpisodeTable(targetRegimenConceptIds = targetRegimenConceptIds,
                                                   connectionDetails = connectionDetails,
                                                   cohortTable =cohortTable,
                                                   cdmDatabaseSchema = cdmDatabaseSchema,
                                                   cohortDatabaseSchema = cohortDatabaseSchema,
                                                   targetCohortId = targetCohortId,
                                                   maxCores = maxCores)

    ## Insert episode colorectal table to database:
    insertEpisodeToDatabase(connectionDetails = connectionDetails,
                            oncologyDatabaseSchema = oncologyDatabaseSchema,
                            episodeTable = episodeTable,
                            episodeEventTable = episodeEventTable,
                            episodeAndEpisodeEvent = episodeAndEpisodeEvent)
  }


}
