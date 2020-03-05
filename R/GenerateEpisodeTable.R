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
#' GenerateEpisodeTable
#' Generate regimen records for target regimen in episode table form and insert to database.
#' @param targetRegimenConceptIds regimen concept ids
#' @param connectionDetails An object of type \code{connectionDetails} as created using the
#'                          \code{\link[DatabaseConnector]{createConnectionDetails}} function in the
#'                          DatabaseConnector package.
#' @param cohortTable The name of the table that will be created in the work database schema.
#'                    This table will hold the exposure and outcome cohorts used in this
#'                    study.
#' @param cdmDatabaseSchema
#' @param cohortDatabaseSchema
#' @param targetCohortId
#' @param maxCores Number of cores using in clusterApply
#' @keywords target regimen, records
#' @return records of the target single regimen
#' @examples

#' @export generateEpisodeTable
generateEpisodeTable <- function(targetRegimenConceptIds,
                                 connectionDetails,
                                 cohortTable,
                                 cdmDatabaseSchema,
                                 cohortDatabaseSchema,
                                 targetCohortId,
                                 maxCores){

  parameters <- parameterSetting(targetRegimenConceptIds=targetRegimenConceptIds)
  ParallelLogger::logInfo("parameter loaded")
  targetRegimenRecordsList <- lapply(1:length(parameters),function(i){
    extractTargetRegimen(parameters =parameters[[i]],
                         connectionDetails=connectionDetails,
                         cohortTable=cohortTable,
                         cdmDatabaseSchema=cdmDatabaseSchema,
                         cohortDatabaseSchema=cohortDatabaseSchema,
                         targetCohortId=targetCohortId,
                         maxCores=maxCores)
  })

  targetRegimenRecords <- data.table::rbindlist(targetRegimenRecordsList)

  if(nrow(targetRegimenRecords) == 0){episodeAndEventTable <-list()}else{
    episodeAndEventTable<-recordsInEpisodeTableForm(targetRegimenRecords)}
  return(episodeAndEventTable)}

#' @export
insertEpisodeToDatabase <- function(connectionDetails,
                                    oncologyDatabaseSchema,
                                    episodeTable,
                                    episodeEventTable,
                                    episodeAndEpisodeEvent){
  conn <- DatabaseConnector::connect(connectionDetails)

  episodeRecordsTable <- episodeAndEpisodeEvent[[1]]
  episodeEventRecordsTable <- episodeAndEpisodeEvent[[2]]
  lastEpisodeId<-findEpisodeIdlength(connectionDetails=connectionDetails,
                                     oncologyDatabaseSchema=oncologyDatabaseSchema,
                                     episodeTable=episodeTable)

  lastEpisodeId<-as.numeric(lastEpisodeId[,1])
  if(is.na(lastEpisodeId)){lastEpisodeId <- 0}
  episodeRecordsTable$episode_id <- as.numeric(episodeRecordsTable$episode_id)+lastEpisodeId
  episodeEventRecordsTable$episode_id <- as.numeric(episodeEventRecordsTable$episode_id)+lastEpisodeId

  DatabaseConnector::insertTable(conn, paste0(oncologyDatabaseSchema,'.',episodeTable)
                                 , episodeRecordsTable,dropTableIfExists = FALSE, createTable = FALSE, progressBar = TRUE )

  episodeEventRecordsTable<-as.data.frame(apply(episodeEventRecordsTable,2,as.numeric))

  DatabaseConnector::insertTable(conn, paste0(oncologyDatabaseSchema,'.',episodeEventTable), episodeEventRecordsTable,dropTableIfExists = FALSE, createTable = FALSE, progressBar = TRUE )
  DatabaseConnector::disconnect(conn)
}
#' @export
findEpisodeIdlength <-function(connectionDetails,
                               oncologyDatabaseSchema,
                               episodeTable){

  connection <- DatabaseConnector::connect(connectionDetails)
  sql <- 'SELECT max(episode_id) FROM @oncology_database_schema.@episode_table'
  sql <- SqlRender::render(sql,
                           oncology_database_schema = oncologyDatabaseSchema,
                           episode_table = episodeTable,
  )
  sql <- SqlRender::translate(sql, targetDialect = connectionDetails$dbms)
  result <- DatabaseConnector::querySql(connection, sql)

  DatabaseConnector::disconnect(connection)
  return(result)}
