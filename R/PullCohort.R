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
#' @param resultDatabaseSchema
#' @param cohortTable
#' @param targetCohortIds
#' @param newCohortDescription
#' @keywords raw data
#' @return raw data for visualization
#' @examples
#' @export
cohortRecords <- function(connectionDetails,
                          resultDatabaseSchema,
                          cohortTable,
                          targetCohortIds){
  connection <- DatabaseConnector::connect(connectionDetails)
  sql <- 'SELECT * FROM @result_database_schema.@cohort_table WHERE cohort_definition_id IN (@target_cohort_ids)'
  sql <- SqlRender::render(sql,
                           result_database_schema = resultDatabaseSchema,
                           cohort_table = cohortTable,
                           target_cohort_ids= targetCohortIds)
  sql <- SqlRender::translate(sql, targetDialect = connectionDetails$dbms)
  Cohort <- DatabaseConnector::querySql(connection, sql)
  colnames(Cohort) <- SqlRender::snakeCaseToCamelCase(colnames(Cohort))
  DatabaseConnector::disconnect(connection)
  return(Cohort)}
#' @export cohortDescription
cohortDescription <- function(){
  cohortDescriptionPath <- paste0(.libPaths()[1],"/CancerTxPathway/csv/cohortDescription.csv")
  cohortDescription<-read.csv(cohortDescriptionPath)
  return(cohortDescription)
}
