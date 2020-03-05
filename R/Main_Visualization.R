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
#' sankey for regimen and surgery
#' Visualization tool for sankey for regimen and surgery
#' @param connectionDetails
#' @param oracleTempSchema
#' @param cdmDatabaseSchema
#' @param cohortDatabaseSchema
#' @param oncologyDatabaseSchema
#' @param vocaDatabaseSchema
#' @param cohortTable
#' @param episodeTable
#' @param outputFolder
#' @param outputFileTitle
#' @param targetCohortIds
#' @param episodeCohortCreate
#' @param createEpisodeCohortTable
#' @param fromYear
#' @param toYear
#' @param identicalSeriesCriteria
#' @param maximumCycleNumber
#' @param heatmapColor
#' @param nodeMinSubject
#' @param collapseDates
#' @param conditionCohortIds
#' @param treatmentLine
#' @param minimumRegimenChange
#' @param surgeryCohortIds
#' @param eventCohortIds
#' @param targetMin
#' @keywords
#' @return CancerTxPatterns plots
#' @examples
#' @import dplyr
#' @import flexdashboard
#' @export CancerTxPatterns
CancerTxPatterns<-function(connectionDetails,
                           oracleTempSchema,
                           cdmDatabaseSchema,
                           cohortDatabaseSchema,
                           oncologyDatabaseSchema,
                           vocaDatabaseSchema,
                           cohortTable,
                           episodeTable,
                           outputFolder,
                           outputFileTitle,
                           targetCohortIds,
                           episodeCohortCreate,
                           createEpisodeCohortTable,
                           fromYear,
                           toYear,
                           identicalSeriesCriteria,
                           maximumCycleNumber,
                           heatmapColor,
                           nodeMinSubject,
                           collapseDates,
                           conditionCohortIds,
                           treatmentLine,
                           minimumRegimenChange,
                           surgeryCohortIds,
                           eventCohortIds,
                           targetMin){

  if(!is.null(outputFolder))
  {if (!file.exists(outputFolder)){dir.create(outputFolder, recursive = TRUE)}}
  if(episodeCohortCreate){
    connection <- DatabaseConnector::connect(connectionDetails = connectionDetails)
    if(createEpisodeCohortTable){ParallelLogger::logInfo("Creating table for the cohorts")
      sql <- SqlRender::loadRenderTranslateSql(sqlFilename= "CreateCohortTable.sql",
                                               packageName = "CancerTxPathway",
                                               dbms = attr(connection,"dbms"),
                                               oracleTempSchema = oracleTempSchema,
                                               cohort_database_schema = cohortDatabaseSchema,
                                               cohort_table = cohortTable)
      DatabaseConnector::executeSql(connection, sql, progressBar = TRUE, reportOverallTime = TRUE)
      DatabaseConnector::disconnect(connection)
    }

    cohortDescription <- cohortDescription() %>% subset(cohortDefinitionId %in% targetCohortIds)
    for(i in 1:length(targetCohortIds)){
      conceptIdSet <- cohortDescription$conceptId[i]
      targetCohortId <- cohortDescription$cohortDefinitionId[i]
      createEpisodeCohort(connectionDetails,
                          oracleTempSchema,
                          cdmDatabaseSchema,
                          cohortDatabaseSchema,
                          oncologyDatabaseSchema,
                          vocabularyDatabaseSchema = vocaDatabaseSchema,
                          cohortTable,
                          episodeTable,
                          conceptIdSet = conceptIdSet,
                          includeConceptIdSetDescendant = F,
                          collapseGapSize = 0,
                          targetCohortId = targetCohortId,
                          cycle = TRUE)}
  }
  ParallelLogger::logInfo("Drawing annual regimen usage graph...")

  usageGraph<-usagePatternGraph(connectionDetails,
                                cohortDatabaseSchema,
                                cohortTable,
                                targetCohortIds,
                                conditionCohortIds,
                                outputFolder,
                                outputFileTitle,
                                fromYear,
                                toYear)
  ParallelLogger::logInfo("Drawing distribution of the regimen iteration heatmap...")
  heatmapPlotData<-heatmapData(connectionDetails,
                               cohortDatabaseSchema,
                               cohortTable,
                               targetCohortIds,
                               outputFolder,
                               outputFileTitle,
                               identicalSeriesCriteria,
                               conditionCohortIds)
  heatmap<-treatmentIterationDistribution(heatmapPlotData,
                                          maximumCycleNumber,
                                          heatmapColor)
  ParallelLogger::logInfo("Drawing a flow chart of the treatment pathway...")
  treatmentPathway<-treatmentPathway(connectionDetails,
                                     cohortDatabaseSchema,
                                     cohortTable,
                                     outputFolder,
                                     outputFileTitle,
                                     conditionCohortIds,
                                     targetCohortIds,
                                     eventCohortIds = surgeryCohortIds,
                                     minimumRegimenChange,
                                     treatmentLine,
                                     collapseDates,
                                     nodeMinSubject)

  ParallelLogger::logInfo("Drawing incidence of the adverse event in each cycle...")
  cycleIncidencePlot <- cycleIncidencePlot(connectionDetails,
                                           cohortDatabaseSchema,
                                           cohortTable,
                                           outputFolder,
                                           outputFileTitle,
                                           targetCohortIds,
                                           conditionCohortIds,
                                           eventCohortIds,
                                           restrictInitialSeries = TRUE,
                                           restricInitialEvent =TRUE,
                                           identicalSeriesCriteria,
                                           eventPeriod = 30,
                                           targetMin)

  ParallelLogger::logInfo("Drawing neutropenia onset timing in each regimen...")
  incidenceDatePlot<- incidenceDatePlot(connectionDetails,
                                        cohortDatabaseSchema,
                                        cohortTable,
                                        targetCohortIds,
                                        outputFolder,
                                        outputFileTitle,
                                        identicalSeriesCriteria = 60,
                                        conditionCohortIds,
                                        eventCohortIds,
                                        restrictEventDate = 90)
  pathToRmd <- system.file("rmd","Treatment_PatternsLocalVer.Rmd",package = "CancerTxPathway")
  rmarkdown::render(pathToRmd,"flex_dashboard",output_dir = outputFolder,output_file = paste0(outputFileTitle,'.','html'),
                    params = list(outputFolder = outputFolder,
                                  outputFileTitle = outputFileTitle,
                                  maximumCycleNumber = maximumCycleNumber,
                                  heatmapColor = heatmapColor),clean = TRUE)
  return(list(usageGraph,heatmap,treatmentPathway,cycleIncidencePlot,incidenceDatePlot))}
