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
#' usagePattern graph
#' usagePattern graph in treatment regimen
#' @param connectionDetails
#' @param vocaDatabaseSchema
#' @param targetRegimen
#' @param fromYear
#' @param toYear
#' @param outputFolder
#' @param outputFileTitle
#' @keywords year
#' @return usagePattern treatment regimen highcharter graph
#' @examples
#' @import dplyr
#' @import tidyr
#' @import highcharter
#' @export usagePatternGraph
usagePatternGraph<-function(connectionDetails,
                            cohortDatabaseSchema,
                            cohortTable,
                            targetCohortIds,
                            conditionCohortIds = NULL,
                            outputFolder = NULL,
                            outputFileTitle = NULL,
                            fromYear,
                            toYear){
  ##cohort##
  cohortDescript <- cohortDescription()
  cohortForGraph<-cohortCycle(connectionDetails,
                              cohortDatabaseSchema,
                              cohortTable,
                              targetCohortIds,
                              identicalSeriesCriteria = 60,
                              conditionCohortIds)
  cohortForGraph <- cohortForGraph %>% subset(cycle == 1)
  cohortForGraph <- cohortForGraph %>% select(-cohortName,-cycle)
  cohortForGraph$cohortStartDate<-as.Date(cohortForGraph$cohortStartDate)
  cohortForGraph$cohortEndDate<-as.Date(cohortForGraph$cohortEndDate)
  cohortForGraph<-dplyr::left_join(cohortForGraph,cohortDescript, by= c("cohortDefinitionId"="cohortDefinitionId"))

  cohortForGraph<-cohortForGraph %>% select(subjectId,cohortName,cohortStartDate)
  cohortForGraph$cohortStartDate<-format(as.Date(cohortForGraph$cohortStartDate, format="Y-%m-%d"),"%Y")

  cohortForGraph<-cohortForGraph %>% group_by(cohortStartDate,cohortName)
  cohortForGraph<-unique(cohortForGraph)
  cohortForGraph<-cohortForGraph %>% summarise(n=n()) %>%ungroup() %>%  arrange(cohortName,cohortStartDate) %>% subset(cohortStartDate <=toYear & cohortStartDate >=fromYear) %>% group_by(cohortStartDate) %>% mutate(total = sum(n)) %>% mutate(proportion = round(n/total*100,1)) %>% select(cohortStartDate,cohortName,proportion)
  colnames(cohortForGraph) <- c('Year','Cohort','proportion')
  cohortForGraph$Year<-as.integer(cohortForGraph$Year)
  Year<-rep(c(fromYear:toYear),length(unique(cohortForGraph$Cohort)))
  Cohort<-sort(rep(unique(cohortForGraph$Cohort),length(c(fromYear:toYear))))
  index<-data.frame(Year,Cohort)
  index$Year <- as.integer(index$Year)
  index$Cohort<-as.character(index$Cohort)
  plotData<-left_join(index,cohortForGraph)
  plotData[is.na(plotData)]<-0
  h<-plotData %>% highcharter::hchart(.,type="line",hcaes(x = Year,y=proportion,group = Cohort)) %>% hc_xAxis(title = list(text = "Year")) %>% hc_yAxis(title = list(text = "Proportion of the regimen treated patients for total chemotherapy received patients (%)"),from = 0, to =70)
  if(!is.null(outputFolder)){
    fileName <- paste0(outputFileTitle,'_','usagePatternRegimenProportion.csv')
    write.csv(plotData, file.path(outputFolder, fileName),row.names = F)}
  return(h)}
