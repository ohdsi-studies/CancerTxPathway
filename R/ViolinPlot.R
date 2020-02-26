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
#' Violin Plot
#' First incidence of event
#' @param connectionDetails
#' @param resultDatabaseSchema
#' @param cohortTable
#' @param targetCohortIds
#' @param identicalSeriesCriteria
#' @param conditionCohortIds
#' @param eventCohortIds
#' @param restrictEventDate
#' @param restrictInitialSeries
#' @keywords Incidence
#' @return Incidence plot
#' @examples
#' @import dplyr
#' @import tidyverse
#' @import ggplot2
#' @import hrbrthemes
#' @import plotly
#' @import viridis
#' @export violinPlot
violinPlot<-function(connectionDetails,
                     resultDatabaseSchema,
                     cohortTable,
                     targetCohortIds,
                     identicalSeriesCriteria,
                     conditionCohortIds,
                     eventCohortIds,
                     restrictEventDate,
                     restrictInitialSeries){
  cohortDescript <-cohortDescription()
  # Pull cohort data
  targetCohort<-cohortCycle(connectionDetails,
                            resultDatabaseSchema,
                            cohortTable,
                            targetCohortIds,
                            identicalSeriesCriteria,
                            conditionCohortIds)
  # Restrict Initial Series
  cohortFirstIndex<-targetCohort %>% subset(cycle == 1) %>% arrange(subjectId,cohortStartDate) %>% group_by(subjectId) %>% mutate(index= row_number())
  indexedCohort<-left_join(targetCohort,cohortFirstIndex)
  indexedCohort$index<-data.table::nafill(indexedCohort$index, type = "locf")
  targetCohort<-indexedCohort %>% subset(index == 1) %>% select(-index)


  eventCohort<-cohortRecords(connectionDetails,
                             resultDatabaseSchema,
                             cohortTable,
                             eventCohortIds)
  eventCohort <- dplyr::left_join(eventCohort,cohortDescript, by= c("cohortDefinitionId"="cohortDefinitionId"))
  eventCohort <- unique(eventCohort %>% mutate (cycle = 0) %>% select(-type) %>% subset(subjectId %in% targetCohort$subjectId))
  # Cohort name cycle
  collapsedCohort<-rbind(targetCohort,eventCohort) %>% arrange(subjectId,cohortStartDate) %>% mutate(cohort_cycle = paste0(cycle,'_',
                                                                                                                           cohortName
  ))
  # Prev record
  collapsedCohort<-collapsedCohort %>% arrange(subjectId,cohortStartDate,desc(cohort_cycle))%>% group_by(subjectId) %>% mutate(prev_c_n_c = lag(cohort_cycle)) %>% mutate(prevDate = lag(cohortStartDate)) %>% ungroup() %>% subset(cycle == 0) %>% subset(cohort_cycle != prev_c_n_c)
  # Event after target
  eventAfterTarget<-unique(na.omit(collapsedCohort %>% subset(cohortName %in% unique(eventCohort$cohortName)))) %>% subset(cohortStartDate-prevDate<= restrictEventDate)

  # Date Diff
  eventAfterTarget<-eventAfterTarget %>% arrange(subjectId,cohortStartDate)%>% group_by(subjectId) %>% slice(1) %>% mutate(dateDiff = as.integer(cohortStartDate - prevDate)) %>% select(subjectId,prev_c_n_c,cohortStartDate,cohortEndDate,dateDiff)

  # Split Cycle Index
  targetIndex <- unique(targetCohort %>% mutate(cohort_cycle = paste0(cycle,'_',cohortName)) %>% group_by(cohort_cycle) %>% select(cohortName,cohort_cycle,cycle))%>% ungroup()

  # Collapse summarised data
  collapsedCohort<-left_join(eventAfterTarget,targetIndex, by=c("prev_c_n_c"='cohort_cycle')) %>% select(-prev_c_n_c)
  collapsedCohort<-as.data.frame(collapsedCohort) %>% subset(cycle == 1)

  plotData<-collapsedCohort %>% arrange(cohortName,dateDiff) %>% mutate(category = ifelse(dateDiff<1,'d1',ifelse(dateDiff<=7,'d2-d8',ifelse(dateDiff<=14,'d9-d15',ifelse(dateDiff<=21,'d16-d22',ifelse(dateDiff<=30,'-d30','>d30'))))))
  plotData$category <- factor(plotData$category,levels = c('d1','d2-d8','d9-d15','d16-d22','-d30','>d30'))
  # plot
  p <- ggplot(plotData,aes(x=cohortName, y=dateDiff)) +
    geom_violin(size=0.2,scale = 'width') +
    ggbeeswarm::geom_quasirandom(size = 1,aes(color = category))+
    scale_color_viridis(discrete=TRUE) + theme_ipsum() +
    theme(
      legend.position= 'bottom',legend.title=element_blank()
    ) +
    coord_flip() +
    xlab("") +
    ylab("Time from discharge (days)")

  p <- plotly::ggplotly(p) %>% layout(legend = list(orientation = 'h',y=-0.1,x=0))
  return(p)
}
