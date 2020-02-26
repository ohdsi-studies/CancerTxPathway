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
#' GapDateExamination
#' check cycle interval in the range of setting
#' @param targetSubjectId
#' @param primaryConceptRecords
#' @param secondaryConceptRecords
#' @param excludingConceptRecords
#' @param drugInspectionDate
#' @param secondaryConceptIdList
#' @param excludingConceptIdList
#' @param gapDateBetweenCycle
#' @param gapDateBefore
#' @param gapDateAfter
#' @param regimenConceptId
#' @keywords gap date
#' @return Cycle list of single subject in cohort
#' @export
#' @import dplyr
#' @import data.table
#' @examples
gapDateExamination<-function(targetSubjectId,
                             primaryConceptRecords,
                             secondaryConceptRecords,
                             excludingConceptRecords,
                             drugInspectionDate,
                             secondaryConceptIdList,
                             excludingConceptIdList,
                             gapDateBetweenCycle,
                             gapDateBefore,
                             gapDateAfter,
                             regimenConceptId){

  drugPassed<-drugRecordExamination(targetSubjectId=targetSubjectId,
                                    primaryConceptRecords=primaryConceptRecords,
                                    secondaryConceptRecords=secondaryConceptRecords,
                                    excludingConceptRecords=excludingConceptRecords,
                                    drugInspectionDate=drugInspectionDate,
                                    secondaryConceptIdList=secondaryConceptIdList,
                                    excludingConceptIdList=excludingConceptIdList)
  if (!nrow(drugPassed)==0){

    drugConditionPassedStartDate<-drugPassed$drugConditionPassedStartDate
    drugConditionPassedDate<-as.data.frame(drugConditionPassedStartDate)

    drugConditionPassedDate$lagdate <- data.table::shift(drugConditionPassedDate$drugConditionPassedStartDate,fill = drugConditionPassedDate$drugConditionPassedStartDate[1])

    drugConditionPassedDate$datediff <-drugConditionPassedDate$drugConditionPassedStartDate-drugConditionPassedDate$lagdate
    drugConditionPassedDate$datediff[1] <- 'first'
    drugConditionPassedStartDate<-subset(drugConditionPassedDate,datediff >= gapDateBetweenCycle-gapDateBefore|datediff == 'first')$drugConditionPassedStartDate
    drugConditionPassedDate<-as.data.frame(drugConditionPassedStartDate)
    ## Generate first date list of cycle. From very first record of date list, until next cycle date cannot be found.

    gapDatePassedDate <- list()
    gapDatePassedDate[[1]]<-drugConditionPassedDate[1,]

    x=1
    repeat{

      i=1
      repeat{

        minimumGapVariationDate <- gapDatePassedDate[[x]][i]+gapDateBetweenCycle-gapDateBefore
        maximumGapVariationDate <- gapDatePassedDate[[x]][i]+gapDateBetweenCycle+gapDateAfter

        gapDatePassedDate[[x]][i+1] <- dplyr::filter(drugConditionPassedDate,between(drugConditionPassedDate$`drugConditionPassedStartDate`,minimumGapVariationDate,maximumGapVariationDate))[1,]

        if(is.na(gapDatePassedDate[[x]][i+1]))
        {gapDatePassedDate[[x+1]]<-subset(drugConditionPassedDate,!drugConditionPassedDate$`drugConditionPassedStartDate` %in% unlist(gapDatePassedDate))[1,]}

        if(is.na(gapDatePassedDate[[x]][i+1])) break
        i = i+1
      }

      if(is.na(gapDatePassedDate[[x+1]])) break
      x = x+1}

    gapDatePassedDate[[length(gapDatePassedDate)]] <- NULL

    ## Generate result of cycle extraction

    subjectCycleList<-lapply(1:length(gapDatePassedDate),function(i){
      cycleStartDate <- gapDatePassedDate[[i]]
      subjectId <-c(targetSubjectId)
      cycleNum <- c(seq_along(cycleStartDate))
      REGIMEN_CONCEPT_ID <- regimenConceptId
      cycle <- data.frame(subjectId,cycleStartDate,cycleNum,regimenConceptId)

      drugPassed<-drugPassed %>% group_by(drugConditionPassedStartDate) %>% slice(which.max(drugConditionPassedEndDate))
      cycle <- dplyr::left_join(cycle,drugPassed, by =c("cycleStartDate" = "drugConditionPassedStartDate"))

      names(cycle) <- c('SUBJECT_ID','CYCLE_START_DATE','CYCLE_NUM','REGIMEN_CONCEPT_ID','CYCLE_END_DATE','EVENT_ITEM')
      treatmentLineStartDate<-cycle %>% slice(which.min(CYCLE_START_DATE)) %>% select(CYCLE_START_DATE)
      treatmentLineEndDate<-cycle %>% slice(which.max(CYCLE_END_DATE)) %>% select(CYCLE_END_DATE)
      treatmentLineNumberPadding <- 0
      treatmentLineEventItemPadding <- '1_1'
      treatmentLine <- data.frame(subjectId,treatmentLineStartDate,treatmentLineNumberPadding,REGIMEN_CONCEPT_ID,treatmentLineEndDate,treatmentLineEventItemPadding)
      names(treatmentLine) <- c('SUBJECT_ID','CYCLE_START_DATE','CYCLE_NUM','REGIMEN_CONCEPT_ID','CYCLE_END_DATE','EVENT_ITEM')
      cycle<-rbind(cycle,treatmentLine)
      return(cycle)
    }
    )

    subjectCycleList<- data.table::rbindlist(subjectCycleList)}else{
      SUBJECT_ID<-c(NA)
      CYCLE_START_DATE <- c(NA)
      CYCLE_NUM<- c(NA)
      CYCLE_END_DATE <- c(NA)
      EVENT_ITEM <- c(NA)
      REGIMEN_CONCEPT_ID <- c(NA)
      subjectCycleList <- data.frame(SUBJECT_ID,CYCLE_START_DATE,CYCLE_NUM,REGIMEN_CONCEPT_ID,CYCLE_END_DATE,EVENT_ITEM)
    }

  return(subjectCycleList)}
