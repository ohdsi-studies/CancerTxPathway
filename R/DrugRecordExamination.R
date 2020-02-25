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
#' DrugRecordExamination
#' check each index dates of single subject whether drug conditions are satisfied or not.
#' @param targetSubjectId
#' @param primaryConceptRecords
#' @param secondaryConceptRecords
#' @param excludingConceptRecords
#' @param drugInspectionDate
#' @param secondaryConceptIdList
#' @param excludingConceptIdList
#' @keywords primaryDrug, secondaryDrug,eliminatoryDrug
#' @return drug condition passed dates list
#' @export
#' @import dplyr
#' @examples
#' drugRecordExamination(subjectId = 11111111)
drugRecordExamination<-function(targetSubjectId,
                                primaryConceptRecords,
                                secondaryConceptRecords,
                                excludingConceptRecords,
                                drugInspectionDate,
                                secondaryConceptIdList,
                                excludingConceptIdList
){
  ## Dispense date of primary drug is index date
  ## Generate index date list in one person
  if (!nrow(primaryConceptRecords)==0){

    indexDateList <- primaryConceptRecords %>% filter(subjectId == targetSubjectId)
    ## Checking all drug condition
    ### The drug observation period is from the index date to the date as long as drug Observation Date.
    ### Secondary drug should be in the range of drug observation period and eliminatory drug should not be in.


    if(length(secondaryConceptIdList)!=0){
      secondaryConceptRecordsOneSubject <-lapply(1:length(secondaryConceptIdList),function(i){secondaryConceptRecords[[i]] %>% filter (subjectId == targetSubjectId)})

    }

    if(length(excludingConceptIdList)!=0){
      excludingConceptRecordsOneSubject <-excludingConceptRecords %>% filter(subjectId == targetSubjectId)}

    drugConditionPassedDate <-c()
    drugConditionPassedStartDate <- c()
    drugConditionPassedEndDate <- c()
    eventItem <- c()
    ###

    for(x in 1:nrow(indexDateList)){

      inResult <- list()
      endDateList <- list()

      ##secondary concept
      if(length(secondaryConceptIdList)!=0){
        for(i in 1:length(secondaryConceptIdList)){

          secondaryConceptInPeriod <- dplyr::filter(secondaryConceptRecordsOneSubject[[i]],between(drugExposureStartDate,indexDateList[x,3]-drugInspectionDate,indexDateList[x,3]+drugInspectionDate))

          inResult<-append(inResult,list(secondaryConceptInPeriod[1,3]))
          if (length(secondaryConceptInPeriod$drugExposureEndDate)!=0){
            endDateList<-append(endDateList,list(unique(max(secondaryConceptInPeriod$drugExposureEndDate,na.rm =TRUE))))}
        }
      }else{secondaryConceptInPeriod <- NULL}
      ##excluding concept
      if(length(excludingConceptIdList)!=0){
        excludingConceptInPeriod <- dplyr::filter(excludingConceptRecordsOneSubject,between(drugExposureStartDate,indexDateList[x,3]-drugInspectionDate,indexDateList[x,3]+drugInspectionDate))

        outResult <- excludingConceptInPeriod[1,3]

      }else{
        outResult<-NA}
      ##
      if(sum(is.na(inResult))==0 & sum(!is.na(outResult))==0){

        if(!is.null(secondaryConceptInPeriod)){
          drugConditionPassedStartDate[x]<- min(c(indexDateList[x,3],secondaryConceptInPeriod$drugExposureStartDate),na.rm =TRUE)
          targetCycleItemSec <- paste0(secondaryConceptInPeriod$drugExposureId,collapse = '_')
          targetCycleItemPri <- paste0(indexDateList[x,5],collapse = '_')
          targetCycleItem<- paste0(c(targetCycleItemSec,targetCycleItemPri),collapse = '_')
        }else
        {drugConditionPassedStartDate[x]<- indexDateList[x,3]
        targetCycleItem<- paste0(indexDateList[x,5],collapse = '_')}

        eventItem[x] <- targetCycleItem
        if(!is.null(drugConditionPassedStartDate)){
          drugConditionPassedEndDate[x]<- max(c(indexDateList[x,3],unlist(endDateList)),na.rm =TRUE)
        }
      }
    }
    ###
    if(!is.null(drugConditionPassedStartDate)){
      drugConditionPassedDate <- data.frame(drugConditionPassedStartDate,drugConditionPassedEndDate,eventItem)
      drugConditionPassedDate <- na.omit(drugConditionPassedDate)
      drugConditionPassedDate <- drugConditionPassedDate[c(order(drugConditionPassedDate$drugConditionPassedStartDate)),]
      drugConditionPassedDate <- unique(drugConditionPassedDate)
      rownames(drugConditionPassedDate) <- NULL
    }else{drugConditionPassedDate <- data.frame()}
  }else{drugConditionPassedDate<-data.frame()}

  return(drugConditionPassedDate)
}
