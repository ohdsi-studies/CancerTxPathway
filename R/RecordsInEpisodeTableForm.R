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
#' RecordsInEpisodeTableForm
#' Transform regimen records into episode table form.
#' @param regimenRecords regimen records
#' @import dplyr
#' @import tidyr
#' @keywords episode
#' @return list of episode table and episode event table
#' @examples recordsInEpisodeTableForm(regimenRecords)
#' @export
#'
recordsInEpisodeTableForm<- function(regimenRecords){

  classIndex<-class(regimenRecords)
  regimenRecords$CYCLE_START_DATE<-as.Date(regimenRecords$CYCLE_START_DATE,origin="1970-01-01")
  regimenRecords$CYCLE_END_DATE<-as.Date(regimenRecords$CYCLE_END_DATE,origin="1970-01-01")
  regimenRecords$episode_type_concept_id <-32545
  regimenRecords$episode_concept_id <-32532
  regimenRecords[regimenRecords$CYCLE_NUM == 0]$episode_concept_id <- 32531
  regimenRecords$episode_parent_id <-NA
  regimenRecords$episode_object_concept_id <-32525
  regimenRecords$episode_id <- seq(nrow(regimenRecords))
  regimenRecords$episode_source_value <-NA
  class(regimenRecords$episode_parent_id)<- 'integer'

  regimenRecords[regimenRecords$CYCLE_NUM == 0]$episode_parent_id <- regimenRecords[regimenRecords$CYCLE_NUM == 0]$episode_id
  regimenRecords<-regimenRecords %>% group_by(SUBJECT_ID) %>% fill(episode_parent_id,.direction = c("up"))
  class(regimenRecords) <- classIndex
  regimenRecords[regimenRecords$CYCLE_NUM == 0]$episode_parent_id <- NA
  regimenRecords[regimenRecords$CYCLE_NUM == 0]$CYCLE_NUM <- NA

  names(regimenRecords) <- c('person_id',
                             'episode_start_datetime',
                             'episode_number',
                             'episode_source_concept_id',
                             'episode_end_datetime',
                             'episode_item',
                             'episode_type_concept_id',
                             'episode_concept_id',
                             'episode_parent_id',
                             'episode_object_concept_id',
                             'episode_id',
                             'episode_source_value'
  )
  cycleList <- data.frame(regimenRecords)
  regimenRecords <- cycleList[,c(11,1,8,2,5,9,3,10,7,12,4,6)]
  episodeTable <- regimenRecords[,c(1:11)]
  regimenRecords <- regimenRecords %>% subset(episode_concept_id == 32532)
  episodeItemTable <- regimenRecords[,c(1,12)]


  ## event table
  EventTable <- data.table::data.table()

  n <- nrow(episodeItemTable) # total number of rows

  for (i in 1:n){

    name_index <- as.character(episodeItemTable[i, 1])
    item_index <- as.character(episodeItemTable[i, 2])

    item_index_split_temp <- data.frame(strsplit(item_index, split = '_'))
    episodeItemTable_temp <- data.frame(cbind(name_index, item_index_split_temp))

    names(episodeItemTable_temp) <- c("episode_id", "drug_exposure_id")

    EventTable <- rbind(EventTable, episodeItemTable_temp)
  }
  ParallelLogger::logInfo("Episode event table generated")
  EventTable$visit_occurrence_id <- NA
  EventTable$condition_occurrence_id <- NA
  EventTable$procedure_occurrence_id <- NA
  EventTable$device_exposure_id <- NA
  EventTable$measurement_id <- NA
  EventTable$specimen_id <- NA
  EventTable$observation_id <- NA
  EventTable$note_id <- NA
  EventTable$cost_id <- NA

  episodeEventTable <- EventTable[,c(1,3,4,5,2,6,7,8,9,10,11)]

  return(list(episodeTable,episodeEventTable))
}
