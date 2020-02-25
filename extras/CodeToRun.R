##__Code_to_run__##

# Details for connecting to the server:
connectionDetails <- DatabaseConnector::createConnectionDetails(dbms='pdw',
                                                                server=Sys.getenv("PDW_SERVER"),
                                                                schema='cdmDatabaseSchema',
                                                                user=NULL,
                                                                password=NULL,
                                                                port='port')


# The name of the database schema and table where the study-specific cohorts will be instantiated:
cohortDatabaseSchema <-'cohort_Database_Schema.dbo'
cdmDatabaseSchema <- 'cdm_Database_Schema.dbo'
vocaDatabaseSchema <- 'voca_Database_Schema.dbo'
oncologyDatabaseSchema <- 'oncology_Database_Schema.dbo'


# The name of the table where the study-specific cohorts will be instantiated:
cohortTable <-'cohort'
episodeTable <- 'episode_table_name'
episodeEventTable <- 'episode_event_table_name'

# Create table in your database
createEpisodeAndEventTable <- FALSE

# Target regimen concept ids(blank = all):
targetRegimenConceptIds <- c(35806596,35804761) #FOLFOX, FOLFIRI

targetCohortId <- 272

# The number of cores in use:
maxCores <- 4

## Episode table and episode Event generation:
episodeAndEpisodeEvent<-generateEpisodeTable(targetRegimenConceptIds,
                                             connectionDetails,
                                             cohortTable,
                                             cdmDatabaseSchema,
                                             cohortDatabaseSchema,
                                             targetCohortId,
                                             maxCores)

## Insert episode table to database:
insertEpisodeToDatabase(connectionDetails,
                        oncologyDatabaseSchema,
                        episodeTable,
                        episodeEventTable,
                        createEpisodeAndEventTable,
                        episodeAndEpisodeEvent)

## Create the cohort for treatmentCycleExtraction:
conceptIdSet <- c(443384,
                  4181344,
                  443381,
                  443390,
                  4180792,
                  4180791,
                  443382,
                  4180790,
                  443391,
                  435754,
                  443383,
                  4089661) #colorectal cancer (condition)

targetCohortId <- 272

createCohort(createCohortTable = FALSE,
             connectionDetails = connectionDetails,
             oracleTempSchema = NULL,
             cdmDatabaseSchema = cdmDatabaseSchema,
             cohortDatabaseSchema = cohortDatabaseSchema,
             vocabularyDatabaseSchema = vocaDatabaseSchema,
             cohortTable = cohortTable,
             conceptIdSet = conceptIdSet,
             includeConceptIdSetDescendant = TRUE,
             targetCohortId = targetCohortId)

## Rule Editor ##
devtools::document()
targetRegimenIds <- c(35806596,35804761)
newJson <- ruleEditor(targetRegimenIds=targetRegimenIds) # Edit your rule
newJson <- ruleEditor(new= TRUE) # Add a new rule
ruleSave(newJson,targetRegimenIds) # Save your rule

#Annual Graph
connectionDetails <- DatabaseConnector::createConnectionDetails(dbms='pdw',
                                                                server=Sys.getenv("PDW_SERVER"),
                                                                schema='cdmDatabaseSchema',
                                                                user=NULL,
                                                                password=NULL,
                                                                port='port')

vocaDatabaseSchema <- 'voca_Database_Schema.dbo'
oncologyDatabaseSchema <- 'oncology_Database_Schema.dbo'
resultDatabaseSchema <- 'result_Database_Schema.dbo'
cohortTable <- 'cohort'
targetCohortIds <- c(45,46,48,50,52,53,54,55,56,60)
fromYear <- 2010
toYear <- 2018
annualGraph(connectionDetails,
            vocaDatabaseSchema,
            resultDatabaseSchema,
            cohortTable,
            targetCohortIds,
            conditionCohortIds = NULL,
            fromYear,
            toYear)


#Incidence
connectionDetails <- DatabaseConnector::createConnectionDetails(dbms='pdw',
                                                                server=Sys.getenv("PDW_SERVER"),
                                                                schema='cdmDatabaseSchema',
                                                                user=NULL,
                                                                password=NULL,
                                                                port='port')
resultDatabaseSchema <- 'result_Database_Schema.dbo'
cohortTable <- 'cohort'
targetCohortIds <- c(45,46,48,50,52,53,54,55,56,60)
identicalSeriesCriteria <- 60
conditionCohortIds <- NULL
eventCohortIds <- 44 #neutropenia
eventPeriod <-30
targetMin<-20
colorSeed<-109
restrictInitialSeries = TRUE
restricInitialEvent =TRUE
incidencePlot(connectionDetails,
              resultDatabaseSchema,
              cohortTable,
              targetCohortIds,
              conditionCohortIds,
              eventCohortIds,
              restrictInitialSeries = TRUE,
              restricInitialEvent =TRUE,
              identicalSeriesCriteria = 60,
              eventPeriod = 30,
              targetMin = 20)


# Heatmap
connectionDetails <- DatabaseConnector::createConnectionDetails(dbms='pdw',
                                                                server=Sys.getenv("PDW_SERVER"),
                                                                schema='cdmDatabaseSchema',
                                                                user=NULL,
                                                                password=NULL,
                                                                port='port')

resultDatabaseSchema <- 'result_Database_Schema.dbo'
cohortTable <- 'cohort'
targetCohortIds <- c(45,46,48,50,52,53,54,55,56,60)
identicalSeriesCriteria = 60
conditionCohortIds = NULL
colorSeed <- 4

heatmapPlotData<-heatmapData(connectionDetails,
                             resultDatabaseSchema,
                             cohortTable,
                             targetCohortIds,
                             identicalSeriesCriteria = 60,
                             conditionCohortIds = NULL)
iterationHeatmap(heatmapPlotData,
                 maximumCycleNumber = 18,
                 colorSeed)

#sankey diagram
connectionDetails <- DatabaseConnector::createConnectionDetails(dbms='pdw',
                                                                server=Sys.getenv("PDW_SERVER"),
                                                                schema='cdmDatabaseSchema',
                                                                user=NULL,
                                                                password=NULL,
                                                                port='port')
resultDatabaseSchema <- 'result_Database_Schema.dbo'
cohortTable <- 'cohort'
conditionCohortIds <- NULL
targetCohortIds <- c(45,46,48,50,52,53,54,55,56,60)
eventCohortIds <- 33
minimumRegimenChange <- 0
treatmentLine <- 3
collapseDates <- 3
nodeMinSubject <-10

sankeyDiagram(connectionDetails,
              resultDatabaseSchema,
              cohortTable,
              conditionCohortIds,
              targetCohortIds,
              eventCohortIds,
              minimumRegimenChange,
              treatmentLine,
              collapseDates,
              nodeMinSubject)


# Violin Plot
connectionDetails <- DatabaseConnector::createConnectionDetails(dbms='pdw',
                                                                server=Sys.getenv("PDW_SERVER"),
                                                                schema='cdmDatabaseSchema',
                                                                user=NULL,
                                                                password=NULL,
                                                                port='port')
resultDatabaseSchema <- 'result_Database_Schema.dbo'
cohortTable <- 'cohort'
targetCohortIds <- c(45,46,48,50,52,53,54,55,56,60)
identicalSeriesCriteria <- 60
conditionCohortIds <- NULL
eventCohortIds <- 44
restrictEventDate<-90
restrictInitialSeries <- TRUE

violinPlot(connectionDetails,
           resultDatabaseSchema,
           cohortTable,
           targetCohortIds,
           identicalSeriesCriteria,
           conditionCohortIds,
           eventCohortIds,
           restrictEventDate,
           restrictInitialSeries)

