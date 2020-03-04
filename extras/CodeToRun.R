# Details for connecting to the server:
connectionDetails <- DatabaseConnector::createConnectionDetails(dbms='pdw',
                                                                server=Sys.getenv("PDW_SERVER"),
                                                                schema='cdmDatabaseSchema',
                                                                user=NULL,
                                                                password=NULL,
                                                                port='port')

oracleTempSchema <- NULL
cdmDatabaseSchema <- "cdm_database_schema.dbo"
cohortDatabaseSchema <- "cohort_database_schema.dbo"
vocaDatabaseSchema <- "voca_database_schema.dbo"
oncologyDatabaseSchema <- "oncology_database_schema.dbo" # Schema for Episode table and Episode_eventtable, default = cdmDatabaseSchema


createCohortTable <- FALSE # Create cohort table for your cohort table
createEpisodeAndEventTable <- TRUE # warning: existing table might be erased

episodeTable <- "episode_table"
episodeEventTable <- "episode_event_table"
cohortTable <- "cohort"

maxCores <- 4

executeExtraction(connectionDetails,
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
                  maxCores = 4)
