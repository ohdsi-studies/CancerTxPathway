install.packages("DatabaseConnector")
install.packages("collapsibleTree")
install.packages("data.table")
install.packages("dplyr")
install.packages("ggplot2")
install.packages("ggthemes")
install.packages("reshape2")
install.packages("scales")
install.packages("highcharter")
install.packages("gridExtra")
install.packages("viridis")
install.packages("tidyverse")
install.packages("hrbrthemes")
install.packages("plotly")
install.packages("SqlRender")
install.packages("listviewer")
install.packages("tidyr")
install.packages("networkD3")
install.packages("ggbeeswarm")
install.packages("flexdashboard")
library(flexdashboard)

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


outputFolder <- 'output folder path'
outputFileTitle <- 'output file title'
targetCohortIds <- c(4:11)
episodeCohortCreate <- TRUE
minSubject <- 0 # under 0 patients are removed from plot

# Usage Pattern graph
fromYear <- 1998
toYear <- 2018

# Iteration Heatmap
identicalSeriesCriteria <- 60 # Regard as a same treatment when gap dates between each cycle less than 60 days
maximumCycleNumber <- 18 # Ignore patients who received regimen more than 18 iteration

# Treatment Pathway
collapseDates <- 0
conditionCohortIds <- 1 # restrict target patients with certain condition_occurrence
treatmentLine <- 3 # Treatment line number for visualize in graph
minimumRegimenChange <- 1 # Target patients for at least 1 regimen change

# Cohort for surgery and event
surgeryCohortIds <- 42 # Colectomy
eventCohortIds <- 45 # Neutropenia

plots <- CancerTxPatterns(connectionDetails,
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
                          episodeCohortCreate = FALSE,
                          createEpisodeCohortTable,
                          fromYear = 1998,
                          toYear = 2018,
                          identicalSeriesCriteria = 60,
                          maximumCycleNumber = 18,
                          minSubject = 0,
                          collapseDates = 0,
                          conditionCohortIds,
                          treatmentLine = 3,
                          minimumRegimenChange = 1,
                          surgeryCohortIds,
                          eventCohortIds)

