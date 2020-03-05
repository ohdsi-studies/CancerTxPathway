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
install.packages("superheat")
install.packages("listviewer")
install.packages("tidyr")
install.packages("RColorBrewer")
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
targetCohortIds <- c(1,2,3,4,5,6,7,8)
episodeCohortCreate <- FALSE

# Usage Pattern graph
fromYear <- 2008
toYear <- 2018

# Iteration Heatmap
identicalSeriesCriteria <- 60
maximumCycleNumber <- 18
heatmapColor <-  "Blues" # 'Reds','Greens'

# Treatment Pathway
nodeMinSubject <- 10 # 10 means under 10 patients nodes are removed from pathway graph
collapseDates <- 0
conditionCohortIds <- NULL # restrict target patients with certain condition_occurrence
treatmentLine <- 3 # Treatment line number for visualize in graph
minimumRegimenChange <- 1 # target patients for at least '1' regimen change

# Cohort for surgery and event
surgeryCohortIds <- 9 # Colectomy
eventCohortIds <- 10 # Neutropenia

targetMin <- 20 # minimum patients number to show in incidence for cycle graph

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
                          targetMin)

