Clinical characterization of cancer treatment using the Oncology CDM
=============

<img src="https://img.shields.io/badge/Study%20Status-Repo%20Created-lightgray.svg" alt="Study Status: Repo Created">

- Analytics use case(s): **Characterization**
- Study type: **Clinical Application**
- Tags: **OHDSI-Korea, FEEDER-NET, Oncology WG**
- Study lead: **Hokyun Jeon, Seng Chan You**
- Study lead forums tag: **[Hokyun Jeon](https://forums.ohdsi.org/u/hokyun)**
- Study start date: **Februrary 25, 2020**
- Study end date: **-**
- Protocol: **-**
- Publications: **-**
- Results explorer: **-**

This study aims to charaterize the cancer treatment based on the **Oncology CDM**

# CancerTxPathway

Introduction
==========
Tool for extracting chemotherapy cycle records from single medication records in CDM database.
Then, visualize the treatment pathway using extracted chemotherapy and treatments.
As a proof of the study, the chemotherapy-induced neutropenia onset timing is suggested.  

Technology
==========
CancerTxPathway is an R package codes for whole process of the study.

Dependencies
============
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

Getting started
============
In R, use the following commands to download and install:

install.packages("devtools")

devtools::install_github("ohdsi-studies/CancerTxPathway")

library(CancerTxPathway)
library(flexdashboard)

How to run
============
# Parameter setting for algorithm :

Database parameters :
```r

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

```

# Then run the following :
Generate episode and episode event table :
```r
## Episode table and episode Event generation
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
```

# Parameter setting for visualization :

Before the setting parameters for visualization, insert your cohort information in inst/csv/cohortDescription.csv file.
Then, set the parameters :
```r

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

# ignore the event in range of +- treatmentEffectDates
treatmentEffectDates <- 2
```
# Visualization results export

Excute after the parameter settings and cohort description in csv file :
```r
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
                          eventCohortIds,
                          treatmentEffectDates = 2)
```

License
=======
  CancerTxPathway is licensed under Apache License 2.0

Development
===========
  CancerTxPathway is being developed in R Studio.


