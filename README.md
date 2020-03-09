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

# treatmentCycleExtraction

Introduction
==========
Tool for extracting treatment cycle from single medication records in CDM database

Technology
==========
treatmentCycleExtraction is an R package.

Dependencies
============
* SqlRender
* DatabaseConnector
* dplyr
* data.table
* rjson

Getting started
============
In R, use the following commands to download and install:

install.packages("devtools")

devtools::install_github("ABMI/treatmentCycleExtraction")

library('treatmentCycleExtraction')

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
# The name of the database schema where the study-specific cohorts will be instantiated:
cohortDatabaseSchema <-'cohort_Database_Schema.dbo'
cdmDatabaseSchema <- 'cdm_Database_Schema.dbo'
vocaDatabaseSchema <- 'voca_Database_Schema.dbo'
oncologyDatabaseSchema <- 'oncology_Database_Schema.dbo'

# The name of the table where the study-specific cohorts will be instantiated:
cohortTable <-'cohort'
episodeTable <- 'episode_table_name'
episodeEventTable <- 'episode_event_table_name'
createEpisodeAndEventTable <- FALSE

# Target regimen concept ids(blank = all):
targetRegimenConceptIds <- c(35806596,35804761)

# Target cohort definition id:
targetCohortId <- 272

# The number of cores in use
maxCores <- 4

```

# Then run the following :
Generate episode and episode event table :
```r
## Episode table and episode Event generation
episodeAndEpisodeEvent<-generateEpisodeTable(targetRegimenConceptIds,
                                             connectionDetails,
                                             cohortTable,
                                             cdmDatabaseSchema,
                                             cohortDatabaseSchema,
                                             targetCohortId,
                                             maxCores)
```
Insert table into database :
```r
insertEpisodeToDatabase(connectionDetails,
                        oncologyDatabaseSchema,
                        episodeTable,
                        episodeEventTable,
                        createEpisodeAndEventTable,
                        episodeAndEpisodeEvent)
```

# Cohort generation

If you do not have a cohort table, create the target cohort for treatmentCycleExtraction :
```r
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
                  4089661) #colorectal cancer

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
```
# Rule Editor

If you need a modification in the rule for algorithm :
```r
targetRegimenIds <- c(35806596,35804761)
newJson <- ruleEditor(targetRegimenIds) # Edit your rule
newJson <- ruleEditor(new= TRUE) # Add a new rule
ruleSave(newJson,targetRegimenIds) # Save your rule
```

License
=======
  treatmentCycleExtraction is licensed under Apache License 2.0

Development
===========
  treatmentCycleExtraction is being developed in R Studio.


