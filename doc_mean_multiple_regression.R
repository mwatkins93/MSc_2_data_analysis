#########################
## Name: Matt Watkins
## Date: Jan 22nd/23
## Project: MSc data analysis
## Objective: Fit multiple regression to mean DOC
## Inputs: 
## Outputs:
#########################

## 0. NOTES ----

## 1. PREPARE ----

rm(list=ls())
options(stringsASfactors = FALSE, scipen = 999, encoding = "UTF-8")

library(tidyverse)
library(readxl)

## 2. IMPORT ----

doc_table <- read_excel("~/Desktop/2020 Trent University/R/Thesis Data/MSc_data_analysis/Watershed_table_v1.xlsx", sheet = 2)

ws_table <- read_excel("~/Desktop/2020 Trent University/R/Thesis Data/MSc_data_analysis/Watershed_table_v1.xlsx")

chem <- readRDS("glfc_chem_cleaned_v1.01.rds")

## 3. TIDY // PROCESS ----

mean_doc <- doc_table %>% 
  select(`Site name`, Mean)

### 3.01 - Attach mean DOC to ws table and run the model

doc_ws_table <- left_join(mean_doc, ws_table, by = "Site name")
  
doc_mregression <- lm(Mean ~ `Drainage Area (km2)` + `Elevation (m a.s.l.)` + `Slope (degrees)` + `Wetland Cover (%)` + `Open Water (%)` + `Total Productive Forest (%)` + `Deciduous Forest (%)` + `Coniferous Forest (%)` + `20-year Harvest Disturbance (%)`, data = doc_ws_table)

summary(doc_mregression)

## 4. PLOTTING ----

## 5. SAVING // EXPORTING ----

## 6. TRIAL // JUNK CODE ----

