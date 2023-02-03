#########################
## Name: Matt Watkins
## Date: Jan 22nd/23
## Project: MSc data analysis
## Objective: Fit multiple regression to mean DOC
## Inputs: glfc chemistry; doc_table excel sheet; ws_table excel sheet
## Outputs: model fits; ggplot regressions
#########################

## 0. NOTES ----

## 1. PREPARE ----

rm(list=ls())
options(stringsASfactors = FALSE, scipen = 999, encoding = "UTF-8")

library(tidyverse)
library(readxl)
library(performance)

## 2. IMPORT ----

doc_table <- read_excel("~/Desktop/2020 Trent University/R/Thesis Data/MSc_data_analysis/Watershed_table_v1.xlsx", sheet = 2)

ws_table <- read_excel("~/Desktop/2020 Trent University/R/Thesis Data/MSc_data_analysis/Watershed_table_v1.xlsx")

chem <- readRDS("glfc_chem_cleaned_v1.01.rds")

## 3. TIDY // PROCESS ----

mean_doc <- doc_table %>% 
  select(`Site name`, Mean)

### 3.01 - Attach mean DOC to ws table and run the model

doc_ws_table <- left_join(mean_doc, ws_table, by = "Site name")
  
doc_mregression <- lm(Mean ~ `Drainage Area (km2)` + Latitude + Longitude + `Elevation (m a.s.l.)` + `Slope (degrees)` + `Wetland Cover (%)` + `Open Water (%)` + `Total Productive Forest (%)` + `Deciduous Forest (%)` + `Coniferous Forest (%)` + `5-year Harvest Disturbance (%)` + `5-year Insect Disturbance (%)` + `5-year Abiotic Disturbance (%)` + `10-year Harvest Disturbance (%)` + `10-year Insect Disturbance (%)` + `15-year Harvest Disturbance (%)` + `15-year Insect Disturbance (%)` + `15-year Wildfire Disturbance (%)` + `20-year Harvest Disturbance (%)` + `20-year Insect Disturbance (%)`, data = doc_ws_table) # run the model with almost all of the variables for now

# Variables that have been selectively removed:
# 1. 5-year wildfire - no sites affected
# 2. 10-year wildfire - only one site affected
# 3. 10-year abiotic - same as 5-year
# 4. 15-year abiotic - same as 5-year
# 5. 20-year abiotic - same as 5-year
# 6. 20-year wildfire - the same percentage as 15-year wildfire

summary(doc_mregression) # check out the statistics - p-value = 0.16 (all of these variables don't help explain mean [DOC] that much; multiple r2 = 0.81, pretty decent amount of variability explained, which makes sense - this is the kitchen sink)

check_model(doc_mregression) # check the model performance based on the requirements - some problems here (extreme collinearity - likely the temporal disturbance, one outlier, linearity, homogeneity of variance and residual normality not obeyed). This is good because it really can only improve from here.

## 4. PLOTTING ----

## 5. SAVING // EXPORTING ----

## 6. TRIAL // JUNK CODE ----

