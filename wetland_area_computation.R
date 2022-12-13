#########################
## Name: Matt Watkins
## Date: Oct. 19th / 22
## Project: MSc data analysis
## Objective: clean up wetland area calculations
## Inputs: weltnad_area excel sheet
## Outputs: summed areas for each catchment
#########################

## 0. NOTES ----

## 1. PREPARE ----

rm(list=ls())
options(stringsASfactors = FALSE, scipen = 999, encoding = "UTF-8")

library(tidyverse)
library(readxl)

## 2. IMPORT ----

### Bring in wetland area excel file

wetland_area <- read_xls("/Volumes/MW/2020 Trent University/GIS/Excel Sheets/wetlands_area_table.xls")

## 3. TIDY // PROCESS ----

### trim dataframe down to only columns I need and summarise area for each catchment

wetarea_trim <- wetland_area %>% 
  select(OGF_ID, Area, interArea, OBJECTID)

colnames(wetarea_trim) <- c("FRI.id", "drainage.area", "intersected.wetland.area", "site")

wetland_out <- wetarea_trim %>% 
  group_by(site) %>% 
  mutate(wt.area.per.site = sum(intersected.wetland.area),
         wt.percent.per.site = (wt.area.per.site / drainage.area) * 100)

### change

## 4. PLOTTING ----

## 5. SAVING // EXPORTING ----

## 6. TRIAL // JUNK CODE ----

