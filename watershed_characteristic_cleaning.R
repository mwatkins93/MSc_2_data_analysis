#########################
## Name: Matt Watkins
## Date: Aug. 6th, 2002
## Project: MSc data analysis
## Objective: save watershed characteristic file as RDS
## Inputs: watershed characterisiic excel file
## Outputs: single, clean RDS
#########################

## 0. NOTES ----

### 0.1 - Brought in the updated master watershed table to make the RDS file for future graph production

## 1. PREPARE ----

rm(list=ls())
options(stringsASfactors = FALSE, scipen = 999, encoding = "UTF-8")

library(tidyverse)
library(readxl)

## 2. IMPORT ----

ws_char <- read_xlsx("/Volumes/MW/2020 Trent University/GIS/Excel Sheets/Watershed_table_v1.xlsx")

## 3. TIDY // PROCESS ----

### Only numeric landscape variables

ws_char_v1 <- ws_char %>% 
  select(-`Discharge Quality`, -`Stage Quality`, -`DBP-FP Sampling`, -Group)

ws_char_out <- ws_char_v1 %>% 
  pivot_longer(cols = 3:29,
               names_to = "variable",
               values_to = "value")

### Change column names

colnames(ws_char_out)[1:2] <- c("site", "catchment.id")

## 4. PLOTTING ----

## 5. SAVING // EXPORTING ----

saveRDS(ws_char_out, file = "watershed_characteristics_cleaned.RDS")

## 6. TRIAL // JUNK CODE ----

