#########################
## Name: Matt Watkins
## Date: Aug. 6th, 2002
## Project: MSc data analysis
## Objective: save watershed characteristic file as RDS
## Inputs: watershed characterisiic excel file
## Outputs: single, clean RDS
#########################

## 0. NOTES ----

## 1. PREPARE ----

rm(list=ls())
options(stringsASfactors = FALSE, scipen = 999, encoding = "UTF-8")

library(tidyverse)
library(readxl)

## 2. IMPORT ----

ws_char <- read_xlsx("/Volumes/MW/2020 Trent University/GIS/Excel Sheets/watershed_characteristics_v1.00.xlsx")

## 3. TIDY // PROCESS ----

ws_char_out <- ws_char %>% 
  pivot_longer(cols = 3:13,
               names_to = "variable",
               values_to = "value")

## 4. PLOTTING ----

## 5. SAVING // EXPORTING ----

saveRDS(ws_char_out, file = "watershed_characteristics_cleaned.RDS")

## 6. TRIAL // JUNK CODE ----

