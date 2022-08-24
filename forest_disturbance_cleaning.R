#########################
## Name: Matt Watkins
## Date: July 21st / 22
## Project: MSc data analysis
## Objective: Clean and save master forest disturbance list as RDS
## Inputs: forest disturbance excel sheet
## Outputs:
#########################

## 0. NOTES ----

## 1. PREPARE ----

rm(list=ls())
options(stringsASfactors = FALSE, scipen = 999, encoding = "UTF-8")

library(tidyverse)
library(readxl)

## 2. IMPORT ----

fd <- read_xlsx("/Volumes/MW/2020 Trent University/GIS/Excel Sheets/Master_ForestDisturbance_List_v1.01.xlsx")

## 3. TIDY // PROCESS ----

### 3.01 - Move some columns around

fd_out <- fd %>% relocate(year, .after = site)

## 4. PLOTTING ----

## 5. SAVING // EXPORTING ----

saveRDS(fd_out, file = "fd_cleaned_v1.00.RDS")

## 6. TRIAL // JUNK CODE ----

