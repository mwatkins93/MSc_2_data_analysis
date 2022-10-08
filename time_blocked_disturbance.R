#########################
## Name: Matt Watkins
## Date: Oct. 3rd
## Project: MSc data analysis
## Objective: Time block disturbance data to update master table
## Inputs: master_disturbance_list excel
## Outputs: time blocked dataframes
#########################

## 0. NOTES ----

## 1. PREPARE ----

rm(list=ls())
options(stringsASfactors = FALSE, scipen = 999, encoding = "UTF-8")

library(tidyverse)
library(readxl)

## 2. IMPORT ----

dist <- read_xlsx("/Volumes/MW/2020 Trent University/GIS/Excel Sheets/Master_ForestDisturbance_List_v1.01.xlsx")

## 3. TIDY // PROCESS ----

### Ws 73

ws11 <- dist %>% 
  filter(site == "WS 11" & disturbance.type %in% c("SE", "SH", "CC") & year >= "2000") %>% 
  summarise(disturbed = sum(percent.disturbed))

rm(ws43)

## 4. PLOTTING ----

## 5. SAVING // EXPORTING ----

## 6. TRIAL // JUNK CODE ----

