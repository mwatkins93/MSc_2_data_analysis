## ORGANISATION ---------
## Name: Matt Watkins
## Date: Mar. 7th '23
## Project: MSc data analysis
## Objective: instantaneous mass fluxes for all 30 sites
## Inputs:
## Outputs:
## ----------------------

## 0. NOTES ----

## 1. PREPARE ----

rm(list=ls())
options(stringsASfactors = FALSE, scipen = 999, encoding = "UTF-8")

library(tidyverse)
library(readxl)

## 2. IMPORT ----

q_estimates <- readRDS("Q_estimates_v2.rds")

watersheds <- read_excel("Watershed_table_v1.xlsx")

glfc_chem <- readRDS("glfc_chem_cleaned_v1.01.rds")

## 3. TIDY // PROCESS ----

### 3.01 - Merge and clean the dataframes ----

doc <- glfc_chem %>% 
  pivot_wider(names_from = "variable", values_from = "value") %>% 
  select(site, date, sample, catchment.id, organic.carbon)

ws_sub <- watersheds %>% 
  select(`Site name`, `Drainage Area (km2)`)

colnames(ws_sub)[1] <- "site"

q_merge <- left_join(ws_sub, q_estimates, by = "site")

doc_merge <- left_join(q_merge, doc, by = c("site", "date"))

### 3.02 - Calculate instantaneous flux ----

inst_flux <- doc_merge %>% 
  mutate(discharge.litres = totalQ * 1000,
         inst.flux = discharge.litres * organic.carbon,
         inst.flux.per.area = inst.flux / `Drainage Area (km2)`)

## 4. PLOTTING ----

## 5. SAVING // EXPORTING ----

## 6. TRIAL // JUNK CODE ----

