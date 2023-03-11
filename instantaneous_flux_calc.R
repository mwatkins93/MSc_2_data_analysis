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

final_doc_tbl <- readRDS("final_doc_tbl.rds")

## 3. TIDY // PROCESS ----

### 3.01 - Merge and clean the dataframes ----

doc <- glfc_chem %>% 
  pivot_wider(names_from = "variable", values_from = "value") %>% 
  select(site, date, sample, catchment.id, organic.carbon)

ws_sub <- watersheds %>% 
  select(`Site name`, `Drainage Area (km2)`)

colnames(ws_sub)[1] <- "site"

q_out <- q_estimates %>% 
  filter(totalQ >= 0) # remove negative q estimates first

q_merge <- left_join(ws_sub, q_out, by = "site")

doc_merge <- left_join(q_merge, doc, by = c("site", "date"))

### 3.02 - Calculate instantaneous flux ----

inst_flux <- doc_merge %>% 
  mutate(discharge.litres = totalQ * 1000,
         inst.flux = discharge.litres * organic.carbon,
         mg.s.area = inst.flux / `Drainage Area (km2)`,
         log.mg.s.area = log(mg.s.area)) %>% 
  na.omit()

### 3.02 - Rearrange as wide data for model

inst_flux_wide <- inst_flux %>% 
  select(site, sample, log.mg.s.area) %>% 
  pivot_wider(names_from = sample, values_from = log.mg.s.area)

colnames(inst_flux_wide)[1:7] <- c("Site name", "inst.flux.1", "inst.flux.2", "inst.flux.3", "inst.flux.4", "inst.flux.5", "inst.flux.6")

### 3.03 - Attach this to the final doc table

final_doc <- final_doc_tbl[, -c(2:7)]

final_doc_tbl <- left_join(inst_flux_wide, final_doc, by = "Site name")

## 4. PLOTTING ----

## 5. SAVING // EXPORTING ----

saveRDS(inst_flux, file = "inst_flux_steps.rds")

saveRDS(inst_flux_wide, file = "inst_flux_calcs.rds")

saveRDS(final_doc_tbl, file = "final_doc_arranged.rds")

## 6. TRIAL // JUNK CODE ----

### 6.01 - log transform, standardise and move column

final_doc_arranged <- final_doc_tbl %>% 
  select(-drainage_st) %>% 
  mutate(drainage_st = zscore(log(`Drainage Area (km2)`))) %>% 
  relocate(drainage_st, .before = elev_st)

