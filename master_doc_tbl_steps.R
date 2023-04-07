## ORGANISATION ---------
## Name: Matt Watkins
## Date: Apr. 7th '23
## Project: MSc data analysis
## Objective: rebuild the master doc table
## Inputs: glfc_data_v4.rds; Watershed_table_v1.xlsx
## Outputs: master_doc_tbl.rds
## ----------------------

## 0. NOTES ----

## 1. PREPARE ----

rm(list=ls())
options(stringsASfactors = FALSE, scipen = 999, encoding = "UTF-8")

library(tidyverse)
library(readxl)
library(mosaic)

## 2. IMPORT ----

water_chem <- readRDS("glfc_data_v4.rds")

ws_tbl <- read_excel("Watershed_table_v1.xlsx")

## 3. TIDY // PROCESS ----

### 3.01 Attach mean doc and suv to ws table ----

mean_doc <- water_chem %>% 
  filter(variable %in% "organic.carbon") %>% 
  group_by(site) %>% 
  summarize(mean.doc = mean(value),
            mean.suva = mean(suva))

colnames(mean_doc)[1] <- "Site name"

ws_tbl_merge <- left_join(ws_tbl, mean_doc, by = "Site name")

### 3.02 Rerun z-score function on predictors ----

upd_doc_tbl <- ws_tbl_merge %>% 
  mutate(lat_st = zscore(Latitude),
         long_st = zscore(Longitude),
         drainage_st = zscore(`Drainage Area (km2)`),
         elev_st = zscore(`Elevation (m a.s.l.)`),
         slope_st = zscore(`Slope (degrees)`),
         wetland_st = zscore(`Wetland Cover (%)`),
         open_wat_st = zscore(`Open Water (%)`),
         tprod_for_st = zscore(`Total Productive Forest (%)`),
         decid_st = zscore(`Deciduous Forest (%)`),
         conifer_st = zscore(`Coniferous Forest (%)`),
         harv5_st = zscore(`5-year Harvest Disturbance (%)`),
         insect5_st = zscore(`5-year Insect Disturbance (%)`),
         abiotic5_st = zscore(`5-year Abiotic Disturbance (%)`),
         harv10_st = zscore(`10-year Harvest Disturbance (%)`),
         insect10_st = zscore(`10-year Insect Disturbance (%)`),
         wildfire15_st = zscore(`15-year Wildfire Disturbance (%)`),
         harv15_st = zscore(`15-year Harvest Disturbance (%)`),
         insect15_st = zscore(`15-year Insect Disturbance (%)`),
         harv20_st = zscore(`20-year Wildfire Disturbance (%)`),
         insect20_st = zscore(`20-year Insect Disturbance (%)`))

### 3.02 Get doc into individual sample format and attach to table ----

ind_sample_doc <- water_chem %>% 
  filter(variable %in% "organic.carbon") %>% 
  select(-date, -glfc.id, -suva) %>% 
  pivot_wider(names_from = "sample", values_from = "value")

colnames(ind_sample_doc)[c(1, 5:10)] <- c("Site name", "doc.s1", "doc.s2", "doc.s3", "doc.s4", "doc.s5", "doc.s6")

sample_doc <- ind_sample_doc[c(1, 5:10)]

master_ws_tbl <- left_join(upd_doc_tbl, sample_doc, by = "Site name")

## 4. PLOTTING ----

## 5. SAVING // EXPORTING ----

saveRDS(master_ws_tbl, file = "master_doc_tbl.rds")

## 6. TRIAL // JUNK CODE ----


