#########################
## Name: Matt Watkins
## Date: Jan. 22nd / 23
## Project: MSc data analysis
## Objective: DOC Correlation and distance relationship
## Inputs: watershed table and glfc chemistry
## Outputs:
#########################

## 0. NOTES ----

## 1. PREPARE ----

rm(list=ls())
options(stringsASfactors = FALSE, scipen = 999, encoding = "UTF-8")

install.packages("ggtree")

library(tidyverse)
library(readxl)
library(geosphere)
library(harrietr)

## 2. IMPORT ----

ws_table <- read_excel("~/Desktop/2020 Trent University/R/Thesis Data/MSc_data_analysis/Watershed_table_v1.xlsx", sheet = 1)

water_chem <- readRDS("glfc_chem_cleaned_v1.00.rds")

## 3. TIDY // PROCESS ----

distm(c(-84.1009, 46.7460), c(-83.8506, 46.7571), fun = distHaversine) # this calculates the distance between two different lat/long points.

site_latlong <- ws_table %>% 
  select(`Site name`, `Catchment ID`, Latitude, Longitude) # prep df for distance calculation

site_latlong <- site_latlong[, c(1, 2, 4, 3)] # rearrange columns for geosphere package

site_dist_mtx<- distm(site_latlong[3:4], fun = distHaversine) # computes all distances between sites

rownames(site_dist_mtx) = site_latlong$`Site name`
colnames(site_dist_mtx) = site_latlong$`Site name`

### 3.01 - Adjust doc in prep for correlation
  
doc <- water_chem %>% 
select(-glfc.id, -date) %>% 
  filter(variable %in% "organic.carbon") %>% 
  pivot_wider(names_from = "sample", values_from = "value")

doc_corr_mtx <- cor(t(doc[4:9]), method="spearman", use = "pairwise.complete.obs") # all correlations between rows completed

rownames(doc_corr_mtx) = doc$site
colnames(doc_corr_mtx) = doc$site



## 4. PLOTTING ----

## 5. SAVING // EXPORTING ----

## 6. TRIAL // JUNK CODE ----

### Find a way to make all unique site pairs ----

site_list <- pairs_split %>% 
  select(site.one)

site_list[nrow(site_list) + 1,] <- "C14" # add final site

site_list$site.two = site_list$site.one # duplicate the column

all_pairs <- expand.grid(site_list)

### Find a way to split the sites into site.one and site.two ----

pairs_split <- site_pairs %>% 
  separate(site.pair, c('site.one', 'site.two')) # this works

