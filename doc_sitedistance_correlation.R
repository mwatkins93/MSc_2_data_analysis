#########################
## Name: Matt Watkins
## Date: Jan. 22nd / 23
## Project: MSc data analysis
## Objective: DOC Correlation and distance relationship
## Inputs:
## Outputs:
#########################

## 0. NOTES ----

## 1. PREPARE ----

rm(list=ls())
options(stringsASfactors = FALSE, scipen = 999, encoding = "UTF-8")

library(tidyverse)
library(readxl)

## 2. IMPORT ----

site_pairs <- read_excel("~/Desktop/2020 Trent University/R/Thesis Data/MSc_data_analysis/Watershed_table_v1.xlsx", sheet = 3)

water_chem <- readRDS("glfc_chem_cleaned_v1.00.rds")

## 3. TIDY // PROCESS ----

### 3.01 - Adjust doc in prep for correlation
  
doc <- water_chem %>% 
select(-glfc.id, -date) %>% 
  filter(variable %in% "organic.carbon") %>% 
  pivot_wider(names_from = "sample", values_from = "value")

corr_matrix <- cor(t(doc[4:9]), method="spearman", use = "pairwise.complete.obs") # all correlations between rows completed


### 3.02 - Find a way to make all unique site pairs ----

site_list <- pairs_split %>% 
  select(site.one)

site_list[nrow(site_list) + 1,] <- "C14" # add final site

site_list$site.two = site_list$site.one # duplicate the column

all_pairs <- expand.grid(site_list)

### 3.03 - Find a way to split the sites into site.one and site.two ----

pairs_split <- site_pairs %>% 
  separate(site.pair, c('site.one', 'site.two')) # this works

## 4. PLOTTING ----

## 5. SAVING // EXPORTING ----

## 6. TRIAL // JUNK CODE ----

