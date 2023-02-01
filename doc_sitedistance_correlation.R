#########################
## Name: Matt Watkins
## Date: Jan. 22nd / 23
## Project: MSc data analysis
## Objective: DOC Correlation and distance relationship
## Inputs: watershed table and glfc chemistry
## Outputs: ggplots
#########################

## 0. NOTES ----

### 0.1 - Can just load the clean dist_coeff_df RDS file now for quick plotting

dist_corr <- readRDS("distance_coeff_clean.rds")

## 1. PREPARE ----

rm(list=ls())
options(stringsASfactors = FALSE, scipen = 999, encoding = "UTF-8")

library(tidyverse)
library(readxl)
library(geosphere)

## 2. IMPORT ----

ws_table <- read_excel("~/Desktop/2020 Trent University/R/Thesis Data/MSc_data_analysis/Watershed_table_v1.xlsx", sheet = 1)

water_chem <- readRDS("glfc_chem_cleaned_v1.00.rds")

## 3. TIDY // PROCESS ----

### 3.01 - Find a way to make a distance matrix for site to site distances

distm(c(-84.1009, 46.7460), c(-83.8506, 46.7571), fun = distHaversine) # this calculates the distance between two different lat/long points.

site_latlong <- ws_table %>% 
  select(`Site name`, `Catchment ID`, Latitude, Longitude) # prep df for distance calculation

site_latlong <- site_latlong[, c(1, 2, 4, 3)] # rearrange columns for geosphere package

site_dist_mtx<- distm(site_latlong[3:4], fun = distHaversine) # computes all distances between sites

rownames(site_dist_mtx) = site_latlong$`Catchment ID` # change row and column names so I can set up site pairs later
colnames(site_dist_mtx) = site_latlong$`Catchment ID`

#### 3.01.1 - Reshape the matrix to a pairwise dataframe (site)

euclid_df <- melt(site_dist_mtx)[melt(upper.tri(site_dist_mtx))$value,] # reshape the matrix into a dataframe with three columns

colnames(euclid_df) <- c("site.one", "site.two", "euclidean.distance") # rename the columns

euclid_df$site.pair <- str_c(euclid_df$site.one, euclid_df$site.two, sep = "-")

euclid_df <- euclid_df[, c(1, 2, 4, 3)]

### 3.02 - Adjust doc in prep for correlation
  
doc <- water_chem %>% 
select(-glfc.id, -date) %>% 
  filter(variable %in% "organic.carbon") %>% 
  pivot_wider(names_from = "sample", values_from = "value")

doc_shift <- doc[30:1, ] # reverse site.name column order

doc_ordered <- doc_shift[c(9, 1:5, 7, 8, 6, 10:13, 15, 14, 16, 17, 26:18, 27:30),] # adjust site order so its the same as the lat/long

doc_corr_mtx <- cor(t(doc_ordered[4:9]), method="spearman", use = "pairwise.complete.obs") # all correlations between rows completed

rownames(doc_corr_mtx) = doc_ordered$catchment.id
colnames(doc_corr_mtx) = doc_ordered$catchment.id

#### 3.02.1 - Reshape the doc matrix to a pairwise dataframe (site)

doc_corr_df <- melt(doc_corr_mtx)[melt(upper.tri(doc_corr_mtx))$value,] # reshape the matrix into a dataframe with three columns

colnames(doc_corr_df) <- c("site.one", "site.two", "correlation.coeff") # rename the columns

doc_corr_df$site.pair <- str_c(doc_corr_df$site.one, doc_corr_df$site.two, sep = "-")

doc_corr_df <- doc_corr_df[, c(1, 2, 4, 3)]

doc_corr_out <- doc_corr_df %>% 
  select(site.pair, correlation.coeff)

### 3.03 - Join the dataframes to have the ideal output - site.one, site.two, site.pair, euclid.dist, corr.coeff

dist_coeff_out <- left_join(doc_corr_out, euclid_df, by = "site.pair")

dist_coeff_out <- dist_coeff_out[, c(3, 4, 1, 5, 2)]

## 4. PLOTTING ----

### 4.01 - Plot the general plot of everything

dist_corr %>% 
  ggplot(aes(x = euclidean.distance, y = correlation.coeff)) +
  geom_point() +
  theme_bw()

### 4.02 - What if we remove the sites with missing samples (C13 and C10)

dist_corr %>% 
  filter(!site.one %in% c("C13", "C10") & !site.two %in% c("C13", "C10")) %>% 
  ggplot(aes(x = euclidean.distance, y = correlation.coeff)) +
  geom_point() +
  theme_bw() +
  geom_smooth(method = "lm", se = FALSE)

## 5. SAVING // EXPORTING ----

### 5.01 - Save distance-coefficient df

dist_coeff_out %>% 
  saveRDS(file = "distance_coeff_clean.rds")

## 6. TRIAL // JUNK CODE ----

