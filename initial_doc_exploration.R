#########################
## Name: Matt Watkins
## Date: Aug. 7th, 2022
## Project: MSc data analysis
## Objective: Explore the doc variability with the new datasets
## Inputs: glfc_water_chem RDS
## Outputs: graphs
#########################

## 0. NOTES ----

## 1. PREPARE ----

rm(list=ls())
options(stringsASfactors = FALSE, scipen = 999, encoding = "UTF-8")

library(tidyverse)
library(readxl)

## 2. IMPORT ----

chem <- readRDS("glfc_chem_cleaned_v1.01.RDS")

ws_char <- readRDS("watershed_characteristics_cleaned.RDS")

## 3. TIDY // PROCESS ----

### 3.01 - Subset to DOC

chem_out <- chem %>% 
  select(-glfc.id)

doc <- chem_out %>% 
  subset(variable == "organic.carbon")

### 3.02 - Connect DOC and catchment area

area <- ws_char %>% 
  subset(variable == "area")

area_join <- left_join(area, doc, by = c("site", "catchment.id")) %>% # join by similar columns
  group_by(site) %>% 
  mutate(mean.doc = mean(value.y)) # take the mean DOC for each site
  
  
## 4. PLOTTING ----

### 4.01 - Look at DOC variability across time for all sites

doc %>% 
  ggplot(aes(date, value)) +
  geom_point() +
  facet_wrap(~ catchment.id) +
  labs(x = "", y = "DOC (mg/L)")

### 4.02 - DOC and catchment area

area_join %>% 
  ggplot(aes(value.x, mean.doc)) +
  #geom_text(hjust = 0, vjust = 0) +
  geom_point() +
  scale_x_log10() +
  labs(x = expression(paste("Drainage Area ", (km^2))), y = "DOC (ppm)")
  #geom_smooth(method='lm', formula= y~x)

# add labels - label = catchment.id

## 5. SAVING // EXPORTING ----

## 6. TRIAL // JUNK CODE ----

range(doc$value)

mean(doc$value)
