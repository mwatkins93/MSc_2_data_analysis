#########################
## Name: Matt Watkins
## Date: 
## Project:
## Objective: 
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

discharge <- readRDS("Q_estimates_v1.RDS")

doc <- readRDS("glfc_data_v3.rds")

watersheds <- read_xlsx("Watershed_table_v1.xlsx")

## 3. TIDY // PROCESS ----

ws_sub <- watersheds %>% 
  select(`Site name`, `Drainage Area (km2)`)

### - 3.01 join the chemistry data ----

doc_sub <- doc %>% 
  select(-glfc.id, -sample) %>% 
  filter(variable %in% "organic.carbon")

cQ <- left_join(discharge, doc_sub)

cQ_sub <- cQ %>% 
  filter(date >= "2021-06-12", # start on the first sample day
         totalQ > 0) # remove the few really bad manual Q measurements

colnames(ws_sub)[1] <- "site"

cQ_out <- left_join(cQ_sub, ws_sub)

cQ_inst_flux <- cQ_out %>% 
  mutate(discharge.litres = totalQ * 1000,
         mass.time = totalQ * value,
         mass.time.area = mass.time / `Drainage Area (km2)`,
         mg.per.day = mass.time.area * 86400)

## 4. PLOTTING ----

cQ_inst_flux %>% 
  ggplot(aes(date, mass.time.area)) +
  geom_point(size = 3) +
  theme_bw(base_size = 16) +
  theme(axis.text.x=element_text(angle = 45, vjust = 0.5), legend.title = element_blank()) +
  facet_wrap(~ catchment.id) +
  scale_y_log10() +
  labs(x = "", y = expression(paste("Instantaneous mass flux " (mg/s/km^2))))


 ## 5. SAVING // EXPORTING ----

## 6. TRIAL // JUNK CODE ----

