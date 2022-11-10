#########################
## Name: Matt Watkins
## Date: 
## Project:
## Objective: Look at how Q relates to DOC in the high quality sites
## Inputs:
## Outputs:
#########################

## 0. NOTES ----

## 1. PREPARE ----

rm(list=ls())
options(stringsASfactors = FALSE, scipen = 999, encoding = "UTF-8")

library(tidyverse)
library(readxl)

theme_update(text = element_text(size=20))

## 2. IMPORT ----

### 2.1 Bring in watershed table and glfc chem table----

watersheds <- read_xlsx("Watershed_table_v1.xlsx")

glfc_chem <- readRDS("glfc_data_v3.rds")



## 3. TIDY // PROCESS ----

### 3.1 Subset to sites with only "good" discharge estimates - meaning the entire field season only and no sites with "partially" good data (i.e., why WS 93 is dropped here) ----

goodQ_sites <- watersheds %>% 
  filter(`Discharge Quality` %in% "Good") # 15 sites

### 3.2 Use the above list to filter the good sites in the chem dataset

glfc_chem_sites <- glfc_chem %>% 
  filter(site %in% c("WS 110", "WS 108", "WS 96", "WS 87", "WS 84", "WS 73", "WS BL1", "WS BL2", "WS 66", "WS 54", "WS 52", "WS 47", "WS 46", "WS 43", "WS 40"))

glfc_wide <- glfc_chem_sites %>% 
  pivot_wider(names_from = "variable", values_from = "value")


## 4. PLOTTING ----

### 4.1 Look at their distribution in terms of group (e.g., disturbance type/control) ----

goodQ_sites %>% 
  ggplot(aes(x = Group)) +
  geom_bar() +
  labs(x = "", y = "") +
  ggtitle("Catchments with good discharge estimates") +
  theme(plot.title = element_text(hjust = 0.5))

### 4.2 Plot Discharge against DOC for each site ----

glfc_wide %>% 
  ggplot(aes(x = daily.discharge, y = organic.carbon)) +
  geom_point() +
  facet_wrap(~ catchment.id) +
  #scale_colour_manual(values = c("Control" = "#E69F00", "Harvest" = "#56B4E9", "Insect" = "#009E73", "Mixed" = "#F0E442")) +
  scale_x_log10() +
  theme_bw(base_size = 16) +
  theme(axis.text.x=element_text(angle = 45, vjust = 0.5), legend.position = "none", plot.title = element_text(hjust = 0.5)) + labs(x = expression(paste("Discharge ", (m^3/s))), y = "DOC (mg/L)")


## 5. SAVING // EXPORTING ----

## 6. TRIAL // JUNK CODE ----

