## ORGANISATION ---------
## Name: Matt Watkins
## Date: Mar. 21st '23
## Project: MSc data analysis
## Objective: Examine DIC and DOC patterns over time
## Inputs: glfc_data_v3
## Outputs: facetted scatter plot
## ----------------------

## 0. NOTES ----

## 1. PREPARE ----

rm(list=ls())
options(stringsASfactors = FALSE, scipen = 999, encoding = "UTF-8")

library(tidyverse)
library(readxl)

## 2. IMPORT ----

water_chem <- readRDS("glfc_data_v3.rds")

## 3. TIDY // PROCESS ----

doc_dic <- water_chem %>% 
  filter(variable %in% "inorganic.carbon" | variable %in% "organic.carbon")

## 4. PLOTTING ----

doc_dic %>% 
  ggplot(aes(x = date, y = value, color = variable), show.legend = "none") +
  geom_point() +
  facet_wrap(~ site) +
  labs(x = "", y = "mg/L") +
  scale_color_manual(labels = c("DIC", "DOC"), values = c("orange", "black")) +
  theme(legend.title = element_blank())
  

## 5. SAVING // EXPORTING ----

## 6. TRIAL // JUNK CODE ----

