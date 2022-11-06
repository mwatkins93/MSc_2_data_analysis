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

### 2.1 Bring in watershed table ----

watersheds <- read_xlsx("Watershed_table_v1.xlsx")

## 3. TIDY // PROCESS ----

### 3.1 Subset to sites with only "good" discharge estimates - meaning the entire field season only and no sites with "partially" good data (i.e., why WS 93 is dropped here) ----

goodQ_sites <- watersheds %>% 
  filter(`Discharge Quality` %in% "Good") # 15 sites


## 4. PLOTTING ----

### 4.1 Look at their distribution in terms of group (e.g., disturbance type/control) ----

goodQ_sites %>% 
  ggplot(aes(x = Group)) +
  geom_bar() +
  labs(x = "", y = "") +
  ggtitle("Catchments with good discharge estimates") +
  theme(plot.title = element_text(hjust = 0.5))

## 5. SAVING // EXPORTING ----

## 6. TRIAL // JUNK CODE ----

