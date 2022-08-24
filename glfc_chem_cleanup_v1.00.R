#########################
## Name: Matt Watkins
## Date: July 21st
## Project: MSC data analysis
## Objective: Clean up the GLFC chem dataset
## Inputs: GLFC chem data excel sheet
## Outputs: cleaned RDS
#########################

## 0. NOTES ----

## 1. PREPARE ----

rm(list=ls())
options(stringsASfactors = FALSE, scipen = 999, encoding = "UTF-8")

library(tidyverse)
library(readxl)
library(lubridate)

## 2. IMPORT ----

glfc_chem <- read_xlsx("/Volumes/MW/2020 Trent University/Data/GLFC-Water Chemistry/GLFC_water_chem.xlsx")

## 3. TIDY // PROCESS ----

### 3.01 - Change date column to date format; Split up sample.id column into site and sample number; order columns like UW dataset

glfc_chem$sample.date <- mdy(glfc_chem$sample.date) # fix date

glfc_chem <- glfc_chem %>% rename(date = sample.date) # rename date column

site_and_number <- str_split_fixed(glfc_chem$sample.id, "-S-0", 2) # split sample.id column

colnames(site_and_number) <- c("site", "sample") # create new column names

glfc_chem_v2 <- cbind(site_and_number, glfc_chem) %>% 
  select(-sample.id, -drainage.area, -slope, -wetland.percent, -deciduous.percent, -submission.date, -orthoP, -E4_E6, -S300_700) # remove unnecessary columns

glfc_chem_v3 <- glfc_chem_v2 %>% 
  relocate(site, sample, .after = date)

### 3.02 - Convert to long data

glfc_chem_out <- glfc_chem_v3 %>% 
  pivot_longer(cols = 6:50,
               names_to = "variable",
               values_to = "value")

## 4. PLOTTING ----

## 5. SAVING // EXPORTING ----

saveRDS(glfc_chem_out, file = "glfc_chem_cleaned_v1.01.RDS")

## 6. TRIAL // JUNK CODE ----

