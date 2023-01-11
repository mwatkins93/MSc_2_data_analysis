#########################
## Name: Matt Watkins
## Date: Jan. 5th/23
## Project: MSc data analysis
## Objective: clean up cQ data for DOC characteristic table
## Inputs: cQ variance data
## Outputs: clean dataframe with columns of interest (mean doc, sd, coefficient of variance of doc)
#########################

## 0. NOTES ----

## 1. PREPARE ----

rm(list=ls())
options(stringsASfactors = FALSE, scipen = 999, encoding = "UTF-8")

library(tidyverse)
library(xtable)
library(readxl)

## 2. IMPORT ----

doc_table <- read_excel("~/Desktop/2020 Trent University/R/Thesis Data/MSc_data_analysis/Watershed_table_v1.xlsx", sheet = 2)

chem <- readRDS("glfc_chem_cleaned_v1.01.rds")

## 3. TIDY // PROCESS ----

doc_data <- chem %>% 
  filter(variable %in% "organic.carbon")

doc_calcs <- doc_data %>% 
group_by(site) %>% 
  mutate(mean.conc = mean(value),
         std.conc = sd(value),
         cv.conc = std.conc / mean.conc) %>% 
  select(-value)

## 4. PLOTTING ----

## 5. SAVING // EXPORTING ----

doc_xtable <- xtable(doc_table)
  
print.xtable(doc_xtable, type = "html", file = "table1_doc_char.html")

## 6. TRIAL // JUNK CODE ----