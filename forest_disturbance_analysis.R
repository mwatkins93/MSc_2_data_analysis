#########################
## Name: Matt Watkins
## Date: May 24th / 22
## Project: MSc data analysis
## Objective: look into the entirety of forest disturbance and year blocks to see if any patterns emerge
## Inputs: water chemistry and complete forest disturbance excel sheets
## Outputs: graphs
#########################

## 0. NOTES ----



## 1. PREPARE ----

rm(list=ls())
options(stringsASfactors = FALSE, scipen = 999, encoding = "UTF-8")

library(tidyverse)
library(readxl)

## 2. IMPORT ----

f_disturbance <- read_xlsx(path = "/Volumes/MW/2020 Trent University/GIS/Excel Sheets/Complete_Disturbance_List.xlsx", sheet = 2)

## 3. TIDY // PROCESS ----

ws11_tot <- f_disturbance %>% 
  filter(watershed %in% "WS 11") %>% 
  filter(year >= 2015)
  

## 4. PLOTTING ----

## 5. SAVING // EXPORTING ----

## 6. TRIAL // JUNK CODE ----

