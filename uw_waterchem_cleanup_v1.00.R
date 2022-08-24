#########################
## Name: Matt Watkins
## Date: July 20th
## Project: MSc data analysis
## Objective: Cleanup UW data to foundational status
## Inputs: UW water chemistry
## Outputs:
#########################

## 0. NOTES ----

## 1. PREPARE ----

rm(list=ls())
options(stringsASfactors = FALSE, scipen = 999, encoding = "UTF-8")

library(tidyverse)
library(readxl)
library(lubridate)

## 2. IMPORT ----

### 2.01 - Bring in UW water chemistry data

uw_chem <- read_xlsx("/Volumes/MW/2020 Trent University/Data/UW-DBP-FP/DBP-FP_Report-BS-Fall_2021_v1.xlsx")

## 3. TIDY // PROCESS ----

### 3.01 - Remove the duplicates - were only able to measure THMsFP for these samples

uw_chem_v1 <- uw_chem[-c(34, 37, 43 ),]

### 3.02 - Shift date portion into a new column; change column names to date and site and bind it back; drop identifier column

site_date <- str_split_fixed(uw_chem_v1$`Sample Identifier`, "_BS-Trent-MW_", 2)

colnames(site_date) <- c("date", "site")

uw_chem_v2 <- cbind(site_date, uw_chem_v1)

uw_chem_v3 <- uw_chem_v2 %>% 
  select(-`Sample Identifier`)

### 3.03 - Make date format; adjust strings in site for consistent naming;

uw_chem_v3$date <- dmy(uw_chem_v3$date)

uw_chem_v4 <- uw_chem_v3 %>% 
  mutate(site.name = case_when(site == "W11" ~ "WS 11",
                          site == "W87" ~ "WS 87",
                          site == "SBC" ~ "WS SBC",
                          site == "W82" ~ "WS 82",
                          site == "W17" ~ "WS 17",
                          site == "W52" ~ "WS 52",
                          site == "W66" ~ "WS 66",
                          site == "KWR" ~ "WS 96"),
         catchment.id = case_when(site == "W11" ~ "M1",
                                   site == "W87" ~ "M6",
                                   site == "SBC" ~ "H1",
                                   site == "W82" ~ "H4",
                                   site == "W17" ~ "I1",
                                   site == "W52" ~ "I3",
                                   site == "W66" ~ "C2",
                                   site == "KWR" ~ "C12"))



### 3.03 - Remove site column, rename new site column

uw_chem_v5 <- uw_chem_v4 %>% 
  select(-site) %>% 
  rename(site = site.name)

### 3.04 - Subset df to only needed columns and change column order

uw_chem_v6 <- uw_chem_v5 %>% 
  select(date, sample.number, pH, `UV254 [cm-1]`, `DOC [ppm]`, `Turbidity [NTU]`, `THMsFP [μg/L]`, `HAAsFP [μg/L]`, site, catchment.id)

uw_chem_v6 <- uw_chem_v6[,c(1,9,2,10,3,4,5,6,7,8)]

### 3.05 Make numeric; change column names; convert to long data 

uw_chem_v6$`HAAsFP [μg/L]` <- as.numeric(uw_chem_v6$`HAAsFP [μg/L]`) # change to numeric so pivot can work

colnames(uw_chem_v6) <- c("date", "site", "sample", "catchment.id", "pH", "UV254", "doc", "turbidity", "thm-fp", "haa-fp")

uw_chem_out <- uw_chem_v6 %>% 
  pivot_longer(cols = 5:10,
               names_to = "variable",
               values_to = "value")

## 4. PLOTTING ----

## 5. SAVING // EXPORTING ----

saveRDS(uw_chem_out, file = "uw_chem_cleaned_v1.02.RDS")



## 6. TRIAL // JUNK CODE ----

