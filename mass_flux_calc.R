#########################
## Name: Matt Watkins
## Date: May, 2022
## Project: MSc data analysis
## Objective: Mass flux estimation
## Inputs: glfc_data_v1
## Outputs:
#########################

## 0. NOTES ----

## 1. PREPARE ----

rm(list=ls())
options(stringsASfactors = FALSE, scipen = 999, encoding = "UTF-8")

library(tidyverse)
library(readxl)
library(scales)
library(lubridate)

## 2. IMPORT ----

glfc_data <- readRDS("glfc_data_v1.rds")

## 3. TIDY // PROCESS ----

### 3.01 - Select necessary columns and multiple discharge by 1000 to get L/s

glfc_orgC <- glfc_data %>% 
  select(catchment, `Catchment ID`, sample.number, `Sample Date`, `Daily Discharge (Q)`, `Organic Carbon`, `Drainage Area`) %>% 
  na.omit() %>% 
  mutate(discharge.litres = `Daily Discharge (Q)` * 1000)

### 3.02 - Multiply concentration by daily discharge and divide by catchment area to obtain mass flux per unit time per unit area (mg/s/km2)

mass_flux <- glfc_orgC %>% 
  mutate(mass.time = `Daily Discharge (Q)` * `Organic Carbon`) %>% 
  mutate(mass.time.area = mass.time / `Drainage Area`)

### 3.03 - Change date column to date format

mass_flux$`Sample Date` <- mdy(mass_flux$`Sample Date`)

## 4. PLOTTING ----

### 4.01 ---- Mass flux across all catchments ----

mass_flux %>% 
  ggplot(aes(`Sample Date`, mass.time.area)) +
  geom_point(size = 3) +
  theme_bw(base_size = 16) +
  theme(axis.text.x=element_text(angle = 45, vjust = 0.5), legend.title = element_blank()) +
  facet_wrap(~ `Catchment ID`) +
  labs(x = "", y = expression(paste("DOC mass flux " (mg/s/km^2))))

### 4.02 ---- Mass flux comparisons between disturbed and controls catchments in close proximity ----

datOut <- mass_flux %>% 
  select(catchment, sample.number, `Organic Carbon`) %>% 
  pivot_wider(names_from = catchment, values_from = `Organic Carbon`)

#### BL1 & BL2 against WS 73

## 5. SAVING // EXPORTING ----

## 6. TRIAL // JUNK CODE ----

