#########################
## Name: Matt Watkins
## Date: Nov. 7th/22
## Project: MSc data analysis
## Objective: Mass flux load estimation analysis - varying methods
## Inputs:
## Outputs: clean dataframe with separate columns for each method
#########################

## 0. NOTES ----

### 0.1 Arbitrary timeframe set: June 5th - Oct. 22nd = 140 days or 12,096,000s

## 1. PREPARE ----

rm(list = ls())
options(stringsASfactors = FALSE, scipen = 999, encoding = "UTF-8")

library(tidyverse)
library(readxl)
library(zoo)

## 2. IMPORT ----

### 2.01 - Bring in streamflow, dailyQ and water chemistry ----

strflow <- readRDS("streamflow_final_v1.02.rds")

wtr_chem <- readRDS("glfc_chem_cleaned_v1.01.rds")


## 3. TIDY // PROCESS ----

### 3.01 - Check which well-behaved sites have days with negative Q estimations: three do (WS BL1, WS 40 and WS 87). Leave these for now and proceed. ----

### 3.02 - Test merge process for one watershed (WS 96) ----

ws40_dailyQ <- readRDS("ws40_dailyQ.rds")

ws40_chem <- wtr_chem %>% 
  select(-glfc.id) %>%
  filter(site %in% "WS 40" & variable %in% "organic.carbon") %>% 
  separate(date, c("year", "month", "day"), sep = "(\\-| )") %>% 
  select(-year)

ws40_out <- left_join(ws40_dailyQ, ws40_chem) # This process works
  
ws40_timeframe <- ws40_out[-c(1, 2, 3, 4, 5, 146, 147), ]
  

### 3.03 - Load range mass flux computation (WS 96) ----

#### 3.03.1 - Check range of WS 96's DOC ----

range(ws40_out$value, na.rm = TRUE)

#### 3.03.2 - DOC flux range calculation: high (6.341) and low (3.176) ----

ws40_load_range <- ws40_timeframe %>% 
  mutate(top.mass.flux = ((dailyQ * 11.646) / 25.5),
         btm.mass.flux = ((dailyQ * 8.427) / 25.5)) # Now in mg/s/km^2

12,096,000 # seconds in the corrected timeframe

### 3.03.3 - Summarise total load per study period

ws40_max <- sum(ws40_load_range$top.mass.flux) * 12096000 / 1000000

ws40_min <- sum(ws40_load_range$btm.mass.flux) * 12096000 / 1000000

### 3.04 - Linear interpolation method (WS 96) ----

### 3.05 - Regression model (WS 96) ----

######### Repeatable process for the other 14 watersheds #############
######################################################################

### 3.06 - Load range method (max and min)

ws54_dailyQ <- readRDS("ws54_dailyQ.rds")

ws54_chem <- wtr_chem %>% 
  select(-glfc.id) %>%
  filter(site %in% "WS 54" & variable %in% "organic.carbon") %>% 
  separate(date, c("year", "month", "day"), sep = "(\\-| )") %>% 
  select(-year)

ws54_out <- left_join(ws54_dailyQ, ws54_chem) # This process works

ws54_timeframe <- ws54_out[-c(1, 2, 3, 4, 5, 6, 7, 8, 149), ] # Remove days outside timeframe

range(ws54_chem$value, na.rm = TRUE) # 17.28 - 4.342

ws54_load_range <- ws54_timeframe %>% 
  mutate(top.mass.flux = ((dailyQ * 4.48) / 46.02),
         btm.mass.flux = ((dailyQ * 3.40) / 46.02)) # Now in mg/s/km^2

ws54_max <- sum(ws54_load_range$top.mass.flux) * 12096000 / 1000000 # sums up max load

ws54_min <- sum(ws54_load_range$btm.mass.flux) * 12096000 / 1000000 # sums up min load

### 3.07 linear interpolation 

ws40_inst_flux <- ws40_timeframe %>% 
  mutate(inst.mass.flux = ((dailyQ * value) / 25.5))

ws40_inst_flux$inst.mass.flux <- na.approx(ws40_inst_flux$inst.mass.flux, na.rm = FALSE)



## 4. PLOTTING ----

### 4.01 Look at Q versus DOC for WS 96

ws96_out %>% 
  ggplot(aes(x = dailyQ, y = value)) +
  geom_point()

## 5. SAVING // EXPORTING ----

## 6. TRIAL // JUNK CODE ----

### 6.01 Figure out timeline between WS 108 and WS 54

ws108_dailyQ <- readRDS("ws108_dailyQ.rds")

ws54_dailyQ <- readRDS("ws54_dailyQ.rds")

