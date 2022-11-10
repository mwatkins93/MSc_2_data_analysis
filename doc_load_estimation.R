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

ws96_dailyQ <- readRDS("ws96_dailyQ.rds")

ws96_chem <- wtr_chem %>% 
  select(-glfc.id) %>%
  filter(site %in% "WS 96" & variable %in% "organic.carbon") %>% 
  separate(date, c("year", "month", "day"), sep = "(\\-| )") %>% 
  select(-year)

ws96_out <- left_join(ws96_dailyQ, ws96_chem) # This process works
  
ws96_timeframe <- ws96_out[-c(1, 142, 143, 144, 145), ]
  

### 3.03 - Load range mass flux computation (WS 96) ----

#### 3.03.1 - Check range of WS 96's DOC ----

range(ws96_out$value, na.rm = TRUE) # 3.176 - 6.341

#### 3.03.2 - DOC flux range calculation: high (6.341) and low (3.176) ----

ws96_load_range <- ws96_timeframe %>% 
  mutate(top.mass.flux = ((dailyQ * 6.341) / 106.8),
         btm.mass.flux = ((dailyQ * 3.176) / 106.8)) # Now in mg/s/km^2

12,096,000

### 3.03.3 - Summarise total load per study period

ws96_max <- sum(ws96_load_range$top.mass.flux)
ws96_tmax <- ws96_max * 12096000

ws96_min <- sum(ws96_load_range$btm.mass.flux) * 12096000

### 3.04 - Linear interpolation method (WS 96) ----

### 3.05 - Regression model (WS 96) ----

lm_ws96 <- lm(value ~ dailyQ, data = ws96_out)
summary(lm_ws96)

## or fit loess
scatter.smooth(ws96_out$dailyQ, ws96_out$value, span = 2, degree = 2)

######### Repeatable process for the other 14 watersheds #############
######################################################################

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

### Summarise total load per study period

ws54_max <- sum(ws54_load_range$top.mass.flux) * 12096000 / 1000000

ws54_min <- sum(ws54_load_range$btm.mass.flux) * 12096000 / 1000000





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

