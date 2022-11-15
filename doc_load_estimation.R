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

ws108_dailyQ <- readRDS("ws108_dailyQ.rds")

ws108_chem <- wtr_chem %>% 
  select(-glfc.id) %>%
  filter(site %in% "WS 108" & variable %in% "organic.carbon") %>% 
  separate(date, c("year", "month", "day"), sep = "(\\-| )") %>% 
  select(-year)

ws108_out <- left_join(ws108_dailyQ, ws108_chem) # This process works
  
ws108_timeframe <- ws108_out[-1, ] # remove the rows outside of the set timeframe

### 3.03 - Load range mass flux computation ----

#### 3.03.1 - Check range of DOC ----

range(ws108_out$value, na.rm = TRUE)

#### 3.03.2 - DOC flux range calculation: high (6.341) and low (3.176) ----

ws108_load_range <- ws108_timeframe %>% 
  mutate(top.mass.flux = ((dailyQ * 4.722) / 30.6),
         btm.mass.flux = ((dailyQ * 2.407) / 30.6)) # Now in mg/s/km^2

12182400 # seconds in the corrected timeframe

### 3.03.3 - Summarise total load per study period

### remove the days past Oct. 23rd for the remaining sites first

ws108_load_range <- ws108_load_range[-c(130, 131, 132), ]

ws108_max <- sum(ws108_load_range$top.mass.flux) * 12182400 / 1000000

ws108_min <- sum(ws108_load_range$btm.mass.flux) * 12182400 / 1000000

ws84_load_range %>% 
  saveRDS(file = "ws84_mass_flux.rds") # save these here for now

######### Repeatable process for the other watersheds ################
######################################################################

### 3.07 linear interpolation 

ws54_dailyQ <- readRDS("ws54_dailyQ.rds")

ws54_chem <- wtr_chem %>% 
  select(-glfc.id) %>%
  filter(site %in% "WS 54" & variable %in% "organic.carbon") %>% 
  separate(date, c("year", "month", "day"), sep = "(\\-| )") %>% 
  select(-year)

ws54_out <- left_join(ws54_dailyQ, ws54_chem) # This process works

ws54_timeframe <- ws54_out[-c(1, 2, 3, 4, 5, 6, 7, 8), ] # remove the rows outside of the set timeframe

ws54_timeframe$value <- na.approx(ws54_timeframe$value, na.rm = FALSE) # linearly interpolate the concentrations between sample days

ws54_timeframe$value[1:7] <- 3.873000  # fill NAs to the concentration of sample day 1 for the days prior to it

ws54_inst_flux <- ws54_timeframe %>% 
  mutate(inst.mass.flux = ((dailyQ * value) / 46)) # calculate instantaneous mass fluxes in  # mg/s/km^2

ws52_inst_flux <- ws52_inst_flux[-142, ] # remove days past Oct. 23rd now that flux is calculated

ws54_int_load_sum <- (sum(ws54_inst_flux$inst.mass.flux) * 12182400) / 1000000

## 4. PLOTTING ----

### 4.01 Look at Q versus DOC for WS 96

# ws96_out %>% ggplot(aes(x = dailyQ, y = value)) + geom_point()

## 5. SAVING // EXPORTING ----

ws54_inst_flux %>% 
  saveRDS(file = "ws54_mass_flux_int.rds")

## 6. TRIAL // JUNK CODE ----

### see what the mass flux rds looks like



### 6.01 Figure out timeline between WS 108 and WS 54

ws108_dailyQ <- readRDS("ws108_dailyQ.rds")

ws54_dailyQ <- readRDS("ws54_dailyQ.rds")

