#########################
## Name: Matt Watkins
## Date: Nov. 7th/22
## Project: MSc data analysis
## Objective: Mass flux load estimation analysis - varying methods
## Inputs:
## Outputs: clean dataframe with separate columns for each method
#########################

## 0. NOTES ----

### 0.1 - arbitrary timeframe set: June 5th - Oct. 22nd = 140 days or 12,096,000s

### 0.2 - bring in the partially complete mass load spreadsheet for graphing

## 1. PREPARE ----

rm(list = ls())
options(stringsASfactors = FALSE, scipen = 999, encoding = "UTF-8")

library(tidyverse)
library(readxl)
library(zoo)
library(ggpubr)

## 2. IMPORT ----

### 2.01 - Bring in streamflow, dailyQ and water chemistry ----

strflow <- readRDS("streamflow_final_v1.02.rds")

wtr_chem <- readRDS("glfc_chem_cleaned_v1.01.rds")

### 2.02 - bring in mass load spreadsheet

mass_loads <- read_xlsx("/Volumes/MW/2020 Trent University/R/Thesis Data/MSc_data_analysis/doc_load_estimates.xlsx")


## 3. TIDY // PROCESS ----

### 3.01 - Test merge process for one watershed ----


ws40_dailyQ <- readRDS("ws40_dailyQ.rds")

ws40_chem <- wtr_chem %>% 
  select(-glfc.id) %>%
  filter(site %in% "WS 40" & variable %in% "organic.carbon") %>% 
  separate(date, c("year", "month", "day"), sep = "(\\-| )") %>% 
  select(-year)

ws40_out <- left_join(ws40_dailyQ, ws40_chem) # This process works
  
ws40_timeframe <- ws40_out[-c(1, 2, 3, 4, 5), ] # remove the rows outside of the set timeframe

### 3.03 - Load range mass flux computation ----

#### 3.03.1 - Check range of DOC ----

range(ws40_out$value, na.rm = TRUE)

#### 3.03.2 - DOC flux range calculation: high (6.341) and low (3.176) ----

ws40_load_range <- ws40_timeframe %>% 
  mutate(top.mass.flux = ((dailyQ * 11.646) / 25.5),
         btm.mass.flux = ((dailyQ * 8.427) / 25.5)) # Now in mg/s/km^2

# 12182400 seconds in the corrected timeframe

### 3.03.3 - Summarise total load per study period

### remove the days past Oct. 23rd for the remaining sites first

ws40_load_range <- ws40_load_range[-142, ]

ws40_max <- sum(ws40_load_range$top.mass.flux) * 12182400 / 1000000 # now in kg / km2 over the study period

ws40_min <- sum(ws40_load_range$btm.mass.flux) * 12182400 / 1000000

ws40_load_range %>% 
  saveRDS(file = "ws40_mass_flux.rds") # save these here for now

######### Repeatable process for the other watersheds ################
######################################################################

### 3.04 - linear interpolation ---- 

ws87_timeframe$value <- na.approx(ws87_timeframe$value, na.rm = FALSE) # linearly interpolate the concentrations between sample days

ws87_timeframe$value[1:10] <- 9.013000  # fill NAs to the concentration of sample day 1 for the days prior to it

ws87_inst_flux <- ws87_timeframe %>% 
  mutate(inst.mass.flux = ((dailyQ * value) / 2.1)) # calculate instantaneous mass fluxes in  # mg/s/km^2

ws87_inst_flux <- ws87_inst_flux[-c(142, 143), ] # remove days past Oct. 23rd now that flux is calculated

ws87_int_load_sum <- (sum(ws87_inst_flux$inst.mass.flux) * 12182400) / 1000000

## 4. PLOTTING ----

### 4.01 - Look at Q versus DOC for WS 96 ----

ws96_out %>% 
  ggplot(aes(x = dailyQ, y = value)) + 
  geom_point()

### 4.02 - Look at preliminary seasonal DOC loads in grams per day ----

### 4.02.1 - relevel calculation factors to plot legend properly and change column names

#colnames(mass_loads)[4, 6, 8] <- c("maximum", "minimum", "linear interpolation")

ml_test <- mass_loads %>%
  select(-`linear regression (kg/season)`, -`linear regression (g/day)`, -`linear interpolation (kg/season)`, -`maximum (kg/season)`, -`minimum (kg/season)`) %>%  # remove unfinished regression and kg/season columns
  pivot_longer(cols = 3:5,
             names_to = "calculation",
             values_to = "doc") # pivot to allow for multiple wide columns to be plotted

ml_test$calculation <- factor(ml_test$calculation, levels = c("maximum (g/day)", "linear interpolation (g/day)", "minimum (g/day)")) # reorder factors for better legend order

ml_test %>% 
  ggplot(aes(x = catchment.id, y = doc)) +
  geom_point(aes(colour = calculation, shape = calculation), size = 2) +
  geom_line(aes(group = catchment.id), alpha = .5) +
  theme(axis.text.x=element_text(angle = 45, vjust = 0.5), legend.title = element_blank(), plot.title = element_text(hjust = 0.5)) + labs(x = "", y = expression(paste("Seasonal DOC mass load " (g/day)))) +
  scale_color_manual(labels = c("Maximum", "Linear interpolation", "Minimum"), values = c("#56B4E9", "#009E73", "#E69F00")) +
  scale_shape_manual(labels = c("Maximum", "Linear interpolation", "Minimum"), values = c(17, 16, 15)) +
  scale_x_discrete(limits = c("C2", "C4", "C8", "C9", "C12", "C14", "H2", "H3", "I2", "I3", "I4", "M3", "M4", "M5", "M6"))

### 4.02.2 Same graph but kilograms per entire field season

ml_kg_szn <- mass_loads %>%
  select(-`linear regression (kg/season)`, -`linear regression (g/day)`, -`linear interpolation (g/day)`, -`maximum (g/day)`, -`minimum (g/day)`) %>%  # remove unfinished regression and kg/season columns
  pivot_longer(cols = 3:5,
               names_to = "calculation",
               values_to = "doc") # pivot to allow for multiple wide columns to be plotted

ml_kg_szn$calculation <- factor(ml_kg_szn$calculation, levels = c("maximum (kg/season)", "linear interpolation (kg/season)", "minimum (kg/season)")) # reorder factors for better legend order

ml_kg_szn %>% 
  ggplot(aes(x = catchment.id, y = doc)) +
  geom_point(aes(colour = calculation, shape = calculation), size = 2) +
  geom_line(aes(group = catchment.id), alpha = .5) +
  theme(axis.text.x=element_text(angle = 45, vjust = 0.5), legend.title = element_blank(), plot.title = element_text(hjust = 0.5)) + labs(x = "", y = expression(paste("Seasonal DOC mass load " (kg/season)))) +
  scale_color_manual(labels = c("Maximum", "Linear interpolation", "Minimum"), values = c("#56B4E9", "#009E73", "#E69F00")) +
  scale_shape_manual(labels = c("Maximum", "Linear interpolation", "Minimum"), values = c(17, 16, 15)) +
  scale_x_discrete(limits = c("C2", "C4", "C8", "C9", "C12", "C14", "H2", "H3", "I2", "I3", "I4", "M3", "M4", "M5", "M6"))

## 5. SAVING // EXPORTING ----

wsBL1_inst_flux %>% 
  saveRDS(file = "wsBL1_mass_flux_int.rds")

## 6. TRIAL // JUNK CODE ----

### see what the mass flux rds looks like



### 6.01 Figure out timeline between WS 108 and WS 54

ws108_dailyQ <- readRDS("ws108_dailyQ.rds")

ws54_dailyQ <- readRDS("ws54_dailyQ.rds")


ws87_estQ <- readRDS("ws87_estQ.rds")

