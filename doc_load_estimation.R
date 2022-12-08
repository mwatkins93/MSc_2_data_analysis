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

rm(list = ls())

strflow <- readRDS("streamflow_final_v1.02.rds")

wtr_chem <- readRDS("glfc_chem_cleaned_v1.01.rds")

### 2.02 - bring in mass load spreadsheet

mass_loads <- read_xlsx("doc_load_estimates.xlsx")

## 3. TIDY // PROCESS ----

### 3.01 - Test merge process for one watershed ----

setwd("/Volumes/MW/2020 Trent University/R/Thesis Data/Water_level_correlation/Discharge Estimates")

ws66_dailyQ <- readRDS("ws66_dailyQ.rds")

ws66_chem <- wtr_chem %>% 
  select(-glfc.id) %>%
  filter(site %in% "WS 66" & variable %in% "organic.carbon") %>% 
  separate(date, c("year", "month", "day"), sep = "(\\-| )") %>% 
  select(-year)

ws66_out <- left_join(ws66_dailyQ, ws66_chem) %>% 
  mutate(Q.litres = dailyQ * 1000) # This process works and also converts Q to L/s so I can obtain mass flux in mg/s

### 3.03 - Load range mass flux computation ----

#### 3.03.1 - Check range of DOC ----

range(ws66_out$value, na.rm = TRUE)

#### 3.03.2 - DOC flux range calculation: high and low ----

ws66_loadrange <- ws66_out %>% # making sure EVERY conversion is correct
  mutate(top.mass.flux = (Q.litres * 10.677),
         btm.mass.flux = (Q.litres * 9.890),
         top.mg.min = (top.mass.flux * 60),
         btm.mg.min = (btm.mass.flux * 60),
         top.mg.hour = (top.mg.min * 60),
         btm.mg.hour = (btm.mg.min * 60),
         top.mg.day = (top.mg.hour * 24),
         btm.mg.day = (btm.mg.hour * 24))

ws66_timeframe <- ws66_loadrange[-c(1, 2, 144), ] # remove the rows outside of the set timeframe

ws66.top.mg.szn <- sum(ws66_timeframe$top.mg.day) # sum the max amount for the study period
ws66.btm.mg.szn <- sum(ws66_timeframe$btm.mg.day) # sum the min amount for the study period

ws66.top.mgC.per.km2.szn <- ws66.top.mg.szn / 1.5 # standardise for catchment area
ws66.btm.mgC.per.km2.szn <- ws66.btm.mg.szn / 1.5
  
ws66.top.kgC.per.km2.szn <- ws66.top.mgC.per.km2.szn / 1000000 # convert to kg C / km2 * szn
ws66.btm.kgC.per.km2.szn <- ws66.btm.mgC.per.km2.szn / 1000000

######### Repeatable process for the other watersheds ################
######################################################################

### 3.04 - linear interpolation ---- 

ws87_loadrange$value <- na.approx(ws87_loadrange$value, na.rm = FALSE) # linearly interpolate the concentrations between sample days

ws87_loadrange$value[1:11] <- 9.013  # fill NAs to the concentration of sample day 1 for the days prior to it

ws87_interpolated <- ws87_loadrange %>% 
  mutate(interpolated.mass.flux = (Q.litres * value)) # calculate instantaneous mass fluxes in mg/s

ws87_int_timeframe <- ws87_interpolated[-c(1, 143, 144), ] %>% 
  mutate(interpolated.mg.day = (interpolated.mass.flux * 86400)) # remove days past Oct. 23rd now that flux is calculated and compute mg per day

ws87.int.mgC.per.km2.szn <- sum(ws87_int_timeframe$interpolated.mg.day) / 2.1

ws87.int.kgC.per.km2.szn <- ws87.int.mgC.per.km2.szn / 1000000


### 3.05 - linear regression ----

ws66_cQ <- lm(value ~ dailyQ, data = ws66_out) # make a model from the 6 manual Q and concurrent concentrations

summary(ws66_cQ)

#### Manually predict concentrations based on regression

ws66_loadrange$reg.predict <- ws66_cQ$coef[1] + ws66_cQ$coef[2]*ws66_loadrange$dailyQ

ws66_reg_mf <- ws66_loadrange %>% 
  mutate(reg.mass.flux = (Q.litres * reg.predict))

ws66_reg_timeframe <- ws66_reg_mf[-c(1, 2, 144), ] %>% 
  mutate(reg.mg.day = (reg.mass.flux * 86400))

ws66.reg.mgC.per.km2.szn <- sum(ws66_reg_timeframe$reg.mg.day) / 1.5

ws66.reg.kgC.per.km2.szn <- ws66.reg.mgC.per.km2.szn / 1000000








## 4. PLOTTING ----

### 4.01 - Look at Q versus DOC for WS 96 ----

ws66_out %>% 
  ggplot(aes(x = dailyQ, y = value)) + 
  geom_point() +
  geom_smooth(method="lm", col="black", se = FALSE)

### 4.02 - Look at preliminary seasonal DOC loads in grams per day ----

### 4.02.1 - relevel calculation factors to plot legend properly and change column names

#colnames(mass_loads)[4, 6, 8] <- c("maximum", "minimum", "linear interpolation")

ml_test <- mass_loads %>%
  select(-`linear regression (kg/season)`, -`linear interpolation (kg/season)`, -`maximum (kg/season)`, -`minimum (kg/season)`) %>%  # remove unfinished regression and kg/season columns
  pivot_longer(cols = 3:6,
             names_to = "calculation",
             values_to = "doc") # pivot to allow for multiple wide columns to be plotted

ml_test$calculation <- factor(ml_test$calculation, levels = c("maximum (g/day)", "linear regression (g/day)", "linear interpolation (g/day)", "minimum (g/day)")) # reorder factors for better legend order

ml_test %>% 
  ggplot(aes(x = catchment.id, y = doc)) +
  geom_point(aes(colour = calculation, shape = calculation), size = 2) +
  geom_line(aes(group = catchment.id), alpha = .5) +
  theme(axis.text.x=element_text(angle = 45, vjust = 0.5), legend.title = element_blank(), plot.title = element_text(hjust = 0.5)) + labs(x = "", y = expression(paste("Seasonal DOC mass load " (g/day)))) +
  scale_color_manual(labels = c("Maximum", "Linear regression", "Linear interpolation", "Minimum"), values = c("#56B4E9", "#CC79A7", "#009E73", "#E69F00")) +
  scale_shape_manual(labels = c("Maximum", "Linear regression", "Linear interpolation", "Minimum"), values = c(17, 18, 16, 15)) +
  scale_x_discrete(limits = c("C2", "C4", "C8", "C9", "C12", "C14", "H2", "H3", "I2", "I3", "I4", "M3", "M4", "M5", "M6"))

### 4.02.2 Same graph but kilograms per entire field season

ml_kg_szn <- mass_loads %>%
  select(-`linear regression (g/day)`, -`linear interpolation (g/day)`, -`maximum (g/day)`, -`minimum (g/day)`) %>%  # remove unfinished regression and kg/season columns
  pivot_longer(cols = 3:6,
               names_to = "calculation",
               values_to = "doc") # pivot to allow for multiple wide columns to be plotted

ml_kg_szn$calculation <- factor(ml_kg_szn$calculation, levels = c("maximum (kg/season)", "linear regression (kg/season)", "linear interpolation (kg/season)", "minimum (kg/season)")) # reorder factors for better legend order

ml_kg_szn %>% 
  ggplot(aes(x = catchment.id, y = doc)) +
  geom_point(aes(colour = calculation, shape = calculation), size = 2) +
  geom_line(aes(group = catchment.id), alpha = .5) +
  theme(axis.text.x=element_text(angle = 45, vjust = 0.5), legend.title = element_blank(), plot.title = element_text(hjust = 0.5)) + labs(x = "", y = expression(paste("Seasonal DOC mass load " (kg/season)))) +
  scale_color_manual(labels = c("Maximum", "Linear regression", "Linear interpolation", "Minimum"), values = c("#56B4E9", "#CC79A7", "#009E73", "#E69F00")) +
  scale_shape_manual(labels = c("Maximum", "Linear regression", "Linear interpolation", "Minimum"), values = c(17, 18, 16, 15)) +
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

