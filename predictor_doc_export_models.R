#########################
## Name: Matt Watkins
## Date: Feb 10th/23
## Project: MSc data analysis
## Objective: Look at predictor variables and DOC export
## Inputs: watershed table, mass flux dataset, glfc chem
## Outputs: 
#########################

## 0. NOTES ----

## 1. PREPARE ----

rm(list=ls())
options(stringsASfactors = FALSE, scipen = 999, encoding = "UTF-8")

library(tidyverse)
library(readxl)

## 2. IMPORT ----

### 2.01 - Bring in glfc chem, watershed table and mass flux estimates ----

glfc_chem <- readRDS("glfc_chem_cleaned_v1.01.rds")

ws_table <- read_excel("Watershed_table_v1.xlsx")

mass_flux <- read_excel("doc_load_estimates.xlsx")

## 3. TIDY // PROCESS ----

### 3.01 - Attach predictors to mass flux estimates (and SUVA from GLFC) ----

predictors <- ws_table[, c(1, 3, 7:16)]

colnames(predictors)[1] <- c("site")

flux_pred <- left_join(mass_flux, predictors, by = "site")

### 3.02 - Run individual models to determine significance ----

summary(mod <- lm(`linear interpolation (g C/m^2/season)` ~ Group, data = flux_pred)) # harvest significant
summary(mod <- lm(`linear interpolation (g C/m^2/season)` ~ `Latitude`, data = flux_pred))
summary(mod <- lm(`linear interpolation (g C/m^2/season)` ~ `Longitude`, data = flux_pred))
summary(mod <- lm(`linear interpolation (g C/m^2/season)` ~ `Drainage Area (km2)`, data = flux_pred))
summary(mod <- lm(`linear interpolation (g C/m^2/season)` ~ `Elevation (m a.s.l.)`, data = flux_pred))
summary(mod <- lm(`linear interpolation (g C/m^2/season)` ~ `Slope (degrees)`, data = flux_pred))
summary(mod <- lm(`linear interpolation (g C/m^2/season)` ~ `Wetland Cover (%)`, data = flux_pred))
summary(mod <- lm(`linear interpolation (g C/m^2/season)` ~ `Open Water (%)`, data = flux_pred))
summary(mod <- lm(`linear interpolation (g C/m^2/season)` ~ `Total Productive Forest (%)`, data = flux_pred))
summary(mod <- lm(`linear interpolation (g C/m^2/season)` ~ `Deciduous Forest (%)`, data = flux_pred))
summary(mod <- lm(`linear interpolation (g C/m^2/season)` ~ `Coniferous Forest (%)`, data = flux_pred))

# Seems as though harvest is the only predictor variable that has any significance explaining the variation in DOC export.

## 4. PLOTTING ----

flux_pred %>% 
  ggplot(aes(x = `Drainage Area (km2)`, y = `linear interpolation (g C/m^2/season)`)) +
  geom_point() +
  scale_x_log10() # drainage area

flux_pred %>% 
  ggplot(aes(x = `Coniferous Forest (%)`, y = `linear interpolation (g C/m^2/season)`)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) # other predictors, just change x-variable

##### Visual observations ####
# Drainage area - nearly two distinct groups here (export seems to skyrocket as catchments go from 0 to 5km in size, while no relationship present for larger catchments). Some sort of scale-driven process limiting export??
# Slope - pretty noisy
# Latitude - outliers skew this positive
# Longitude - noisy
# Elevation - nothing (not a wide range of elevation in my study)
# Wetland - outliers skew this, but small positive relationship (in agreement with literature)
# Open water - no relationship
# Productive forest - no relationship
# Deciduous - not much here
# Coniferous - small positive relationship?

# Cross-check these observations with individual models

## 5. SAVING // EXPORTING ----

## 6. TRIAL // JUNK CODE ----

