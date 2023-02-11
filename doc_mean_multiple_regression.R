#########################
## Name: Matt Watkins
## Date: Jan 22nd/23
## Project: MSc data analysis
## Objective: Fit multiple regression to mean DOC
## Inputs: glfc chemistry; doc_table excel sheet; ws_table excel sheet
## Outputs: model fits; ggplot regressions
#########################

## 0. NOTES ----

## 1. PREPARE ----

rm(list=ls())
options(stringsASfactors = FALSE, scipen = 999, encoding = "UTF-8")

library(tidyverse)
library(readxl)
library(performance)
library(glmulti)
library(MuMIn)

## 2. IMPORT ----

doc_table <- read_excel("~/Desktop/2020 Trent University/R/Thesis Data/MSc_data_analysis/Watershed_table_v1.xlsx", sheet = 2)

ws_table <- read_excel("~/Desktop/2020 Trent University/R/Thesis Data/MSc_data_analysis/Watershed_table_v1.xlsx")

## 3. TIDY // PROCESS ----

mean_doc <- doc_table %>% 
  select(`Site name`, Mean)

doc_ws_table <- left_join(mean_doc, ws_table, by = "Site name")

### 3.01 - Run the model (all variables) ----
  
doc_mregression <- lm(Mean ~ `Drainage Area (km2)` + Latitude + Longitude + `Elevation (m a.s.l.)` + `Slope (degrees)` + `Wetland Cover (%)` + `Open Water (%)` + `Total Productive Forest (%)` + `Deciduous Forest (%)` + `Coniferous Forest (%)` + `5-year Harvest Disturbance (%)` + `5-year Insect Disturbance (%)` + `5-year Abiotic Disturbance (%)` + `10-year Harvest Disturbance (%)` + `10-year Insect Disturbance (%)` + `15-year Harvest Disturbance (%)` + `15-year Insect Disturbance (%)` + `15-year Wildfire Disturbance (%)` + `20-year Harvest Disturbance (%)` + `20-year Insect Disturbance (%)`, data = doc_ws_table)

# Variables that have been selectively removed:
# 1. 5-year wildfire - no sites affected
# 2. 10-year wildfire - only one site affected
# 3. 10-year abiotic - same as 5-year
# 4. 15-year abiotic - same as 5-year
# 5. 20-year abiotic - same as 5-year
# 6. 20-year wildfire - the same percentage as 15-year wildfire

summary(doc_mregression) # check out the statistics - p-value = 0.16, multiple r2 = 0.81, pretty decent amount of variability explained, which makes sense - this is the kitchen sink)

check_model(doc_mregression) # check the model performance based on the requirements - some problems here (extreme collinearity - likely the temporal disturbance, one outlier, linearity, homogeneity of variance and residual normality not obeyed). This is good because it really can only improve from here.

### 3.02 - Let's be a bit more selective with the model and choose only a few perceived strong predictors ----

doc_mreg_concise <- lm(Mean ~ `Drainage Area (km2)` + `Wetland Cover (%)` + `Open Water (%)` + `Coniferous Forest (%)` + `10-year Harvest Disturbance (%)` + `10-year Insect Disturbance (%)`, data = doc_ws_table)

summary(doc_mreg_concise) # some higher individual predictor significance here, but overall, doesn't explain the total variability very well (multiple r2 = 0.48). P-value of 0.01, which signifies that these variables do help and the model is better than a model with only the intercept.

check_model(doc_mreg_concise)

### 3.03 - Concise model test without drainage area or open water since their r = 0.52 ----

doc_mreg_openw <- lm(Mean ~ `Wetland Cover (%)` + `Open Water (%)` + `Coniferous Forest (%)` + `10-year Harvest Disturbance (%)` + `10-year Insect Disturbance (%)`, data = doc_ws_table) # remove drainage

summary(doc_mreg_openw)

check_model(doc_mreg_openw)

doc_mreg_drainage <- lm(Mean ~ `Wetland Cover (%)` + `Drainage Area (km2)` + `Coniferous Forest (%)` + `10-year Harvest Disturbance (%)` + `10-year Insect Disturbance (%)`, data = doc_ws_table) # remove open water

summary(doc_mreg_drainage)

check_model(doc_mreg_drainage)

### 3.04 - Use glmulti and find out which model is best

model_fit <- glmulti(y = doc_mregression, level = 1, crit = "aicc")

# level 1 = just look at main effects
# crit = "aicc" for small sample size
# this also throws an error and I can't figure out why...

### 3.05 Generate a model selection table with the MuMin package ----

options(na.action = "na.fail") # change na options so dredge function can work

kitchen_sink_table <- dredge(doc_mregression, rank = "AICc") # test out the global model - DON'T run this right now!

model_table <- dredge(doc_mreg_concise, rank = "AICc") # generate a model selection table based on AICc

# The 'kitchen sink' model has so many variables it takes forever to load. Therefore, I have tried this out with the more concise model (6 variables). 
# AICc suggests that the best model (#25) contains only drainage area and open water (which are somewhat correlated - r = 0.52). Second best is drainage area, open water and coniferous forest
summary(model.avg(model_table)) # multimodel inference

sw(model_table) # relative importance of the included predictor variables

# Interesting that wetland cover has the lowest importance relative to the other variables

### 3.06 - Jason's model analysis and PCA exploration ----

#### 3.06.1 - Model exploration ----

mod1 <- lm(Mean ~ Group + `Drainage Area (km2)`, data = doc_ws_table) # remove open water

mod2 <- lm(Mean ~ Group + log10(`Drainage Area (km2)`) + `Elevation (m a.s.l.)` + `Slope (degrees)` + `Wetland Cover (%)` + `Open Water (%)` + `Coniferous Forest (%)`, data = doc_ws_table)

mod2_upd <- lm(Mean ~ Group + log10(`Drainage Area (km2)`) + `Elevation (m a.s.l.)` + `Wetland Cover (%)` + `Open Water (%)` + `Coniferous Forest (%)`, data = doc_ws_table)

## based on PCA results below - probably should only keep one of (a) slope or (b) wetland cover. Also, should only keep one of (a) deciduous or (b) coniferous

#mod3 <- lm(Mean ~ `Drainage Area (km2)` + `Elevation (m a.s.l.)` + `Slope (degrees)` + `Wetland Cover (%)` + `Open Water (%)` + `Coniferous Forest (%)` + `Deciduous Forest (%)`, data = doc_ws_table)

summary(mod2)
summary(mod2_upd)

model_table <- dredge(mod2, rank = "AICc") # generate a model selection table based on AICc

subset(model_table, delta <= 4, recalc.weights=FALSE)

summary(model.avg(model_table)) # multimodel inference

plot(model.avg(model_table))

sw(model_table) # relative importance of the included predictor variables

model.avg(model_table) # look at correlations between predictor variables

# look at all models
summary(mod <- lm(Mean ~ Group, data = doc_ws_table))
summary(mod <- lm(Mean ~ `Drainage Area (km2)`, data = doc_ws_table)) # sig
summary(mod <- lm(Mean ~ `Elevation (m a.s.l.)`, data = doc_ws_table))
summary(mod <- lm(Mean ~ `Slope (degrees)`, data = doc_ws_table))
summary(mod <- lm(Mean ~ `Wetland Cover (%)`, data = doc_ws_table))
summary(mod <- lm(Mean ~ `Open Water (%)`, data = doc_ws_table)) # sig
summary(mod <- lm(Mean ~ `Deciduous Forest (%)`, data = doc_ws_table))
summary(mod <- lm(Mean ~ `Coniferous Forest (%)`, data = doc_ws_table))
summary(mod <- lm(Mean ~ `5-year Wildfire Disturbance (%)`, data = doc_ws_table))
summary(mod <- lm(Mean ~ `5-year Harvest Disturbance (%)`, data = doc_ws_table))
summary(mod <- lm(Mean ~ `5-year Insect Disturbance (%)`, data = doc_ws_table))
summary(mod <- lm(Mean ~ `5-year Abiotic Disturbance (%)`, data = doc_ws_table))
summary(mod <- lm(Mean ~ `10-year Wildfire Disturbance (%)`, data = doc_ws_table))
summary(mod <- lm(Mean ~ `10-year Harvest Disturbance (%)`, data = doc_ws_table))
summary(mod <- lm(Mean ~ `10-year Insect Disturbance (%)`, data = doc_ws_table))
summary(mod <- lm(Mean ~ `10-year Abiotic Disturbance (%)`, data = doc_ws_table))
summary(mod <- lm(Mean ~ `15-year Wildfire Disturbance (%)`, data = doc_ws_table))
summary(mod <- lm(Mean ~ `15-year Harvest Disturbance (%)`, data = doc_ws_table))
summary(mod <- lm(Mean ~ `15-year Insect Disturbance (%)`, data = doc_ws_table))
summary(mod <- lm(Mean ~ `15-year Abiotic Disturbance (%)`, data = doc_ws_table))
summary(mod <- lm(Mean ~ `20-year Wildfire Disturbance (%)`, data = doc_ws_table))
summary(mod <- lm(Mean ~ `20-year Harvest Disturbance (%)`, data = doc_ws_table))
summary(mod <- lm(Mean ~ `20-year Insect Disturbance (%)`, data = doc_ws_table))
summary(mod <- lm(Mean ~ `20-year Abiotic Disturbance (%)`, data = doc_ws_table))

# Seems that disturbance, deciduous cover and wetland percentage aren't really important for explaining variability..
# Log of catchment area really changes its importance level
# Open water % seems to be significant throughout 

#### 3.05.2 - Predictor PCA analysis ----

pca.dat <- doc_ws_table[,c(10:14, 16:17)] # select columns for pca analysis

pca <- prcomp(pca.dat, center = T, scale. = T) # run the pca

summary(pca) # display the pca results in table format

plot(pca)
biplot(pca)

ggplot(aes(`Wetland Cover (%)`, `Slope (degrees)`), data = doc_ws_table) + geom_point()
ggplot(aes(`Deciduous Forest (%)`, `Coniferous Forest (%)`), data = doc_ws_table) + geom_point()
ggplot(aes(`Elevation (m a.s.l.)`, log10(`Drainage Area (km2)`)), data = doc_ws_table) + geom_point()

ggplot(aes(`Wetland Cover (%)`, Mean), data = doc_ws_table) + geom_point() + geom_smooth(method = 'lm')

ggplot(aes(`Site name`, Mean), data = doc_ws_table) + geom_point() + geom_smooth(method = 'lm')

## site 67 is an outlier

chem %>%
  filter(variable == 'organic.carbon') %>%
  ggplot(aes(date, value)) +
  facet_wrap(~site) +
  geom_point()

## 4. PLOTTING ----

## 5. SAVING // EXPORTING ----

## 6. TRIAL // JUNK CODE ----

