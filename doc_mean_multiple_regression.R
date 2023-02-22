## ORGANISATION --------------
## Name: Matt Watkins
## Date: Jan 22nd/23
## Project: MSc data analysis
## Objective: Fit multiple regression to mean DOC
## Inputs: glfc chemistry; doc_table excel sheet; ws_table excel sheet
## Outputs: model fits; ggplot regressions
## ---------------------------

## 0. NOTES ----

### 0.1 Set NA options here so dredge works throughout ----

options(na.action = "na.fail")

### 0.2 The best models as of right now, based on MuMIn dredge AICc criteria, are:
### (1) log of drainage area, coniferous forest, open water and 10-year insect disturbance
### (2) log of drainage area, coniferous forest, open water and 10-year insect disturbance, 10-year harvest

## 1. PREPARE ----

rm(list=ls())
options(stringsASfactors = FALSE, scipen = 999, encoding = "UTF-8")

library(tidyverse)
library(readxl)
library(performance)
library(MuMIn)
library(mosaic)
library(broom)

## 2. IMPORT ----

doc_table <- read_excel("~/Desktop/2020 Trent University/R/Thesis Data/MSc_data_analysis/Watershed_table_v1.xlsx", sheet = 2)

ws_table <- read_excel("~/Desktop/2020 Trent University/R/Thesis Data/MSc_data_analysis/Watershed_table_v1.xlsx")

water_chem <- readRDS("glfc_chem_cleaned_v1.01.rds")

## 3. TIDY // PROCESS ----

mean_doc <- doc_table %>% 
  select(`Site name`, Mean) # mean DOC for main models

doc_ws_table <- left_join(mean_doc, ws_table, by = "Site name") # (Part 1 - all sites)

mean_doc_min67 <- doc_table %>% 
  filter(!`Site name` %in% "WS 67") %>% 
  select(`Site name`, Mean) # mean DOC without WS 67 (Part 2 - no WS 67 (outlier)

doc_sub_merge <- left_join(mean_doc_min67, ws_table, by = "Site name")

### 3.01 - Part 1: Run the model (all variables, all sites) ----
  
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

# model_fit <- glmulti(y = doc_mregression, level = 1, crit = "aicc")

# level 1 = just look at main effects
# crit = "aicc" for small sample size
# this also throws an error and I can't figure out why...

### 3.05 Generate a model selection table with the MuMin package ----

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

#### 3.06.2 - Predictor PCA analysis ----

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

### 3.07 Part 2: No WS 67 ############### ----

#### 3.07.1 - Run the model (all variables, all sites) ----

doc_mreg_sub <- lm(Mean ~ log(`Drainage Area (km2)`) + Latitude + Longitude + `Elevation (m a.s.l.)` + `Slope (degrees)` + `Wetland Cover (%)` + `Open Water (%)` + `Total Productive Forest (%)` + `Deciduous Forest (%)` + `Coniferous Forest (%)` + `5-year Harvest Disturbance (%)` + `5-year Insect Disturbance (%)` + `5-year Abiotic Disturbance (%)` + `10-year Harvest Disturbance (%)` + `10-year Insect Disturbance (%)` + `15-year Harvest Disturbance (%)` + `15-year Insect Disturbance (%)` + `15-year Wildfire Disturbance (%)` + `20-year Harvest Disturbance (%)` + `20-year Insect Disturbance (%)`, data = doc_sub_merge)

summary(doc_mreg_sub) # no significance seen - remember this is everything
s
#### 3.07.2 - More selective with the model and choose only a few perceived strong predictors ----

doc_mreg_sub2 <- lm(Mean ~ log(`Drainage Area (km2)`) + `Wetland Cover (%)` + `Open Water (%)` + `Coniferous Forest (%)` + `10-year Harvest Disturbance (%)` + `10-year Insect Disturbance (%)`, data = doc_sub_merge)

summary(doc_mreg_sub2)

# lots of significance here - area, open water, coniferous, 10-year insect
# still interesting that wetland is so insignificant in explaining variability

#### 3.07.3 - Start tinkering based on PCA ----

doc_mreg_slope <- lm(Mean ~ log(`Drainage Area (km2)`) + `Slope (degrees)` + `Open Water (%)` + `Coniferous Forest (%)` + `10-year Harvest Disturbance (%)` + `10-year Insect Disturbance (%)`, data = doc_sub_merge) # slope, no wetland

summary(doc_mreg_slope) # low significance for slope as well

doc_mreg_noharv <- lm(Mean ~ log(`Drainage Area (km2)`) + `Slope (degrees)` + `Open Water (%)` + `Coniferous Forest (%)` + `10-year Insect Disturbance (%)`, data = doc_sub_merge) # no harvest variable

summary(doc_mreg_noharv) # similar, slight increases to all p-values except slope (large decrease)

doc_mreg_decid <- lm(Mean ~ log(`Drainage Area (km2)`) + `Slope (degrees)` + `Open Water (%)` + `Deciduous Forest (%)` + `10-year Harvest Disturbance (%)` + `10-year Insect Disturbance (%)`, data = doc_sub_merge) # decid, no coniferous

summary(doc_mreg_decid) # very low significance here - I predict a low ranking model come MuMIn

doc_mreg_group <- lm(Mean ~ Group + log(`Drainage Area (km2)`) + `Slope (degrees)` + `Open Water (%)` + `Coniferous Forest (%)` + `10-year Harvest Disturbance (%)` + `10-year Insect Disturbance (%)`, data = doc_sub_merge) # include group class and switch to conifer

summary(doc_mreg_group) # no group significance here

### 3.08 - Part 3: Model selection with MuMIn, No WS 67 ----

no67_mtable1 <- dredge(doc_mreg_sub2, rank = "AICc") #1. the concise variable model

##### Model analysis
summary(model.avg(no67_mtable1)) # multimodel inference
plot(model.avg(no67_mtable1))
sw(no67_mtable1) # relative importance of the included predictor variables
model.avg(no67_mtable1) # look at correlations between predictor variables

# Best models with the variables included are: (1) area, conifer, open water and 10-year insect; (2) model 1 + 10-year harvest.

no67_mtable_slope <- dredge(doc_mreg_slope, rank = "AICc") #2. the concise variable model, but slope replaces wetland

##### Model analysis
summary(model.avg(no67_mtable_slope)) # multimodel inference
plot(model.avg(no67_mtable_slope))
sw(no67_mtable_slope) # relative importance of the included predictor variables
model.avg(no67_mtable1_slope) # look at correlations between predictor variables

# Slope does not change the top two models, it is first included in model #9

no67_mtable_noharv <- dredge(doc_mreg_noharv, rank = "AICc") #3. the concise variable model, no harvest + slope

# No change as expected

no67_mtable_decid <- dredge(doc_mreg_decid, rank = "AICc") #4. deciduous, no coniferous with slope

# Actually makes model #1 just 10-year insect and drainage area - likely because decid is far less important than coniferous; however it also removes open water.

no67_mtable_group <- dredge(doc_mreg_group, rank = "AICc") #5. group variable included, slope + conifer

# No change as expected.

### 3.09 - Part 4: Look at all individual predictors in separate models again ----

summary(mod <- lm(Mean ~ Group, data = doc_sub_merge)) # harvest sig 0.02
summary(mod <- lm(Mean ~ log(`Drainage Area (km2)`), data = doc_sub_merge)) # sig (0.016), log less sig (0.017)
summary(mod <- lm(Mean ~ `Elevation (m a.s.l.)`, data = doc_sub_merge)) # sig 0.049
summary(mod <- lm(Mean ~ `Slope (degrees)`, data = doc_sub_merge))
summary(mod <- lm(Mean ~ `Wetland Cover (%)`, data = doc_sub_merge))
summary(mod <- lm(Mean ~ `Open Water (%)`, data = doc_sub_merge)) # sig 0.01
summary(mod <- lm(Mean ~ `Deciduous Forest (%)`, data = doc_sub_merge))
summary(mod <- lm(Mean ~ `Coniferous Forest (%)`, data = doc_sub_merge))
summary(mod <- lm(Mean ~ `5-year Wildfire Disturbance (%)`, data = doc_sub_merge))
summary(mod <- lm(Mean ~ `5-year Harvest Disturbance (%)`, data = doc_sub_merge))
summary(mod <- lm(Mean ~ `5-year Insect Disturbance (%)`, data = doc_sub_merge))
summary(mod <- lm(Mean ~ `5-year Abiotic Disturbance (%)`, data = doc_sub_merge))
summary(mod <- lm(Mean ~ `10-year Wildfire Disturbance (%)`, data = doc_sub_merge))
summary(mod <- lm(Mean ~ `10-year Harvest Disturbance (%)`, data = doc_sub_merge))
summary(mod <- lm(Mean ~ `10-year Insect Disturbance (%)`, data = doc_sub_merge)) # sig 0.008
summary(mod <- lm(Mean ~ `10-year Abiotic Disturbance (%)`, data = doc_sub_merge))
summary(mod <- lm(Mean ~ `15-year Wildfire Disturbance (%)`, data = doc_sub_merge))
summary(mod <- lm(Mean ~ `15-year Harvest Disturbance (%)`, data = doc_sub_merge))
summary(mod <- lm(Mean ~ `15-year Insect Disturbance (%)`, data = doc_sub_merge)) # sig 0.008
summary(mod <- lm(Mean ~ `15-year Abiotic Disturbance (%)`, data = doc_sub_merge))
summary(mod <- lm(Mean ~ `20-year Wildfire Disturbance (%)`, data = doc_sub_merge))
summary(mod <- lm(Mean ~ `20-year Harvest Disturbance (%)`, data = doc_sub_merge))
summary(mod <- lm(Mean ~ `20-year Insect Disturbance (%)`, data = doc_sub_merge))
summary(mod <- lm(Mean ~ `20-year Abiotic Disturbance (%)`, data = doc_sub_merge))

# Definitely a bit more individual significance without WS 67 - 10- and 15-year insect disturbance become highly significant; others include open water, elevation, area and log of area, and harvest classification

### 3.10 - Part 5: Individual sample campaign models ----

doc <- water_chem %>% 
  select(-glfc.id, -date) %>% 
  filter(variable %in% "organic.carbon") %>% 
  pivot_wider(names_from = "sample", values_from = "value") %>% 
  select(-variable)

colnames(doc) <- c("Site name", "catchment.id", "doc.s1", "doc.s2", "doc.s3", "doc.s4", "doc.s5", "doc.s6")

sample_campaign_tbl <- left_join(doc, ws_table, by = "Site name") # Individual sample campaigns attached to ws table

#### 3.10.1 - Run the concise model for each of the sample campaigns ----

sc1_mreg_concise <- lm(doc.s1 ~ log(`Drainage Area (km2)`) + `Wetland Cover (%)` + `Open Water (%)` + `Coniferous Forest (%)` + `10-year Harvest Disturbance (%)` + `10-year Insect Disturbance (%)`, data = sample_campaign_tbl) # Sample 1

summary(sc1_mreg_concise)
check_model(sc1_mreg_concise)

# Thoughts
# r2 = 0.69 Not the greatest model fit
# Lots of significance: log(area) - extremely sig; open water (.03); coniferous (.001); 10-year harvest (.01); 10-year insect (.0016)

sc2_mreg_concise <- lm(doc.s2 ~ log(`Drainage Area (km2)`) + `Wetland Cover (%)` + `Open Water (%)` + `Coniferous Forest (%)` + `10-year Harvest Disturbance (%)` + `10-year Insect Disturbance (%)`, data = sample_campaign_tbl) # Sample 2

summary(sc2_mreg_concise)
check_model(sc2_mreg_concise)

# Thoughts
# r2 = 0.67
# 10-year harvest gets less significant

sc3_mreg_concise <- lm(doc.s3 ~ log(`Drainage Area (km2)`) + `Wetland Cover (%)` + `Open Water (%)` + `Coniferous Forest (%)` + `10-year Harvest Disturbance (%)` + `10-year Insect Disturbance (%)`, data = sample_campaign_tbl) # Sample 3

summary(sc3_mreg_concise)
check_model(sc3_mreg_concise)

# Thoughts
# r2 = .74 - pretty good model fit for this campaign
# Drainage area and 10-year insect becomes very significant here; wetland cover becomes quite significant here (.004); open water is not significant

sc4_mreg_concise <- lm(doc.s4 ~ log(`Drainage Area (km2)`) + `Wetland Cover (%)` + `Open Water (%)` + `Coniferous Forest (%)` + `10-year Harvest Disturbance (%)` + `10-year Insect Disturbance (%)`, data = sample_campaign_tbl) # Sample 4

summary(sc4_mreg_concise)
check_model(sc4_mreg_concise)

# Thoughts
# r2 = 0.47 - pretty poor fit and low significance across the variables

sc5_mreg_concise <- lm(doc.s5 ~ log(`Drainage Area (km2)`) + `Wetland Cover (%)` + `Open Water (%)` + `Coniferous Forest (%)` + `10-year Harvest Disturbance (%)` + `10-year Insect Disturbance (%)`, data = sample_campaign_tbl) # Sample 5

summary(sc5_mreg_concise)
check_model(sc5_mreg_concise)

# Thoughts 
# r2 = 0.63
# Coniferous forest becomes very sig here, along with log(area) - which is consistently significant

sc6_mreg_concise <- lm(doc.s6 ~ log(`Drainage Area (km2)`) + `Wetland Cover (%)` + `Open Water (%)` + `Coniferous Forest (%)` + `10-year Harvest Disturbance (%)` + `10-year Insect Disturbance (%)`, data = sample_campaign_tbl) # Sample 6

summary(sc6_mreg_concise)
check_model(sc6_mreg_concise)

# Thoughts
# r2 = .58 - fairly poor fit
# Only generally higher significance across the board than campaign #4.

#### 3.10.2 - Using the best AICc models to examine individual sample campaigns ----

sc1_mreg_best <- lm(doc.s1 ~ log(`Drainage Area (km2)`) + `Open Water (%)` + `Coniferous Forest (%)` + `10-year Insect Disturbance (%)`, data = sample_campaign_tbl) # Sample 1

summary(sc1_mreg_best)
check_model(sc1_mreg_best)

sc2_mreg_best <- lm(doc.s2 ~ log(`Drainage Area (km2)`) + `Open Water (%)` + `Coniferous Forest (%)` + `10-year Insect Disturbance (%)`, data = sample_campaign_tbl) # Sample 2

summary(sc2_mreg_best)
check_model(sc2_mreg_best)

sc3_mreg_best <- lm(doc.s3 ~ log(`Drainage Area (km2)`) + `Open Water (%)` + `Coniferous Forest (%)` + `10-year Insect Disturbance (%)`, data = sample_campaign_tbl) # Sample 3

summary(sc3_mreg_best)
check_model(sc3_mreg_best)

sc4_mreg_best <- lm(doc.s4 ~ log(`Drainage Area (km2)`) + `Open Water (%)` + `Coniferous Forest (%)` + `10-year Insect Disturbance (%)`, data = sample_campaign_tbl) # Sample 4

summary(sc4_mreg_best)
check_model(sc4_mreg_best)

sc5_mreg_best <- lm(doc.s5 ~ log(`Drainage Area (km2)`) + `Open Water (%)` + `Coniferous Forest (%)` + `10-year Insect Disturbance (%)`, data = sample_campaign_tbl) # Sample 5

summary(sc5_mreg_best)
check_model(sc5_mreg_best)

# Thoughts
# Coniferous forest pops out here, highly significant

sc6_mreg_best <- lm(doc.s6 ~ log(`Drainage Area (km2)`) + `Open Water (%)` + `Coniferous Forest (%)` + `10-year Insect Disturbance (%)`, data = sample_campaign_tbl) # Sample 6

summary(sc6_mreg_best)
check_model(sc6_mreg_best)

# Thoughts
# Open water becomes significant here

### 3.11 - Examining the best models ----

mreg_best <- lm(Mean ~ log(`Drainage Area (km2)`) + `Open Water (%)` + `Coniferous Forest (%)` + `10-year Insect Disturbance (%)`, data = doc_ws_table)

summary(mreg_best)
check_model(mreg_best)

mreg_2nd_best <- lm(Mean ~ log(`Drainage Area (km2)`) + `Open Water (%)` + `Coniferous Forest (%)` + `10-year Harvest Disturbance (%)` + `10-year Insect Disturbance (%)`, data = doc_ws_table)

summary(mreg_2nd_best)
check_model(mreg_2nd_best)

## 4. PLOTTING ----

## 5. SAVING // EXPORTING ----

## 6. TRIAL // JUNK CODE ----

# Look at no 67 mean doc and wetland cover relationships

doc_sub_merge %>% 
  ggplot(aes(`Wetland Cover (%)`, Mean)) +
  geom_point() # no real relationship here

doc_sub_merge %>% 
  ggplot(aes(`Elevation (m a.s.l.)`, Mean)) +
  geom_point()

### 6.01 Standardisation of predictor variables ---

# 1. 5-year wildfire removed (0 sites)
# 2. 10-year wildfire removed (1 site)
# 3. 10-year abiotic - same as 5-year
# 4. 15-year abiotic - same as 5-year
# 5. 20-year abiotic - same as 5-year
# 6. 20-year wildfire - the same percentage as 15-year wildfire

ws_table_std <- ws_table %>% 
  mutate(lat_st = zscore(Latitude),
         long_st = zscore(Longitude),
         drainage_st = zscore(`Drainage Area (km2)`),
         elev_st = zscore(`Elevation (m a.s.l.)`),
         slope_st = zscore(`Slope (degrees)`),
         wetland_st = zscore(`Wetland Cover (%)`),
         open_wat_st = zscore(`Open Water (%)`),
         tprod_for_st = zscore(`Total Productive Forest (%)`),
         decid_std = zscore(`Deciduous Forest (%)`),
         conifer_st = zscore(`Coniferous Forest (%)`),
         harv5_st = zscore(`5-year Harvest Disturbance (%)`),
         insect5_st = zscore(`5-year Insect Disturbance (%)`),
         abiotic5_st = zscore(`5-year Abiotic Disturbance (%)`),
         harv10_st = zscore(`10-year Harvest Disturbance (%)`),
         insect10_st = zscore(`10-year Insect Disturbance (%)`),
         wildfire15_st = zscore(`15-year Wildfire Disturbance (%)`),
         harv15_st = zscore(`15-year Harvest Disturbance (%)`),
         insect15_st = zscore(`15-year Insect Disturbance (%)`),
         harv20_st = zscore(`20-year Wildfire Disturbance (%)`),
         insect20_st = zscore(`20-year Insect Disturbance (%)`))
         
         
         

