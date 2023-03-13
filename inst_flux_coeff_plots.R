## ORGANISATION ---------
## Name: Matt Watkins
## Date: Mar. 8th '23
## Project: MSc data analysis
## Objective: Multiple regression for instantaneous fluxes
## Inputs: final doc table + legacy mreg code
## Outputs: plots
## ----------------------

## 0. NOTES ----

## 0.1 No C3 dataframe ----

no_c3 <- doc_table %>% 
  filter(!`Catchment ID` %in% "C3") # df with removal of C3

### 0.2 - Solution for extracting best model only in certain cases!

get.models(themodeltable, subset = 1)[[1]]

## 1. PREPARE ----

rm(list=ls())
options(stringsASfactors = FALSE, scipen = 999, encoding = "UTF-8", na.action = "na.fail")

library(tidyverse)
library(readxl)
library(performance)
library(MuMIn)
library(broom.mixed)
library(modelsummary)
library(dotwhisker)
library(sjPlot)
library(factoextra)

## 2. IMPORT ----

doc_table <- readRDS("final_doc_tbl.rds")

## 3. TIDY // PROCESS ----

### 3.01 - Instantaneous flux w/ base model ----

#### 3.04.1 - inst flux 1 ----

if1_base_model <- lm(inst.flux.1 ~ Group + conifer_st + drainage_st + elev_st + open_wat_st + slope_st, data = no_c3)

if1_base_table <- dredge(if1_base_model, rank = "AICc")

if1_base_model_avg <- model.avg(subset(if1_base_table, delta <= 2, recalc.weights = FALSE), fit = TRUE)

#### 3.04.3 - inst flux 2 ----

if2_sub <- no_c3[-3, ]

if2_base_model <- lm(inst.flux.2 ~ Group + conifer_st + drainage_st + elev_st + open_wat_st + slope_st, data = if2_sub)

if2_base_table <- dredge(if2_base_model, rank = "AICc")

if2_base_model_avg <- model.avg(subset(if2_base_table, delta <= 2, recalc.weights = FALSE), fit = TRUE)

#### 3.04.4 - inst flux 3 ----

if3_sub <- no_c3[c(-3, -6), ]

if3_base_model <- lm(inst.flux.3 ~ Group + conifer_st + drainage_st + elev_st + open_wat_st + slope_st, data = if3_sub)

if3_base_table <- dredge(if3_base_model, rank = "AICc")

if3_base_model_avg <- model.avg(subset(if3_base_table, delta <= 2, recalc.weights = FALSE), fit = TRUE)

if3_base_model_avg <- get.models(if3_base_table, subset = 1)[[1]]

#### 3.04.5 - inst flux 4 ----

if4_sub <- no_c3[c(-3, -6, -12, -22), ]

if4_base_model <- lm(inst.flux.4 ~ Group + conifer_st + drainage_st + elev_st + open_wat_st + slope_st, data = if4_sub)

if4_base_table <- dredge(if4_base_model, rank = "AICc")

if4_base_model_avg <- model.avg(subset(if4_base_table, delta <= 2, recalc.weights = FALSE), fit = TRUE)

#### 3.04.6 - inst flux 5 ----

if5_sub <- no_c3[-3, ]

if5_base_model <- lm(inst.flux.5 ~ Group + conifer_st + drainage_st + elev_st + open_wat_st + slope_st, data = if5_sub)

if5_base_table <- dredge(if5_base_model, rank = "AICc")

if5_base_model_avg <- model.avg(subset(if5_base_table, delta <= 2, recalc.weights = FALSE), fit = TRUE)

#### 3.04.7 - inst flux 6 ----

if6_sub <- no_c3[c(-3, -5, -12, -17, -18, -22, -24), ]

if6_base_model <- lm(inst.flux.6 ~ Group + conifer_st + drainage_st + elev_st + open_wat_st + slope_st, data = if6_sub)

if6_base_table <- dredge(if6_base_model, rank = "AICc")

if6_base_model_avg <- model.avg(subset(if6_base_table, delta <= 2, recalc.weights = FALSE), fit = TRUE)

### 3.05 - Merge all model items into a list for plotting ----

inst_flux_base_model_avgs <- list(if1_base_model_avg, if2_base_model_avg, if3_base_model_avg, if4_base_model_avg, if5_base_model_avg, if6_base_model_avg)

## 4. PLOTTING ----

inst_flux_base_model_plot <- dwplot(inst_flux_base_model_avgs) %>%
  relabel_predictors(c(
    conifer_st = "Coniferous",
    decid_std = "Deciduous",
    tprod_for_st = "Total Productive Forest",
    drainage_st = "Catchment Area",
    slope_st = "Slope",
    open_wat_st = "Open Water",
    wetland_st = "Wetland",
    elev_st = "Elevation",
    GroupHarvest = "Harvest Class",
    GroupInsect = "Insect Class",
    GroupMixed = "Mixed Class",
    harv20_st = "20-year Harvest",
    harv15_st = "15-year Harvest",
    harv10_st = "10-year Harvest",
    harv5_st = "5-year Harvest",
    insect20_st = "20-year Infestation",
    insect10_st = "10-year Infestation",
    insect5_st = "5-year Infestation")) +
  theme_bw() +
  labs(title = "Averaged instantaneous flux coefficients - open water + slope") +
  theme(legend.title = element_blank(), plot.title = element_text(hjust = 0.5, face="bold")) +
  geom_vline(xintercept = 0) +
  scale_colour_manual(labels = c("IF1", "IF2", "IF3", "IF4", "IF5", "IF6"),
                      values = c("#c4c1c6", "#005155", "#e9b22a", "#8c6d31", "#6600ff", "#000000"))

# view plot
inst_flux_base_model_plot

## 5. SAVING // EXPORTING ----

## 6. TRIAL // JUNK CODE ----

### 6.01 - Check model requirements for each instantaneous flux campaign

check_model(if1_base_model)

# Base model:
# the base model for instantaneous flux 1 is awful (C3 is the problem)
# the base model for instantaneous flux 2 is much better (C2 and C3 are problematic)
# the base model for instantaneous flux 3 is awful (C3 is the problem)
# the base model for instantaneous flux 4 is okay (C3 is the problem)
# the base model for instantaneous flux 5 is okay (C3 problem)
# the base model for instantaneous flux 6 is okay/poor (C3 is NA here)

# No C3 model: 
# if1 - C2 causes big issues
# if2 - C2 causes issues in if2
# if3 - pretty good
# if4 - SBC a bit of an outlier
# if5 - poor/okay
# if6 - BL1, 92 and 11 are outliers here 




