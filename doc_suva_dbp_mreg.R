## ORGANISATION ---------
## Name: Matt Watkins
## Date: June 7th/23
## Project: MSc data analysis
## Objective: Finalised multiple regression plots for all response variables
## Inputs: standardised ws table RDS
## Outputs: paneled ggplot
## ----------------------

## 0. NOTES ----

options(na.action = "na.fail") # set for MuMIn dredge

### 0.1 - Solution for extracting best model only in certain cases!

get.models(themodeltable, subset = 1)[[1]]

### 0.2 - The base model is now excluding site C3 (reasons stated in methods section)

### 0.3 - Not enough statistical power to run THM or HAA through the multiple regression (n = 8 is likely why). Bring this up to Jason and move on.

### 1.0 PREPARE ----

rm(list=ls())
options(stringsASfactors = FALSE, scipen = 999, encoding = "UTF-8", na.action = "na.fail")

library(tidyverse)
library(readxl)
library(performance)
library(MuMIn)
library(broom.mixed)
library(modelsummary)
library(dotwhisker)
library(factoextra)
library(mosaic)
library(ggpubr)

plot_theme <- theme(legend.text = element_text(size = 16), # sets legend text size
                    legend.key.size = unit(1, 'cm'), # sets legend icon size
                    legend.position = "top") # sets legend position

## 2. IMPORT ----

doc_tbl <- readRDS("master_mreg_tbl.rds")

## 3. TIDY // PROCESS ----

doc_predictors <- doc_tbl[-16, c(1, 3, 33:78)] # Removes unwanted columns and site C3

doc_arranged <- doc_predictors %>% 
  relocate(mean.doc, .after = "insect20_st") %>% 
  relocate(mean.suva, .after = "doc.s6")

### 3.01 - DOC w/ base model ----

#### 3.01.1 Mean DOC ----

mean_base_model <- lm(mean.doc ~ Group + conifer_st + drainage_st + elev_st + open_wat_st + wetland_st + slope_st, data = doc_arranged)

mean_base_model_tbl <- dredge(mean_base_model, rank = "AICc")

mean_base_model_avg <- get.models(mean_base_model_tbl, subset = 1)[[1]]

#### 3.01.2 - Sample campaign 1 ----

sc1_base_model <- lm(doc.s1 ~ Group + conifer_st + drainage_st + elev_st + open_wat_st + wetland_st + slope_st, data = doc_arranged)

sc1_base_table <- dredge(sc1_base_model, rank = "AICc")

sc1_base_model_avg <- get.models(mean_base_model_tbl, subset = 1)[[1]]

#### 3.04.3 - Sample campaign 2 ----

sc2_base_model <- lm(doc.s2 ~ Group + conifer_st + drainage_st + elev_st + open_wat_st + wetland_st + slope_st, data = doc_arranged)

sc2_base_table <- dredge(sc2_base_model, rank = "AICc")

sc2_base_model_avg <- model.avg(subset(sc2_base_table, delta <= 2, recalc.weights = FALSE), fit = TRUE)

#### 3.04.4 - Sample campaign 3 ----

doc_s3_sub <- doc_arranged[-3, ]

sc3_base_model <- lm(doc.s3 ~ Group + conifer_st + drainage_st + elev_st + open_wat_st + wetland_st + slope_st, data = doc_s3_sub)

sc3_base_table <- dredge(sc3_base_model, rank = "AICc")

sc3_base_model_avg <- model.avg(subset(sc3_base_table, delta <= 2, recalc.weights = FALSE), fit = TRUE)

#### 3.04.5 - Sample campaign 4 ----

doc_s4_sub <- doc_arranged[c(-3, -6), ]

sc4_base_model <- lm(doc.s4 ~ Group + conifer_st + drainage_st + elev_st + open_wat_st + wetland_st + slope_st, data = doc_s4_sub)

sc4_base_table <- dredge(sc4_base_model, rank = "AICc")

sc4_base_model_avg <- model.avg(subset(sc4_base_table, delta <= 2, recalc.weights = FALSE), fit = TRUE)

#### 3.04.6 - Sample campaign 5 ----

doc_s5_sub <- doc_arranged[-3, ]

sc5_base_model <- lm(doc.s5 ~ Group + conifer_st + drainage_st + elev_st + open_wat_st + wetland_st + slope_st, data = doc_s5_sub)

sc5_base_table <- dredge(sc5_base_model, rank = "AICc")

sc5_base_model_avg <- model.avg(subset(sc5_base_table, delta <= 2, recalc.weights = FALSE), fit = TRUE)

#### 3.04.7 - Sample campaign 6 ----

sc6_base_model <- lm(doc.s6 ~ Group + conifer_st + drainage_st + elev_st + open_wat_st + wetland_st + slope_st, data = doc_arranged)

sc6_base_table <- dredge(sc6_base_model, rank = "AICc")

sc6_base_model_avg <- model.avg(subset(sc6_base_table, delta <= 2, recalc.weights = FALSE), fit = TRUE)

#### 3.04.8 - Attach all base model averages ----

base_model_avgs <- list(sc1_base_model_avg, sc2_base_model_avg, sc3_base_model_avg, sc4_base_model_avg, sc5_base_model_avg, sc6_base_model_avg, mean_base_model_avg)

doc_base_model_plot <- dwplot(base_model_avgs) %>%
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
    GroupInsect = "Infestation Class",
    GroupMixed = "Mixed Class",
    harv20_st = "20-year Harvest",
    harv15_st = "15-year Harvest",
    harv10_st = "10-year Harvest",
    harv5_st = "5-year Harvest",
    insect20_st = "20-year Infestation",
    insect10_st = "10-year Infestation",
    insect5_st = "5-year Infestation")) +
  theme_bw() +
  labs(title = "[DOC]") +
  theme(legend.title = element_blank(), plot.title = element_text(hjust = 0.5, face="bold")) +
  geom_vline(xintercept = 0) +
  scale_colour_manual(labels = c("SC 1", "SC 2", "SC 3", "SC 4", "SC 5", "SC 6", "Mean"),
                      values = c("#99ccff", "#c4c1c6", "#005155", "#e9b22a", "#8c6d31", "#6600ff", "#000000"))

doc_base_model_plot

### 3.02 - Inst. flux w/ base model ----



### 3.03 - SUVA w/ base model ----

#### 3.01.1 Mean SUVA ----

suva_mean_base_model <- lm(mean.suva ~ Group + conifer_st + drainage_st + elev_st + open_wat_st + wetland_st + slope_st, data = doc_arranged)

suva_mean_base_model_tbl <- dredge(suva_mean_base_model, rank = "AICc")

suva_mean_base_model_avg <- model.avg(subset(suva_mean_base_model_tbl, delta <= 2, recalc.weights = FALSE), fit = TRUE)

#### 3.01.2 - Sample campaign 1 ----

suva1_base_model <- lm(suva.1 ~ Group + conifer_st + drainage_st + elev_st + open_wat_st + wetland_st + slope_st, data = doc_arranged)

suva1_base_tbl <- dredge(suva1_base_model, rank = "AICc")

suva1_base_model_avg <- model.avg(subset(suva1_base_tbl, delta <= 2, recalc.weights = FALSE), fit = TRUE)

#### 3.04.3 - Sample campaign 2 ----

suva2_base_model <- lm(suva.2 ~ Group + conifer_st + drainage_st + elev_st + open_wat_st + wetland_st + slope_st, data = doc_arranged)

suva2_base_table <- dredge(suva2_base_model, rank = "AICc")

suva2_base_model_avg <- model.avg(subset(suva2_base_table, delta <= 2, recalc.weights = FALSE), fit = TRUE)

#### 3.04.4 - Sample campaign 3 ----

suva3_sub <- doc_arranged[-3, ]

suva3_base_model <- lm(suva.3 ~ Group + conifer_st + drainage_st + elev_st + open_wat_st + wetland_st + slope_st, data = suva3_sub)

suva3_base_table <- dredge(suva3_base_model, rank = "AICc")

suva3_base_model_avg <- model.avg(subset(suva3_base_table, delta <= 2, recalc.weights = FALSE), fit = TRUE)

#### 3.04.5 - Sample campaign 4 ----

suva4_sub <- doc_arranged[c(-3, -6), ]

suva4_base_model <- lm(suva.4 ~ Group + conifer_st + drainage_st + elev_st + open_wat_st + wetland_st + slope_st, data = suva4_sub)

suva4_base_table <- dredge(suva4_base_model, rank = "AICc")

suva4_base_model_avg <- model.avg(subset(suva4_base_table, delta <= 2, recalc.weights = FALSE), fit = TRUE)

#### 3.04.6 - Sample campaign 5 ----

suva5_sub <- doc_arranged[-3, ]

suva5_base_model <- lm(suva.5 ~ Group + conifer_st + drainage_st + elev_st + open_wat_st + wetland_st + slope_st, data = suva5_sub)

suva5_base_table <- dredge(suva5_base_model, rank = "AICc")

suva5_base_model_avg <- model.avg(subset(suva5_base_table, delta <= 2, recalc.weights = FALSE), fit = TRUE)

#### 3.04.7 - Sample campaign 6 ----

suva6_base_model <- lm(suva.6 ~ Group + conifer_st + drainage_st + elev_st + open_wat_st + wetland_st + slope_st, data = doc_arranged)

suva6_base_table <- dredge(suva6_base_model, rank = "AICc")

suva6_base_model_avg <- model.avg(subset(suva6_base_table, delta <= 2, recalc.weights = FALSE), fit = TRUE)

#### 3.04.8 - Attach all base model averages ----

suva_base_model_avgs <- list(suva1_base_model_avg, suva2_base_model_avg, suva3_base_model_avg, suva4_base_model_avg, suva5_base_model_avg, suva6_base_model_avg, suva_mean_base_model_avg)

suva_base_model_plot <- dwplot(suva_base_model_avgs) %>%
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
    GroupInsect = "Infestation Class",
    GroupMixed = "Mixed Class",
    harv20_st = "20-year Harvest",
    harv15_st = "15-year Harvest",
    harv10_st = "10-year Harvest",
    harv5_st = "5-year Harvest",
    insect20_st = "20-year Infestation",
    insect10_st = "10-year Infestation",
    insect5_st = "5-year Infestation")) +
  theme_bw() +
  labs(title = "SUVA") +
  theme(legend.title = element_blank(), plot.title = element_text(hjust = 0.5, face="bold")) +
  geom_vline(xintercept = 0) +
  scale_colour_manual(labels = c("SUVA 1", "SUVA 2", "SUVA 3", "SUVA 4", "SUVA 5", "SUVA 6", "Mean"),
                      values = c("#99ccff", "#c4c1c6", "#005155", "#e9b22a", "#8c6d31", "#6600ff", "#000000"))

suva_base_model_plot

### 4. - Arranged plot ----

mreg_arranged_plot <- ggarrange(doc_base_model_plot,
                     suva_base_model_plot,
                     inst_flux_base_model_plot,
                     align = "h") # remember to open "inst_flux_coeff_plots.R and run that for the IF plot!

mreg_arranged_plot + plot_theme


### 5. Versions (Github control) ----

# 5.1 Base model



