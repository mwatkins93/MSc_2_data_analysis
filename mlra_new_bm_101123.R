library(tidyverse)
library(performance)
library(broom.mixed)
library(modelsummary)
library(dotwhisker)
library(factoextra)
library(mosaic)
library(ggpubr)
library(grid)
library(gridExtra)

doc_tbl <- readRDS("master_mreg_tbl.rds")

## scale the log(drainage area) and log(Open water)
doc_tbl$drainage_st <- scale(log(doc_tbl$`Drainage Area (km2)`))
doc_tbl$open_wat_st <- scale(log(doc_tbl$`Open Water (%)` + 1))

doc_predictors <- doc_tbl[-16, c(1, 3, 33:78)] # Removes unwanted columns and site C3

doc_tidy <- readRDS("mlra_doc_tidy.rds")

### Run models 

### DOC
mDOC_bm <- lm(mean.doc ~ conifer_st + drainage_st + open_wat_st + slope_st + harv10_st + insect10_st, data = doc_predictors)

doc1_bm <- lm(doc.s1 ~ conifer_st + drainage_st + open_wat_st + slope_st + harv10_st + insect10_st, data = doc_predictors)

doc2_bm <- lm(doc.s2 ~ conifer_st + drainage_st + open_wat_st + slope_st + harv10_st + insect10_st, data = doc_predictors)

doc3_bm <- lm(doc.s3 ~ conifer_st + drainage_st + open_wat_st + slope_st + harv10_st + insect10_st, data = doc_predictors)

doc4_bm <- lm(doc.s4 ~ conifer_st + drainage_st + open_wat_st + slope_st + harv10_st + insect10_st, data = doc_predictors)

doc5_bm <- lm(doc.s5 ~ conifer_st + drainage_st + open_wat_st + slope_st + harv10_st + insect10_st, data = doc_predictors)

doc6_bm <- lm(doc.s6 ~ conifer_st + drainage_st + open_wat_st + slope_st + harv10_st + insect10_st, data = doc_predictors)

### SUVA

meansuva_bm <- lm(mean.suva ~ conifer_st + drainage_st + open_wat_st + slope_st + harv10_st + insect10_st, data = doc_predictors)

suva1_bm <- lm(suva.1 ~ conifer_st + drainage_st + open_wat_st + slope_st + harv10_st + insect10_st, data = doc_predictors)

suva2_bm <- lm(suva.2 ~ conifer_st + drainage_st + open_wat_st + slope_st + harv10_st + insect10_st, data = doc_predictors)

suva3_bm <- lm(suva.3 ~ conifer_st + drainage_st + open_wat_st + slope_st + harv10_st + insect10_st, data = doc_predictors)

suva4_bm <- lm(suva.4 ~ conifer_st + drainage_st + open_wat_st + slope_st + harv10_st + insect10_st, data = doc_predictors)

suva5_bm <- lm(suva.5 ~ conifer_st + drainage_st + open_wat_st + slope_st + harv10_st + insect10_st, data = doc_predictors)

suva6_bm <- lm(suva.6 ~ conifer_st + drainage_st + open_wat_st + slope_st + harv10_st + insect10_st, data = doc_predictors)

### Instantaneous DOC flux

if1_bm <- lm(inst.flux.1 ~ conifer_st + drainage_st + open_wat_st + slope_st + harv10_st + insect10_st, data = doc_tidy)

if2_bm <- lm(inst.flux.2 ~ conifer_st + drainage_st + open_wat_st + slope_st + harv10_st + insect10_st, data = doc_tidy)

if3_bm <- lm(inst.flux.3 ~ conifer_st + drainage_st + open_wat_st + slope_st + harv10_st + insect10_st, data = doc_tidy)

if4_bm <- lm(inst.flux.4 ~ conifer_st + drainage_st + open_wat_st + slope_st + harv10_st + insect10_st, data = doc_tidy)

if5_bm <- lm(inst.flux.5 ~ conifer_st + drainage_st + open_wat_st + slope_st + harv10_st + insect10_st, data = doc_tidy)

if6_bm <- lm(inst.flux.6 ~ conifer_st + drainage_st + open_wat_st + slope_st + harv10_st + insect10_st, data = doc_tidy)

### Put all models into their respective lists for plots

doc_models <- list(mDOC_bm, doc1_bm, doc2_bm, doc3_bm, doc4_bm, doc5_bm, doc6_bm)

suva_models <- list(meansuva_bm, suva1_bm, suva2_bm, suva3_bm, suva4_bm, suva5_bm, suva6_bm)

if_models <- list(if1_bm, if2_bm, if3_bm, if4_bm, if5_bm, if6_bm)

### Plots ---

### DOC

doc_bm_plot <- dwplot(doc_models, dot_args = c(shape = 19), dodge_size = .6) %>%
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
    insect15_st = "15-year Infestation",
    insect10_st = "10-year Infestation",
    insect5_st = "5-year Infestation")) +
  theme_bw(base_size = 20) +
  labs(title = "[DOC]") +
  theme(legend.title = element_blank(), legend.position = "none", plot.title = element_text(hjust = 0.5, face="bold")) +
  geom_vline(xintercept = 0) +
  scale_colour_manual(labels = c("SC 1", "SC 2", "SC 3", "SC 4", "SC 5", "SC 6", "Mean"),
                      values = c("#99ccff", "#c4c1c6", "#005155", "#e9b22a", "#8c6d31", "#6600ff", "#000000"))

### SUVA

suva_bm_plot <- dwplot(suva_models, dot_args = c(shape = 19), dodge_size = .6) %>%
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
    insect15_st = "15-year Infestation",
    insect10_st = "10-year Infestation",
    insect5_st = "5-year Infestation")) +
  theme_bw(base_size = 20) +
  labs(title = "SUVA") +
  theme(legend.title = element_blank(), legend.position = "none", plot.title = element_text(hjust = 0.5, face="bold")) +
  geom_vline(xintercept = 0) +
  scale_colour_manual(labels = c("SUVA 1", "SUVA 2", "SUVA 3", "SUVA 4", "SUVA 5", "SUVA 6", "Mean"),
                      values = c("#99ccff", "#c4c1c6", "#005155", "#e9b22a", "#8c6d31", "#6600ff", "#000000"))

### IF Flux
if_flux_plot <- dwplot(if_models, dot_args = c(shape = 19), dodge_size = .6) %>%
  relabel_predictors(c(
    conifer_st = "Coniferous",
    decid_st = "Deciduous",
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
    insect15_st = "15-year Infestation",
    insect10_st = "10-year Infestation",
    insect5_st = "5-year Infestation")) +
  theme_bw(base_size = 20, ) +
  labs(title = "Instantaneous DOC flux", x = "Coefficient estimate") +
  theme(legend.title = element_blank(), legend.position = "none", plot.title = element_text(hjust = 0.5, face="bold"), axis.title.x = element_text(size = 16)) +
  geom_vline(xintercept = 0) +
  scale_colour_manual(labels = c("SC 1", "SC 2", "SC 3", "SC 4", "SC 5", "SC 6"),
                      values = c("#99ccff", "#c4c1c6", "#005155", "#e9b22a", "#8c6d31", "#6600ff"))


### Arranged plot

mlra_arranged_plot <- ggarrange(doc_bm_plot,
                                suva_bm_plot,
                                if_flux_plot,
                                align = "h",
                                common.legend = TRUE)

mlra_arranged_plot

## Adjusted r2 values

summary(if6_bm)

### 2.0 - Partial F tests on various models (to see what is adding explanatory power!) ----

#### 2.1 - Physical characteristic inclusion (wetland and elevation) ----

# BM = open water + coniferous + slope + drainage area

mDOC_bm <- lm(mean.doc ~ conifer_st + drainage_st + open_wat_st + slope_st, data = doc_tidy)

summary(mDOC_bm)

# BM + wetland cover

mDOC_wet <- lm(mean.doc ~ conifer_st + drainage_st + open_wat_st + slope_st + wetland_st, data = doc_tidy)

summary(mDOC_wet)

# BM + elevation

mDOC_elev <- lm(mean.doc ~ conifer_st + drainage_st + open_wat_st + slope_st + elev_st, data = doc_tidy)

summary(mDOC_elev)

# BM + wetland & elevation

mDOC_wet_elev <- lm(mean.doc ~ conifer_st + drainage_st + open_wat_st + slope_st + wetland_st + elev_st, data = doc_tidy)

summary(mDOC_wet_elev)

## 2.1.1 Anova Assess ----

anova(mDOC_bm, mDOC_wet) ## Failed to reject null; model is not sign. better
anova(mDOC_bm, mDOC_elev) ## Failed to reject null; model is not sign. better
anova(mDOC_bm, mDOC_wet_elev) ## Failed to reject null; model is not sign. better

#### 2.2 - Forest disturbance characteristic inclusion (wetland and elevation) ----

### Harvest
mDOC_harv5 <- lm(mean.doc ~ conifer_st + drainage_st + open_wat_st + slope_st + harv5_st, data = doc_tidy)

summary(mDOC_harv5)

mDOC_harv10 <- lm(mean.doc ~ conifer_st + drainage_st + open_wat_st + slope_st + harv10_st, data = doc_tidy)

summary(mDOC_harv10)

mDOC_harv15 <- lm(mean.doc ~ conifer_st + drainage_st + open_wat_st + slope_st + harv15_st, data = doc_tidy)

summary(mDOC_harv15)

mDOC_harv20 <- lm(mean.doc ~ conifer_st + drainage_st + open_wat_st + slope_st + harv20_st, data = doc_tidy)

summary(mDOC_harv20)

### Infest

mDOC_inf5 <- lm(mean.doc ~ conifer_st + drainage_st + open_wat_st + slope_st + insect5_st, data = doc_tidy)

summary(mDOC_inf15)

mDOC_inf10 <- lm(mean.doc ~ conifer_st + drainage_st + open_wat_st + slope_st + insect10_st, data = doc_tidy)


mDOC_inf15 <- lm(mean.doc ~ conifer_st + drainage_st + open_wat_st + slope_st + insect15_st, data = doc_tidy)

### Mixed H + I

mDOC_hi5 <- lm(mean.doc ~ conifer_st + drainage_st + open_wat_st + slope_st + insect5_st + harv5_st, data = doc_tidy)

mDOC_hi10 <- lm(mean.doc ~ conifer_st + drainage_st + open_wat_st + slope_st + insect10_st + harv10_st, data = doc_tidy)

mDOC_hi15 <- lm(mean.doc ~ conifer_st + drainage_st + open_wat_st + slope_st + insect15_st + harv15_st, data = doc_tidy)

summary(mDOC_hi15)

### 2.2.1 Anova Assess - forest disturbance ----

anova(mDOC_bm, mDOC_harv5) ## Failed to reject null; model is not sign. better
anova(mDOC_bm, mDOC_harv10) ## Failed to reject null; model is not sign. better
anova(mDOC_bm, mDOC_harv15) ## FtR; Nsb
anova(mDOC_bm, mDOC_harv20) ## Failed to reject null; model is not sign. better
anova(mDOC_bm, mDOC_inf5) ## Failed to reject null; model is not sign. better
anova(mDOC_bm, mDOC_inf10) ## Rejected null; sign. better
anova(mDOC_bm, mDOC_inf15) ## Rejected null; sign. better
anova(mDOC_bm, mDOC_hi5) ## Failed to reject null; model is not sign. better
anova(mDOC_bm, mDOC_hi10) ## Rejected null; sign. better
anova(mDOC_bm, mDOC_hi15) ## Rejected null; sign. better

### So, why does the H10 + I10 and H15 and I15 models increase explanatory power if those individual harvest years show little explanatory power when isolated? 

anova(mDOC_inf10, mDOC_hi10) # this is saying that harvest + insect is sign. better than just insect
anova(mDOC_inf15, mDOC_hi15) # same thing here; the harv + insect is a sign. better model than just insect. 


