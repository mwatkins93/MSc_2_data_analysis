## ORGANISATION ---------
## Name: Matt Watkins
## Date: Feb. 22nd/23
## Project: MSc data analysis
## Objective: Create a variable effect model coefficient graph that displays how each variable behaves with respect to each sample campaign and mean DOC
## Inputs: standardised ws table RDS
## Outputs: paneled ggplot
## ----------------------

## 0. NOTES ----

options(na.action = "na.fail") # set for MuMIn dredge

### 0.1 Can load in good model support RDSs

sc1 <- readRDS("sc1_doc_std_models.rds")
sc2 <- readRDS("sc2_doc_std_models.rds")
sc3 <- readRDS("sc3_doc_std_models.rds")
sc4 <- readRDS("sc4_doc_std_models.rds")
sc5 <- readRDS("sc5_doc_std_models.rds")
sc6 <- readRDS("sc6_doc_std_models.rds")
mean_doc <- readRDS("mean_doc_std_models.rds")

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


## 2. IMPORT ----

standard_doc_tbl <- readRDS("final_std_doc_table.rds")

## 3. TIDY // PROCESS ----

### 3.01 - Run a PCA on the standardised predictor variables to determine redundancy ----

pca.dat <- standard_doc_tbl[,c(34:53)] # select columns for pca analysis

pca <- prcomp(pca.dat, center = T, scale. = T) # run the pca

summary(pca) # display the pca results in table format

plot(pca)
biplot(pca)

# Update the PCA to the remaining variables below and recheck redundancy

pca.dat <- standard_doc_sub[,c(34:42, 44, 46:48)]
pca <- prcomp(pca.dat, center = T, scale. = T) # run the pca
summary(pca) # display the pca results in table format
plot(pca)
biplot(pca)

pairs(std_predictors)

std_predictors <- standard_doc_tbl[34:53] # look at correlation


# Some definite redundancy here, for now choose from the following to refine:
# 1. deciduous or coniferous (coniferous)
# 2. longitude or latitude (latitude)
# 3. insect 10 and 15 or insect 20 (insect 10 and 15)
# 4. open water or elevation (open water)
# 5. insect 10 or 15 (insect 15) - these are almost identical

# Removing abiotic 5 since only 5 sites have data
# Probably just keep 20-year harvest instead of 20-year and 15-year (similar enough, but harv20 has slightly higher %s for a few sites)

### 3.02 - Remove those variables and run the model for mean DOC

standard_doc_sub <- standard_doc_tbl %>% 
  select(-decid_std, -long_st, -insect20_st, -elev_st)

regr_model <- lm(mean.doc ~ drainage_st + lat_st + slope_st + wetland_st + open_wat_st + tprod_for_st + conifer_st + harv5_st + insect5_st + harv10_st + wildfire15_st + harv15_st + insect15_st, data = standard_doc_tbl)

summary(regr_model)
check_model(regr_model)

meanDOC_table <- dredge(regr_model, rank = "AICc")

meanDOC_goodsupport <- subset(meanDOC_table, delta <= 2, recalc.weights=FALSE) # 14 models within 2 AICc, I need to retain these coefficients

meanDOC_avg <- model.avg(meanDOC_goodsupport, fit = TRUE)

modelplot(meanDOC_avg, coef_omit = 1) +
  xlab("Averaged coefficient estimates for Mean DOC with 95% confidence")

### 3.03 - Run model for sample campaign 1, 2, 3, 4, 5, 6

options(na.action = "na.omit") # reset this so the campaigns with NAs work

### Sample 1
sc1_model <- lm(doc.s1 ~ drainage_st + lat_st + slope_st + wetland_st + open_wat_st + tprod_for_st + conifer_st + harv5_st + insect5_st + harv10_st + wildfire15_st + harv15_st + insect15_st, data = standard_doc_tbl)

summary(sc1_model)
check_model(sc1_model)

sc1_table <- dredge(sc1_model, rank = "AICc")

sc1_goodmodels <- subset(sc1_table, delta <= 2, recalc.weights = FALSE)

### Sample 2
sc2_model <- lm(doc.s2 ~ drainage_st + lat_st + slope_st + wetland_st + open_wat_st + tprod_for_st + conifer_st + harv5_st + insect5_st + harv10_st + wildfire15_st + harv15_st + insect15_st, data = standard_doc_tbl)

summary(sc2_model)
check_model(sc2_model)

sc2_table <- dredge(sc2_model, rank = "AICc")

sc2_goodmodels <- subset(sc2_table, delta <= 2, recalc.weights = FALSE)

### Sample 3 - NAs become an issue here

doc_s3_sub <- standard_doc_sub[-3, c(1:49, 52)] # subset to remove the NA value

sc3_model <- lm(doc.s3 ~ drainage_st + lat_st + slope_st + wetland_st + open_wat_st + tprod_for_st + conifer_st + harv5_st + insect5_st + harv10_st + wildfire15_st + harv15_st + insect15_st, data = doc_s3_sub)

summary(sc3_model)
check_model(sc3_model)

sc3_table <- dredge(sc3_model, rank = "AICc")

sc3_goodmodels <- subset(sc3_table, delta <= 2, recalc.weights = FALSE)

### Sample 4

doc_s4_sub <- standard_doc_sub[c(-3, -6), c(1:49, 53)]

sc4_model <- lm(doc.s4 ~ drainage_st + lat_st + slope_st + wetland_st + open_wat_st + tprod_for_st + conifer_st + harv5_st + insect5_st + harv10_st + wildfire15_st + harv15_st + insect15_st, data = doc_s4_sub)

summary(sc4_model)
check_model(sc4_model)

sc4_table <- dredge(sc4_model, rank = "AICc")

sc4_goodmodels <- subset(sc4_table, delta <= 2, recalc.weights = FALSE)

### Sample 5

doc_s5_sub <- standard_doc_sub[-3, c(1:49, 54)]

sc5_model <- lm(doc.s5 ~ drainage_st + lat_st + slope_st + wetland_st + open_wat_st + tprod_for_st + conifer_st + harv5_st + insect5_st + harv10_st + wildfire15_st + harv15_st + insect15_st, data = doc_s5_sub)

summary(sc5_model)
check_model(sc5_model)

sc5_table <- dredge(sc5_model, rank = "AICc")

sc5_goodmodels <- subset(sc5_table, delta <= 2, recalc.weights = FALSE)

### Sample 6 - 
sc6_model <- lm(doc.s6 ~ drainage_st + lat_st + slope_st + wetland_st + open_wat_st + tprod_for_st + conifer_st + harv5_st + insect5_st + harv10_st + wildfire15_st + harv15_st + insect15_st, data = standard_doc_tbl)

summary(sc6_model)
check_model(sc6_model)

sc6_table <- dredge(sc6_model, rank = "AICc")

sc6_goodmodels <- subset(sc6_table, delta <= 2, recalc.weights = FALSE)

### 3.04 - Extract averaged coefficients and merge into a new dataframe for better plotting ----

#### 3.04.1 - sc1 coefficient attempt ----

sc1_coeff <- as.data.frame(sc1_avg[[2]])
  
sc1_coeff_out <- sc1_coeff[-2, -1] %>% # take the full avg - based on model weight, which seems to be more accurate
  pivot_longer(cols = 1:8, names_to = "variable", values_to = "sc1.coefficient")

#### Try sc2
sc2_coeff <- as.data.frame(sc2_avg[[2]])

sc2_coeff_out <- sc2_coeff[-2, -1] %>%
  pivot_longer(cols = 1:11, names_to = "variable", values_to = "sc2.coefficient")

#### Try joining to get the NAs back

coeff_join <- full_join(sc1_coeff_out, sc2_coeff_out, by = "variable") # this works, continue with 3:6 and mean

#### sc3

sc3_coeff <- as.data.frame(sc3_avg[[2]])

sc3_coeff_out <- sc3_coeff[-2, -1] %>%
  pivot_longer(cols = 1:9, names_to = "variable", values_to = "sc3.coefficient")
  
#### sc4

sc4_coeff <- as.data.frame(sc4_avg[[2]])

sc4_coeff_out <- sc4_coeff[-2, -1] %>%
  pivot_longer(cols = 1:7, names_to = "variable", values_to = "sc4.coefficient")

#### sc5

sc5_coeff <- as.data.frame(sc5_avg[[2]])

sc5_coeff_out <- sc5_coeff[-2, -1] %>%
  pivot_longer(cols = 1:8, names_to = "variable", values_to = "sc5.coefficient")

#### sc6

sc6_coeff <- as.data.frame(sc6_avg[[2]])

sc6_coeff_out <- sc6_coeff[-2, -1] %>%
  pivot_longer(cols = 1:10, names_to = "variable", values_to = "sc6.coefficient")

#### mean DOC

meanDOC_coeff <- as.data.frame(meanDOC_avg[[2]])

meanDOC_coeff_out <- meanDOC_coeff[-2, -1] %>%
  pivot_longer(cols = 1:11, names_to = "variable", values_to = "meandoc.coefficient")

#### Merge into a new dataframe

doc_coeff_list <- list(sc1_coeff_out, sc2_coeff_out, sc3_coeff_out, sc4_coeff_out, sc5_coeff_out, sc6_coeff_out, meanDOC_coeff_out) # put dfs in list so I can reduce them to one

doc_coeff_out <- doc_coeff_list %>% reduce(full_join, by = "variable") %>% 
  pivot_longer(cols = 2:8, names_to = "campaign", values_to = "avg.coefficient")

## 4. PLOTTING ----

### 4.01 - Try plotting the coefficients for one model

modelplot(regr_model, coef_omit = 1) # this works and also removes the intercept - we don't need to see it

### 4.02 - Averaged coefficients for Mean DOC ----

meandoc_pl <- modelplot(meanDOC_avg, coef_omit = 1) +
  xlab("Mean DOC")

doc_avg <- model.avg(mean_doc, fit = TRUE)

### 4.03 - Sample campaign 1 plot ----
sc1_avg <- model.avg(sc1, fit = TRUE)

sc1_pl <- modelplot(sc1_avg, coef_omit = 1) +
  xlab("Sample campaign 1")

### 4.04 - Sample campaign 2 plot ----

sc2_avg <- model.avg(sc2, fit = TRUE)

sc2_pl <- modelplot(sc2_avg, coef_omit = 1) +
  xlab("Sample campaign 2")

### 4.05 - Sample campaign 3 plot ----

sc3_avg <- model.avg(sc3, fit = TRUE)

sc3_pl <- modelplot(sc3_avg, coef_omit = 1) +
  xlab("Sample campaign 3")

### 4.06 - Sample campaign 4 plot ----

sc4_avg <- model.avg(sc4, fit = TRUE)

sc4_pl <- modelplot(sc4_avg, coef_omit = 1) +
  xlab("Sample campaign 4")

### 4.07 - Sample campaign 5 plot ----

sc5_avg <- model.avg(sc5, fit = TRUE)

sc5_pl <- modelplot(sc5_avg, coef_omit = 1) +
  xlab("Sample campaign 5")

### 4.08 - Sample campaign 6 plot ----

sc6_avg <- model.avg(sc6, fit = TRUE)

sc6_pl <- modelplot(sc6_avg, coef_omit = 1) +
  xlab("Sample campaign 6")

### Put all the averages in a list

avg.coeff.list <- list(sc1_avg, sc2_avg, sc3_avg, sc4_avg, sc5_avg, sc6_avg, doc_avg)

### 4.09 - Try combining all of the plots ----

#### 4.09.1 - 1st attempt ----

coef_pl_rd <- plot_grid(meandoc_pl,
          sc1_pl,
          sc2_pl,
          sc3_pl,
          sc4_pl,
          sc5_pl,
          sc6_pl,
          label_x = 0.2,
          nrow = 3) # not a terrible rough draft, however I can make it better

title <- ggdraw() + draw_label("Averaged model coefficients with 95% confidence", fontface = 'bold') # make a quick title

plot_grid(title, coef_pl_rd, ncol=1, rel_heights=c(0.1, 1))

#### 4.09.2 - sjPlot variation ----

sjplot <- plot_models(sc1_avg, sc2_avg, sc3_avg, sc4_avg, sc5_avg, sc6_avg, doc_avg, grid = TRUE, vline.color = "black", show.legend = FALSE)

sjplot +
  theme_sjplot2()

#### 4.09.3 - dotwhisker plot variation ----

coeff_plot <- dwplot(avg.coeff.list) %>%
  relabel_predictors(c(
           conifer_st = "Coniferous",
           drainage_st = "Catchment Area",
           tprod_for_st = "Total Productive Forest",
           slope_st = "Slope",
           open_wat_st = "Open Water",
           lat_st = "Latitude",
           wetland_st = "Wetland",
           harv5_st = "5-year Harvest",
           harv10_st = "10-year Harvest",
           harv15_st = "15-year Harvest",
           insect5_st = "5-year Infestation",
           insect15_st = "15-year Infestation",
           wildfire15_st = "15-year Wildfire")) +
  theme_bw() +
  theme(legend.title = element_blank()) +
  geom_vline(xintercept = 0) +
  scale_colour_manual(labels = c("SC1", "SC2", "SC3", "SC4", "SC5", "SC6", "Mean DOC"),
                                 values = c("#99ccff", "#c4c1c6", "#005155", "#e9b22a", "#8c6d31", "#6600ff", "#000000"))

coeff_plot # this works for now, somehow need to customize it. We'll see what Jason says.

### 4.02 - Averaged sample campaign and mean DOC coefficients on one plot

doc_coeff_out  %>%
  ggplot(aes(x = avg.coefficient, y = variable)) +
  geom_point() +
  theme_bw() +
  facet_grid(~ campaign) +
  labs(x = "", y = "")

## 5. SAVING // EXPORTING ----

### 5.01 Save all of the model outputs for now (6 sample campaigns and 1 mean.doc)

saveRDS(meanDOC_goodsupport, file = "mean_doc_std_models.rds")
saveRDS(sc1_goodmodels, file = "sc1_doc_std_models.rds")
saveRDS(sc2_goodmodels, file = "sc2_doc_std_models.rds")
saveRDS(sc3_goodmodels, file = "sc3_doc_std_models.rds")
saveRDS(sc4_goodmodels, file = "sc4_doc_std_models.rds")
saveRDS(sc5_goodmodels, file = "sc5_doc_std_models.rds")
saveRDS(sc6_goodmodels, file = "sc6_doc_std_models.rds")

## 6. TRIAL // JUNK CODE ----

### 6.01 Try and see what the coefplot does in MuMIn pkg

fm <- glm(Prop ~ dose + I(dose^2) + log(dose) + I(log(dose)^2),
          data = Beetle, family = binomial, na.action = na.fail)
ma <- model.avg(dredge(fm))

# default coefficient plot:
plot(ma, full = NA, intercept = FALSE)

# Add colours per coefficient type
# Replicate each colour n(=number of coefficients) times
clr <- c("black", "red2")
i <- rep(1:2, each = length(coef(ma)) - 1)
plot(ma, full = NA, intercept = FALSE,
     pch = 22, dotcex = 1.5,
     col = clr[i], bg = clr[i],
     lwd = 6, lend = 1, width = 0, horizontal = 0)

