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

tprod_for_st

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

standard_doc_tbl <- readRDS("final_std_doc_table.rds")

## 3. TIDY // PROCESS ----

### 3.01 - Run a PCA on the standardised predictor variables to determine redundancy ----

pca.dat <- standard_doc_tbl[,c(34:53)] # select columns for pca analysis

pca <- prcomp(pca.dat, center = T, scale. = T) # run the pca

summary(pca) # display the pca results in table format

plot(pca)
biplot(pca)

fviz_pca_var(pca,
             ggtheme = theme_gray(),
             alpha.var=0.3,
             col.var = "steelblue",
             repel=TRUE,
             title = "PCA - Predictor variables",
             xlab = "PC1 (22.8%)",
             ylab = "PC2 (17.6%")

# look at correlation 

std_predictors <- standard_doc_tbl[34:53]

pairs(std_predictors)

# Some definite redundancy here, for now choose from the following to refine:
# 1. deciduous or coniferous (coniferous)
# 2. longitude or latitude (latitude)
# 3. insect 10 and 15 or insect 20 (insect 10 and 15)
# 4. open water or elevation (open water)
# 5. insect 10 or 15 (insect 15) - these are almost identical

# Removing abiotic 5 since only 5 sites have data
# Probably just keep 20-year harvest instead of 20-year and 15-year (similar enough, but harv20 has slightly higher %s for a few sites)

### 3.02 - Remove those variables and run the model for mean DOC ----

# standard_doc_sub <- standard_doc_tbl %>% 
  # select(-decid_std, -long_st, -insect20_st, -elev_st)

regr_model <- lm(mean.doc ~ drainage_st + lat_st + slope_st + wetland_st + open_wat_st + tprod_for_st + conifer_st + harv5_st + insect5_st + harv10_st + wildfire15_st + harv15_st + insect15_st, data = standard_doc_tbl)

summary(regr_model)
check_model(regr_model)

meanDOC_table <- dredge(regr_model, rank = "AICc")

meanDOC_goodsupport <- subset(meanDOC_table, delta <= 2, recalc.weights=FALSE) # 14 models within 2 AICc, I need to retain these coefficients

doc_avg <- model.avg(meanDOC_goodsupport, fit = TRUE)

### 3.03 - Run model for sample campaign 1, 2, 3, 4, 5, 6 ----

options(na.action = "na.omit") # reset this so the campaigns with NAs work

### Sample 1
sc1_model <- lm(doc.s1 ~ drainage_st + lat_st + slope_st + wetland_st + open_wat_st + tprod_for_st + conifer_st + harv5_st + insect5_st + harv10_st + wildfire15_st + harv15_st + insect15_st, data = standard_doc_tbl)

summary(sc1_model)
check_model(sc1_model)

sc1_table <- dredge(sc1_model, rank = "AICc")

sc1_goodmodels <- subset(sc1_table, delta <= 2, recalc.weights = FALSE)

sc1_avg <- model.avg(sc1_goodmodels, fit = TRUE)

### Sample 2
sc2_model <- lm(doc.s2 ~ drainage_st + lat_st + slope_st + wetland_st + open_wat_st + tprod_for_st + conifer_st + harv5_st + insect5_st + harv10_st + wildfire15_st + harv15_st + insect15_st, data = standard_doc_tbl)

summary(sc2_model)
check_model(sc2_model)

sc2_table <- dredge(sc2_model, rank = "AICc")

sc2_goodmodels <- subset(sc2_table, delta <= 2, recalc.weights = FALSE)

sc2_avg <- model.avg(sc2_goodmodels, fit = TRUE)

### Sample 3 - NAs become an issue here

doc_s3_sub <- standard_doc_sub[-3, c(1:49, 52)] # subset to remove the NA value

sc3_model <- lm(doc.s3 ~ drainage_st + lat_st + slope_st + wetland_st + open_wat_st + tprod_for_st + conifer_st + harv5_st + insect5_st + harv10_st + wildfire15_st + harv15_st + insect15_st, data = doc_s3_sub)

summary(sc3_model)
check_model(sc3_model)

sc3_table <- dredge(sc3_model, rank = "AICc")

sc3_goodmodels <- subset(sc3_table, delta <= 2, recalc.weights = FALSE)

sc3_avg <- model.avg(sc3_goodmodels, fit = TRUE)

### Sample 4

doc_s4_sub <- standard_doc_sub[c(-3, -6), c(1:49, 53)]

sc4_model <- lm(doc.s4 ~ drainage_st + lat_st + slope_st + wetland_st + open_wat_st + tprod_for_st + conifer_st + harv5_st + insect5_st + harv10_st + wildfire15_st + harv15_st + insect15_st, data = doc_s4_sub)

summary(sc4_model)
check_model(sc4_model)

sc4_table <- dredge(sc4_model, rank = "AICc")

sc4_goodmodels <- subset(sc4_table, delta <= 2, recalc.weights = FALSE)

sc4_avg <- model.avg(sc4_goodmodels, fit = TRUE)

### Sample 5

doc_s5_sub <- standard_doc_sub[-3, c(1:49, 54)]

sc5_model <- lm(doc.s5 ~ drainage_st + lat_st + slope_st + wetland_st + open_wat_st + tprod_for_st + conifer_st + harv5_st + insect5_st + harv10_st + wildfire15_st + harv15_st + insect15_st, data = doc_s5_sub)

summary(sc5_model)
check_model(sc5_model)

sc5_table <- dredge(sc5_model, rank = "AICc")

sc5_goodmodels <- subset(sc5_table, delta <= 2, recalc.weights = FALSE)

sc5_avg <- model.avg(sc5_goodmodels, fit = TRUE)

### Sample 6
sc6_model <- lm(doc.s6 ~ drainage_st + lat_st + slope_st + wetland_st + open_wat_st + tprod_for_st + conifer_st + harv5_st + insect5_st + harv10_st + wildfire15_st + harv15_st + insect15_st, data = standard_doc_tbl)

summary(sc6_model)
check_model(sc6_model)

sc6_table <- dredge(sc6_model, rank = "AICc")

sc6_goodmodels <- subset(sc6_table, delta <= 2, recalc.weights = FALSE)

sc6_avg <- model.avg(sc6_goodmodels, fit = TRUE)

#### 3.03.1 Put all the averages in a list ----

avg.coeff.list <- list(sc1_avg, sc2_avg, sc3_avg, sc4_avg, sc5_avg, sc6_avg, doc_avg)

### 3.04 - Base model and sensitivity analysis ----

doc_tbl_no_c3 <- standard_doc_tbl %>% 
  filter(!`Catchment ID` %in% "C3") # df with removal of C3

#### 3.04.1 - Mean DOC ----
mean_base_model <- lm(mean.doc ~ Group + conifer_st + drainage_st + elev_st + open_wat_st + wetland_st + slope_st + insect20_st, data = standard_doc_tbl)

# summary(mean_base_model)
# check_model(mean_base_model)

mean_base_model_tbl <- dredge(mean_base_model, rank = "AICc")

mean_base_model_avg <- model.avg(subset(mean_base_model_tbl, delta <= 2, recalc.weights=FALSE), fit = TRUE)

#### 3.04.2 - Sample campaign 1 ----

sc1_base_model <- lm(doc.s1 ~ Group + conifer_st + drainage_st + elev_st + open_wat_st + wetland_st + slope_st + insect20_st, data = standard_doc_tbl)

sc1_base_table <- dredge(sc1_base_model, rank = "AICc")

sc1_base_model_avg <- model.avg(subset(sc1_base_table, delta <= 2, recalc.weights = FALSE), fit = TRUE)

#### 3.04.3 - Sample campaign 2 ----

sc2_base_model <- lm(doc.s2 ~ Group + conifer_st + drainage_st + elev_st + open_wat_st + wetland_st + slope_st + insect20_st, data = standard_doc_tbl)

sc2_base_table <- dredge(sc2_base_model, rank = "AICc")

sc2_base_model_avg <- model.avg(subset(sc2_base_table, delta <= 2, recalc.weights = FALSE), fit = TRUE)

#### 3.04.4 - Sample campaign 3 ----

doc_s3_sub <- standard_doc_tbl[-3, c(1:53, 56)]

sc3_base_model <- lm(doc.s3 ~ Group + conifer_st + drainage_st + elev_st + open_wat_st + wetland_st + slope_st + insect20_st, data = doc_s3_sub)

sc3_base_table <- dredge(sc3_base_model, rank = "AICc")

sc3_base_model_avg <- model.avg(subset(sc3_base_table, delta <= 2, recalc.weights = FALSE), fit = TRUE)

#### 3.04.5 - Sample campaign 4 ----

doc_s4_sub <- standard_doc_tbl[c(-3, -6), c(1:53, 57)]

sc4_base_model <- lm(doc.s4 ~ Group + conifer_st + drainage_st + elev_st + open_wat_st + wetland_st + slope_st + insect20_st, data = doc_s4_sub)

sc4_base_table <- dredge(sc4_base_model, rank = "AICc")

sc4_base_model_avg <- model.avg(subset(sc4_base_table, delta <= 2, recalc.weights = FALSE), fit = TRUE)

#### 3.04.6 - Sample campaign 5 ----

doc_s5_sub <- standard_doc_tbl[-3, c(1:53, 58)]

sc5_base_model <- lm(doc.s5 ~ Group + conifer_st + drainage_st + elev_st + open_wat_st + wetland_st + slope_st + insect20_st, data = doc_s5_sub)

sc5_base_table <- dredge(sc5_base_model, rank = "AICc")

sc5_base_model_avg <- model.avg(subset(sc5_base_table, delta <= 2, recalc.weights = FALSE), fit = TRUE)

#### 3.04.7 - Sample campaign 6 ----

sc6_base_model <- lm(doc.s6 ~ Group + conifer_st + drainage_st + elev_st + open_wat_st + wetland_st + slope_st + insect20_st, data = standard_doc_tbl)

sc6_base_table <- dredge(sc6_base_model, rank = "AICc")

sc6_base_model_avg <- model.avg(subset(sc6_base_table, delta <= 2, recalc.weights = FALSE), fit = TRUE)

#### 3.04.8 - Attach all base model averages ----

base_model_avgs <- list(sc1_base_model_avg, sc2_base_model_avg, sc3_base_model_avg, sc4_base_model_avg, sc5_base_model_avg, sc6_base_model_avg, mean_base_model_avg)

## 4. PLOTTING ----

### 4.01 - Try plotting the coefficients for one model

modelplot(regr_model, coef_omit = 1) # this works and also removes the intercept - we don't need to see it

meandoc_pl <- modelplot(meanDOC_avg, coef_omit = 1) +
  xlab("Mean DOC") # Mean DOC

sc1_pl <- modelplot(sc1_avg, coef_omit = 1) +
  xlab("Sample campaign 1") # SC1

sc2_pl <- modelplot(sc2_avg, coef_omit = 1) +
  xlab("Sample campaign 2") # SC2

sc3_pl <- modelplot(sc3_avg, coef_omit = 1) +
  xlab("Sample campaign 3") # SC3

sc4_pl <- modelplot(sc4_avg, coef_omit = 1) +
  xlab("Sample campaign 4") # SC4

sc5_pl <- modelplot(sc5_avg, coef_omit = 1) +
  xlab("Sample campaign 5") # SC5

sc6_pl <- modelplot(sc6_avg, coef_omit = 1) +
  xlab("Sample campaign 6") # SC6

### 4.02 - Try combining all of the plots ----

#### 4.02.1 - 1st attempt ----

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

#### 4.02.2 - sjPlot variation ----

sjplot <- plot_models(doc_avg, sc6_avg, sc5_avg, sc4_avg, sc3_avg, sc2_avg, sc1_avg,
                      grid = TRUE, 
                      vline.color = "black", 
                      show.legend = FALSE, 
                      colors = "black",
                      dot.size = 2,
                      axis.title = "",
                      m.labels = c("Mean DOC", "SC6", "SC5", "SC4", "SC3", "SC2", "SC1"))
                      #axis.labels = c("15-year Wildfire", "15-year Infestation", "5-year Infestation", "15-year Harvest", "10-year Harvest", "5-year Harvest", "Wetland", "Latitude", "Open Water", "Slope", "Total Productive Forest", "Catchment Area", "Coniferous"))

sjplot +
  theme_sjplot2()

#### 4.02.3 - dotwhisker plot variation ----

mean_base_model_plot <- dwplot(base_model_avgs) %>%
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
           insect20_st = "20-year Infestation")) +
  theme_bw() +
  labs(title = "Average multiple regression coefficients - 20-year Infestation") +
  theme(legend.title = element_blank(), plot.title = element_text(hjust = 0.5, face="bold")) +
  geom_vline(xintercept = 0) +
  scale_colour_manual(labels = c("SC1", "SC2", "SC3", "SC4", "SC5", "SC6", "Mean DOC"),
                                 values = c("#99ccff", "#c4c1c6", "#005155", "#e9b22a", "#8c6d31", "#6600ff", "#000000"))

# view plot
mean_base_model_plot

### 4.03 - Averaged sample campaign and mean DOC coefficients on one plot ----

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

# scale_colour_manual(labels = c("SC1", "SC2", "SC3", "SC4", "SC5", "SC6", "Mean DOC"),
                    values = c("#99ccff", "#c4c1c6", "#005155", "#e9b22a", "#8c6d31", "#6600ff", "#000000")) # save for redoing plots later

### 6.01 - Try and see what the coefplot does in MuMIn pkg ----

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

### 6.02 - Look at summary graph of best model for Mean DOC ----

best_meanDOC <- lm(mean.doc ~ conifer_st + drainage_st + harv5_st + insect15_st + insect5_st + tprod_for_st, data = standard_doc_sub)

summary(best_meanDOC)
check_model(best_meanDOC)

### 6.03 - Quickly recheck the forest cover scatterplots ----

standard_doc_tbl %>% 
  ggplot(aes(x = `Total Productive Forest (%)`, y = `Deciduous Forest (%)`)) +
  geom_point()

# conifer and total productive forest = no real visual relationship
# conifer and decid = negative relationship (as expected)
# decid and total productive forest = slight positive relationship?

cor(standard_doc_tbl$`Deciduous Forest (%)`, standard_doc_tbl$`Coniferous Forest (%)`, method = "spearman")

# -0.20 conifer and total productive forest
# 0.31 deciduous and total productive forest
# -.58 conifer and deciduous

### 6.04 - Look at harvest/infestation scatterplots and correlations ----

standard_doc_tbl %>% 
  ggplot(aes(x = `20-year Insect Disturbance (%)`, y = `5-year Insect Disturbance (%)`)) +
  geom_point()

# Harvest

# 15 and 20 year: strong linearity
# 15 and 10 year: strong linearity
# 15 and 5 year: strong linearity (less that 15 and 10)
# 10 and 5 year: strong linearity

# Infestation

# 20 and 15 year: appears linear but not due to a large number of 0% sites at 15-year that are 100% at 20-year
# 20 and 5-year: no relationship
# 15 and 10-year: strongly linear
# 15 and 5-year: no relationship
# 10 and 5-year: no relationship


cor(standard_doc_tbl$`15-year Insect Disturbance (%)`, standard_doc_tbl$`5-year Insect Disturbance (%)`, method = "spearman")

# Harvest

# 15 and 20 year: 0.86 - strongly correlated!
# 15 and 10 year: 0.94 - strongly correlated
# 15 and 5 year: 0.77 - still quite strong
# 10 and 5 year: 0.84 - strongly correlated

# Infestation

# 20 and 15 year: -0.44
# 20 and 5 year: -0.33
# 15 and 10 year: 1 (perfect correlation)
# 10 and 5 year: 0.75


