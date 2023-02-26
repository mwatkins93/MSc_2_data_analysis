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

## 1. PREPARE ----

rm(list=ls())
options(stringsASfactors = FALSE, scipen = 999, encoding = "UTF-8", na.action = "na.fail")

library(tidyverse)
library(readxl)
library(performance)
library(MuMIn)

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

meanDOC_avg <- model.avg(meanDOC_goodsupport)

plot(meanDOC_avg, full = NA, intercept = FALSE)

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


## 4. PLOTTING ----

## 5. SAVING // EXPORTING ----

### 5.01 Save all of the model outputs for now (6 sample campaigns and 1 mean.doc)

saveRDS(meanDOC_goodsupport, file = "mean_doc_std_models.rds")
saveRDS(sc1_goodmodels, file = "sc1_doc_std_models.rds")
saveRDS(sc2_goodmodels, file = "sc2_doc_std_models.rds")
saveRDS(sc3_goodmodels, file = "sc3_doc_std_models.rds")
saveRDS(sc4_goodmodels, file = "sc4_doc_std_models.rds")
saveRDS(sc5_goodmodels, file = "sc6_doc_std_models.rds")
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

