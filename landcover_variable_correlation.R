#########################
## Name: Matt Watkins
## Date: Jan. 11th/23
## Project: MSc Data Analysis
## Objective: Run normality and correlation on land cover variables
## Inputs: master watershed table
## Outputs: normality data and correlation tables
#########################

## 0. NOTES ----

## 1. PREPARE ----

rm(list=ls())
options(stringsASfactors = FALSE, scipen = 999, encoding = "UTF-8")

library(tidyverse)
library(readxl)
library(ggpubr)
library(corrplot)

## 2. IMPORT ----

ws_table <- read_excel("Watershed_table_v1.xlsx")

## 3. TIDY // PROCESS ----

### 3.01 Landscape predictor correlation matrix ----

ws_landcover_var <- ws_table[, 7:17]

ws_landcover_out <- ws_landcover_var[, -10] # get down to only the variable columns needed

### 3.02 Test for normality and run a Spearman's correlation ----

apply(ws_landcover_out, 2, shapiro.test) # 8 out of 10 variables are not normally distributed which suggets I should do a Spearmans's correlation. I'm basing this on the assumption that all variables need to appear normal (which they don't) to run a Pearson's correlation.

cor_spearman <- cor(ws_landcover_out, method = "spearman") # runs the spearman correlation and displays a table in the console

### 3.03 Full predictor correlation matrix (landscape and disturbance) ----

full_corr <- ws_table[, c(7:16, 18:23, 25:27, 29:31)] # Remove 5-year wildfire (zero); and 10-, 15- and 20-year abiotic (identical to 5-year) 

full_corr_spearman <- cor(full_corr, method = "spearman")

## 4. PLOTTING ----

### 4.01 Preliminary plots of the variables ----

plot(ws_landcover_out) # basic view of all the relationships

ggdensity(ws_landcover_out$`Deciduous Forest (%)`) # double-checking that deciduous/coniferous appears normal (as per Shapiro)

### 4.02 Landscape correlation plot (section 3.01) ----

corrplot(cor_spearman, method = "number")

png(file="landscape_correlation.png", res=300, width=7000, height=7000)
corrplot(cor_spearman, tl.cex = 3, tl.col = "black", method = "color", 
         outline = T,  order="hclust", 
         addCoef.col = "black", number.digits = 2, number.cex = 3, 
         cl.pos = 'b', cl.cex = 3, addrect = 3, rect.lwd = 3, 
         col = colorRampPalette(c("midnightblue", "white","darkred"))(100))
dev.off()

### 4.03 Full correlation plot (section 3.03) ----

corrplot(full_corr_spearman, method = "number")

png(file="full_predictor_corrplot.png", res=300, width=7000, height=7000)
corrplot(full_corr_spearman, tl.cex = 3, tl.col = "black", method = "color", 
         outline = T,  order="hclust", 
         addCoef.col = "black", number.digits = 2, number.cex = 3, 
         cl.pos = 'b', cl.cex = 3, addrect = 3, rect.lwd = 3, 
         col = colorRampPalette(c("midnightblue", "white","darkred"))(100))
dev.off()

## 5. SAVING // EXPORTING ----

## 6. TRIAL // JUNK CODE ----

