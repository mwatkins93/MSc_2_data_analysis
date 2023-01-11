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

ws_landcover_var <- ws_table[, 7:17]

ws_landcover_out <- ws_landcover_var[, -10] # get down to only the variable columns needed

### 3.01 - Test for normality and run a Spearman's correlation ----

apply(ws_landcover_out, 2, shapiro.test) # 8 out of 10 variables are not normally distributed which suggets I should do a Spearmans's correlation. I'm basing this on the assumption that all variables need to appear normal (which they don't) to run a Pearson's correlation.

cor_spearman <- cor(ws_landcover_out, method = "spearman") # runs the spearman correlation and displays a table in the console


## 4. PLOTTING ----

### 4.01 Preliminary plots of the variables ----

plot(ws_landcover_out) # basic view of all the relationships

ggdensity(ws_landcover_out$`Deciduous Forest (%)`) # double-checking that deciduous/coniferous appears normal (as per Shapiro)

### 4.02 Spearman correlation plot (section 3.01) ----

corrplot(cor_spearman, method = "number")

## 5. SAVING // EXPORTING ----

## 6. TRIAL // JUNK CODE ----

