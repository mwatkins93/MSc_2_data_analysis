#########################
## Name: Matt Watkins
## Date: Feb. 15th/23
## Project: MSc data analysis
## Objective: Investigate WS 67 water chemistry - are there any clues or anomalies here that can explain it outlier status in terms of DOC
## Inputs: water chem
## Outputs:
#########################

## 0. NOTES ----

## 1. PREPARE ----

rm(list=ls())
options(stringsASfactors = FALSE, scipen = 999, encoding = "UTF-8")

library(tidyverse)
library(readxl)

## 2. IMPORT ----

water_chem <- readRDS("glfc_chem_cleaned_v1.01.rds")

## 3. TIDY // PROCESS ----

wide_chem <- water_chem %>% 
  pivot_wider(names_from = "variable", values_from = "value")

range((wide_chem)[,11])

mean.chem <- wide_chem %>% 
  group_by(site) %>% 
  mutate(mean.total.n = mean(total.n),
         mean.sodium = mean(Na),
         mean.k = mean(K),
         mean.ca = mean(Ca),
         mean.inorg.c = mean(inorganic.carbon),
         mean.cl = mean(Cl))

## 4. PLOTTING ----

expl_plots <- function(x, y) {
  ggplot(mean.chem, aes(x = .data[[x]], y = .data[[y]])) +
    geom_point()
}

### 4.01 Total nitrogen ----
total.n %>% 
ggplot(aes(x = site, y = total.n)) +
  geom_point() # (1) look at mean total n, 67 sits highest; (2) look at variability and its range is similar to other sites (some lower values as well)

### 4.02 Sodium ----

expl_plots(x = "site", y = "mean.sodium")

mean.chem %>% 
  ggplot(aes(x = site, y = Na)) +
  geom_point() # (1)67 is heavily skewed by one extremely high sample - the rest fall right in line with other sites; (2) 92 is the other outlier and all of its other samples are higher than 67

### 4.03 Potassium ----

expl_plots(x = "site", y = "K") # (1) again, skewed heavily by 1 sample (Sample 4); rest (i.e., mean value and sample range outside of 4) fall right in line with the other sites; (2) 36 seems to be the high outlier - consistenly high sample values

### 4.04 Calcium ----

expl_plots(x = "site", y = "Ca") # (1) that one sample is such an outlier value, mean is fine; 36 sample values are quite high again

### 4.05 Inorganic carbon ----

expl_plots(x = "site", y = "inorganic.carbon") # (1) same as Ca, one outlier across samples; (2) 36 is high; (3) mean is fine!

### 4.06 Chlorine ----

expl_plots(x = "site", y = "Cl") # just a single sample outlier skews this mean

### 4.07 Conductivity ----

expl_plots(x = "site", y = "conductivity") # same thing, single huge outlier sample skews the mean for 67




## 5. SAVING // EXPORTING ----

## 6. TRIAL // JUNK CODE ----

