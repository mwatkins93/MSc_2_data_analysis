#########################
## Name: Matt Watkins
## Date: Nov. 11th 22
## Project: MSc data analysis
## Objective: Compute coefficient of variance for concentration and Q and compare ratios
## Inputs: mass_flux_estimates_cleaned_v1.00.rds
## Outputs: 
#########################

## 0. NOTES ----

## 1. PREPARE ----

rm(list=ls())
options(stringsASfactors = FALSE, scipen = 999, encoding = "UTF-8")

library(tidyverse)

theme_update(text = element_text(size = 20))

## 2. IMPORT ----

## 2.01 - import mass flux RDS

mass_flux <- readRDS("mass_flux_estimates_cleaned_v1.00.rds")

## 3. TIDY // PROCESS ----

### 3.01 - subset the df to just the necessary columns

mass_flux_sub <- mass_flux %>% 
  select(group, catchment, catchment.id, sample.number, sample.date, daily.discharge, organic.carbon)

### 3.02 - compute mean, std and coefficient of variance for concentration and Q

mf_cv <- mass_flux_sub %>% 
  group_by(catchment) %>% 
  mutate(mean.conc = mean(organic.carbon),
         mean.q = mean(daily.discharge),
         std.conc = sd(organic.carbon),
         std.q = sd(daily.discharge),
         cv.conc = std.conc / mean.conc,
         cv.q = std.q / mean.q,
         cvc.cvq.ratio = cv.conc / cv.q)

## 4. PLOTTING ----

mf_cv %>% 
  ggplot(aes(x = catchment.id, y = cvc.cvq.ratio)) +
  geom_point() +
  theme_bw() +
  scale_x_discrete(limits = c("C2", "C4", "C8", "C9", "C12", "C14", "H2", "H3", "I2", "I3", "I4", "M3", "M4", "M5", "M6")) +
  labs(x = "", y = "CVC / CVQ")

## 5. SAVING // EXPORTING ----

mf_cv %>% 
  saveRDS(file = "cQ_coeffiecient_of_variance_calc.rds")

## 6. TRIAL // JUNK CODE ----

