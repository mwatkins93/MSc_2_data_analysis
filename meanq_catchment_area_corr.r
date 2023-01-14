#########################
## Name: Matt Watkins
## Date: Jan. 13th/23
## Project: MSc data analysis
## Objective: Does mean streamflow increase as drainage area increases?
## Inputs: (1) master_ws_table; Q_estimates_v1 
## Outputs: scatterplot
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

q_estimates <- readRDS("Q_estimates_v2.rds")

## 3. TIDY // PROCESS ----

## 3.01 - Take mean of streamflow estimates for each site ----

mean_q <- q_estimates_v2 %>% 
  group_by(site) %>% 
  mutate(mean_q = mean(totalQ))

## 3.02 - Join mean_q to ws_table

area_sub <- ws_table %>% 
  select(`Site name`, `Catchment ID`, Group, `Drainage Area (km2)`)

colnames(area_sub)[1] <- "site"

site_meanQ_join <- left_join(area_sub, mean_q) %>%
  filter(!site %in% c("WS 77", "WS 82", "WS 75", "WS 76")) # remove the non Q estimate sites

## 3.03 - Run normality and a correlation between the two variables

shapiro.test(area_sub$`Drainage Area (km2)`) # significantly not normal!
shapiro.test(mean_q$mean_q) # significantly not normal

corr_variables <- site_meanQ_join %>% 
  select(`Drainage Area (km2)`, mean_q)

corr_spearman <- cor(corr_variables, method = "spearman")

corr_spearman # there is a strong correlation (r = 0.86) between drainage area and mean discharge (per site)


## 4. PLOTTING ----

site_meanQ_join %>% 
  ggplot(aes(x = `Drainage Area (km2)`, y = mean_q)) +
  geom_point() +
  theme_bw() # strong correlation between drainage area and mean Q estimate for each site - 

## 4.02 - Check normality of drainage area and mean_q

ggdensity(area_sub$`Drainage Area (km2)`) +
  xlab(expression("Catchment area " (km^2)))# not normal
ggdensity(mean_q$mean_q) +
  xlab("Mean discharge (per site)")# not normal

## 5. SAVING // EXPORTING ----

## 6. TRIAL // JUNK CODE ----

