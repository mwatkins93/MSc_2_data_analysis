#########################
## Name: Matt Watkins
## Date: Aug. 25th / '22
## Project: MSc data analysis
## Objective: Make catchment characteristic plots to identify confounding variables
## Inputs: ws_char RDS
## Outputs: ggplots
#########################

## 0. NOTES ----

## 1. PREPARE ----

rm(list=ls())
options(stringsASfactors = FALSE, scipen = 999, encoding = "UTF-8")

library(tidyverse)
library(readxl)
library(rlang)

## 2. IMPORT ----

ws_char <- readRDS("watershed_characteristics_cleaned.RDS")

## 3. TIDY // PROCESS ----

ws_wide <- ws_char %>% 
  spread(variable, value) %>% 
  select(-site, -catchment.id)# make data wide format to setup for auto graphs

response <- names(ws_wide)[11]
expl <- names(ws_wide)[1:10]

## 4. PLOTTING ----

### 4.01 - Plot the type of graph I want once

ws_wide %>% 
  ggplot(aes(area, bog)) +
  geom_point() +
  geom_smooth(method = "lm", formula = y~x, se = FALSE, colour = "black") +
  scale_x_log10() +
  labs(x = "Area", y = "Bog (%)") # standard area-bog scatter plot

### 4.02 - Change this to a function to automate similar graphs

expl_plots <- function(x, y) {
    ggplot(ws_wide, aes(x = .data[[x]], y = .data[[y]])) +
    geom_point() +
    geom_smooth(method = "lm", formula = y~x, se = FALSE, colour = "black")
}

expl_plots(x = "wetland", y = "deciduous") # test the function - it works


## 5. SAVING // EXPORTING ----

## 6. TRIAL // JUNK CODE ----

