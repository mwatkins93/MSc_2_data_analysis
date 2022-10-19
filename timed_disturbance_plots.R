#########################
## Name: Matt Watkins
## Date: Oct. 5th / 22
## Project: MSc data analysis
## Objective: Create graphs of disturbance in timed bins - updated landscape cover plots
## Inputs: watershed table and DOC chemistry
## Outputs: scatterplots
#########################

## 0. NOTES ----

## 1. PREPARE ----

rm(list=ls())
options(stringsASfactors = FALSE, scipen = 999, encoding = "UTF-8")

library(tidyverse)
library(readxl)

## 2. IMPORT ----

### Bring in watershed table and DOC chemistry data

water_chem <- readRDS("glfc_data_v2.rds")
  
watersheds <- read_xlsx("Watershed_table_v1.xlsx")

## 3. TIDY // PROCESS ----

### Take DOC means and subset in prep for merge

doc <- water_chem %>% 
  select(catchment, sample.number, `catchment.id`, `organic.carbon`) %>% 
  pivot_longer(`organic.carbon`, names_to = "variable", values_to = "value")

doc_means <- doc %>% 
  group_by(catchment) %>% 
  mutate(mean.doc = mean(value),
         doc.sd = sd(value))

### Change column name for merge

colnames(watersheds)[1] <- "catchment"

### Merge dataframes

ws_merge <- left_join(watersheds, doc_means, by = "catchment")
  
## 4. PLOTTING ----

### DOC & sparse forest %

ws_merge[ws_merge == 0] <- NA

ws_merge %>% 
  ggplot(aes(`15-year wildfire disturbance (%)`, mean.doc)) +
  geom_point() +
  geom_smooth(method = "lm", formula = y~x, se = FALSE, colour = "black") +
  geom_errorbar(aes(ymin = mean.doc - doc.sd, ymax = mean.doc + doc.sd), width = 0) +
  labs(x = "15-year wildfire disturbance (%)", y = "DOC (mg/L)")

rm(ins_20plot)


## 5. SAVING // EXPORTING ----

## 6. TRIAL // JUNK CODE ----

expl_plots <- function(x, y) {
  ggplot(ws_merge, aes(x = .data[[x]], y = .data[[y]])) +
    geom_point() +
    geom_smooth(method = "lm", formula = y~x, se = FALSE, colour = "black")
}

expl_plots(x = "5-year Harvest Disturbance (%)", y = "mean.doc")

