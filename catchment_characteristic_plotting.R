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

ws_table <- read_excel("Watershed_table_v1.xlsx")

## 3. TIDY // PROCESS ----

ws_sub <- ws_table[, 7:17] %>% 
  select(-`Mixed Forest (%)`)# make data wide format to setup for auto graphs

colnames(ws_sub) <- c("lat", "long", "area", "elev", "slope", "wetland", "water", "forest", "deciduous", "coniferous")

expl_1 <- names(ws_sub)[1:10]
expl_2 <- names(ws_sub)[1:10]

## 4. PLOTTING ----

### 4.01 - Plot the type of graph I want once ----

ws_sub %>% 
  ggplot(aes(area, elev)) +
  geom_point() +
  geom_smooth(method = "lm", formula = y~x, se = FALSE, colour = "black") +
  scale_x_log10() +
  labs(x = "Area", y = "Bog (%)") # standard area-bog scatter plot

### 4.02 - Change this to a function to automate similar graphs ----

expl_plots <- function(x, y) {
    ggplot(ws_sub, aes(x = .data[[x]], y = .data[[y]])) +
    geom_point()
}

expl_plots(x = "long", y = "lat") # test the function - it works

wetland_plots = map(expl_1, ~expl_plots(.x, "wetland") ) # test map from purr - it works

allplots_made = pmap(plot_subset8, ~expl_plots(x = .y, y = .x) ) # makes all of the plots and places them in a list

allplots_names = pmap(unique_plots, ~paste0(.x, "_", .y, ".png")) # gives unique names to all of the plots based on their column names

allplots_names[1:90] # views all of the plots in the console


### 4.03 - Checking for collinearity between all of the predictor variables ----

summary(lm(ws_sub$wetland ~ ws_sub$water))

allplots_made

## 5. SAVING // EXPORTING ----

pdf("landcover_var_scatterplots.pdf")
allplots_made
dev.off()


### Save all unique combos of predictor variables to a RDS

saveRDS(plot_subset8, file = "landcover_variable_combos.RDS")
## 6. TRIAL // JUNK CODE ----

