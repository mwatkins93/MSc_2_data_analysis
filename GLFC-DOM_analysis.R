#########################
## Name: Matt Watkins
## Date: Feb. 22nd / 22
## Project: MSC_data_analysis
## Objective: Explore the GLFC-DOM data
## Inputs: DOM csv
## Outputs: exploratory plots for A 254, DOC, etc
#########################

## 0. NOTES ----

## 1. PREPARE ----

rm(list=ls())
options(stringsASfactors = FALSE, scipen = 999, encoding = "UTF-8")

library(tidyverse)
library(readxl)

## 2. IMPORT ----

### Bring in DOM data

dom <- read_csv("/Volumes/MW/2020 Trent University/Data/GLFC-DOM/TUMW_2021_DOM_indices.csv", skip = 3)

## 3. TIDY // PROCESS ----

### Split number in sample.id into own column

id_number <- str_split_fixed(dom$sample.ID, pattern = "-S-0", 2)

### Rename columns and bind them back to the original dataset

colnames(id_number) <- c("site", "sample.number")

dom_v1 <- cbind(dom, id_number)

## 4. PLOTTING ----

a254_plot <- dom_v1 %>% 
  ggplot(aes(x = reorder(site, +a254), y = a254)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter() +
  theme_classic() +
  theme(axis.text.x=element_text(angle = 45, vjust = 0.5), legend.position="none") +
  labs(x = "Site", y = "Absorbance coefficient at 254nm")

a254_plot # how can I better visualise this??

## 5. SAVING // EXPORTING ----

## 6. TRIAL // JUNK CODE ----