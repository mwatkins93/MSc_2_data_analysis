####-------------------------
## Name: Matt Watkins
## Date: Jan. 13th/22
## Project: MSc data analysis (water chemistry)
## Objective: Explore the Waterloo chemistry
## Inputs: UW DBP-FP Fall 2021 report
## Outputs: exploratory plots for DOC
####-------------------------

## 0. Notes
##---------------------------

# Main purpose of this script is to explore the DOC concentrations between sites

## 1. Prepare
##---------------------------

library(tidyverse)
library(readxl)
library(stringr)
library(plotly)
library(RColorBrewer)

## 2. Import
##---------------------------

# Bring in UW water chemistry data

water_chem <- read_xlsx("/Volumes/MW/2020 Trent University/Data/UW-DBP-FP/DBP-FP_Report-BS-Fall_2021_v1.xlsx")

## 3. Tidy / Process
##---------------------------

# Remove the duplicates - were only able to measure THMsFP for these samples

water_chem_v1 <- water_chem[-c(34, 37, 43 ),]

# Shift date portion into a new column

site_date <- str_split_fixed(water_chem_v1$`Sample Identifier`, "_BS-Trent-MW_", 2)

# Change column names to date and site

colnames(site_date) <- c("date", "site")

# Bind it back so I can group and graph

water_chem_v2 <- cbind(site_date, water_chem_v1)

## 4. Plotting
##---------------------------

# Explore doc values 
doc <- water_chem_v2 %>% 
  ggplot(aes(x = site, y = `DOC [ppm]`)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter() +
  theme(axis.text.x=element_text(angle = 45, vjust = 0.5))

# Add color

doc + scale_

  


## 5. Saving / Exporting
##---------------------------

## 6. Notes / Junk Code
##---------------------------
