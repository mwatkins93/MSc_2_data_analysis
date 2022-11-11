#########################
## Name: Matt Watkins
## Date: May, 2022
## Project: MSc data analysis
## Objective: Mass flux estimation
## Inputs: glfc_data_v1
## Outputs:
#########################

## 0. NOTES ----

### 0.1 - Fri. Nov. 4th update: rechecking this process and updating the code with the revised glfc water chemistry dataset (glfc_data_v2.rds)


## 1. PREPARE ----

rm(list=ls())
options(stringsASfactors = FALSE, scipen = 999, encoding = "UTF-8")

library(tidyverse)
library(readxl)
library(scales)
library(lubridate)

theme_update(text = element_text(size=20)) # Make graph text slightly bigger for readability

colours <- scale_fill_manual(values = c("#E69F00", "#56B4E9", "#009E73", 
                                        "#F0E442")) # Set colour scheme and font size; 
                                                    # other colours #0072B2", "#D55E00", "#CC79A7"

## 2. IMPORT ----

glfc_data <- readRDS("/Volumes/MW/2020 Trent University/Data/GLFC Water Chemistry/glfc_data_v2.rds")

watersheds <- read_xlsx("Watershed_table_v1.xlsx")

## 3. TIDY // PROCESS ----

ws_prep <- watersheds %>% 
  select(`Site name`, Group) # select only columns needed for adding group

colnames(ws_prep)[1:2] <- c("catchment", "group") # change column name to match

ws_merge <- left_join(ws_prep, glfc_data, by = "catchment") # left join by the catchment column so groups are assigned

### 3.01 - Select necessary columns and multiple discharge by 1000 to get L/s

glfc_orgC <- ws_merge %>% 
  select(catchment, catchment.id, sample.number, sample.date, daily.discharge, organic.carbon, drainage.area, group) %>% 
  na.omit() %>% 
  mutate(discharge.litres = daily.discharge * 1000)

### 3.02 - Multiply concentration by daily discharge and divide by catchment area to obtain mass flux per unit time per unit area (mg/s/km2)

inst_mass_flux <- glfc_orgC %>% 
  mutate(mass.time = daily.discharge * organic.carbon) %>% 
  mutate(mass.time.area = mass.time / drainage.area)

### 3.03 - Only keep the sites with "good" Q status and not partial/poor that are left (SBC / WS 93)

goodQ_mf <- inst_mass_flux %>% 
  filter(catchment != "WS SBC" & catchment != "WS 93")

### 3.04 - Calculate mean mass flux across entire field season

flux_means <- goodQ_mf %>%
  group_by(catchment, group) %>% 
  summarise(flux.means = mean(mass.time.area))

## 4. PLOTTING ----

### 4.01 ---- Mass flux across all good Q catchments ----

goodQ_mf %>% 
  ggplot(aes(sample.date, mass.time.area)) +
  geom_point(size = 3) +
  theme_bw(base_size = 16) +
  theme(axis.text.x=element_text(angle = 45, vjust = 0.5), legend.title = element_blank()) +
  facet_wrap(~ catchment.id) +
  labs(x = "", y = expression(paste("DOC mass flux " (mg/s/km^2))))

### 4.02 ---- Mean mass flux comparison between catchment types ----

mean_flux_plot <- flux_means %>% 
  ggplot(aes(x = group, y = flux.means, fill = group)) +
  geom_boxplot(outlier.shape = NA) + 
  scale_fill_manual(values = c("#E69F00", "#56B4E9", "#009E73", 
                               "#F0E442")) +
  geom_jitter() +
  theme_classic(base_size = 16) +
  theme(axis.text.x=element_text(angle = 45, vjust = 0.5), legend.title = element_blank(), legend.position = "none", plot.title = element_text(hjust = 0.5)) + 
  labs(x = "", y = expression(paste("DOC mass flux " (mg/s/km^2))))
  
mean_flux_plot

## 5. SAVING // EXPORTING ----

### 5.01 - Save the cleaned mass flux calculation dataframe

goodQ_mf %>% 
  saveRDS(file = "mass_flux_estimates_cleaned_v1.00.rds")

## 6. TRIAL // JUNK CODE ----

### 6.01 Compute the range of DOC flux for each catchment group ----

insect_mf <- mean_mf%>% 
  filter(group %in% "Insect")

### 6.02 Spit out the mean flux values of each catchment group to double check with 4.02 graph

flux_means <- mean_mf %>%
  group_by(catchment, group) %>% 
  summarise(flux.means = mean(mass.time.area))
    
  




