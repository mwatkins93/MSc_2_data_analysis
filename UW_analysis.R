####-------------------------
## Name: Matt Watkins
## Date: Jan. 13th/22
## Project: MSc data analysis (water chemistry)
## Objective: Explore the Waterloo chemistry
## Inputs: UW DBP-FP Fall 2021 report
## Outputs: exploratory plots for water quality
####-------------------------

## 0. Notes
##---------------------------

# Main purpose of this script is to explore the DOC and THM-FP concentrations between sites

## 1. Prepare
##---------------------------

library(tidyverse)
library(readxl)
library(stringr)
library(RColorBrewer)

### Set colour scheme

colours <- scale_fill_manual(values = c("brown1", "chartreuse4", "cyan3", "darkorchid2"))

## 2. Import
##---------------------------

# Bring in UW water chemistry data

water_chem <- read_xlsx("/Volumes/MW/2020 Trent University/Data/UW-DBP-FP/DBP-FP_Report-BS-Fall_2021_v1.xlsx")

## 3. Tidy / Process
##---------------------------

### Remove the duplicates - were only able to measure THMsFP for these samples

water_chem_v1 <- water_chem[-c(34, 37, 43 ),]

### Shift date portion into a new column

site_date <- str_split_fixed(water_chem_v1$`Sample Identifier`, "_BS-Trent-MW_", 2)

### Change column names to date and site

colnames(site_date) <- c("date", "site")

### Bind it back so I can group and graph

water_chem_v2 <- cbind(site_date, water_chem_v1)

### Create a new column for treatment type

water_chem_v3 <- water_chem_v2 %>% 
  mutate(type = case_when(site == "W11" ~ "Mixed",
                          site == "W87" ~ "Mixed",
                          site == "SBC" ~ "Harvest",
                          site == "W82" ~ "Harvest",
                          site == "W17" ~ "Insect",
                          site == "W52" ~ "Insect",
                          site == "W66" ~ "Control",
                          site == "KWR" ~ "Control"))

### Calculate SUVA

water_chem_v4 <- water_chem_v3 %>% 
  mutate(SUVA = (`UV254 [cm-1]` / `DOC [ppm]`) * 100)

suva_test <- water_chem_v4 %>% 
  filter(!type %in% "Control")

mean(suva_test$SUVA)

range(suva_test$SUVA)

## 3.1 - Calculate group means ----
###################################

### DOC

doc_means <- water_chem_v4 %>% 
  group_by(type) %>% 
  summarise(doc_means = mean(`DOC [ppm]`)) %>% 
  ggplot(aes(x = type, y = doc_means)) +
  geom_point()

doc_means

### THM-FP

thmfp_means <- water_chem_v4 %>% 
  group_by(type) %>% 
  summarise(thmfp_means = mean(`THMsFP [μg/L]`))

### SUVA

suva_means <- water_chem_v4 %>% 
  group_by(type) %>% 
  summarise(suva_means = mean(SUVA))

## 4. Plotting
##---------------------------

## 4.1 Explore doc values 

doc <- water_chem_v4 %>% 
  group_by(type) %>% 
  ggplot(aes(x = reorder(type, `DOC [ppm]`), y = `DOC [ppm]`, fill = type)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter() +
  theme_classic() +
  theme(axis.text.x=element_text(angle = 45, vjust = 0.5), legend.position = "none", plot.title = element_text(hjust = 0.5)) +
  labs(x = "", y = "DOC (mg/L)") +
  ggtitle("Mean [DOC] in eight headwater catchments")
  
doc + colours

## 4.2 Explore THM-FP

thm_fp <- water_chem_v4 %>% 
  group_by(type) %>% 
  ggplot(aes(x = type, y = `THMsFP [μg/L]`, fill = type)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter() +
  theme_classic() +
  theme(axis.text.x=element_text(angle = 45, vjust = 0.5), legend.position = "none", plot.title = element_text(hjust = 0.5)) +
  labs(x = "", y = "THM-FP (μg/L)") +
  ggtitle("Mean [THM-FP] in eight headwater catchments")

thm_fp + colours

## 4.3 Explore SUVA

suva <- water_chem_v4 %>% 
  group_by(type) %>% 
  ggplot(aes(x = type, y = SUVA, fill = type)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter() +
  theme_classic() +
  theme(axis.text.x=element_text(angle = 45, vjust = 0.5), legend.position = "none", plot.title = element_text(hjust = 0.5)) +
  labs(x = "", y = "SUVA (L/mg-C/m)") +
  ggtitle("Mean DOC quality in eight headwater catchments")

suva + colours

## 4.4 Explore DOC and THM-FP

### Reference

ref_doc_thm_plot <- water_chem_v4 %>% 
  filter(site %in% c("KWR", "W66")) %>% 
  ggplot(aes(x = `DOC [ppm]`, y = `THMsFP [μg/L]`, colour = site)) +
  geom_smooth(aes(group = 1), method = "lm", se = FALSE, colour = "black", size = 1) +
  geom_point() +
  theme_classic() +
  theme(axis.text.x=element_text(angle = 45, vjust = 0.5), legend.title = element_blank(), plot.title = element_text(hjust = 0.5)) + 
  labs(x = "DOC (ppm)", y = "THM-FP (μg/L)") +
  ggtitle("[DOC] - [THM-FP] relationship in two \ncontrol headwater catchments\n")

ref_doc_thm_plot

### Treatment

dist_doc_thm_plot <- water_chem_v3 %>% 
  filter(site %in% c("W87", "W82", "W52", "SBC", "W11", "W17")) %>% 
  ggplot(aes(x = `DOC [ppm]`, y = `THMsFP [μg/L]`, colour = type)) +
  geom_smooth(aes(group = 1), method = "lm", se = FALSE, colour = "black", size = 1) +
  geom_point() +
  geom_jitter() +
  theme_classic() +
  theme(axis.text.x=element_text(angle = 45, vjust = 0.5), legend.title = element_blank(), plot.title = element_text(hjust = 0.5)) + 
  labs(x = "DOC (ppm)", y = "THM-FP (μg/L)") +
  ggtitle("[DOC] - [THM-FP] relationship in \nsix disturbed headwater catchments\n")

dist_doc_thm_plot

## 4.5 Explore DOC, SUVA, THM-FP and HAA-FP

### SUVA - THM-FP reference

ref_suva_thm_plot <- water_chem_v4 %>% 
filter(site %in% c("KWR", "W66")) %>% 
  ggplot(aes(x = SUVA, y = `THMsFP [μg/L]`, colour = site)) +
  geom_smooth(aes(group = 1), method = "lm", se = FALSE, colour = "black", size = 1) +
  geom_point() +
  geom_jitter() +
  theme_classic() +
  theme(axis.text.x=element_text(angle = 45, vjust = 0.5), legend.title = element_blank(), plot.title = element_text(hjust = 0.5)) + 
  labs(x = "SUVA (L/mg-C/m)", y = "THM-FP (μg/L)") +
  ggtitle("DOC quality and [THM-FP] relationship in \ntwo control headwater catchments\n")

ref_suva_thm_plot

### SUVA - THM-FP disturbed

dist_suva_thm_plot <- water_chem_v4 %>% 
  filter(site %in% c("W87", "W82", "W52", "SBC", "W11", "W17")) %>% 
  ggplot(aes(x = SUVA, y = `THMsFP [μg/L]`, colour = type)) +
  geom_point() +
  geom_jitter() +
  theme_classic() +
  theme(axis.text.x=element_text(angle = 45, vjust = 0.5), legend.title = element_blank(), plot.title = element_text(hjust = 0.5)) + 
  labs(x = "SUVA (L/mg-C/m)", y = "THM-FP (μg/L)") +
  ggtitle("DOC quality and [THM-FP] relationship in \nsix disturbed headwater catchments\n")

dist_suva_thm_plot





### 4.6 - Plot group means ----
###################################




## 5. Saving / Exporting
##---------------------------

## 6. Notes / Junk Code
##---------------------------
