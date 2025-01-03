####-------------------------
## Name: Matt Watkins
## Date: Jan. 13th/22
## Project: MSc data analysis (water chemistry)
## Objective: Explore the Waterloo chemistry
## Inputs: UW DBP-FP Fall 2021 report
## Outputs: exploratory plots for water quality
####-------------------------

## Testing PAT key change for github 

uw_suva <- uw_chem %>%
  mutate(suva = (UV254 / doc) * 100)

uw_suva_means <- uw_suva %>%
  group_by(site) %>% 
  mutate(mean.suva = mean(suva))

ws <- read_excel("Watershed_table_v1.xlsx")

ws_area <- ws %>% 
  select(`Site name`, `Catchment ID`, `Drainage Area (km2)`)

colnames(ws_area)[1] <- "site"

uw_suva_area <- left_join(ws_area, uw_suva_means, by = "site")

## 0. Notes
##---------------------------

# Main purpose of this script is to explore the DOC and THM-FP concentrations between sites

### - 0.1 - Nov. 9th update: add type column with catchment types and re-save for future use

uw_chem_dat <- uw_chem_wide %>% 
  mutate(type = case_when(site == "WS 11" ~ "Mixed",
                          site == "WS 87" ~ "Mixed",
                          site == "WS SBC" ~ "Harvest",
                          site == "WS 82" ~ "Harvest",
                          site == "WS 17" ~ "Insect",
                          site == "WS 52" ~ "Insect",
                          site == "WS 66" ~ "Control",
                          site == "WS 96" ~ "Control"))

### - 0.2 - Just bring in cleaned RDS file now

uw_chem <- readRDS("uw_chem_cleaned_v1.03.rds")

### - 0.3 - Update the HAA NAs (samples that could only do THM because chlorine levels were too low)

uw_chem[33, 10] <- NA # remove 2021-09-25 HAA for H1
uw_chem[35, 10] <- NA # remove 2021-09-25 HAA for I1
uw_chem[40, 10] <- NA # remove 2021-09-28 HAA for C12

### Updated save (May 29th/23)

saveRDS(uw_chem, file = "uw_chem_cleaned_v1.04.rds")

## 1. Prepare
##---------------------------

library(tidyverse)
library(readxl)
library(stringr)
library(RColorBrewer)
library(ggpubr)
library(lubridate)

### Set colour scheme and font size

colours <- scale_fill_manual(values = c("#E69F00", "#56B4E9", "#009E73", 
                                        "#F0E442"))

# other colours #0072B2", "#D55E00", "#CC79A7"
duo_colours <- scale_fill_manual(values = c("#E69F00", "#56B4E9"))

theme_update(text = element_text(size=20))

ggpubr::show_point_shapes()

## 2. Import
##---------------------------

# Bring in UW water chemistry data

water_chem <- read_xlsx("/Volumes/MW/2020 Trent University/Data/UW-DBP-FP/DBP-FP_Report-BS-Fall_2021_v1.xlsx")

uw_chem <- readRDS("uw_chem_cleaned_v1.03.RDS")

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
                          site == "KWR" ~ "Control"),
         site.id = case_when(site == "W11" ~ "WS 11",
                            site == "W87" ~ "WS 87",
                            site == "SBC" ~ "WS SBC",
                            site == "W82" ~ "WS 82",
                            site == "W17" ~ "WS 17",
                            site == "W52" ~ "WS 52",
                            site == "W66" ~ "WS 66",
                            site == "KWR" ~ "WS 96"))

water_chem_v4 <- water_chem_v3[,-2] # remove old site column

### Calculate SUVA

water_chem_v5 <- water_chem_v4 %>% 
  mutate(SUVA = (`UV254 [cm-1]` / `DOC [ppm]`) * 100)



### Add in landscape characteristics - slope, wetland %, deciduous, drainage area, disturbance % ----

water_chem_v6 <- water_chem_v5 %>% 
  mutate(area = case_when(site.id == "WS 11" ~ 14.2,
                   site.id == "WS 87" ~ 2.08,
                   site.id == "WS SBC" ~ 1.65,
                   site.id == "WS 82" ~ .043,
                   site.id == "WS 17" ~ 11.64,
                   site.id == "WS 52" ~ 1.23,
                   site.id == "WS 66" ~ 1.50,
                   site.id == "WS 96" ~ 106.77), # drainage area
         slope = case_when(site.id == "WS 11" ~ 10.73,
                           site.id == "WS 87" ~ 7.45,
                           site.id == "WS SBC" ~ 13.11,
                           site.id == "WS 82" ~ 6.29,
                           site.id == "WS 17" ~ 14.26,
                           site.id == "WS 52" ~ 13.05,
                           site.id == "WS 66" ~ 4.90,
                           site.id == "WS 96" ~ 13.27), # slope
         wetland.cover = case_when(site.id == "WS 11" ~ 6.55,
                                   site.id == "WS 87" ~ 3.65,
                                   site.id == "WS SBC" ~ .61,
                                   site.id == "WS 82" ~ 24.45,
                                   site.id == "WS 17" ~ 2.15,
                                   site.id == "WS 52" ~ 9.99,
                                   site.id == "WS 66" ~ 10.67,
                                   site.id == "WS 96" ~ 3.26), # wetland
         deciduous.cover = case_when(site.id == "WS 11" ~ 78.33,
                                     site.id == "WS 87" ~ 20.83,
                                     site.id == "WS SBC" ~ 66.79,
                                     site.id == "WS 82" ~ 4.17,
                                     site.id == "WS 17" ~ 68.80,
                                     site.id == "WS 52" ~ 7.97,
                                     site.id == "WS 66" ~ 4.35,
                                     site.id == "WS 96" ~ 28.52)) # deciduous couver

### Change date column to date format for time series exploration and change sample number to character

water_chem_v6$date <- dmy(water_chem_v6$date)

water_chem_v6$sample.number <- as.character(water_chem_v6$sample.number)

### Change HAAs to numeric

water_chem_v6$`HAAsFP [μg/L]` <- as.numeric(water_chem_v6$`HAAsFP [μg/L]`)


## 3.1 - Calculate group means ----
###################################

### DOC

doc_means <- water_chem_v5 %>% 
  group_by(type) %>% 
  summarise(doc_means = mean(`DOC [ppm]`)) %>% 
  ggplot(aes(x = type, y = doc_means)) +
  geom_point()

doc_means

### THM-FP

thmfp_means <- water_chem_v5 %>% 
  group_by(type) %>% 
  summarise(thmfp_means = mean(`THMsFP [μg/L]`))

### SUVA

suva_means <- water_chem_v5 %>% 
  group_by(type) %>% 
  summarise(suva_means = mean(SUVA))

## 4. Plotting
##---------------------------

## 4.1 Explore doc values 

doc <- water_chem_v5 %>% 
  group_by(type) %>% 
  ggplot(aes(x = type, y = `DOC [ppm]`, fill = type)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter() +
  theme_classic(base_size = 16) +
  theme(axis.text.x=element_text(angle = 45, vjust = 0.5), legend.position = "none", plot.title = element_text(hjust = 0.5)) +
  labs(x = "", y = "DOC (mg/L)")
  
doc + colours

## 4.2 Explore THM-FP

thm_fp <- water_chem_v5 %>% 
  group_by(type) %>% 
  ggplot(aes(x = type, y = `THMsFP [μg/L]`, fill = type)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter() +
  theme_classic(base_size = 16) +
  theme(axis.text.x=element_text(angle = 45, vjust = 0.5), legend.position = "none", plot.title = element_text(hjust = 0.5)) +
  labs(x = "", y = "THM-FP (μg/L)")

thm_fp + colours

## 4.3 Explore SUVA

suva <- water_chem_v5 %>% 
  group_by(type) %>% 
  ggplot(aes(x = type, y = SUVA, fill = type)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter() +
  theme_classic(base_size = 16) +
  theme(axis.text.x=element_text(angle = 45, vjust = 0.5), legend.position = "none", plot.title = element_text(hjust = 0.5)) +
  labs(x = "", y = "SUVA (L/mg-C/m)")

suva + colours

## 4.4 Explore DOC and THM-FP

### Reference

ref_doc_thm_plot <- water_chem_v5 %>% 
  filter(site.id %in% c("WS 96", "WS 66")) %>% 
  ggplot(aes(x = `DOC [ppm]`, y = `THMsFP [μg/L]`, colour = site.id, shape = site.id)) +
  geom_point(size = 3) +
  scale_color_manual(values = c("#E69F00", "#56B4E9")) +
  theme_classic(base_size = 16) +
  theme(axis.text.x=element_text(angle = 45, vjust = 0.5), legend.title = element_blank(), plot.title = element_text(hjust = 0.5)) + 
  labs(x = "DOC (ppm)", y = "THM-FP (μg/L)")

ref_doc_thm_plot

### Disturbed sites
dist_doc_thm_plot <- uw_chem %>% 
  filter(site %in% c("WS 87", "WS 82", "WS 52", "WS SBC", "WS 11", "WS 17")) %>% 
  ggplot(aes(x = doc, y = `thm-fp`, colour = type, shape = type)) +
  geom_point(size = 3) +
  scale_colour_manual(values = c("#0072B2", "#D55E00", "#CC79A7")) +
  geom_jitter() +
  theme_classic(base_size = 16) +
  theme(axis.text.x=element_text(angle = 45, vjust = 0.5), legend.title = element_blank(), plot.title = element_text(hjust = 0.5)) + 
  labs(x = "DOC (mg/L)", y = "THM-FP (μg/L)")

dist_doc_thm_plot

## 4.5 Explore DOC, SUVA, THM-FP and HAA-FP

### SUVA - THM-FP reference

ref_suva_thm_plot <- water_chem_v5 %>% 
filter(site.id %in% c("WS 96", "WS 66")) %>% 
  ggplot(aes(x = SUVA, y = `THMsFP [μg/L]`, colour = site.id, shape = site.id)) +
  geom_point(size = 3) +
  scale_color_manual(values = c("#E69F00", "#56B4E9")) +
  geom_jitter() +
  theme_classic(base_size = 16) +
  theme(axis.text.x=element_text(angle = 45, vjust = 0.5), legend.title = element_blank(), plot.title = element_text(hjust = 0.5)) + 
  labs(x = "SUVA (L/mg-C/m)", y = "THM-FP (μg/L)")

ref_suva_thm_plot

### SUVA - THM-FP disturbed

dist_suva_thm_plot <- water_chem_v5 %>% 
  filter(site.id %in% c("WS 87", "WS 82", "WS 52", "WS SBC", "WS 11", "WS 17")) %>% 
  ggplot(aes(x = SUVA, y = `THMsFP [μg/L]`, colour = type, shape = type)) +
  geom_point(size = 3) +
  scale_colour_manual(values = c("#0072B2", "#D55E00", "#CC79A7")) +
  geom_jitter() +
  theme_classic(base_size = 16) +
  theme(axis.text.x=element_text(angle = 45, vjust = 0.5), legend.title = element_blank(), plot.title = element_text(hjust = 0.5)) + 
  labs(x = "SUVA (L/mg-C/m)", y = "THM-FP (μg/L)")

dist_suva_thm_plot


### DOC - HAA-FP

dist_doc_haa_plot <- uw_chem %>% 
  filter(site %in% c("WS 87", "WS 82", "WS 52", "WS SBC", "WS 11", "WS 17")) %>% 
  ggplot(aes(x = doc, y = `haa-fp`, colour = type, shape = type)) +
  geom_point(size = 3) +
  scale_colour_manual(values = c("#0072B2", "#D55E00", "#CC79A7")) +
  geom_jitter() +
  theme_classic(base_size = 16) +
  theme(axis.text.x=element_text(angle = 45, vjust = 0.5), legend.title = element_blank(), plot.title = element_text(hjust = 0.5)) + 
  labs(x = "DOC (mg/L)", y = "HAA-FP (μg/L)")

dist_doc_haa_plot


### 4.6 - Plot landscape characteristics ----

### 4.6.1 - DOC landscape variables ----

slope_plot <- water_chem_v6 %>% 
  ggplot(aes(slope, `DOC [ppm]`, colour = type, shape = type)) +
  geom_point(size = 3) +
  scale_colour_manual(values = c("#E69F00", "#56B4E9", "#009E73", 
                                 "#000000")) +
  scale_shape_manual(values = c(18, 16, 17, 5)) +
  theme_classic(base_size = 16) +
  theme(axis.text.x=element_text(angle = 45, vjust = 0.5), legend.title = element_blank(), plot.title = element_text(hjust = 0.5)) + 
  labs(x = "Slope (%)", y = "")

slope_plot ###

drainage_area_plot <- water_chem_v6 %>% 
  ggplot(aes(area, `DOC [ppm]`, colour = type, shape = type)) +
  geom_point(size = 3) +
  scale_colour_manual(values = c("#E69F00", "#56B4E9", "#009E73", 
                                 "#000000")) +
  scale_shape_manual(values = c(18, 16, 17, 5)) +
  theme_classic(base_size = 16) +
  theme(axis.text.x=element_text(angle = 45, vjust = 0.5), legend.title = element_blank(), plot.title = element_text(hjust = 0.5)) + 
  labs(x = expression(paste("Catchment Area ", (km^2))), y = "")

drainage_area_plot ###

wetland_plot <- water_chem_v6 %>% 
  ggplot(aes(wetland.cover, `DOC [ppm]`, colour = type, shape = type)) +
  geom_point(size = 3) +
  scale_colour_manual(values = c("#E69F00", "#56B4E9", "#009E73", 
                                 "#000000")) +
  scale_shape_manual(values = c(18, 16, 17, 5)) +
  theme_classic(base_size = 16) +
  theme(axis.text.x=element_text(angle = 45, vjust = 0.5), legend.title = element_blank(), plot.title = element_text(hjust = 0.5)) + 
  labs(x = "Wetland Cover (%) ", y = "")

wetland_plot ###

deciduous_cover_plot <- water_chem_v6 %>% 
  ggplot(aes(deciduous.cover, `DOC [ppm]`, colour = type, shape = type)) +
  geom_point(size = 3) +
  scale_colour_manual(values = c("#E69F00", "#56B4E9", "#009E73", 
                                 "#000000")) +
  scale_shape_manual(values = c(18, 16, 17, 5)) +
  theme_classic(base_size = 16) +
  theme(axis.text.x=element_text(angle = 45, vjust = 0.5), legend.title = element_blank(), plot.title = element_text(hjust = 0.5)) + 
  labs(x = "Deciduous Cover (%) ", y = "")

deciduous_cover_plot

doc_landscape <- ggarrange(slope_plot, drainage_area_plot, wetland_plot, deciduous_cover_plot, 
          ncol = 2, nrow = 2,
          common.legend = TRUE)

annotate_figure(doc_landscape,
                left = text_grob("DOC (ppm)", color = "black", rot = 90, size = 16))

### 4.6.2 - SUVA landscape variables ----

decid_plot <- function(df){ 
  ggplot(data = df, mapping = aes(x = deciduous.cover, y = SUVA, colour = type, shape = type)) +
  geom_point(size = 3) +
  scale_colour_manual(values = c("#E69F00", "#56B4E9", "#009E73", 
                                 "#000000")) +
  scale_shape_manual(values = c(18, 16, 17, 5)) +
  theme_classic(base_size = 16) +
  theme(axis.text.x=element_text(angle = 45, vjust = 0.5), legend.title = element_blank(), plot.title = element_text(hjust = 0.5)) + 
  labs(x = "Deciduous Cover (%)", y = "")
}

suva_decid <- decid_plot(water_chem_v6)

suva_wetland <- wetland_plot(water_chem_v6)

suva_area <- area_plot(water_chem_v6)

suva_slope <- slope_plot(water_chem_v6)

suva_landscape <- ggarrange(suva_slope, suva_area, suva_wetland, suva_decid, 
                           ncol = 2, nrow = 2,
                           common.legend = TRUE)

annotate_figure(suva_landscape,
                left = text_grob("SUVA (L/mg-C/m)", color = "black", rot = 90, size = 16))

### 4.6.3 - DBP-FP catchment characteristics ----

slope_plot <- function(df){ 
  ggplot(data = df, mapping = aes(x = slope, y = `THMsFP [μg/L]`, colour = type, shape = type)) +
    geom_point(size = 3) +
    scale_colour_manual(values = c("#E69F00", "#56B4E9", "#009E73", 
                                   "#000000")) +
    scale_shape_manual(values = c(18, 16, 17, 5)) +
    theme_classic(base_size = 16) +
    theme(axis.text.x=element_text(angle = 45, vjust = 0.5), legend.title = element_blank(), plot.title = element_text(hjust = 0.5)) + 
    labs(x = "Slope (%)", y = "")
}

thm_decid <- decid_plot(water_chem_v6)

thm_wetland <- wetland_plot(water_chem_v6)

thm_area <- area_plot(water_chem_v6)

thm_slope <- slope_plot(water_chem_v6)

thm_landscape <- ggarrange(thm_slope, thm_area, thm_wetland, thm_decid, 
                            ncol = 2, nrow = 2,
                            common.legend = TRUE)

annotate_figure(thm_landscape,
                left = text_grob("THM-FP (μg/L)", color = "black", rot = 90, size = 16))

### 4.7 - Time series plot of response variables

### 4.7.1 - DOC

doc_season <- water_chem_v6 %>% 
  ggplot(aes(sample.number, `DOC [ppm]`, colour = type, shape = type)) +
  geom_point(size = 3) +
  scale_colour_manual(values = c("#E69F00", "#56B4E9", "#009E73", 
                                 "#000000")) +
  scale_shape_manual(values = c(18, 16, 17, 5)) +
  theme_classic(base_size = 16) +
  theme(axis.text.x=element_text(angle = 45, vjust = 0.5), legend.title = element_blank(), plot.title = element_text(hjust = 0.5)) + 
  labs(x = "", y = "DOC (ppm)")

### 4.7.2 - SUVA

suva_season <- water_chem_v6 %>% 
  ggplot(aes(sample.number, SUVA, fill = type)) +
  geom_boxplot(outlier.shape = NA) +
  scale_fill_manual(values = c("#E69F00", "#56B4E9", "#009E73", 
                               "#F0E442")) +
  scale_shape_manual(values = c(18, 16, 17, 5)) +
  theme_classic(base_size = 16) +
  theme(axis.text.x=element_text(angle = 45, vjust = 0.5), legend.position = "none", plot.title = element_text(hjust = 0.5)) + 
  labs(x = "Sample number", y = "SUVA (L/mg-C/m)") +
  facet_wrap(~ type)

suva_season

### 4.7.3 - THM-FP

uw_chem %>% 
  ggplot(aes(date, `THMsFP [μg/L]`, fill = type)) +
  geom_boxplot(outlier.shape = NA) +
  scale_fill_manual(values = c("#E69F00", "#56B4E9", "#009E73", 
                               "#F0E442")) +
  scale_shape_manual(values = c(18, 16, 17, 5)) +
  scale_y_log10() +
  theme_classic(base_size = 16) +
  theme(axis.text.x=element_text(angle = 45, vjust = 0.5), legend.position = "none", plot.title = element_text(hjust = 0.5)) + 
  labs(x = "Sample number", y = "THM-FP (μg/L)") +
  facet_wrap(~ type)

p <- ggplot() +
  geom_point(data = uw_chem_dat, aes(x = date, y = `thm-fp`, colour = type)) + 
  scale_colour_manual(values = c("Control" = "#E69F00", "Harvest" = "#56B4E9", "Insect" = "#009E73", "Mixed" = "#F0E442")) +
  facet_wrap(~ catchment.id) +
  theme_classic(base_size = 16) +
  theme(axis.text.x=element_text(angle = 45, vjust = 0.5), legend.position = "none", plot.title = element_text(hjust = 0.5)) + labs(x = "", y = "THM-FP (μg/L)")

p

#####

thm_season

haa_season <- water_chem_v6 %>% 
  ggplot(aes(sample.number, `HAAsFP [μg/L]`, fill = type)) +
  geom_boxplot(outlier.shape = NA) +
  scale_fill_manual(values = c("#E69F00", "#56B4E9", "#009E73", 
                               "#F0E442")) +
  scale_shape_manual(values = c(18, 16, 17, 5)) +
  scale_y_log10() +
  theme_classic(base_size = 16) +
  theme(axis.text.x=element_text(angle = 45, vjust = 0.5), legend.title = element_blank(), plot.title = element_text(hjust = 0.5)) + 
  labs(x = "", y = "HAA-FP (μg/L)")

haa_season

response_v_season <- ggarrange(doc_season, suva_season, thm_season, haa_season,
                           ncol = 2, nrow = 2,
                           common.legend = TRUE)

annotate_figure(response_v_season,
                bottom = text_grob("Sample Number", color = "black", size = 16)) # adding common x-axis label

response_v_season

## 5. Saving / Exporting
##---------------------------

### 5.01 - Resave the updated file (Nov. 9th)

uw_chem_dat %>% 
  saveRDS(file = "uw_chem_cleaned_v1.03.rds")

## 6. Notes / Junk Code
##---------------------------

uw_suva_area %>% 
  ggplot(aes(x = `Drainage Area (km2)`, y = mean.suva)) +
  geom_point() +
  scale_x_log10() +
  geom_smooth(method = "lm", se = FALSE) # Potentially negative relationship?

## DOC - HAA all site

doc_haa_plot <- uw_chem %>% 
  ggplot(aes(x = doc, y = `haa-fp`, colour = type)) +
  geom_point(size = 3) +
  scale_colour_manual(values = c("#E69F00", "#56B4E9", "#009E73", 
                                          "#F0E442")) +
  geom_jitter() +
  theme_bw(base_size = 16) +
  theme(axis.text.x=element_text(angle = 45, vjust = 0.5), legend.title = element_blank(), plot.title = element_text(hjust = 0.5)) + 
  labs(x = "DOC (mg/L)", y = "HAA-FP (μg/L)")

doc_haa_plot

doc_thm_plot <- uw_chem %>% 
  ggplot(aes(x = doc, y = `thm-fp`, colour = type)) +
  geom_point(size = 3) +
  scale_colour_manual(values = c("#E69F00", "#56B4E9", "#009E73", 
                                          "#F0E442")) +
                                            geom_jitter() +
  theme_bw(base_size = 16) +
  theme(axis.text.x=element_text(angle = 45, vjust = 0.5), legend.title = element_blank(), plot.title = element_text(hjust = 0.5)) + 
  labs(x = "DOC (mg/L)", y = "THM-FP (μg/L)")

doc_thm_plot

suva_thm_plot <- uw_suva %>% 
  ggplot(aes(x = suva, y = `thm-fp`, colour = type)) +
  geom_point(size = 3) +
  scale_colour_manual(values = c("#E69F00", "#56B4E9", "#009E73", 
                                          "#F0E442")) +
                                            geom_jitter() +
  theme_bw(base_size = 16) +
  theme(axis.text.x=element_text(angle = 45, vjust = 0.5), legend.title = element_blank(), plot.title = element_text(hjust = 0.5)) + 
  labs(x = "SUVA (L/mg-C/m)", y = "THM-FP (μg/L)")

suva_thm_plot

suva_haa_plot <- uw_suva %>% 
  ggplot(aes(x = suva, y = `haa-fp`, colour = type)) +
  geom_point(size = 3) +
  scale_colour_manual(values = c("#E69F00", "#56B4E9", "#009E73", 
                                          "#F0E442")) +
                                            geom_jitter() +
  theme_bw(base_size = 16) +
  theme(axis.text.x=element_text(angle = 45, vjust = 0.5), legend.title = element_blank(), plot.title = element_text(hjust = 0.5)) + 
  labs(x = "SUVA (L/mg-C/m)", y = "HAA-FP (μg/L)")

suva_haa_plot

### Arrang them all

suva_dbp <- ggarrange(suva_thm_plot + theme(axis.title.x = element_blank()),
                      suva_haa_plot + theme(axis.title.x = element_blank()),
                               ncol = 2,
                               common.legend = TRUE)

suva_dbps_plot <- annotate_figure(suva_dbp,
                bottom = text_grob("SUVA (L/mg-C/m)", color = "black", size = 16)) # adding common x-axis label
suva_dbps_plot

doc_dbp <- ggarrange(doc_thm_plot + theme(axis.title.x = element_blank()),
                     doc_haa_plot + theme(axis.title.x = element_blank()),
                             ncol = 2,
                             common.legend = TRUE)

doc_dbps_plot <- annotate_figure(doc_dbp,
                                  bottom = text_grob("DOC (mg/L)", color = "black", size = 16)) # adding common x-axis label
doc_dbps_plot
