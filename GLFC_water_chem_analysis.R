#########################
## Name: Matt Watkins
## Date: May, 2022
## Project: MSC Data Analysis
## Objective: Explore the GLFC water chemistry data
## Inputs: GLFC water chem spreadsheet
## Outputs: many plots - initially DOC-focused
#########################

## 0. NOTES ----

### 0.1 - Need to tidy up the spreadsheet a little, so to start: (1) convert the sample date to an actual date; (2) split the sample column into watershed ID and sample measurement #

## 1. PREPARE ----

rm(list=ls())
options(stringsASfactors = FALSE, scipen = 999, encoding = "UTF-8")

library(tidyverse)
library(readxl)
library(lubridate)
library(ggpubr)

### 1.01 ---- Import updated dataframe ----

glfc_data <- readRDS("glfc_chem_cleaned_v1.01.RDS")

### 1.02 - Set graphical parameters similar to UW ----

colours <- scale_fill_manual(values = c("#E69F00", "#56B4E9", "#009E73", 
                                        "#F0E442")) # Set colour scheme and font size; # other colours #0072B2", "#D55E00", "#CC79A7"

duo_colours <- scale_fill_manual(values = c("#E69F00", "#56B4E9"))

theme_update(text = element_text(size=20))

ggpubr::show_point_shapes()

## 2. IMPORT ----

### 2.1 Bring in water chem spreadsheet

setwd("/Volumes/MW/2020 Trent University/Data/GLFC Water Chemistry") # for now do this to access spreadsheet

glfc_chem <- read_xlsx("GLFC_water_chem.xlsx")

## 3. TIDY // PROCESS ----

### 3.01 ---- Convert sample date to date format ----

glfc_chem$`sample.date` <- mdy(glfc_chem$`sample.date`)

### 3.02 Tidy up some small column stuff

ws_split <- str_split_fixed(glfc_chem$`sample.id`, "-S-", 2) # split into two columns and drop the S

colnames(ws_split) <- c("catchment", "sample.number") # rename columns

glfc_chem_out <- cbind(ws_split, glfc_chem) # merge back

glfc_chem_dat <- glfc_chem_out %>% 
  select(-`sample.id`) # remove old column

glfc_chem_dat$sample.number <- str_remove_all(glfc_chem_dat$sample.number, "0")

### 3.03 ---- Calculate SUVA based on Erik's correction: (SUVA = (a254/2.302585)/DOC) ----

glfc_data_wide <- glfc_datOut %>% 
  pivot_wider(names_from = variable, values_from = value)

glfc_suva <- glfc_data_wide %>% 
  mutate(suva = (a254 / 2.302585) / organic.carbon)

glfc_long <- glfc_suva %>% 
  pivot_longer(cols = 7:51, names_to = "variable", values_to = "value")



### 3.04 ---- Integrate landscape variables and disturbance percentages ----

glfc_datOut <- glfc_data %>% 
  mutate(type = case_when(site == "WS 11" ~ "Mixed",
                          site == "WS SSR" ~ "Mixed",
                          site == "WS 47" ~ "Mixed",
                          site == "WS 40" ~ "Mixed",
                          site == "WS 43" ~ "Mixed",
                          site == "WS 87" ~ "Mixed",
                          site == "WS SBC" ~ "Harvest",
                          site == "WS BL1" ~ "Harvest",
                          site == "WS BL2" ~ "Harvest",
                          site == "WS 82" ~ "Harvest",
                          site == "WS 17" ~ "Insect",
                          site == "WS 54" ~ "Insect",
                          site == "WS 52" ~ "Insect",
                          site == "WS 46" ~ "Insect",
                          site == "WS 45" ~ "Insect",
                          site == "WS 44" ~ "Insect",
                          site == "WS 108" ~ "Control",
                          site == "WS 104" ~ "Control",
                          site == "WS 96" ~ "Control",
                          site == "WS 93" ~ "Control",
                          site == "WS 92" ~ "Control",
                          site == "WS 84" ~ "Control",
                          site == "WS 110" ~ "Control",
                          site == "WS 77" ~ "Control",
                          site == "WS 76" ~ "Control",
                          site == "WS 75" ~ "Control",
                          site == "WS 73" ~ "Control",
                          site == "WS 67" ~ "Control",
                          site == "WS 66" ~ "Control",
                          site == "WS 36" ~ "Control")) # assign classes for easy plotting like UW data

glfc_dist_perc <- glfc_ws_class %>% 
  mutate(fire.disturbance = case_when(catchment == "WS 87" ~ 25.3,
                                      catchment == "WS SSR" ~ 23.0),
         harvest.disturbance = case_when(catchment == "WS 11" ~ 25.7,
                                         catchment == "WS SBC" ~ 23.8,
                                         catchment == "WS SSR" ~ 18.4,
                                         catchment == "WS 40" ~ 11.8,
                                         catchment == "WS 47" ~ 16.1,
                                         catchment == "WS BL2" ~ 21.4,
                                         catchment == "WS BL1" ~ 38.3,
                                         catchment == "WS 73" ~ 1.0,
                                         catchment == "WS 76" ~ 1.88,
                                         catchment == "WS 82" ~ 52.5,
                                         catchment == "WS 87" ~ 32.3,
                                         catchment == "WS 96" ~ .8,
                                         catchment == "WS 110" ~ .02),
         insect.disturbance = case_when(catchment == "WS 11" ~ 44.5,
                                        catchment == "WS 17" ~ 46.0,
                                        catchment == "WS SSR" ~ 2.9,
                                        catchment == "WS 36" ~ 2.02,
                                        catchment == "WS 40" ~ 32.4,
                                        catchment == "WS 43" ~ 16.7,
                                        catchment == "WS 44" ~ 57.1,
                                        catchment == "WS 45" ~82.1,
                                        catchment == "WS 46" ~ 100.0,
                                        catchment == "WS 47" ~ 98.5,
                                        catchment == "WS 52" ~ 25.0,
                                        catchment == "WS 54" ~ 31.0,
                                        catchment == "WS 73" ~ .5,
                                        catchment == "WS 96" ~ .6),
         abiotic.disturbance = case_when(catchment == "WS 40" ~ .8,
                                         catchment == "WS 43" ~ 23.0,
                                         catchment == "WS 47" ~ 4.3))
         
### 3.05 - For now, subset to completed DOC samples only ----

glfc_doc <- glfc_ws_class %>% 
  select(catchment, sample.number, `Sample Date`, `Catchment ID`, type, `Organic Carbon`, `Drainage Area`) %>% 
  na.omit()

### 3.06 - Second: disturbance DOC subset

glfc_disturbance <- glfc_dist_perc %>% 
  select(catchment, sample.number, `Sample Date`, `Catchment ID`, type, `Organic Carbon`, fire.disturbance, harvest.disturbance, insect.disturbance, abiotic.disturbance)

### 3.07 - Third: landscape DOC subset

glfc_landscape <- glfc_dist_perc %>% 
  select(catchment, sample.number, `Sample Date`, `Catchment ID`, type, `Organic Carbon`, `Drainage Area`, Slope, `Wetland Coverage`, `Deciduous Coverage`)

### 3.08 - Fourth: Runoff DOC subset (test)

glfc_runoff <- glfc_dist_perc %>% 
  select(catchment, sample.number, `Sample Date`, `Catchment ID`, type, `Organic Carbon`, `Daily Runoff (mm)`, `Daily Discharge (Q)`) %>% 
  filter(catchment %in% c("WS 40", "WS 96", "WS 108", "WS 87")) %>% 
  na.omit()

### 3.09 - Remove the 0 from the sample.number column

glfc_data$sample.number <- str_remove_all(glfc_data$sample.number, "0")




## 4. PLOTTING ----

### 4.01 - Basic DOC and SUVA plots ----

doc <- glfc_data %>% 
  ggplot(aes(x = type, y = `Organic Carbon`, fill = type)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter() +
  theme_classic(base_size = 16) +
  scale_y_log10() +
  theme(axis.text.x=element_text(angle = 45, vjust = 0.5), legend.position = "none", plot.title = element_text(hjust = 0.5)) +
  labs(x = "", y = "DOC (ppm)")

doc + colours # mean doc across catchment types

doc_season <- glfc_data %>% 
  ggplot(aes(sample.number, `Organic Carbon`, fill = type)) +
  geom_boxplot(outlier.shape = NA) +
  scale_fill_manual(values = c("#E69F00", "#56B4E9", "#009E73", 
                                 "#F0E442")) +
  scale_shape_manual(values = c(18, 16, 17, 5)) +
  scale_y_log10() +
  theme_classic(base_size = 16) +
  theme(axis.text.x=element_text(angle = 45, vjust = 0.5), legend.title = element_blank(), plot.title = element_text(hjust = 0.5)) + 
  labs(x = "", y = "DOC (ppm)")

doc_season # how does doc vary across the study period

suva <- glfc_data %>% 
  ggplot(aes(x = type, y = suva, fill = type)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter() +
  theme_classic(base_size = 16) +
  theme(axis.text.x=element_text(angle = 45, vjust = 0.5), legend.position = "none", plot.title = element_text(hjust = 0.5)) +
  labs(x = "", y = "SUVA (L/mg-C/m)")

suva + colours # mean SUVA across catchment classifications

doc_suva_lin <- glfc_data %>% 
  ggplot(aes(x = `Organic Carbon`, y = suva, colour = type)) +
  geom_point() +
  geom_jitter() +
  theme_classic(base_size = 16) +
  theme(axis.text.x=element_text(angle = 45, vjust = 0.5), plot.title = element_text(hjust = 0.5)) +
  labs(x = "DOC (ppm)", y = "SUVA (L/mg-C/m)")

doc_suva_lin + scale_colour_manual(values = c("#E69F00", "#56B4E9", "#009E73", 
                                   "#000000"))

### 4.01.1 Temporal SUVA across all catchments


p <- ggplot() +
  geom_point(data = glfc_suva, aes(x = date, y = suva, colour = type)) + 
  scale_colour_manual(values = c("Control" = "#E69F00", "Harvest" = "#56B4E9", "Insect" = "#009E73", "Mixed" = "#F0E442")) +
  facet_wrap(~ catchment.id) +
  theme_classic(base_size = 16) +
  theme(axis.text.x=element_text(angle = 45, vjust = 0.5), legend.position = "none", plot.title = element_text(hjust = 0.5)) + labs(x = "", y = "SUVA (L/mg-C/m)")

p

### 4.02 - DOC and Landscape characteristic plots

drainage_doc <- glfc_data %>% 
  ggplot(aes(`Drainage Area`, `Organic Carbon`, colour = type, shape = type)) +
  geom_point(size = 3) +
  scale_colour_manual(values = c("#E69F00", "#56B4E9", "#009E73", 
                                 "#000000")) +
  scale_shape_manual(values = c(18, 16, 17, 5)) +
  theme_classic(base_size = 16) +
  theme(axis.text.x=element_text(angle = 45, vjust = 0.5), legend.title = element_blank(), plot.title = element_text(hjust = 0.5)) + 
  labs(x = expression(paste("Catchment Area ", (km^2))), y = "")

drainage_doc # how does DOC concentration vary with respect to catchment area

slope_doc <- glfc_dist_perc %>% 
  ggplot(aes(Slope, `Organic Carbon`, colour = type, shape = type)) +
  geom_point(size = 3) +
  scale_colour_manual(values = c("#E69F00", "#56B4E9", "#009E73", 
                                 "#000000")) +
  scale_shape_manual(values = c(18, 16, 17, 5)) +
  theme_classic(base_size = 16) +
  theme(axis.text.x=element_text(angle = 45, vjust = 0.5), legend.title = element_blank(), plot.title = element_text(hjust = 0.5)) + 
  labs(x = "Slope %", y = "")

slope_doc # how DOC varies w.r.t. slope

wetland_doc <- glfc_data %>% 
  ggplot(aes(`Wetland Coverage`, `Organic Carbon`, group = type)) +
  geom_boxplot(outlier.shape = NA) +
  scale_fill_manual(values = c("#E69F00", "#56B4E9", "#009E73", 
                               "#F0E442", "#000000")) +
  theme_classic(base_size = 16) +
  theme(axis.text.x=element_text(angle = 45, vjust = 0.5), legend.title = element_blank(), plot.title = element_text(hjust = 0.5)) + 
  labs(x = "Wetland Coverage %", y = "")

wetland_doc # how DOC varies w.r.t. wetland cover

decid_doc <- glfc_dist_perc %>% 
  ggplot(aes(`Deciduous Coverage`, `Organic Carbon`, colour = type, shape = type)) +
  geom_point(size = 3) +
  scale_colour_manual(values = c("#E69F00", "#56B4E9", "#009E73", 
                                 "#000000")) +
  scale_shape_manual(values = c(18, 16, 17, 5)) +
  theme_classic(base_size = 16) +
  theme(axis.text.x=element_text(angle = 45, vjust = 0.5), legend.title = element_blank(), plot.title = element_text(hjust = 0.5)) + 
  labs(x = "Deciduous Coverage %", y = "")

decid_doc # how DOC varies w.r.t. deciduous coverage

landscape_plots <- ggarrange(drainage_doc, slope_doc, wetland_doc, decid_doc,
                               ncol = 2, nrow = 2,
                               common.legend = TRUE) # combining all plots with arrange

annotate_figure(landscape_plots,
                left = text_grob("DOC (ppm)", color = "black", rot = 90, size = 16)) # adding one left DOC axis

### 4.03 - DOC and disturbance plots

fire_doc <- glfc_disturbance %>% 
  ggplot(aes(fire.disturbance, `Organic Carbon`, colour = type, shape = type)) +
  geom_point(size = 3) +
  scale_colour_manual(values = c("#E69F00", "#56B4E9", "#009E73", 
                                 "#000000")) +
  scale_shape_manual(values = c(18, 16, 17, 5)) +
  theme_classic(base_size = 16) +
  theme(axis.text.x=element_text(angle = 45, vjust = 0.5), legend.title = element_blank(), plot.title = element_text(hjust = 0.5)) + 
  labs(x = "Wildfire extent (%)", y = "")

fire_doc # how DOC varies with wildfire extent - too small of a sample size..

insect_doc <- glfc_disturbance %>% 
  ggplot(aes(insect.disturbance, `Organic Carbon`, colour = type, shape = type)) +
  geom_point(size = 3) +
  scale_colour_manual(values = c("#E69F00", "#56B4E9", "#009E73", 
                                 "#000000")) +
  scale_shape_manual(values = c(18, 16, 17, 5)) +
  theme_classic(base_size = 16) +
  theme(axis.text.x=element_text(angle = 45, vjust = 0.5), legend.title = element_blank(), plot.title = element_text(hjust = 0.5)) + 
  labs(x = "Infestation extent (%)", y = "")

insect_doc # how DOC varies with infestation extent - seemingly increases

harvest_doc <- glfc_disturbance %>% 
  ggplot(aes(harvest.disturbance, `Organic Carbon`, colour = type, shape = type)) +
  geom_point(size = 3) +
  scale_colour_manual(values = c("#E69F00", "#56B4E9", "#009E73", 
                                 "#000000")) +
  scale_shape_manual(values = c(18, 16, 17, 5)) +
  theme_classic(base_size = 16) +
  theme(axis.text.x=element_text(angle = 45, vjust = 0.5), legend.title = element_blank(), plot.title = element_text(hjust = 0.5)) + 
  labs(x = "Harvest extent (%)", y = "")

harvest_doc # how DOC varies with harvest extent

abiotic_doc <- glfc_disturbance %>% 
  ggplot(aes(abiotic.disturbance, `Organic Carbon`, colour = type, shape = type)) +
  geom_point(size = 3) +
  scale_colour_manual(values = c("#E69F00", "#56B4E9", "#009E73", 
                                 "#000000")) +
  scale_shape_manual(values = c(18, 16, 17, 5)) +
  theme_classic(base_size = 16) +
  theme(axis.text.x=element_text(angle = 45, vjust = 0.5), legend.title = element_blank(), plot.title = element_text(hjust = 0.5)) + 
  labs(x = "Abiotic event extent (%)", y = "")

abiotic_doc # how DOC varies with abiotic (blowdown and ice storms) event extent - slight decrease, but very small site selection

disturbance_plots <- ggarrange(fire_doc, harvest_doc, insect_doc, abiotic_doc,
                               ncol = 2, nrow = 2,
                               common.legend = TRUE) # combining all plots with ggarrange

annotate_figure(disturbance_plots,
                left = text_grob("DOC (ppm)", color = "black", rot = 90, size = 16)) # adding one left DOC axis

### 4.04 - DOC and runoff/discharge plots

#### Test for WS 40 and WS 96

glfc_runoff %>% 
  ggplot(aes(`Daily Runoff (mm)`, `Organic Carbon`, colour = `Catchment ID`)) +
  geom_point(size = 3) +
  scale_colour_manual(values = c("#E69F00", "#56B4E9", "#009E73", 
                                 "#000000")) +
  scale_shape_manual(values = c(18, 16, 17, 5)) +
  theme_classic(base_size = 16) +
  theme(axis.text.x=element_text(angle = 45, vjust = 0.5), legend.title = element_blank(), plot.title = element_text(hjust = 0.5)) + 
  labs(x = "Daily Runoff (mm)", y = "DOC (ppm)") # how DOC relates to runoff in the first 4 catchments


glfc_runoff %>% 
  ggplot(aes(`Daily Runoff (mm)`, `Organic Carbon`)) +
  geom_point(size = 3) +
  theme_classic(base_size = 16) +
  theme(axis.text.x=element_text(angle = 45, vjust = 0.5), legend.title = element_blank(), plot.title = element_text(hjust = 0.5)) + 
  labs(x = "Daily Runoff (mm)", y = "DOC (ppm)") +
  facet_wrap(~ `Catchment ID`, scales = "free") # facetted version of the plot above

#### Q Estimate test related to DOC

glfcQ_DOC <- glfc_data %>% 
  select(`Daily Discharge (Q)`, `Catchment ID`, `Organic Carbon`) %>% 
  filter(`Daily Discharge (Q)` > 0) %>% 
  ggplot(aes(`Daily Discharge (Q)`, `Organic Carbon`)) +
  geom_point(size = 3) +
  theme_bw(base_size = 16) +
  theme(axis.text.x=element_text(angle = 45, vjust = 0.5), legend.title = element_blank(), plot.title = element_text(hjust = 0.5)) + 
  labs(x = expression(paste("Discharge ", (m^3/s))), y = "DOC (ppm)") +
  facet_wrap(~ `Catchment ID`, scales = "free_x")

glfcQ_DOC + scale_y_log10()

## 5. SAVING // EXPORTING ----

### 5.01 ---- Updated save of the master dataset at this point

setwd("/Volumes/MW/2020 Trent University/R/Thesis Data/MSc_data_analysis") # save it here!!!!!!

glfc_chem_dat %>% 
  saveRDS(file = "glfc_data_v2.rds") ## Updated save for Oct samples (Oct. 7th)

### 5.03 ---- Nov. 9th Updated save of the water chemistry file (added type and SUVA so I don't have to rerun line 75)

glfc_long %>% 
  saveRDS(file = "glfc_data_v4.rds")

## 6. TRIAL // JUNK CODE ----

ws43_check <- glfc_data %>% 
  filter(catchment %in% "WS 43")


### 6.01 - Try facetting the seasonality plots differently

#glfc_data$`Sample Date` <- as.character(glfc_data$`Sample Date`)

glfc_doc <- glfc_data %>% 
  select(catchment, sample.number, `Sample Date`, `Catchment ID`, type, `Organic Carbon`)

doc_season <- glfc_data %>% 
  filter(sample.number < "6") %>% 
  ggplot(aes(sample.number, `Organic Carbon`, fill = type)) +
  geom_boxplot(outlier.shape = NA) +
  scale_fill_manual(values = c("#E69F00", "#56B4E9", "#009E73", 
                               "#F0E442")) +
  scale_shape_manual(values = c(18, 16, 17, 5)) +
  scale_y_log10() +
  theme_classic(base_size = 16) +
  theme(axis.text.x=element_text(angle = 45, vjust = 0.5), legend.position="none", plot.title = element_text(hjust = 0.5)) + 
  labs(x = "Sample number", y = "DOC (ppm)") +
  facet_wrap(~ type)

doc_season

#######

organC_sub <- glfc_datOut %>% 
  filter(variable %in% "organic.carbon") %>% 
  pivot_wider(names_from = variable, values_from = value)



p <- ggplot() +
  geom_point(data = organC_sub, aes(x = date, y = organic.carbon, colour = type)) + 
  scale_colour_manual(values = c("Control" = "#E69F00", "Harvest" = "#56B4E9", "Insect" = "#009E73", "Mixed" = "#F0E442")) +
  scale_y_log10() +
  facet_wrap(~ catchment.id) +
  theme_classic(base_size = 16) +
  theme(axis.text.x=element_text(angle = 45, vjust = 0.5), legend.position = "none", plot.title = element_text(hjust = 0.5)) + labs(x = "", y = "DOC (mg/L)")

p