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

### 0.2 - Tues. Dec. 6th update: rechecked the math, instantaneous flux is calculated correctly. In addition, I checked the daily discharge values with the values from the xx_dailyQ.RDS files - these are correct too.


## 1. PREPARE ----

rm(list=ls())
options(stringsASfactors = FALSE, scipen = 999, encoding = "UTF-8")

library(tidyverse)
library(readxl)
library(scales)
library(lubridate)
library(devtools)
library(ggpmisc)

theme_update(text = element_text(size=20)) # Make graph text slightly bigger for readability

colours <- scale_fill_manual(values = c("#E69F00", "#56B4E9", "#009E73", 
                                        "#F0E442")) # Set colour scheme and font size; 
                                                    # other colours #0072B2", "#D55E00", "#CC79A7"

## 2. IMPORT ----

glfc_data <- readRDS("glfc_data_v3.rds")

watersheds <- read_xlsx("Watershed_table_v1.xlsx")

## 3. TIDY // PROCESS ----

ws_prep <- watersheds %>% 
  select(`Site name`, `Drainage Area (km2)`) # select only columns needed for adding group

colnames(ws_prep)[1] <- "site" # change column name to match

ws_merge <- left_join(ws_prep, glfc_data, by = "site") # left join by the catchment column so groups are assigned

### 3.01 - Select necessary columns and multiple discharge by 1000 to get L/s

glfc_orgC <- ws_merge %>% 
  pivot_wider(names_from = "variable", values_from = "value") %>% 
  select(site, catchment.id, sample, date, daily.discharge, organic.carbon, `Drainage Area (km2)`) %>% 
  na.omit() %>% 
  mutate(discharge.litres = daily.discharge * 1000)

### 3.02 - Multiply concentration by daily discharge and divide by catchment area to obtain mass flux per unit time per unit area (mg/s/km2)

inst_mass_flux <- glfc_orgC %>% 
  mutate(mass.time = daily.discharge * organic.carbon) %>% 
  mutate(mass.time.area = mass.time / `Drainage Area (km2)`)

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
  
##################### Temporal instantaneous flux plot per site ----

q_estimates <- readRDS("Q_estimates_v2.rds")

inst_flux_steps %>% 
  ggplot(aes(x = date, y = mg.s.area)) +
  geom_point() +
  facet_wrap(~ catchment.id) +
  scale_y_log10() +
  theme_bw(base_size = 16) +
  xlab("") +
  ylab(expression(paste("Instantaneous mass flux ", (mg/s/km^2))))

############## instantaneous flux paired catchments (disturbed / control) examination ----

c9_and_m6 <- inst_flux_steps %>% 
  select(-`Drainage Area (km2)`, -site, -totalQ, -discharge.litres, -inst.flux, -log.mg.s.area, -organic.carbon) %>% 
  filter(catchment.id %in% "C9" | catchment.id %in% "M6") %>% 
  pivot_wider(names_from = "catchment.id", values_from = "mg.s.area")

c8_and_m6 <- inst_flux_steps %>% 
  select(-`Drainage Area (km2)`, -site, -totalQ, -discharge.litres, -inst.flux, -log.mg.s.area, -organic.carbon) %>% 
  filter(catchment.id %in% "C8" | catchment.id %in% "M6") %>% 
  pivot_wider(names_from = "catchment.id", values_from = "mg.s.area")

c8_and_h3 <- inst_flux_steps %>% 
  select(-`Drainage Area (km2)`, -site, -totalQ, -discharge.litres, -inst.flux, -log.mg.s.area, -organic.carbon) %>% 
  filter(catchment.id %in% "C8" | catchment.id %in% "H3") %>% 
  pivot_wider(names_from = "catchment.id", values_from = "mg.s.area")

c8_and_h2 <- inst_flux_steps %>% 
  select(-`Drainage Area (km2)`, -site, -totalQ, -discharge.litres, -inst.flux, -log.mg.s.area, -organic.carbon) %>% 
  filter(catchment.id %in% "C8" | catchment.id %in% "H2") %>% 
  pivot_wider(names_from = "catchment.id", values_from = "mg.s.area")

### Graphs
c9m6_plot <- c9_and_m6 %>% 
  ggplot(aes(x = C9, y = M6, na.rm = FALSE)) +
  geom_point() +
  theme_bw(base_size = 16) +
  scale_x_continuous(expand = c(0, 0), limits = c(0, 45)) + 
  scale_y_continuous(expand = c(0, 0), limits = c(0, 45)) +
  geom_smooth(method = "lm", se = FALSE, colour = "black", lty = "dotted") +
  xlab(bquote('C9 DOC instantaneous flux '(mg/s/km^2))) +
  ylab("") +
  stat_poly_eq(aes(label = paste(after_stat(eq.label),
                                 after_stat(rr.label), sep = "*\", \"*"))) +
  geom_abline(slope = 1, lty = "dotted", colour = "orange", linewidth = 1)

c8m6_plot <- c8_and_m6 %>% 
  ggplot(aes(x = C8, y = M6, na.rm = FALSE)) +
  geom_point() +
  theme_bw(base_size = 16) +
  scale_x_continuous(expand = c(0, 0), limits = c(0, 45)) + 
  scale_y_continuous(expand = c(0, 0), limits = c(0, 45)) +
  geom_smooth(method = "lm", se = FALSE, colour = "black", lty = "dotted") +
  xlab(bquote('C8 DOC instantaneous flux '(mg/s/km^2))) +
  ylab("") +
  stat_poly_eq(aes(label = paste(after_stat(eq.label),
                                 after_stat(rr.label), sep = "*\", \"*"))) +
  geom_abline(slope = 1, lty = "dotted", colour = "orange", linewidth = 1)

c8_and_h3 %>% 
  ggplot(aes(x = C8, y = H3, na.rm = FALSE)) +
  geom_point() +
  theme_bw(base_size = 16) +
  geom_smooth(method = "lm", se = FALSE, colour = "black", lty = "dotted") # different magnitudes - distance probably too great

c8_and_h2 %>% 
  ggplot(aes(x = C8, y = H2, na.rm = FALSE)) +
  geom_point() +
  theme_bw(base_size = 16) +
  geom_smooth(method = "lm", se = FALSE, colour = "black", lty = "dotted") # different magnitudes - distance probably too great

##### Arrange above figures ----

paired_catchment_figure <- ggarrange(c9m6_plot,
                                     c8m6_plot,
                                     labels = NULL,
                                     nrow = 2)

paired_catchment_figure

annotate_figure(paired_catchment_figure, left = textGrob(bquote('M6 DOC instantaneous flux '(mg/s/km^2)), rot = 90, hjust = 0.45, vjust = 1.5, gp = gpar(cex = 1.3), ))




