#########################
## Name: Matt Watkins
## Date: Aug. 7th, 2022
## Project: MSc data analysis
## Objective: Explore the doc variability with the new datasets
## Inputs: glfc_water_chem RDS
## Outputs: graphs
#########################

## 0. NOTES ----

### colours - c("#56B4E9", "#CC79A7", "#009E73", "#E69F00")
### shapes - c(17, 18, 16, 15)

### insert this to update graphs:   

geom_point(aes(colour = Group),  size = 2) +
theme(legend.title=element_blank()) +
scale_colour_manual(values = c("#E69F00", "#56B4E9", "#009E73", 
                                 "#F0E442"))

## 1. PREPARE ----

rm(list=ls())
options(stringsASfactors = FALSE, scipen = 999, encoding = "UTF-8")

library(tidyverse)
library(readxl)

## 2. IMPORT ----

chem <- readRDS("glfc_chem_cleaned_v1.01.RDS")

watershed_table <- read_excel("/Volumes/MW/2020 Trent University/R/Thesis Data/MSc_data_analysis/Watershed_table_v1.xlsx")

doc_export <- read_excel("doc_load_estimates.xlsx")

## 3. TIDY // PROCESS ----

### 3.01 - Subset to DOC

chem_out <- chem %>%
  select(-glfc.id)

doc <- chem_out %>%
  subset(variable == "organic.carbon")

### 3.02 - Connect DOC and watershed table

colnames(watershed_table)[1:2] <- c("site", "catchment.id")

ws_doc <- left_join(watershed_table, doc, by = c("site","catchment.id")) %>%
  group_by(site) %>% 
  mutate(mean.doc = mean(value),
         doc.sd = sd(value))

### 3.03 - Connect DOC export and watershed table

ws_export <- left_join(watershed_table, doc_export, by = c("site", "catchment.id"))


## 4. PLOTTING ----

### 4.1 - Look at DOC variability across time for all sites ----

doc %>% 
  ggplot(aes(date, value)) +
  geom_point() +
  facet_wrap(~ catchment.id) +
  labs(x = "", y = "DOC (mg/L)") # point plot

doc %>% 
  ggplot(aes(date, value)) +
  geom_line() +
  facet_wrap(~ catchment.id) +
  labs(x = "", y = "DOC (mg/L)") # line plot variation

### 4.1.1 - DOC and catchment area ----

ws_doc %>% 
  ggplot(aes(`Drainage Area (km2)`, mean.doc)) +
  #geom_text(hjust = 0, vjust = 0) +
  geom_point(aes(colour = Group),  size = 2) +
  scale_x_log10() +
  geom_errorbar(aes(ymin = mean.doc - doc.sd, ymax = mean.doc + doc.sd)) +
  theme(legend.title=element_blank()) +
  scale_colour_manual(values = c("#E69F00", "#56B4E9", "#009E73", 
                        "#F0E442")) +
  labs(x = expression(paste("Drainage Area ", (km^2))), y = "DOC (mg/L)") +
  geom_smooth(method='lm', formula= y~x, se = FALSE)

# add labels - label = catchment.id

### 4.1.2 - DOC and slope ----

ws_doc %>% 
  ggplot(aes(`Slope (degrees)`, mean.doc)) +
  #geom_text(hjust = 0, vjust = 0) +
  geom_smooth(method = "lm", formula = y~x, se = FALSE) +
  geom_point(aes(colour = Group),  size = 2) +
  theme(legend.title=element_blank()) +
  scale_colour_manual(values = c("#E69F00", "#56B4E9", "#009E73", 
                                 "#F0E442")) +
  geom_errorbar(aes(ymin = mean.doc - doc.sd, ymax = mean.doc + doc.sd)) +
  xlab(expression("Slope " ( degree))) +
  labs(y = "DOC (mg/L)")

### 4.1.3 - DOC and latitude ----



ws_doc %>% 
ggplot(aes(Latitude, mean.doc)) +
  geom_point(aes(colour = Group),  size = 2) +
  theme(legend.title=element_blank()) +
  scale_colour_manual(values = c("#E69F00", "#56B4E9", "#009E73", 
                                 "#F0E442")) +
  geom_smooth(method = "lm", formula = y~x, se = FALSE) +
  geom_errorbar(aes(ymin = mean.doc - doc.sd, ymax = mean.doc + doc.sd)) +
  labs(x = "Latitude", y = "DOC (mg/L)")

### 4.1.4 - DOC and longitude ----

ws_doc %>% 
  ggplot(aes(Longitude, mean.doc)) +
  geom_point(aes(colour = Group),  size = 2) +
  theme(legend.title=element_blank()) +
  scale_colour_manual(values = c("#E69F00", "#56B4E9", "#009E73", 
                                 "#F0E442")) +
  geom_smooth(method = "lm", formula = y~x, se = FALSE) +
  geom_errorbar(aes(ymin = mean.doc - doc.sd, ymax = mean.doc + doc.sd)) +
  labs(x = "Longitude", y = "DOC (mg/L)")

### 4.1.5 - DOC and elevation ----

ws_doc %>% 
  ggplot(aes(`Elevation (m a.s.l.)`, mean.doc)) +
  geom_point(aes(colour = Group),  size = 2) +
  theme(legend.title=element_blank()) +
  scale_colour_manual(values = c("#E69F00", "#56B4E9", "#009E73", 
                                 "#F0E442")) +
  geom_smooth(method = "lm", formula = y~x, se = FALSE) +
  geom_errorbar(aes(ymin = mean.doc - doc.sd, ymax = mean.doc + doc.sd)) +
  labs(x = "Elevation (masl)", y = "DOC (mg/L)")

### 4.1.6 - DOC and open water ----

ws_doc%>% 
  ggplot(aes(`Open Water (%)`, mean.doc)) +
  geom_point(aes(colour = Group),  size = 2) +
  theme(legend.title=element_blank()) +
  scale_colour_manual(values = c("#E69F00", "#56B4E9", "#009E73", 
                                 "#F0E442")) +
  geom_smooth(method = "lm", formula = y~x, se = FALSE) +
  geom_errorbar(aes(ymin = mean.doc - doc.sd, ymax = mean.doc + doc.sd)) +
  labs(x = "Open water (%)", y = "DOC (mg/L)")

### 4.1.7 - DOC and wetland ----

ws_doc %>%  
  ggplot(aes(`Wetland Cover (%)`, mean.doc)) +
  geom_point(aes(colour = Group),  size = 2) +
  theme(legend.title=element_blank()) +
  scale_colour_manual(values = c("#E69F00", "#56B4E9", "#009E73", 
                                 "#F0E442")) +
  geom_smooth(method = "lm", formula = y~x, se = FALSE) +
  geom_errorbar(aes(ymin = mean.doc - doc.sd, ymax = mean.doc + doc.sd)) +
  labs(x = "Wetland (%)", y = "DOC (mg/L)")

### 4.1.8 - DOC and deciduous ----

ws_doc %>% 
  ggplot(aes(`Deciduous Forest (%)`, mean.doc)) +
  geom_point(aes(colour = Group),  size = 2) +
  theme(legend.title=element_blank()) +
  scale_colour_manual(values = c("#E69F00", "#56B4E9", "#009E73", 
                                 "#F0E442")) +
  geom_smooth(method = "lm", formula = y~x, se = FALSE,) +
  geom_errorbar(aes(ymin = mean.doc - doc.sd, ymax = mean.doc + doc.sd)) +
  labs(x = "Deciduous forest cover (%)", y = "DOC (mg/L)")

### 4.1.9 - DOC and conifer ----

ws_doc %>% 
  ggplot(aes(`Coniferous Forest (%)`, mean.doc)) +
  geom_point(aes(colour = Group),  size = 2) +
  theme(legend.title=element_blank()) +
  scale_colour_manual(values = c("#E69F00", "#56B4E9", "#009E73", 
                                 "#F0E442")) +
  geom_smooth(method = "lm", formula = y~x, se = FALSE) +
  geom_errorbar(aes(ymin = mean.doc - doc.sd, ymax = mean.doc + doc.sd)) +
  labs(x = "Coniferous forest cover (%)", y = "DOC (mg/L)")

### 4.1.10 - DOC and mixed tree coverage ----

mixed <- ws_doc[ws_doc$variable.x == "Mixed Forest (%)", ]

mixed %>% 
  ggplot(aes(value.x, mean.doc)) +
  geom_point() +
  geom_smooth(method = "lm", formula = y~x, se = FALSE) +
  geom_errorbar(aes(ymin = mean.doc - doc.sd, ymax = mean.doc + doc.sd)) +
  labs(x = "Mixed tree cover (%)", y = "DOC (mg/L)")

### 4.1.11 - DOC and total productive forest ----

ws_doc %>% 
  ggplot(aes(`Total Productive Forest (%)`, mean.doc)) +
  geom_point(aes(colour = Group),  size = 2) +
  theme(legend.title=element_blank()) +
  scale_colour_manual(values = c("#E69F00", "#56B4E9", "#009E73", 
                                 "#F0E442")) +
  geom_smooth(method = "lm", formula = y~x, se = FALSE) +
  geom_errorbar(aes(ymin = mean.doc - doc.sd, ymax = mean.doc + doc.sd)) +
  labs(x = "Total productive forest (%)", y = "DOC (mg/L)")

### 4.2 - DOC export and explanatory landscape variables ----

### 4.2.1 - Export and drainage area ----
ws_export %>% 
  ggplot(aes(`Drainage Area (km2)`, `linear interpolation (g C/m^2/season)`)) +
  geom_point(aes(colour = Group),  size = 2) +
  theme(legend.title=element_blank()) +
  scale_colour_manual(values = c("#E69F00", "#56B4E9", "#009E73", 
                                 "#F0E442")) +
  geom_errorbar(aes(ymin = `minimum (g C/m^2/season)`, ymax = `maximum (g C/m^2/season)`)) +
  labs(x = expression(paste("Drainage Area ", (km^2))), y = expression(paste("DOC export ", ("g C"/m^2/"season")))) +
  geom_smooth(method='lm', formula= y~x, se = FALSE)

### 4.2.2 - Export and wetland area

ws_export %>% 
  ggplot(aes(`Wetland Cover (%)`, `linear interpolation (g C/m^2/season)`)) +
  geom_point(aes(colour = Group),  size = 2) +
  theme(legend.title=element_blank()) +
  scale_colour_manual(values = c("#E69F00", "#56B4E9", "#009E73", 
                                 "#F0E442")) +
  geom_errorbar(aes(ymin = `minimum (g C/m^2/season)`, ymax = `maximum (g C/m^2/season)`)) +
  labs(x = "Wetland area (%)", y = expression(paste("DOC export ", ("g C"/m^2/"season")))) +
  geom_smooth(method='lm', formula= y~x, se = FALSE)

### 4.2.3 = Export and open water

ws_export %>% 
  ggplot(aes(`Open Water (%)`, `linear interpolation (g C/m^2/season)`)) +
  geom_point(aes(colour = Group),  size = 2) +
  theme(legend.title=element_blank()) +
  scale_colour_manual(values = c("#E69F00", "#56B4E9", "#009E73", 
                                 "#F0E442")) +
  geom_errorbar(aes(ymin = `minimum (g C/m^2/season)`, ymax = `maximum (g C/m^2/season)`)) +
  labs(x = "Open water (%)", y = expression(paste("DOC export ", ("g C"/m^2/"season")))) +
  geom_smooth(method='lm', formula= y~x, se = FALSE)

### 4.2.4 = Export and total forest cover

ws_export %>% 
  ggplot(aes(`Total Productive Forest (%)`, `linear interpolation (g C/m^2/season)`)) +
  geom_point(aes(colour = Group),  size = 2) +
  theme(legend.title=element_blank()) +
  scale_colour_manual(values = c("#E69F00", "#56B4E9", "#009E73", 
                                 "#F0E442")) +
  geom_errorbar(aes(ymin = `minimum (g C/m^2/season)`, ymax = `maximum (g C/m^2/season)`)) +
  labs(x = "Total productive forest (%)", y = expression(paste("DOC export ", ("g C"/m^2/"season")))) +
  geom_smooth(method='lm', formula= y~x, se = FALSE)

### 4.2.5 = Export and coniferous forest cover

ws_export %>% 
  ggplot(aes(`Coniferous Forest (%)`, `linear interpolation (g C/m^2/season)`)) +
  geom_point(aes(colour = Group),  size = 2) +
  theme(legend.title=element_blank()) +
  scale_colour_manual(values = c("#E69F00", "#56B4E9", "#009E73", 
                                 "#F0E442")) +
  geom_errorbar(aes(ymin = `minimum (g C/m^2/season)`, ymax = `maximum (g C/m^2/season)`)) +
  labs(x = "Coniferous forest (%)", y = expression(paste("DOC export ", ("g C"/m^2/"season")))) +
  geom_smooth(method='lm', formula= y~x, se = FALSE)

### 4.2.6 = Export and deciduous forest cover

ws_export %>% 
  ggplot(aes(`Deciduous Forest (%)`, `linear interpolation (g C/m^2/season)`)) +
  geom_point(aes(colour = Group),  size = 2) +
  theme(legend.title=element_blank()) +
  scale_colour_manual(values = c("#E69F00", "#56B4E9", "#009E73", 
                                 "#F0E442")) +
  geom_errorbar(aes(ymin = `minimum (g C/m^2/season)`, ymax = `maximum (g C/m^2/season)`)) +
  labs(x = "Deciduous forest (%)", y = expression(paste("DOC export ", ("g C"/m^2/"season")))) +
  geom_smooth(method='lm', formula= y~x, se = FALSE) +
  scale_x_continuous(limits=c(20, 60))

## 5. SAVING // EXPORTING ----

## 6. TRIAL // JUNK CODE ----

range(doc$value)

mean(doc$value)
