#########################
## Name: Matt Watkins
## Date: Aug. 7th, 2022
## Project: MSc data analysis
## Objective: Explore the doc variability with the new datasets
## Inputs: glfc_water_chem RDS
## Outputs: graphs
#########################

## 0. NOTES ----

## 1. PREPARE ----

rm(list=ls())
options(stringsASfactors = FALSE, scipen = 999, encoding = "UTF-8")

library(tidyverse)
library(readxl)

## 2. IMPORT ----

chem <- readRDS("glfc_chem_cleaned_v1.01.RDS")

ws_char <- readRDS("watershed_characteristics_cleaned.RDS")

watershed_table <- read_xlsx("/Volumes/MW/2020 Trent University/GIS/Excel Sheets/Watershed_table_v1.xlsx")

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

## 4. PLOTTING ----

### 4.01 - Look at DOC variability across time for all sites

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

### 4.02 - DOC and catchment area

area_doc <- ws_doc[ws_doc$variable.x == "Drainage Area (km2)", ]


area_doc %>% 
  ggplot(aes(value.x, mean.doc)) +
  #geom_text(hjust = 0, vjust = 0) +
  geom_point() +
  scale_x_log10() +
  geom_errorbar(aes(ymin = mean.doc - doc.sd, ymax = mean.doc + doc.sd)) +
  labs(x = expression(paste("Drainage Area ", (km^2))), y = "DOC (ppm)") +
  geom_smooth(method='lm', formula= y~x, se = FALSE)

# add labels - label = catchment.id

### 4.03 - DOC and slope

slope_doc <- ws_doc[ws_doc$variable.x == "slope", ]

slope_doc %>% 
  ggplot(aes(value.x, mean.doc)) +
  #geom_text(hjust = 0, vjust = 0) +
  geom_smooth(method = "lm", formula = y~x, se = FALSE) +
  geom_point() +
  geom_errorbar(aes(ymin = mean.doc - doc.sd, ymax = mean.doc + doc.sd)) +
  labs(x = "Slope (%)", y = "DOC (ppm)")

### 4.04 - DOC and latitude

lat <- ws_doc[ws_doc$variable.x == "latitude", ]

lat %>% 
ggplot(aes(value.x, mean.doc)) +
  geom_point() +
  geom_smooth(method = "lm", formula = y~x, se = FALSE) +
  geom_errorbar(aes(ymin = mean.doc - doc.sd, ymax = mean.doc + doc.sd)) +
  labs(x = "Latitude", y = "DOC (mg/L)")

### 4.05 - DOC and longitude

long <- ws_doc[ws_doc$variable.x == "longitude", ]

long %>% 
  ggplot(aes(value.x, mean.doc)) +
  geom_point() +
  geom_smooth(method = "lm", formula = y~x, se = FALSE) +
  geom_errorbar(aes(ymin = mean.doc - doc.sd, ymax = mean.doc + doc.sd)) +
  labs(x = "Longitude", y = "DOC (mg/L)")

### 4.06 - DOC and elevation

elev <- ws_doc[ws_doc$variable.x == "elevation" & ws_doc$mean.doc, ]

elev %>% 
  ggplot(aes(value.x, mean.doc)) +
  geom_point() +
  geom_smooth(method = "lm", formula = y~x, se = FALSE) +
  geom_errorbar(aes(ymin = mean.doc - doc.sd, ymax = mean.doc + doc.sd)) +
  labs(x = "Elevation (masl)", y = "DOC (mg/L)")

### 4.07 - DOC and open water

open_w <- ws_doc[ws_doc$variable.x == "open.water", ]

open_w %>% 
  ggplot(aes(value.x, mean.doc)) +
  geom_point() +
  geom_smooth(method = "lm", formula = y~x, se = FALSE) +
  geom_errorbar(aes(ymin = mean.doc - doc.sd, ymax = mean.doc + doc.sd)) +
  labs(x = "Open water (%)", y = "DOC (mg/L)")

### 4.08 - DOC and wetland

wetland <- ws_doc[ws_doc$variable.x == "Wetland Cover (%)", ]

wetland %>% 
  ggplot(aes(value.x, mean.doc)) +
  geom_point() +
  geom_smooth(method = "lm", formula = y~x, se = FALSE) +
  geom_errorbar(aes(ymin = mean.doc - doc.sd, ymax = mean.doc + doc.sd)) +
  labs(x = "Wetland (%)", y = "DOC (mg/L)")

### 4.09 - DOC and bog

bog <- ws_doc[ws_doc$variable.x == "bog", ]

bog %>% 
  ggplot(aes(value.x, mean.doc)) +
  geom_point() +
  geom_smooth(method = "lm", formula = y~x, se = FALSE) +
  geom_errorbar(aes(ymin = mean.doc - doc.sd, ymax = mean.doc + doc.sd)) +
  labs(x = "Bog (%)", y = "DOC (mg/L)")

### 4.10 - DOC and deciduous

deciduous <- ws_doc[ws_doc$variable.x == "Deciduous Forest (%)", ]

deciduous %>% 
  ggplot(aes(value.x, mean.doc)) +
  geom_point() +
  geom_smooth(method = "lm", formula = y~x, se = FALSE,) +
  geom_errorbar(aes(ymin = mean.doc - doc.sd, ymax = mean.doc + doc.sd)) +
  labs(x = "Deciduous tree cover (%)", y = "DOC (mg/L)")

### 4.11 - DOC and conifer

conifer <- ws_doc[ws_doc$variable.x == "Coniferous Forest (%)", ]

conifer %>% 
  ggplot(aes(value.x, mean.doc)) +
  geom_point() +
  geom_smooth(method = "lm", formula = y~x, se = FALSE) +
  geom_errorbar(aes(ymin = mean.doc - doc.sd, ymax = mean.doc + doc.sd)) +
  labs(x = "Coniferous tree cover (%)", y = "DOC (mg/L)")

### 4.12 - DOC and mixed tree coverage

mixed <- ws_doc[ws_doc$variable.x == "Mixed Forest (%)", ]

mixed %>% 
  ggplot(aes(value.x, mean.doc)) +
  geom_point() +
  geom_smooth(method = "lm", formula = y~x, se = FALSE) +
  geom_errorbar(aes(ymin = mean.doc - doc.sd, ymax = mean.doc + doc.sd)) +
  labs(x = "Mixed tree cover (%)", y = "DOC (mg/L)")

### 4.13 - DOC and sparse tree coverage

sparse <- ws_doc[ws_doc$variable.x == "Sparse Forest (%)", ]

sparse %>% 
  ggplot(aes(value.x, mean.doc)) +
  geom_point() +
  geom_smooth(method = "lm", formula = y~x, se = FALSE) +
  geom_errorbar(aes(ymin = mean.doc - doc.sd, ymax = mean.doc + doc.sd)) +
  labs(x = "Sparse tree cover (%)", y = "DOC (mg/L)")

## 5. SAVING // EXPORTING ----

## 6. TRIAL // JUNK CODE ----

range(doc$value)

mean(doc$value)
