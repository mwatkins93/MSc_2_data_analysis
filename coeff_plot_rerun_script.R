options(stringsASfactors = FALSE, scipen = 999, encoding = "UTF-8", na.action = "na.fail")

library(tidyverse)
library(readxl)
library(performance)
library(MuMIn)
library(broom.mixed)
library(modelsummary)
library(dotwhisker)
library(sjPlot)

doc_table <- readRDS("final_doc_tbl.rds")

get.models(themodeltable, subset = 1)[[1]]

#######################

doc_tbl_no_c3 <- doc_table %>% 
  filter(!`Catchment ID` %in% "C3") # df with removal of C3

#### 3.04.1 - Mean DOC ----
mean_base_model <- lm(mean.doc ~ Group + conifer_st + insect5_st + harv10_st + drainage_st + elev_st + wetland_st + open_wat_st + slope_st, data = doc_tbl_no_c3)

mean_base_model_tbl <- dredge(mean_base_model, rank = "AICc")

mean_base_model_avg <- model.avg(subset(mean_base_model_tbl, delta <= 2, recalc.weights=FALSE), fit = TRUE)

#### 3.04.2 - Sample campaign 1 ----

sc1_base_model <- lm(doc.s1 ~ Group + conifer_st + insect5_st + harv10_st + drainage_st + elev_st + wetland_st + open_wat_st + slope_st, data = doc_tbl_no_c3)

sc1_base_table <- dredge(sc1_base_model, rank = "AICc")

sc1_base_model_avg <- model.avg(subset(sc1_base_table, delta <= 2, recalc.weights = FALSE), fit = TRUE)

# sc1_base_model_avg <- get.models(sc1_base_table, subset = 1)[[1]]

#### 3.04.3 - Sample campaign 2 ----

sc2_base_model <- lm(doc.s2 ~ Group + conifer_st + insect5_st + harv10_st + drainage_st + elev_st + wetland_st + open_wat_st + slope_st, data = doc_tbl_no_c3)

sc2_base_table <- dredge(sc2_base_model, rank = "AICc")

sc2_base_model_avg <- model.avg(subset(sc2_base_table, delta <= 2, recalc.weights = FALSE), fit = TRUE)

# sc2_base_model_avg <- get.models(sc2_base_table, subset = 1)[[1]]

#### 3.04.4 - Sample campaign 3 ----

doc_s3_sub <- doc_tbl_no_c3[-3, ]

sc3_base_model <- lm(doc.s3 ~ Group + conifer_st + insect5_st + harv10_st + drainage_st + elev_st + wetland_st + open_wat_st + slope_st, data = doc_s3_sub)

sc3_base_table <- dredge(sc3_base_model, rank = "AICc")

sc3_base_model_avg <- model.avg(subset(sc3_base_table, delta <= 2, recalc.weights = FALSE), fit = TRUE)

#### 3.04.5 - Sample campaign 4 ----

doc_s4_sub <- doc_tbl_no_c3[c(-3, -6), ]

sc4_base_model <- lm(doc.s4 ~ Group + conifer_st + insect5_st + harv10_st + drainage_st + elev_st + wetland_st + open_wat_st + slope_st, data = doc_s4_sub)

sc4_base_table <- dredge(sc4_base_model, rank = "AICc")

sc4_base_model_avg <- model.avg(subset(sc4_base_table, delta <= 2, recalc.weights = FALSE), fit = TRUE)

#### 3.04.6 - Sample campaign 5 ----

doc_s5_sub <- doc_tbl_no_c3[-3, ]

sc5_base_model <- lm(doc.s5 ~ Group + conifer_st + insect5_st + harv10_st + drainage_st + elev_st + wetland_st + open_wat_st + slope_st, data = doc_s5_sub)

sc5_base_table <- dredge(sc5_base_model, rank = "AICc")

sc5_base_model_avg <- model.avg(subset(sc5_base_table, delta <= 2, recalc.weights = FALSE), fit = TRUE)

#### 3.04.7 - Sample campaign 6 ----

sc6_base_model <- lm(doc.s6 ~ Group + conifer_st + insect5_st + harv10_st + drainage_st + elev_st + wetland_st + open_wat_st + slope_st, data = doc_tbl_no_c3)

sc6_base_table <- dredge(sc6_base_model, rank = "AICc")

sc6_base_model_avg <- model.avg(subset(sc6_base_table, delta <= 2, recalc.weights = FALSE), fit = TRUE)

# sc6_base_model_avg <- get.models(sc6_base_table, subset = 1)[[1]]

#### 3.04.8 - Attach all base model averages ----

base_model_avgs <- list(sc1_base_model_avg, sc2_base_model_avg, sc3_base_model_avg, sc4_base_model_avg, sc5_base_model_avg, sc6_base_model_avg, mean_base_model_avg)

mean_base_model_plot <- dwplot(base_model_avgs) %>%
  relabel_predictors(c(
    conifer_st = "Coniferous",
    decid_st = "Deciduous",
    tprod_for_st = "Total Productive Forest",
    drainage_st = "Catchment Area",
    slope_st = "Slope",
    open_wat_st = "Open Water",
    wetland_st = "Wetland",
    elev_st = "Elevation",
    GroupHarvest = "Harvest Class",
    GroupInsect = "Insect Class",
    GroupMixed = "Mixed Class",
    harv20_st = "20-year Harvest",
    harv15_st = "15-year Harvest",
    harv10_st = "10-year Harvest",
    harv5_st = "5-year Harvest",
    insect20_st = "20-year Infestation",
    insect10_st = "10-year Infestation",
    insect5_st = "5-year Infestation")) +
  theme_bw() +
  labs(title = "Averaged multiple regression coefficients - 5-year insect + 10-year harvest") +
  theme(legend.title = element_blank(), plot.title = element_text(hjust = 0.5, face = "bold")) +
  geom_vline(xintercept = 0) +
  scale_colour_manual(labels = c("SC1", "SC2", "SC3", "SC4", "SC5", "SC6", "Mean DOC"),
                      values = c("#99ccff", "#c4c1c6", "#005155", "#e9b22a", "#8c6d31", "#6600ff", "#000000"))

# view plot
mean_base_model_plot
