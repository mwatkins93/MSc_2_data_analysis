## Set environment ----

library(tidyverse)

## Import ----

chem_dat <- readRDS("glfc_data_v4.rds")

uw_chem <- readRDS("uw_chem_cleaned_v1.04.rds")

master_tbl <- readRDS("master_doc_tbl.rds")

## Tidy ----

### SUVA

chem_dat$sample <- as.numeric(chem_dat$sample)

suva_unq <- chem_dat %>% 
  select(site, sample, suva) %>% 
  unique() %>% 
  pivot_wider(names_from = "sample", values_from = "suva")

colnames(suva_unq) <- c("Site name", "suva.1", "suva.2", "suva.3", "suva.4", "suva.5", "suva.6")

### THM and HAA

dbps <- uw_chem %>% 
  select(site, sample, `thm-fp`, `haa-fp`) %>% 
  pivot_wider(names_from = "sample", values_from = c("thm-fp", "haa-fp"))

colnames(dbps) <- gsub("_", ".", colnames(dbps))

colnames(dbps)[1] <- "Site name"

## Join ----

tbl_suva_join <- left_join(master_tbl, suva_unq, by = "Site name")

tbl_dbp_join <- left_join(tbl_suva_join, dbps, by = "Site name")

## Save ----

saveRDS(tbl_dbp_join, file = "master_mreg_tbl.rds") # save a master multiple regression table (variant of DOC table)

