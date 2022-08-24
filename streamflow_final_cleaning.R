library(tidyverse)


WSBL2_final <- cbind(wsBL2_estQ, site = "WS BL2")

strflow_final18 <- bind_rows(WSBL2_final, strflow_final17)

saveRDS(strflow_final18, file = "streamflow_final_v1.00")

#####

strflow_final <- readRDS("streamflow_final_v1.01") # read in master streamflow file

strflow_final_upd <- strflow_final %>% 
  filter(stage > 0)

unique(strflow_final[c("site")]) ## see the names of the watersheds I already have included

wsSSR <- wsSSR_offset %>% 
  ungroup() %>% 
  select(time, water.metres.adj) %>% 
  mutate(Q.estimate = NA, site = NA) %>% 
  rename(stage = water.metres.adj)

wsSSR$site <- "WS SSR"
  

strflow_final_13 <- bind_rows(strflow_final_12, wsSSR)

unique(strflow_final_13[c("site")])

#### Save

saveRDS(strflow_final_upd, file = "streamflow_final_v1.02")
             