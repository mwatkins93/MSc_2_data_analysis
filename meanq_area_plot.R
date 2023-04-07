theme_update(text = element_text(size=20))


q <- readRDS("Q_estimates_v2.rds")

mean_q <- q %>% 
  group_by(site) %>% 
  summarise(mean.q = mean(totalQ))

colnames(mean_q)[1] <- "Site name" 


doc_table_q <- left_join(mean_q, doc_table, by = "Site name")


doc_table_q %>% 
  ggplot(aes(x = `Drainage Area (km2)`, y = mean.q)) +
  geom_point() +
  theme_bw() +
  labs(x = expression(paste("Drainage area ", (km^2))), y = expression(paste("Mean discharge ", (m^3/s)))) +
  theme(text = element_text(size=20))
