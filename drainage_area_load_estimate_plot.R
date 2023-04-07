load_estimate <- read_excel("doc_load_estimates.xlsx")

colnames(load_estimate)[1] <- "Site name"

area_subset <- doc_table %>% 
  select(`Site name`, `Drainage Area (km2)`, Group)

loads_and_area <- left_join(load_estimate, area_subset, by = "Site name")

loads_and_area %>% 
  ggplot(aes(`Drainage Area (km2)`, `linear interpolation (g C/m^2/season)`)) +
  geom_point(aes(colour = Group),  size = 2) +
  theme_bw() +
  theme(legend.title=element_blank()) +
  scale_colour_manual(values = c("#E69F00", "#56B4E9", "#009E73", 
                                          "#F0E442")) +
                                            geom_errorbar(aes(ymin = `minimum (g C/m^2/season)`, ymax = `maximum (g C/m^2/season)`)) +
  labs(x = expression(paste("Drainage Area ", (km^2))), y = expression(paste("g C"/m^2/"season"))) +
  theme(text = element_text(size = 20))
