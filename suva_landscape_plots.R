library(ggpubr)
library(grid)

ws <- read_excel("Watershed_table_v1.xlsx")

colnames(suva_sub)[1] <- "Site name"

colours <- scale_fill_manual(values = c("#E69F00", "#56B4E9", "#009E73", 
                                                 "#F0E442"))

ws_suva <- left_join(suva_sub, ws, by = "Site name")

ws_suva_means <- ws_suva %>% 
  group_by(`Site name`) %>% 
  mutate(mean.suva = mean(suva)) %>% 
  select(`Site name`, `Drainage Area (km2)`, type, `Open Water (%)`, mean.suva)

###### Suva - landscape graphs

openwater_suva <- ws_suva_means %>% 
  ggplot(aes(x = `Open Water (%)`, y = mean.suva, colour = type)) +
  geom_point() +
  scale_colour_manual(values = c("#E69F00", "#56B4E9", "#009E73", 
                                        "#F0E442")) +
  theme_bw(base_size = 16) +
  theme(legend.title = element_blank()) +
  ylab("Mean SUVA (L/mg-C/m)")
  #xlab(expression(paste("Drainage Area ", (km^2))))

drainage_suva <- ws_suva_means %>% 
  ggplot(aes(x = `Drainage Area (km2)`, y = mean.suva, colour = type)) +
  geom_point() +
  scale_colour_manual(values = c("#E69F00", "#56B4E9", "#009E73", 
                                          "#F0E442")) +
                                            theme_bw(base_size = 16) +
  theme(legend.title = element_blank()) +
  ylab("Mean SUVA (L/mg-C/m)") +
  scale_x_log10() +
  xlab(expression(paste("Drainage Area ", (km^2))))

drainage_suva
openwater_suva


#### arranged

figure1 <- ggarrange(drainage_suva + theme(axis.title.y = element_blank()),
                     openwater_suva + theme(axis.title.y = element_blank()),
                     labels = NULL,
                     common.legend = TRUE,
                     legend = "bottom",
                     align = "h")

annotate_figure(figure1, left = textGrob("Mean SUVA (L/mg-C/m)", rot = 90, hjust = 0.2, gp = gpar(cex = 1.3)))

