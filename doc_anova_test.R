# [DOC] - ANOVA calculation

doc_table <- readRDS("final_doc_tbl.rds")


doc <- doc_table %>% 
  select(`Site name`, doc.s1, doc.s2, doc.s3, doc.s4, doc.s5, doc.s6, Group)

doc_long <- doc %>% 
  pivot_longer(cols = 2:7, names_to = "organic.carbon", values_to = "doc") %>% 
  select(-organic.carbon)

doc_long$Group <- as.factor(doc_long$Group)

levels(doc_long$Group)

# Compute statistics by group ----

doc_stats <- doc_long %>% 
  group_by(Group) %>% 
  summarise(count = n(),
         mean = mean(doc, na.rm = TRUE),
         sd = sd(doc, na.rm = TRUE))

doc_long %>% 
  filter(`Group` %in% "Harvest") %>% 
  summarise(mean = mean(doc, na.rm = TRUE)) # check means - seems legit

# Check requirements ----

## Normal distribution ----

doc_sub <- doc_long %>% 
  filter(Group %in% "Mixed")

shapiro.test(doc_sub$doc)

ggdensity(doc_sub$doc)

## Control - highly different than normal
## Harvest - very normal!
## Insect - highly different than normal
## Mixed - highly different than normal 

# Proceed with Kruskal-Wallis test (non-parametric) ----

kruskal.test(doc ~ Group, data = doc_long)

## Conclusion - significant difference between the groups

pairwise.wilcox.test(doc_long$doc, doc_long$Group, p.adjust.method = "BH")




