  library("cowplot")

Phenology <- droplevels(Phenology)

Phenology %>% 
  #filter(siteID == "Hogsete") %>% 
  filter(pheno.unit == "DaysSinceSM", pheno.stage == "Flower", pheno.var == "first") %>% 
  select(siteID, species, newTT, SMDiff, value) %>%
  group_by(siteID, species, newTT, SMDiff) %>% 
  summarise(n = n(), mean = mean(value)) %>% 
  spread(key = newTT, value = mean) %>% print(n = 37)
  ggplot(aes(x = SMDiff, y = value, color = newTT)) +
  geom_point(size = 2) +
  scale_colour_manual(name = "Treatment:", values = c("grey", "red", "blue", "purple"))


  
ddd <- EventDiffData %>% 
  filter(newTT %in% c("Control", "Warmer"))
  
fit <- lm(value ~ newTT + SMDiff, ddd)  
summary(fit)

ggplot(ddd, aes(x = SMDiff, y = value, color = newTT)) + geom_point() +
  geom_smooth(method='lm',formula=y~x)


ddd <- Phenology %>% 
  filter(pheno.unit == "DaysSinceSM", pheno.stage == "Flower", pheno.var == "first")

fit <- lmer(value ~ newTT + Temp_valueRescale * Prec_valueRescale + newTT:Prec_valueRescale + newTT:Temp_valueRescale + (1|species) + (1|siteID/blockID), ddd)  
summary(fit)

