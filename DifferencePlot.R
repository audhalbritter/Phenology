# DIFFERENCE PLOTS FOR 7 COMMON SPECIES

#### Common species ####
# Find species that occur in Control and at least one other treatment, and more than 2 sites  
dd <- Phenology %>% 
  filter(pheno.unit == "DaysSinceSM", pheno.stage == "Flower", pheno.var == "first") %>% 
  select(siteID, species, newTT, value) %>%
  group_by(siteID, species, newTT) %>% 
  summarise(mean = mean(value)) %>% 
  spread(key = newTT, value = mean) %>% 
  mutate(TooFew = ifelse(is.na(Warm) & is.na(Wet) & is.na(WarmWet), "few", "enough")) %>% 
  filter(TooFew != "few") %>% 
  select(-TooFew) %>%
  filter(!is.na(Control))

ddd <- data.frame(table(dd$species, dd$siteID))
colnames(ddd) <- c("species","siteID", "Frequency")
sp.list <- ddd %>% 
  spread(key = siteID, value = Frequency) %>% 
  mutate(sum = rowSums(.[2:7])) %>% 
  filter(sum > 2) %>% 
  distinct(species)


#### Difference plots ####
### Select 7 species, calc diff. control-treatment,plot
EventDiffData <- Phenology %>% 
  filter(species %in% c("Alc.alp", "Ant.odo", "Bis.viv", "Luz.mul", "Nar.str", "Pin.vul", "Pot.ere")) %>% 
  filter(siteID != "Veskre" | newTT != "Control") %>% # remove Control at Veskre
  filter(pheno.unit == "DaysSinceSM", pheno.stage == "Flower", pheno.var == "first") %>% 
  #mutate(orig = plyr::mapvalues(orig, c("GUD", "RAM", "SKJ"), c("Alpine-early", "Subalpine-early", "Alpine-late"))) %>%
  #mutate(orig = factor(orig, levels = c("Alpine-early", "Alpine-late", "Subalpine-early"))) %>%
  group_by(newTT, destSiteID, siteID, species) %>% 
  summarise(N = n(), mean = mean(value, na.rm = TRUE), se = sd(value, na.rm = TRUE)/sqrt(N)) %>% 
  filter(species == "Pot.ere")

EventDiff <- EventDiffData %>% 
  ungroup(destSiteID) %>% 
  select(-destSiteID,-N) %>% 
  unite(united, mean, se, sep = "_") %>%
  spread(key = newTT, value = united) %>% 
  separate(col = Control, into = c("Control_mean", "Control_se"), sep = "_", convert = TRUE) %>% 
  separate(col = Warmer, into = c("Warmer_mean", "Warmer_se"), sep = "_", convert = TRUE) %>% 
  separate(col = LaterSM, into = c("LaterSM_mean", "LaterSM_se"), sep = "_", convert = TRUE) %>% 
  separate(col = WarmLate, into = c("WarmLate_mean", "WarmLate_se"), sep = "_", convert = TRUE) %>% 
  # calculate difference between control and treatment in mean
  mutate(Warmer_mean = Warmer_mean - Control_mean, LaterSM_mean = LaterSM_mean - Control_mean, WarmLate_mean = WarmLate_mean - Control_mean) %>% 
  # calculate SE for difference
  mutate(Warmer_se = sqrt(Control_se^2 + Warmer_se^2), LaterSM_se = sqrt(Control_se^2 + LaterSM_se^2), WarmLate_se = sqrt(Control_se^2 + WarmLate_se^2)) %>% 
  select(-Control_mean, -Control_se) %>% 
  unite(Warmer, Warmer_mean, Warmer_se, sep = "_") %>% 
  unite(LaterSM, LaterSM_mean, LaterSM_se, sep = "_") %>% 
  unite(WarmLate, WarmLate_mean, WarmLate_se, sep = "_") %>% 
  gather(key = Treatment, value = united, -siteID, -species) %>% 
  separate(col = united, into = c("mean", "se"), sep = "_", convert = TRUE) %>% 
  #left_join(xAxis1, by = c("Treatment", "orig")) %>% 
  filter(!is.na(mean))

SMD <- Phenology %>% 
  filter(species %in% c("Alc.alp", "Ant.odo", "Bis.viv", "Luz.mul", "Nar.str", "Pin.vul", "Pot.ere")) %>% 
  filter(siteID != "Veskre" | newTT != "Control") %>% # remove Control at Veskre
  filter(newTT != "Control") %>% 
  filter(pheno.unit == "DaysSinceSM", pheno.stage == "Flower", pheno.var == "first") %>% 
  group_by(newTT, siteID, species) %>% 
  summarise(SMDiff = mean(SMDiff, na.rm = TRUE))


ClimateContext <- data_frame(
  siteID = c("Lavisdalen", "Gudmedalen", "Skjellingahaugen", "Lavisdalen", "Gudmedalen", "Hogsete", "Rambera", "Lavisdalen", "Gudmedalen"),
  Treatment = c(rep("Warmer", 3), rep("LaterSM", 4), rep("WarmLate", 2)),
  Shape = c("dry", "intermediate", "wet", "alpine", "alpine", "subalpine", "subalpine", "dry", "intermediate")
)


EventDiff %>% 
  left_join(SMD, by = c("siteID", "species", "Treatment" = "newTT")) %>% 
  left_join(ClimateContext, by = c("siteID", "Treatment")) %>% 
  mutate(Treatment = plyr::mapvalues(Treatment, c("Warmer", "LaterSM", "WarmLate"), c("Warmer", "Later SM", "Warm & late SM"))) %>%
  mutate(Treatment = factor(Treatment, levels = c("Warmer", "Later SM", "Warm & late SM"))) %>%
  mutate(Shape = factor(Shape, levels = c("dry", "intermediate", "wet", "alpine", "subalpine"))) %>%
  ggplot(aes(x = SMDiff, y = mean, color = Treatment, shape = Shape)) +
  geom_hline(yintercept = 0, color = "grey", linetype = "dashed") +
  geom_point(size = 2) +
  scale_colour_manual(name = "Treatment:", values = c("red", "blue", "purple")) +
  scale_shape_manual(name = "Climate Context:", values = c(1, 18, 16, 17, 15)) +
  labs(y = "Difference in onset of flowering [days] after SMT \n between treatment and origin-control", x = "Difference in SMT between origin and destination site [weeks]") +
  facet_wrap(~ species)


