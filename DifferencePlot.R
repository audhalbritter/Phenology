# DIFFERENCE PLOTS FOR 7 COMMON SPECIES
load(file = "PhenoLong.RData")
library("cowplot")

#### Common species ####
# Find species that occur in Control and at least one other treatment, and more than 2 sites  
dd <- Phenology %>% 
  filter(pheno.unit == "DaysSinceSM", pheno.stage == "Flower", pheno.var == "peak") %>% 
  select(siteID, species, newTT, value) %>%
  group_by(siteID, species, newTT) %>% 
  summarise(n = n(), mean = mean(value)) %>% 
  spread(key = newTT, value = mean) %>%
  filter(!is.na(Control)) %>% 
  mutate(TooFew = ifelse(is.na(Warmer) & is.na(LaterSM) & is.na(WarmLate), "few", "enough")) %>% 
  filter(TooFew != "few") %>% 
  select(-TooFew) %>% 
  gather(key = Treatment, value = value, -siteID, -species)

sp.list <- sort(unique(dd$species))

# species that occur in one treatment and control in at least 2 blocks
sp.list <- c("Agr.cap", "Alc.alp", "Alc.sp", "Ant.odo", "Bis.viv", "Cam.rot", "Car.cap", "Nar.str", "Pin.vul", "Pot.ere", "Tha.alp", "Vio.bif")


# SMDiff by siteID and treatments
EventDiffData %>% 
  #filter(newTT != "Control") %>% 
  ggplot(aes(x = SMDiff, y = value, color = newTT, shape = siteID)) +
  geom_vline(xintercept = 0, color = "grey", linetype = "dashed") +
  geom_jitter() + 
  scale_colour_manual(name = "Treatment:", values = c("grey", "red", "blue", "purple")) +
  facet_wrap(~ species)

  
####################
#### PLASTICITY ####
####################

#### Difference plots ####
### Select species, pheno.unit, etc.
EventDiffData <- Phenology %>% 
  #filter(species %in% sp.list) %>% 
  filter(siteID != "Veskre" | newTT != "Control") # remove Control at Veskre

# Calculate SMDifference within siteID
SMD <- EventDiffData %>% 
  filter(newTT != "Control") %>% 
  group_by(newTT, siteID, species) %>% 
  summarise(SMDiff = mean(SMDiff, na.rm = TRUE))

EventDiff <- EventDiffData %>% 
  select(blockID, species, value, siteID, newTT, SMDiff, pheno.unit, pheno.stage, pheno.var) %>% 
  #filter(species == "Bis.viv") %>% 
  group_by(newTT, blockID, species, pheno.unit, pheno.stage, pheno.var) %>% 
  # mean value per treatment, origin and destination site and species 
  summarise(N = n(), mean = mean(value, na.rm = TRUE), se = sd(value, na.rm = TRUE)/sqrt(N)) %>% 
  select(-N) %>% 
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
  gather(key = Treatment, value = united, -blockID, -species, -pheno.unit, -pheno.stage, -pheno.var) %>% 
  separate(col = united, into = c("mean", "se"), sep = "_", convert = TRUE) %>% 
  filter(!is.na(mean)) %>% # NA's because Controls in some blocks are missing, no flowering plant in some blocks
  left_join(MetaData, by = c("blockID", "Treatment")) %>% 
  mutate(Treatment = factor(Treatment)) %>% 
  mutate(Treatment = plyr::mapvalues(Treatment, c("Warmer", "LaterSM", "WarmLate"), c("Warmer", "Later SM", "Warm & late SM"))) %>%
  mutate(Treatment = factor(Treatment, levels = c("Warmer", "Later SM", "Warm & late SM"))) %>%
  mutate(Shape = factor(Shape, levels = c("alpine-dry", "alpine-intermediate", "alpine-wet", "subalpine-dry", "subalpine-intermediate"))) %>% 
  mutate(Alpha = factor(Alpha, levels = c("alpine-dry", "alpine-intermediate", "alpine-wet", "subalpine-dry", "subalpine-intermediate")))



EventDiff %>% 
  filter(pheno.unit == "DaysSinceSM", pheno.stage == "Flower", pheno.var == "peak") %>%
  #group_by(species, Treatment) %>%
  #summarise(n = n()) %>% filter(n > 1) %>% distinct(species)
  ggplot(aes(x = SMDiff, y = mean, color = Treatment, shape = Shape, alpha = Alpha)) +
  geom_hline(yintercept = 0, color = "grey", linetype = "dashed") +
  geom_jitter() +
  labs(y = "Difference in phenological event [days/cumtemp] after SMT \n between treatment and origin-control", x = "Difference in SMT between destination and origin site [days]") +
  scale_colour_manual(name = "Treatment:", values = c("red", "blue", "purple")) +
  scale_shape_manual(name = "Climate Context:", values = c(17, 17, 17, 16, 16)) +
  scale_alpha_manual(name = "Climate Context:", values = c(1, 0.5, 1, 1, 0.5)) +
  facet_wrap(~ species)



EventDiff %>% 
  filter(pheno.unit == "DaysSinceSM", pheno.stage == "Flower", pheno.var == "first") %>%
  ggplot(aes(x = mean, y = species, color = Treatment, shape = Shape, alpha = Alpha)) +
  #geom_smooth(method = 'lm', formula = y ~ x, se = FALSE) +
  geom_point() +
  geom_vline(xintercept = 0, color = "grey", linetype = "dashed") +
  labs(y = "", x = "Difference in phenological event [days/cumtemp] after SMT between treatment and origin-control") +
  scale_colour_manual(name = "Treatment:", values = c("red", "blue", "purple")) +
  scale_shape_manual(name = "Climate Context:", values = c(2, 17, 17, 1, 16)) +
  scale_alpha_manual(name = "Climate Context:", values = c(1, 0.5, 1, 1, 0.5)) +
  facet_wrap(~ Treatment) +
  theme_minimal()
  


ddd <- EventDiffData %>% 
  filter(pheno.unit == "DaysSinceSM", pheno.stage == "Flower", pheno.var == "first") %>% 
  mutate(SMDiff_rescaled = (SMDiff - min(SMDiff)) / (max(SMDiff) - min(SMDiff)))


fit <- glmer(value ~ newTT + SMDiff_rescaled + (1|species) + (1|blockID), ddd, family = "poisson")  
fit <- glm(value ~ newTT * Temperature_level, ddd, family = "poisson")  
summary(fit)
plot(fit)
qqnorm(residuals(fit))


# Histogram of data
EventDiffData %>% 
  filter(pheno.unit == "DaysSinceSM", pheno.stage != "RipeSeed") %>% 
  ggplot(aes(x = value)) +
  geom_histogram() +
  facet_grid(pheno.stage ~ pheno.var)


####################
#### ADAPTATION ####
####################

### Select species, pheno.unit, etc.
EventDiffDataAdapt <- Phenology %>% 
  #filter(species %in% sp.list) %>% 
  filter(siteID != "Lavisdalen" | newTT != "Control") # remove Control at Lav

# Calculate SMDifference within siteID
SMDAdapt <- EventDiffDataAdapt %>% 
  filter(newTT != "Control") %>% 
  group_by(newTT, siteID, species) %>% 
  summarise(SMDiff = mean(SMDiff, na.rm = TRUE))


EventDiffAdapt <- EventDiffDataAdapt %>% 
  select(destBlockID, species, value, destSiteID, newTT, SMDiff, pheno.unit, pheno.stage, pheno.var) %>%
  group_by(newTT, destBlockID, species, pheno.unit, pheno.stage, pheno.var) %>% 
  # mean value per treatment, origin and destination site and species 
  summarise(N = n(), mean = mean(value, na.rm = TRUE), se = sd(value, na.rm = TRUE)/sqrt(N)) %>% 
  select(-N) %>% 
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
  gather(key = Treatment, value = united, -destBlockID, -species, -pheno.unit, -pheno.stage, -pheno.var) %>% 
  separate(col = united, into = c("mean", "se"), sep = "_", convert = TRUE) %>% 
  filter(!is.na(mean)) %>% # NA's because Controls in some blocks are missing, no flowering plant in some blocks
  left_join(MetaDataAdapt, by = c("destBlockID", "Treatment")) %>% 
  mutate(Treatment = factor(Treatment)) %>% 
  mutate(Treatment = plyr::mapvalues(Treatment, c("Warmer", "LaterSM", "WarmLate"), c("Warmer", "Later SM", "Warm & late SM"))) %>%
  mutate(Treatment = factor(Treatment, levels = c("Warmer", "Later SM", "Warm & late SM"))) %>%
  mutate(Shape = factor(Shape, levels = c("alpine-intermediate", "alpine-wet", "subalpine-dry", "subalpine-intermediate", "subalpine-wet"))) %>% 
  mutate(Alpha = factor(Alpha, levels = c("alpine-intermediate", "alpine-wet", "subalpine-dry", "subalpine-intermediate", "subalpine-wet")))


EventDiffAdapt %>% 
  filter(pheno.unit == "DaysSinceSM", pheno.stage == "Flower", pheno.var == "first") %>%
  #group_by(species, Treatment) %>%
  #summarise(n = n()) %>% filter(n > 1) %>% distinct(species)
  ggplot(aes(x = SMDiff, y = mean, color = Treatment, shape = Shape, alpha = Alpha)) +
  geom_hline(yintercept = 0, color = "grey", linetype = "dashed") +
  geom_jitter() +
  labs(y = "Difference in phenological event [days/cumtemp] after SMT \n between treatment and destination-control", x = "Difference in SMT between destination and origin site [days]") +
  scale_colour_manual(name = "Treatment:", values = c("red", "blue", "purple")) +
  scale_shape_manual(name = "Climate Context:", values = c(17, 17, 1, 16, 16)) +
  scale_alpha_manual(name = "Climate Context:", values = c(0.5, 1, 1, 0.5, 1)) +
  facet_wrap(~ species)



EventDiffAdapt %>% 
  filter(pheno.unit == "DaysSinceSM", pheno.stage == "Flower", pheno.var == "first") %>%
  ggplot(aes(x = mean, y = species, color = Treatment, shape = Shape, alpha = Alpha)) +
  #geom_smooth(method = 'lm', formula = y ~ x, se = FALSE) +
  geom_point() +
  geom_vline(xintercept = 0, color = "grey", linetype = "dashed") +
  labs(y = "", x = "Difference in phenological event [days/cumtemp] after SMT between treatment and destination-control") +
  scale_colour_manual(name = "Treatment:", values = c("red", "blue", "purple")) +
  scale_shape_manual(name = "Climate Context:", values = c(17, 17, 1, 16, 16)) +
  scale_alpha_manual(name = "Climate Context:", values = c(0.5, 1, 1, 0.5, 1)) +
  facet_wrap(~ Treatment) +
  theme_minimal()
