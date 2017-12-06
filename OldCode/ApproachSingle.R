#### GRADIENT ####
# SNOWMELT DATE
turfs.15 %>% 
  filter(siteID %in% c("Lavisdalen", "Gudmedalen", "Skjellingahaugen", "Hogsete", "Rambera", "Veskre")) %>% 
  mutate(siteID = factor(siteID, levels = c("Lavisdalen", "Gudmedalen", "Skjellingahaugen", "Hogsete", "Rambera", "Veskre"))) %>% # order levels
  ggplot() +
  geom_point(aes(x = jitter(Precipitation_level), y = o.dosm, color = factor(Precipitation_level), shape = factor(Temperature_level)), size = 3) +
  ylab("Day of snowmelt") + xlab("Precipitation level") +
  theme(legend.position= "top") + 
  scale_shape_manual(name = "Temperature:", labels = c("alpine", "subalpine"), values = c(17,16)) +
  scale_colour_manual(name = "Precipitation:", labels = c("dry", "intermediate", "wet"), values = precblue)

# GRADIENT PLOT
GradientPlotTemp <- Phenology %>% 
  filter(pheno.unit == "Days", newTT == "Control", !pheno.var == "duration", !pheno.stage == "SMBudDest") %>% 
  filter(pheno.stage != "RipeSeed") %>%
  group_by(Precipitation_level, Temperature_level, pheno.stage, pheno.var) %>% 
  summarize(mean = mean(value), sd = sd(value)) %>% 
  ggplot(aes(x = Precipitation_level, y = mean, color = factor(Precipitation_level), shape = factor(Temperature_level), group = factor(Temperature_level), ymin = (mean - sd), ymax = (mean + sd))) +
  geom_errorbar(color = "grey", width = 0.1, position = position_dodge(width = 0.2)) +
  geom_point(size = 3, position = position_dodge(width = 0.2)) +
  scale_shape_manual(name = "Temperature:", labels = c("alpine", "subalpine"), values = c(17,16)) +
  scale_colour_manual(name = "Precipitation:", labels = c("dry", "intermediate", "wet"), values = precblue) +
  facet_grid(pheno.stage ~ pheno.var, scales = "free")

GradientPlotDOY + theme_grey(base_size = 20) + theme(legend.title=element_blank()) + ggtitle("DOY")
GradientPlotSM + theme_grey(base_size = 20) + theme(legend.title=element_blank()) + ggtitle("Days since SM")
GradientPlotTemp + theme_grey(base_size = 20) + theme(legend.title=element_blank()) + ggtitle("Temp since SM")


# TEST DIFFERENCE BETWEEN C AND TT1
# Run model with and without treatment and compare with model selection
GlmerModelControls <- function(dd){
  print(unique(dd$pheno.var))
  mod01 <- glmer(value ~ TTtreat + (1|siteID) + (1|species), data = dd, family = "poisson")
  mod02 <- glmer(value ~ 1 + (1|siteID) + (1|species), data = dd, family = "poisson")
  modsel(list(mod01, mod02), 1000)
}

# subset data for unit and each stage. Makes 4 loops for each variable
ControlData <- Phenology %>% 
  filter(newTT == "Control") %>% 
  filter(pheno.unit == "DOY", pheno.var != "duration", pheno.stage == "Bud") %>% 
  group_by(pheno.var) %>% 
  do(GlmerModelControls(.))

# make plot
Phenology %>% 
  filter(newTT == "Control") %>% 
  filter(pheno.unit == "Temp since SM") %>% 
  ggplot() +
  geom_boxplot(aes(x = TTtreat, y = value)) +
  facet_grid(~pheno.var ~ pheno.stage)



#### WARM TREATMENT ####
# subset data
warm <- SelectDataForTreatment(plasticity, "plastic", "warm")
warmadapt <- SelectDataForTreatment(adaptation, "adapt", "warm")

### PLASTICITY
### MAKE PLOT PER PREC LEVEL
PrecLevelPlot(warm, "plastic", "DOY") + theme_grey(base_size = 20) + theme(legend.title=element_blank())
PrecLevelPlot(warm, "plastic", "Days") + theme_grey(base_size = 20) + theme(legend.title=element_blank())
PrecLevelPlot(warm, "plastic", "DaysSinceSM") + theme_grey(base_size = 20) + theme(legend.title=element_blank())
PrecLevelPlot(warm, "plastic", "TempSinceSM") + theme_grey(base_size = 20) + theme(legend.title=element_blank())


### Plot for duration
DurationPlot(warm, "plastic")

#### ANALYSE PER TREATMENT ####
dd <- SelectData(warm, p.unit = "DOY", p.stage = "Flower", p.var = "first")
ModelSelectionPhenology(dd = dd, dat = "plastic")
PredictResponsePhenology(dd, "plastic")

# Get Predictions
newvalues <- warm %>% 
  filter(pheno.unit == "Days") %>% 
  group_by(pheno.unit, pheno.stage, pheno.var) %>% 
  do(PredictResponsePhenology(dd=., "plastic"))
  
# DIFFERENCE PLOT
newvalues %>% 
  group_by(pheno.stage, pheno.var, Precipitation_level) %>% 
  ggplot(aes(x = pheno.var, y = diff, color = factor(Precipitation_level))) +
  geom_point(stat = "identity", size = 3) +
  scale_colour_manual(name = "Precipitation:", labels = c("dry", "intermediate", "wet"), values = precblue) +
  ylim(-40, 40) +
  facet_wrap(~pheno.stage)


### ADAPTATION
PrecLevelPlot(warmadapt, "adapt", "DOY") + theme_grey(base_size = 20) + theme(legend.title=element_blank())
PrecLevelPlot(warmadapt, "adapt", "Days since SM dest") + theme_grey(base_size = 20) + theme(legend.title=element_blank())
PrecLevelPlot(warmadapt, "adapt", "Temp since SM dest") + theme_grey(base_size = 20) + theme(legend.title=element_blank())

DurationPlot(warmadapt, "adapt")

#### ANALYSE PER TREATMENT ####
dd <- warmadapt %>% filter(pheno.unit == "Temp since SM dest", pheno.stage == "Seed", pheno.var == "first")
ModelSelectionPhenology(dd = dd, dat = "adapt")
newdat <- PredictResponsePhenology(dd, "adapt")
newdat %>% spread(key = newTT, value = value) %>% mutate(diff = Warm - Control)

table(warmadapt$species, warmadapt$newTT)
# interesting sp: Leo.aut, Pin.vul, Car.cap, Ave.fle, Bis.viv, Des.ces, Fes.ovi
Antodo <- warmadapt %>% filter(species == "Bis.viv")
PrecLevelPlot(Antodo, "adapt", "DOY") + theme_grey(base_size = 20) + theme(legend.title=element_blank())






ModelSelectionPhenology <- function(dd, dat){
  if(dat == "plastic"){
    if(unique(dd$pheno.unit) != "TempSinceSM"){
      # run models
      mod01 <- glmer(value ~ newTT*factor(Precipitation_level) + (1|species), data = dd, family = "poisson")
      mod02 <- glmer(value ~ newTT+factor(Precipitation_level) + (1|species), data = dd, family = "poisson")
      mod03 <- glmer(value ~ newTT + (1|species), data = dd, family = "poisson")
      mod04 <- glmer(value ~ factor(Precipitation_level) + (1|species), data = dd, family = "poisson")
      mod05 <- glmer(value ~ 1 + (1|species), data = dd, family = "poisson")
    }
    else if(unique(dd$pheno.unit) == "TempSinceSM"){
      mod01 <- lmer(value ~ newTT*factor(Precipitation_level) + (1|species), data = dd)
      mod02 <- lmer(value ~ newTT+factor(Precipitation_level) + (1|species), data = dd)
      mod03 <- lmer(value ~ newTT + (1|species), data = dd)
      mod04 <- lmer(value ~ factor(Precipitation_level) + (1|species), data = dd)
      mod05 <- lmer(value ~ 1 + (1|species), data = dd)
    }
  }
  else if(dat == "adapt"){
    if(unique(dd$pheno.unit) != "TempSinceSMDest"){
      # run models
      mod01 <- glmer(value ~ newTT*factor(destP_level) + (1|species), data = dd, family = "poisson")
      mod02 <- glmer(value ~ newTT+factor(destP_level) + (1|species), data = dd, family = "poisson")
      mod03 <- glmer(value ~ newTT + (1|species), data = dd, family = "poisson")
      mod04 <- glmer(value ~ factor(destP_level) + (1|species), data = dd, family = "poisson")
      mod05 <- glmer(value ~ 1 + (1|species), data = dd, family = "poisson")
    }
    else if(unique(dd$pheno.unit) == "TempSinceSMDest"){
      mod01 <- lmer(value ~ newTT*factor(destP_level) + (1|species), data = dd)
      mod02 <- lmer(value ~ newTT+factor(destP_level) + (1|species), data = dd)
      mod03 <- lmer(value ~ newTT + (1|species), data = dd)
      mod04 <- lmer(value ~ factor(destP_level) + (1|species), data = dd)
      mod05 <- lmer(value ~ 1 + (1|species), data = dd)
    }
  }
  
  # model selection
  print(unique(paste(dd$pheno.unit, dd$pheno.var, dd$pheno.stage, sep = "_")))
  modsel(list(mod01, mod02, mod03, mod04, mod05), 1000)
}



PredictResponsePhenology <- function(dd, dat){
  # create new data
  if(dat == "plastic"){
    if(unique(dd$pheno.unit) != "TempSinceSM"){
      mod01 <- glmer(value ~ newTT*factor(Precipitation_level) + (1|species), data = dd, family = "poisson")
    }
    else if(unique(dd$pheno.unit) == "TempSinceSM"){
      mod01 <- lmer(value ~ newTT*factor(Precipitation_level) + (1|species), data = dd)
    }
    newdat <- expand.grid(
      newTT=c("Control", "Warm")
      , Precipitation_level=c(2,3,4)
      , value = 0
    )
  }
  else if(dat == "adapt"){
    if(unique(dd$pheno.unit) != "TempSinceSMDest"){
      mod01 <- glmer(value ~ newTT*factor(destP_level) + (1|species), data = dd, family = "poisson")
    }
    else if(unique(dd$pheno.unit) == "TempSinceSMDest"){
      mod01 <- lmer(value ~ newTT*factor(destP_level) + (1|species), data = dd)
    }
    newdat <- expand.grid(
      newTT=c("Control", "Warm")
      , destP_level=c(2,3,4)
      , value = 0
    )
  }
  mm <- model.matrix(terms(mod01), newdat)
  newdat$value <- predict(mod01, newdat,re.form=NA, type="response")  # re.form=NA without random part; re.form=NULL with random part
  newdat <- newdat %>% spread(key = newTT, value = value) %>% mutate(diff = Warm - Control)
  return(newdat)
}



# analyse by hand
ddd <- warm %>% filter(pheno.unit == "DOY", pheno.stage == "Flower", pheno.var == "peak")
mod01 <- glmer(value ~ newTT*factor(Precipitation_level) + (1|species), data = ddd, family = "poisson")
mod02 <- glmer(value ~ newTT+factor(Precipitation_level) + (1|species), data = ddd, family = "poisson")
mod03 <- glmer(value ~ newTT + (1|species), data = ddd, family = "poisson")
mod04 <- glmer(value ~ factor(Precipitation_level) + (1|species), data = ddd, family = "poisson")
mod05 <- glmer(value ~ 1 + (1|species), data = ddd, family = "poisson")
modsel(list(mod01, mod02, mod03, mod04, mod05), 1000)
summary(mod)

# Calculate Difference between Control and Treamtent
newdat %>%
  spread(key = newTT, value = value) %>% 
  mutate(diff = Warm - Control)




#### WET TREATMENT ####
# subset data
wet <- SelectDataForTreatment(plasticity, "plastic", "wet")
wetadapt <- SelectDataForTreatment(adaptation, "adapt", "wet")

PrecTempLevelPlot(wet, "plastic", "DOY") + theme_grey(base_size = 20) + theme(legend.title=element_blank())
PrecTempLevelPlot(wet, "plastic", "Days since SM") + theme_grey(base_size = 20) + theme(legend.title=element_blank())
PrecTempLevelPlot(wet, "plastic", "Temp since SM") + theme_grey(base_size = 20) + theme(legend.title=element_blank())

DurationPrecTempPlot(wet, "plastic")


dd <- wet %>% filter(pheno.unit == "DOY", pheno.stage == "Seed", pheno.var == "first")
mod01 <- glmer(value ~ newTT*factor(Precipitation_level)*factor(Temperature_level) + (1|species), data = dd, family = "poisson")
mod02 <- glmer(value ~ newTT+factor(Precipitation_level)*factor(Temperature_level) + (1|species), data = dd, family = "poisson")
mod03 <- glmer(value ~ newTT*factor(Precipitation_level)+factor(Temperature_level) + (1|species), data = dd, family = "poisson")
mod04 <- glmer(value ~ newTT+factor(Precipitation_level)+factor(Temperature_level) + (1|species), data = dd, family = "poisson")
mod05 <- glmer(value ~ newTT*factor(Precipitation_level) + (1|species), data = dd, family = "poisson")
mod06 <- glmer(value ~ newTT+factor(Precipitation_level) + (1|species), data = dd, family = "poisson")
mod07 <- glmer(value ~ newTT*factor(Temperature_level) + (1|species), data = dd, family = "poisson")
mod08 <- glmer(value ~ newTT+factor(Temperature_level) + (1|species), data = dd, family = "poisson")
mod09 <- glmer(value ~ newTT + (1|species), data = dd, family = "poisson")
mod10 <- glmer(value ~ 1 + (1|species), data = dd, family = "poisson")
modsel(list(mod01, mod02, mod03, mod04, mod05, mod06, mod07, mod08, mod09, mod10), 1000)

newdat <- expand.grid(
  newTT=c("Control", "Wet")
  , Precipitation_level=c(2,3)
  , Temperature_level=c(1,2)
  , value = 0
)
mm <- model.matrix(terms(mod01), newdat)
newdat$value <- predict(mod01, newdat,re.form=NA, type="response")
newdat %>% spread(key = newTT, value = value) %>% mutate(diff = Wet - Control)


# TEMPERATURE SINCE SM
dd <- wet %>% filter(pheno.unit == "Temp since SM", pheno.stage == "Flower", pheno.var == "first")
mod01 <- lmer(value ~ newTT*factor(Precipitation_level)*factor(Temperature_level) + (1|species), data = dd)
mod02 <- lmer(value ~ newTT+factor(Precipitation_level)*factor(Temperature_level) + (1|species), data = dd)
mod03 <- lmer(value ~ newTT*factor(Precipitation_level)+factor(Temperature_level) + (1|species), data = dd)
mod04 <- lmer(value ~ newTT+factor(Precipitation_level)+factor(Temperature_level) + (1|species), data = dd)
mod05 <- lmer(value ~ newTT*factor(Precipitation_level) + (1|species), data = dd)
mod06 <- lmer(value ~ newTT+factor(Precipitation_level) + (1|species), data = dd)
mod07 <- lmer(value ~ newTT*factor(Temperature_level) + (1|species), data = dd)
mod08 <- lmer(value ~ newTT+factor(Temperature_level) + (1|species), data = dd)
mod09 <- lmer(value ~ newTT + (1|species), data = dd)
mod10 <- lmer(value ~ 1 + (1|species), data = dd)
modsel(list(mod01, mod02, mod03, mod04, mod05, mod06, mod07, mod08, mod09, mod10), 1000)

newdat <- expand.grid(
  newTT=c("Control", "Wet")
  , Precipitation_level=c(2,3)
  , Temperature_level=c(1,2)
  , value = 0
)
mm <- model.matrix(terms(mod01), newdat)
newdat$value <- predict(mod01, newdat,re.form=NA, type="response")
newdat %>% spread(key = newTT, value = value) %>% mutate(diff = Wet - Control)
hist(dd$value)

PrecTempLevelPlot(wetadapt, "adapt", "DOY") + theme_grey(base_size = 20) + theme(legend.title=element_blank())
PrecTempLevelPlot(wetadapt, "adapt", "Days since SM") + theme_grey(base_size = 20) + theme(legend.title=element_blank())
PrecTempLevelPlot(wetadapt, "adapt", "Temp since SM") + theme_grey(base_size = 20) + theme(legend.title=element_blank())

table(wetadapt$species, wetadapt$newTT)
# interesting sp: Leo.aut, Pin.vul, Car.cap, Ave.fle, Bis.viv, Des.ces, Fes.ovi
Antodo <- wetadapt %>% filter(species == "Pin.vul")
PrecTempLevelPlot(Antodo, "adapt", "Temp since SM dest") + theme_grey(base_size = 20) + theme(legend.title=element_blank())

dd <- wetadapt %>% filter(pheno.unit == "DOY", pheno.stage == "Bud", pheno.var == "first")
#mod01 <- glmer(value ~ newTT*factor(Precipitation_level)*factor(Temperature_level) + (1|species), data = dd, family = "poisson")
mod02 <- glmer(value ~ newTT+factor(Precipitation_level)*factor(Temperature_level) + (1|species), data = dd, family = "poisson")
#mod03 <- glmer(value ~ newTT*factor(Precipitation_level)+factor(Temperature_level) + (1|species), data = dd, family = "poisson")
mod04 <- glmer(value ~ newTT+factor(Precipitation_level)+factor(Temperature_level) + (1|species), data = dd, family = "poisson")
#mod05 <- glmer(value ~ newTT*factor(Precipitation_level) + (1|species), data = dd, family = "poisson")
mod06 <- glmer(value ~ newTT+factor(Precipitation_level) + (1|species), data = dd, family = "poisson")
mod07 <- glmer(value ~ newTT*factor(Temperature_level) + (1|species), data = dd, family = "poisson")
mod08 <- glmer(value ~ newTT+factor(Temperature_level) + (1|species), data = dd, family = "poisson")
mod09 <- glmer(value ~ newTT + (1|species), data = dd, family = "poisson")
mod10 <- glmer(value ~ 1 + (1|species), data = dd, family = "poisson")
modsel(list(mod02, mod04, mod06, mod07, mod08, mod09, mod10), 1000)

newdat <- expand.grid(
  newTT=c("Control", "Wet")
  , Precipitation_level=c(2,3)
  , Temperature_level=c(1,2)
  , value = 0
)
mm <- model.matrix(terms(mod02), newdat)
newdat$value <- predict(mod02, newdat,re.form=NA, type="response")
newdat %>% spread(key = newTT, value = value) %>% mutate(diff = Wet - Control)


#### WARM & WET TREATMENT ####
# subset data
warmwet <- SelectDataForTreatment(plasticity, "plastic", "ww")
warmwetadapt <- SelectDataForTreatment(adaptation, "adapt", "ww")

PrecLevelPlot(warmwet, "plastic", "DOY") + theme_grey(base_size = 20) + theme(legend.title=element_blank())
PrecLevelPlot(warmwet, "plastic", "Days since SM") + theme_grey(base_size = 20) + theme(legend.title=element_blank())
PrecLevelPlot(warmwet, "plastic", "Temp since SM") + theme_grey(base_size = 20) + theme(legend.title=element_blank())


dd <- warmwet %>% filter(pheno.unit == "DOY", pheno.stage == "Flower", pheno.var == "end")
mod01 <- glmer(value ~ newTT*factor(Precipitation_level) + (1|species), data = dd, family = "poisson")
mod02 <- glmer(value ~ newTT+factor(Precipitation_level) + (1|species), data = dd, family = "poisson")
mod03 <- glmer(value ~ factor(Precipitation_level) + (1|species), data = dd, family = "poisson")
mod04 <- glmer(value ~ newTT + (1|species), data = dd, family = "poisson")
mod05 <- glmer(value ~ 1 + (1|species), data = dd, family = "poisson")
modsel(list(mod01, mod02, mod03, mod04, mod05), 1000)

newdat <- expand.grid(
  newTT=c("Control", "Warm & wet")
  , Precipitation_level=c(2,3)
  , value = 0
)
mm <- model.matrix(terms(mod02), newdat)
newdat$value <- predict(mod02, newdat,re.form=NA, type="response")
newdat %>% spread(key = newTT, value = value)


### ADAPTATION
PrecLevelPlot(warmwetadapt, "adapt", "DOY") + theme_grey(base_size = 20) + theme(legend.title=element_blank())
PrecLevelPlot(warmwetadapt, "adapt", "Days since SM") + theme_grey(base_size = 20) + theme(legend.title=element_blank())
PrecLevelPlot(warmwetadapt, "adapt", "Temp since SM") + theme_grey(base_size = 20) + theme(legend.title=element_blank())

