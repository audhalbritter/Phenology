
#### ORIGIN 

# Run model and predict
GlmerModelAndPredictOrigin2 <- function(dd){
  if(unique(dd$pheno.unit) == "Temp since SM"){
    modelfit <- lmer(value ~ Temp_value * Prec_value + TransWarm + TransWet + (1|species), data = dd)
  }
  else if(unique(dd$pheno.unit) != "Temp since SM"){
    modelfit <- glmer(value ~ Temp_value * Prec_value + TransWarm + TransWet + (1|species), data = dd, family = "poisson")
  }
  newdat <- expand.grid(
    Temp_value=c(0, 0.6)
    , Prec_value=c(0.2857143, 0.6666667, 1)
    #, newTT=c("Control", "Warm", "Wet", "Warm & wet")
    , TransWarm=c(0,1)
    , TransWet=c(0,1)
    , value = 0
  )
  mm <- model.matrix(terms(modelfit), newdat)
  newdat$value <- predict(modelfit,newdat,re.form=NA, type="response")  # re.form=NA without random part; re.form=NULL with random part
  
  means <- newdat %>% 
    group_by(Temp_value, Prec_value, TransWarm, TransWet) %>% 
    summarize(mean = mean(value), sd = sd(value))
  return(means)
}

# Create data and run prediction
OrigDat2 <- pheno.long %>% 
  filter(pheno.unit %in% c("doy", "o.snowmelt", "oCumTemp")) %>% # for origin
  filter(pheno.stage != "r") %>%
  filter(pheno.var != "duration") %>% 
  filter(Precipitation_level != 1) %>% #remove turfs transplanted from Ulv and Alr, because they have no control
  mutate(pheno.var = factor(pheno.var, levels = c("first", "peak", "end"))) %>% 
  mutate(newTT = plyr::mapvalues(newTT, c("control", "TT2", "TT3", "TT4"), c("Control", "Warm", "Wet", "Warm & wet"))) %>%
  mutate(newTT = factor(newTT, levels = c("Control", "Warm", "Wet", "Warm & wet"))) %>% 
  mutate(pheno.stage = plyr::mapvalues(pheno.stage, c("b", "f", "s"), c("Bud", "Flower", "Seed"))) %>%
  mutate(pheno.stage = factor(pheno.stage, levels = c("Bud", "Flower", "Seed"))) %>% 
  mutate(pheno.unit = plyr::mapvalues(pheno.unit, c("doy", "o.snowmelt", "oCumTemp"), c("DOY", "Days since SM", "Temp since SM"))) %>%
  mutate(pheno.unit = factor(pheno.unit, levels = c("DOY", "Days since SM", "Temp since SM"))) %>%
  group_by(pheno.unit, pheno.var, pheno.stage) %>% 
  mutate_each(funs(as.factor), species, flowering.time, functionalGroup, occurrence.2) %>% 
  mutate(TransWarm = ifelse(newTT %in% c("Warm", "Warm & wet"), 1, 0)) %>% 
  mutate(TransWet = ifelse(newTT %in% c("Wet", "Warm & wet"), 1, 0)) %>%
  mutate(NoTrans = ifelse(newTT == "control", 1, 0)) %>%
  do(GlmerModelAndPredictOrigin2(.))

OrigDat1 %>% 
  filter(pheno.unit=="Temp since SM") %>% 
  ggplot2::ggplot(aes(x = value)) +
  geom_histogram(binwidth = 10) +
  facet_wrap(~pheno.stage ~ pheno.var)


# TEST BY HAND
dd <- OrigDat2 %>% filter(pheno.unit=="DOY", pheno.var=="first", pheno.stage == "Bud") %>% 
  ggplot2::ggplot(aes(x = value)) +
  geom_histogram(binwidth = 10) +
  facet_wrap(~pheno.stage ~ pheno.var)

# poisson model
dd <- OrigDat1 %>% filter(pheno.unit=="Temp since SM", pheno.var=="end", pheno.stage == "Seed") %>% 
  group_by(pheno.var, pheno.stage) %>% 
  do(RunModel(.))

RunModel <- function(dd){
  mod <- glmer(value ~ Temp_value * Prec_value + TransWarm + TransWet + (1|species), data = dd, family = "poisson")
  mod
}



ModelFull <- glmer(value ~ Temp_value * Prec_value + TransWarm + TransWet + (1|species), data = dd, family = "poisson")
ModelWarm <- glmer(value ~ Temp_value * Prec_value + TransWarm + (1|species), data = dd, family = "poisson")
ModelWet <- glmer(value ~ Temp_value * Prec_value + TransWet + (1|species), data = dd, family = "poisson")
ModelNull <- glmer(value ~ Temp_value * Prec_value + (1|species), data = dd, family = "poisson")
modsel(list(ModelFull, ModelWarm, ModelWet, ModelNull), 1000)
modsel(list(ModelWarm, ModelWet, ModelNull), 1000)
OrigDat2 %>% filter(pheno.unit=="DOY", pheno.stage=="Flower") %>% ModelSelectionOrig(.)

# normal model for temp
dd <- OrigDat1 %>% filter(pheno.unit=="Temp since SM", pheno.var=="end", pheno.stage == "Bud")
ModelFull <- lmer(value ~ Temp_value * Prec_value + TransWarm + TransWet + (1|species), data = dd)
ModelWarm <- lmer(value ~ Temp_value * Prec_value + TransWarm  + (1|species), data = dd)
ModelWet <- lmer(value ~ Temp_value * Prec_value + TransWet + (1|species), data = dd)
ModelNull <- lmer(value ~ Temp_value * Prec_value + (1|species), data = dd)
modsel(list(ModelFull, ModelWarm, ModelWet, ModelNull), 1000)


# MAKE PLOT
newplot <- OrigDat2 %>% 
  mutate(Treatment = paste(TransWarm, TransWet, sep = "")) %>% 
  mutate(Treatment = plyr::mapvalues(Treatment, c("00", "01", "10", "11"), c("Control", "Wet", "Warm", "Warm & wet"))) %>%
  mutate(Treatment = factor(Treatment, levels = rev(c("Control", "Warm", "Wet", "Warm & wet")))) %>% 
  ggplot() +
  geom_point(aes(x = mean, y = Treatment, shape = pheno.var, color = pheno.var), size = 3) +
  ylab(paste("")) + xlab(paste("Estimated mean value")) +
  scale_shape_manual(name = "", values = c(16,17,15)) +
  scale_colour_manual(name = "", values = c(col.red, col.dblue, col.orange)) +
  #geom_errorbarh(aes(xmin=mean-sd, xmax=mean+sd), height=0.1, color = "gray") +
  facet_grid(~pheno.stage ~ pheno.unit, scales = "free")

newplot + theme_grey(base_size = 20) + theme(legend.title=element_blank()) 
FunctionalGroup + theme_grey(base_size = 20) + theme(legend.title=element_blank())   
 
# calc difference between treatments
OrigDat2 %>% 
  mutate(Treatment = paste(TransWarm, TransWet, sep = "")) %>% 
  mutate(Treatment = plyr::mapvalues(Treatment, c("00", "01", "10", "11"), c("Control", "Wet", "Warm", "Warm & wet"))) %>%
  mutate(Treatment = factor(Treatment, levels = rev(c("Control", "Warm", "Wet", "Warm & wet")))) %>% 
  select(pheno.unit, pheno.var, pheno.stage, mean, Treatment) %>% 
  spread(key = Treatment, value = mean) %>% 
  gather(key = Treatment, value = value, -pheno.stage, -pheno.var, -pheno.unit, -Control) %>% # devide control and treamtents
  mutate(diff = (Control - value)) %>% 
  filter(pheno.unit == "DOY", pheno.var == "first", pheno.stage == "Flower")


#### LEVEL PLOT ####
GlmerModelAndPredictOrigin3 <- function(dd){
  if(unique(dd$pheno.unit) == "oCumTemp"){
    modelfit <- lmer(value ~ Temp_value * Prec_value + TransWarm + TransWet + (1|species), data = dd)
  }
  else if(unique(dd$pheno.unit) != "oCumTemp"){
    modelfit <- glmer(value ~ Temp_value * Prec_value + TransWarm + TransWet + (1|species), data = dd, family = "poisson")
  }
  newdat <- expand.grid(
    Temp_value=c(0, 0.6)
    , Prec_value=c(0, 0.2857143, 0.6666667, 1)
    #, newTT=c("Control", "Warm", "Wet", "Warm & wet")
    , TransWarm=c(0,1)
    , TransWet=c(0,1)
    #, functionalGroup=c("forb", "graminoid")
    #, flowering.time=c("always", "early", "late")
    , value = 0
  )
  mm <- model.matrix(terms(modelfit), newdat)
  newdat$value <- predict(modelfit,newdat,re.form=NA, type="response")  # re.form=NA without random part; re.form=NULL with random part
  
  means <- newdat %>% 
    group_by(Temp_value, Prec_value, TransWarm, TransWet) %>% 
    summarize(mean = mean(value), sd = sd(value))
  return(means)
}

# Create data and run prediction
OrigDat3 <- pheno.long %>% 
  filter(pheno.unit %in% c("doy", "o.snowmelt", "oCumTemp")) %>% # for origin
  filter(pheno.stage != "r") %>%
  filter(pheno.var != "duration") %>% 
  mutate(pheno.var = factor(pheno.var, levels = c("first", "peak", "end"))) %>% 
  mutate(newTT = plyr::mapvalues(newTT, c("control", "TT2", "TT3", "TT4"), c("Control", "Warm", "Wet", "Warm & wet"))) %>%
  mutate(newTT = factor(newTT, levels = c("Control", "Warm", "Wet", "Warm & wet"))) %>% 
  mutate(pheno.stage = plyr::mapvalues(pheno.stage, c("b", "f", "s"), c("Bud", "Flower", "Seed"))) %>%
  mutate(pheno.stage = factor(pheno.stage, levels = c("Bud", "Flower", "Seed"))) %>% 
  mutate(pheno.unit = plyr::mapvalues(pheno.unit, c("doy", "o.snowmelt", "oCumTemp"), c("DOY", "Days since SM", "Temp since SM"))) %>%
  mutate(pheno.unit = factor(pheno.unit, levels = c("DOY", "Days since SM", "Temp since SM"))) %>%
  group_by(pheno.unit, pheno.var, pheno.stage) %>% 
  mutate_each(funs(as.factor), species, flowering.time, functionalGroup, occurrence.2) %>% 
  mutate(TransWarm = ifelse(newTT %in% c("Warm", "Warm & wet"), 1, 0)) %>% 
  mutate(TransWet = ifelse(newTT %in% c("Wet", "Warm & wet"), 1, 0)) %>%
  mutate(NoTrans = ifelse(newTT == "control", 1, 0)) %>%
  do(GlmerModelAndPredictOrigin3(.))

### New Plot with Temp and Prec levels
LevelPlot <- OrigDat3 %>% 
  filter(pheno.unit == "DOY", pheno.stage == "Flower") %>%
  filter(Prec_value != "0") %>% 
  mutate(Treatment = paste(TransWarm, TransWet, sep = "")) %>% 
  mutate(Treatment = plyr::mapvalues(Treatment, c("00", "01", "10", "11"), c("Control", "Wet", "Warm", "Warm & wet"))) %>%
  mutate(Treatment = factor(Treatment, levels = rev(c("Control", "Warm", "Wet", "Warm & wet")))) %>% 
  mutate(Temp_value = plyr::mapvalues(Temp_value, c(0, 0.6), c("alpine", "intermediate"))) %>%
  mutate(Temp_value = factor(Temp_value, levels = c("alpine", "intermediate"))) %>%
  mutate(Prec_value = plyr::mapvalues(Prec_value, c(0.2857143, 0.6666667, 1), c("dry", "intermediate", "wet"))) %>%
  mutate(Prec_value = factor(Prec_value, levels = c("dry", "intermediate", "wet"))) %>%
  ggplot() +
  geom_point(aes(x = mean, y = Treatment, shape = Temp_value, color = Prec_value), size = 3) +
  ylab(paste("")) + xlab(paste("Mean value")) +
  ggtitle("Flowering in DOY") +
  scale_shape_manual(name = "", values = c(16,17,15)) +
  scale_colour_manual(name = "Precipitation:", labels = c("dry", "intermediate", "wet"), values = c("lightblue","blue", "darkblue")) +
  #geom_errorbarh(aes(xmin=mean-sd, xmax=mean+sd), height=0.1, color = "gray") +
  facet_grid(~pheno.var) +
  coord_fixed(ratio = 18)
LevelPlot + theme_grey(base_size = 20) + theme(legend.title=element_blank())



#### DURATION ####
GlmerModelAndPredictDurationOrigin <- function(dd){
  dd$newFactor <- rnorm(nrow(dd))
  modelfit <- glmer(value ~ Temp_value * Prec_value + TransWarm + TransWet + (1|species) + (1|newFactor), data = dd, family = "poisson")
  newdat <- expand.grid(
    Temp_value=c(0, 0.6)
    , Prec_value=c(0, 0.2857143, 0.6666667, 1)
    , TransWarm=c(0,1)
    , TransWet=c(0,1)
    , value = 0
  )
  mm <- model.matrix(terms(modelfit), newdat)
  newdat$value <- predict(modelfit,newdat,re.form=NA, type="response")  # re.form=NA without random part; re.form=NULL with random part
  
  means <- newdat %>% 
    group_by(TransWarm, TransWet) %>% 
    summarize(mean = mean(value), sd = sd(value))
  return(means)
}

OrigDatDuration <- pheno.long %>% 
  filter(pheno.unit == "doy", pheno.var == "duration") %>%
  filter(pheno.stage != "r") %>%
  mutate(pheno.var = factor(pheno.var, levels = c("duration"))) %>% 
  mutate(newTT = plyr::mapvalues(newTT, c("control", "TT2", "TT3", "TT4"), c("Control", "Warm", "Wet", "Warm & wet"))) %>%
  mutate(newTT = factor(newTT, levels = rev(c("Control", "Warm", "Wet", "Warm & wet")))) %>% 
  mutate(pheno.stage = plyr::mapvalues(pheno.stage, c("b", "f", "s"), c("Bud", "Flower", "Seed"))) %>%
  mutate(pheno.stage = factor(pheno.stage, levels = c("Bud", "Flower", "Seed"))) %>% 
  mutate(pheno.unit = plyr::mapvalues(pheno.unit, c("doy"), c("DOY"))) %>%
  mutate(pheno.unit = factor(pheno.unit, levels = c("DOY"))) %>%
  group_by(pheno.stage) %>% 
  mutate_each(funs(as.factor), species, newTT) %>% 
  mutate(TransWarm = ifelse(newTT %in% c("Warm", "Warm & wet"), 1, 0)) %>% 
  mutate(TransWet = ifelse(newTT %in% c("Wet", "Warm & wet"), 1, 0)) %>% 
  do(GlmerModelAndPredictDurationOrigin(.))

OriginDurationPlot <- OrigDatDuration %>% 
  mutate(Treatment = paste(TransWarm, TransWet, sep = "")) %>% 
  mutate(Treatment = plyr::mapvalues(Treatment, c("00", "01", "10", "11"), c("Control", "Wet", "Warm", "Warm & wet"))) %>%
  mutate(Treatment = factor(Treatment, levels = rev(c("Control", "Warm", "Wet", "Warm & wet")))) %>% 
  ggplot() +
  geom_point(aes(x = mean, y = Treatment), size = 3) +
  ylab(paste("")) + xlab(paste("Estimated duration in days")) +
  facet_grid(~pheno.stage) +
  coord_fixed(ratio = 5)

OriginDurationPlot + theme_grey(base_size = 20) + theme(legend.title=element_blank())

# ONLY DATA
OrigDatDuration1 <- pheno.long %>% 
  filter(pheno.unit == "doy", pheno.var == "duration") %>%
  filter(pheno.stage != "r") %>%
  mutate(pheno.var = factor(pheno.var, levels = c("duration"))) %>% 
  mutate(newTT = plyr::mapvalues(newTT, c("control", "TT2", "TT3", "TT4"), c("Control", "Warm", "Wet", "Warm & wet"))) %>%
  mutate(newTT = factor(newTT, levels = rev(c("Control", "Warm", "Wet", "Warm & wet")))) %>% 
  mutate(pheno.stage = plyr::mapvalues(pheno.stage, c("b", "f", "s"), c("Bud", "Flower", "Seed"))) %>%
  mutate(pheno.stage = factor(pheno.stage, levels = c("Bud", "Flower", "Seed"))) %>% 
  mutate(pheno.unit = plyr::mapvalues(pheno.unit, c("doy"), c("DOY"))) %>%
  mutate(pheno.unit = factor(pheno.unit, levels = c("DOY"))) %>%
  group_by(pheno.stage) %>% 
  mutate_each(funs(as.factor), species, newTT) %>% 
  mutate(TransWarm = ifelse(newTT %in% c("Warm", "Warm & wet"), 1, 0)) %>% 
  mutate(TransWet = ifelse(newTT %in% c("Wet", "Warm & wet"), 1, 0))

# TEST BY HAND
dd <- OrigDatDuration1 %>% filter(pheno.stage == "Seed")
dd$newFactor <- rnorm(nrow(dd))
ModelFull <- glmer(value ~ Temp_value * Prec_value + TransWarm + TransWet + (1|species) + (1|newFactor), data = dd, family = "poisson")
disp(ModelFull, OrigDatDuration1)
ModelWarm <- glmer(value ~ Temp_value * Prec_value + TransWarm + (1|species) + (1|newFactor), data = dd, family = "poisson")
ModelWet <- glmer(value ~ Temp_value * Prec_value + TransWet + (1|species) + (1|newFactor), data = dd, family = "poisson")
ModelNull <- glmer(value ~ Temp_value * Prec_value + (1|species) + (1|newFactor), data = dd, family = "poisson")
modsel(list(ModelFull, ModelWarm, ModelWet, ModelNull), 1000)

dd <- TestData %>% filter(pheno.unit == "DOY", pheno.var == "first", pheno.stage == "Flower")
modelfit <- glmer(value ~ Temp_value * Prec_value + TransWarm + (1|species), data = dd, family = "poisson")
summary(modelfit)

newdat <- expand.grid(
  Temp_value=c(0, 0.6)
  , Prec_value=c(0, 0.2857143, 0.6666667, 1)
  , newTT=c("Control", "Warm", "Wet", "Warm & wet")
  , TransWarm=c(0,1)
  , TransWet=c(0,1)
  #, functionalGroup=c("forb", "graminoid")
  #, flowering.time=c("always", "early", "late")
  , value = 0
)
mm <- model.matrix(terms(modelfit), newdat)
newdat$value <- predict(modelfit,newdat,re.form=NA, type="response")  # re.form=NA without random part; re.form=NULL with random part

newdat %>% 
  group_by(TransWarm, TransWet) %>% 
  summarize(mean = mean(value), sd = sd(value))


#### FUNCTIONS
QAICc <- function(mod, scale, QAICc = TRUE) {
  ll <- as.numeric(logLik(mod))
  df <- attr(logLik(mod), "df")
  n <- length(resid(mod))
  if (QAICc)
    qaic = as.numeric(-2 * ll/scale + 2 * df + 2 * df * (df + 1)/(n - df - 1))
  else qaic = as.numeric(-2 * ll/scale + 2 * df)
  qaic
}


## code for model selection. First fit mod01, then run this code.
modsel <- function(mods,x){	
  phi=1
  dd <- data.frame(Model=1:length(mods), K=1, QAIC=1)
  for(j in 1:length(mods)){
    dd$K[j] = attr(logLik(mods[[j]]),"df")
    dd$QAIC[j] = QAICc(mods[[j]],phi)
  }
  dd$delta.i <- dd$QAIC - min(dd$QAIC)
  dd <- subset(dd,dd$delta.i<x)
  dd$re.lik <- round(exp(-0.5*dd$delta.i),3)
  sum.aic <- sum(exp(-0.5*dd$delta.i))
  wi <- numeric(0)
  for (i in 1:length(dd$Model)){wi[i] <- round(exp(-0.5*dd$delta.i[i])/sum.aic,3)}; dd$wi<-wi
  print(dds <- dd[order(dd$QAIC), ])
  assign("mstable",dd,envir=.GlobalEnv)
}




#### DESTINATION 

# Run model and predict
GlmerModelAndPredictDestination2 <- function(dd){
  if(unique(dd$pheno.unit) == "Temp since SM"){
    modelfit <- lmer(value ~ destTemp_value * destPrec_value + TransWarm + TransWet + (1|species), data = dd)
  }
  else if(unique(dd$pheno.unit) != "Temp since SM"){
    modelfit <- glmer(value ~ destTemp_value * destPrec_value + TransWarm + TransWet + (1|species), data = dd, family = "poisson")
  }
  newdat <- expand.grid(
    destTemp_value=c(0, 0.6)
    , destPrec_value=c(0.2857143, 0.6666667, 1)
    #, newTT=c("Control", "Warm", "Wet", "Warm & wet")
    , TransWarm=c(0,1)
    , TransWet=c(0,1)
    #, functionalGroup=c("forb", "graminoid")
    #, flowering.time=c("always", "early", "late")
    , value = 0
  )
  mm <- model.matrix(terms(modelfit), newdat)
  newdat$value <- predict(modelfit,newdat,re.form=NA, type="response")  # re.form=NA without random part; re.form=NULL with random part
  
  means <- newdat %>% 
    group_by(TransWarm, TransWet) %>% 
    summarize(mean = mean(value), sd = sd(value))
  return(means)
}

# Create data and run prediction
DestDat2 <- pheno.long %>% 
  filter(pheno.unit %in% c("doy", "d.snowmelt", "dCumTemp")) %>% # for origin
  filter(pheno.stage != "r") %>%
  filter(pheno.var != "duration") %>% 
  mutate(pheno.var = factor(pheno.var, levels = c("first", "peak", "end"))) %>% 
  mutate(newTT = plyr::mapvalues(newTT, c("control", "TT2", "TT3", "TT4"), c("Control", "Warm", "Wet", "Warm & wet"))) %>%
  mutate(newTT = factor(newTT, levels = c("Control", "Warm", "Wet", "Warm & wet"))) %>% 
  mutate(pheno.stage = plyr::mapvalues(pheno.stage, c("b", "f", "s"), c("Bud", "Flower", "Seed"))) %>%
  mutate(pheno.stage = factor(pheno.stage, levels = c("Bud", "Flower", "Seed"))) %>% 
  mutate(pheno.unit = plyr::mapvalues(pheno.unit, c("doy", "d.snowmelt", "dCumTemp"), c("DOY", "Days since SM", "Temp since SM"))) %>%
  mutate(pheno.unit = factor(pheno.unit, levels = c("DOY", "Days since SM", "Temp since SM"))) %>%
  group_by(pheno.unit, pheno.var, pheno.stage) %>% 
  mutate_each(funs(as.factor), species, flowering.time, functionalGroup, occurrence.2) %>% 
  mutate(TransWarm = ifelse(newTT %in% c("Warm", "Warm & wet"), 1, 0)) %>% 
  mutate(TransWet = ifelse(newTT %in% c("Wet", "Warm & wet"), 1, 0)) %>%
  mutate(NoTrans = ifelse(newTT == "control", 1, 0)) %>%
  do(GlmerModelAndPredictDestination2(.))


# TEST BY HAND
dd <- DestDat1 %>% filter(pheno.unit=="Temp since SM", pheno.var=="first", pheno.stage == "Flower") %>% 
  ggplot2::ggplot(aes(x = value)) +
  geom_histogram(binwidth = 10) +
  facet_wrap(~pheno.stage ~ pheno.var)

# poisson model
dd <- DestDat1 %>% filter(pheno.unit=="Days since SM", pheno.var=="first", pheno.stage == "Bud")
ModelFull <- glmer(value ~ destTemp_value * destPrec_value + TransWarm + TransWet + (1|species), data = dd, family = "poisson")
ModelWarm <- glmer(value ~ destTemp_value * destPrec_value + TransWarm + (1|species), data = dd, family = "poisson")
ModelWet <- glmer(value ~ destTemp_value * destPrec_value + TransWet + (1|species), data = dd, family = "poisson")
ModelNull <- glmer(value ~ destTemp_value * destPrec_value + (1|species), data = dd, family = "poisson")
modsel(list(ModelFull, ModelWarm, ModelWet, ModelNull), 1000)
modsel(list(ModelFull, ModelWarm, ModelNull), 1000)
OrigDat2 %>% filter(pheno.unit=="DOY", pheno.stage=="Flower") %>% ModelSelectionOrig(.)

# normal model for temp
dd <- DestDat1 %>% filter(pheno.unit=="Temp since SM", pheno.var=="first", pheno.stage == "Seed")
ModelFull <- lmer(value ~ destTemp_value * destPrec_value + TransWarm + TransWet + (1|species), data = dd)
ModelWarm <- lmer(value ~ destTemp_value * destPrec_value + TransWarm  + (1|species), data = dd)
ModelWet <- lmer(value ~ destTemp_value * destPrec_value + TransWet + (1|species), data = dd)
ModelNull <- lmer(value ~ destTemp_value * destPrec_value + (1|species), data = dd)
modsel(list(ModelFull, ModelWarm, ModelWet, ModelNull), 1000)

DestDat2 %>% filter(pheno.unit=="Temp since SM")

# MAKE PLOT
DestinationSummaryPlot <- DestDat2 %>% 
  mutate(Treatment = paste(TransWarm, TransWet, sep = "")) %>% 
  mutate(Treatment = plyr::mapvalues(Treatment, c("00", "01", "10", "11"), c("Control", "Wet", "Warm", "Warm & wet"))) %>%
  mutate(Treatment = factor(Treatment, levels = rev(c("Control", "Warm", "Wet", "Warm & wet")))) %>% 
  ggplot() +
  geom_point(aes(x = mean, y = Treatment, shape = pheno.var, color = pheno.var), size = 3) +
  ylab(paste("")) + xlab(paste("Mean value")) +
  scale_shape_manual(name = "", values = c(16,17,15)) +
  scale_colour_manual(name = "", values = c(col.red, col.dblue, col.orange)) +
  #geom_errorbarh(aes(xmin=mean-sd, xmax=mean+sd), height=0.1, color = "gray") +
  facet_grid(~pheno.stage ~ pheno.unit, scales = "free")

DestinationSummaryPlot + theme_grey(base_size = 20) + theme(legend.title=element_blank()) 