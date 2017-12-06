#### ANALYSE DATA ####

#### LIBRARIES ####
library("lme4")
library("tidyr")
library("dplyr")
library("lubridate")
library("ggplot2")
library("cowplot")

#### IMPORT PHENO.LONG ####
load("PhenoLong.RData", verbose = TRUE)

### FUNCTIONS
# Function to perform Model and predict new values
GlmerModelAndPredictOrigin <- function(dd){
  if(unique(dd$pheno.unit) == "oCumTemp"){
    modelfit <- lmer(value ~ Temp_value * Prec_value + newTT + (1|species), data = dd)
  }
  else if(unique(dd$pheno.unit) != "oCumTemp"){
    modelfit <- glmer(value ~ Temp_value * Prec_value + newTT + (1|species), data = dd, family = "poisson")
  }
  
  #print(summary(modelfit))
  newdat <- expand.grid(
    Temp_value=c(0, 0.6)
    , Prec_value=c(0, 0.2857143, 0.6666667, 1)
    , newTT=rev(c("Control", "Warm", "Wet", "Warm & wet"))
    , value = 0
  )
  mm <- model.matrix(terms(modelfit), newdat)
  newdat$value <- predict(modelfit,newdat,re.form=NA, type="response")  # re.form=NA without random part; re.form=NULL with random part
  
  means <- newdat %>% 
    group_by(newTT, Temp_value, Prec_value) %>% 
    summarize(mean = mean(value), sd = sd(value))
  return(means)
}


### Create ORIGIN Data (doy, o.snowmelt, oCumTemp)
OrigDat <- pheno.long %>% 
  filter(pheno.unit %in% c("doy", "o.snowmelt", "oCumTemp")) %>% # for origin
  filter(pheno.stage != "r") %>%
  filter(pheno.var != "duration") %>% 
  mutate(pheno.var = factor(pheno.var, levels = c("first", "peak", "end"))) %>% 
  mutate(newTT = plyr::mapvalues(newTT, c("control", "TT2", "TT3", "TT4"), c("Control", "Warm", "Wet", "Warm & wet"))) %>%
  mutate(newTT = factor(newTT, levels = rev(c("Control", "Warm", "Wet", "Warm & wet")))) %>% 
  mutate(pheno.stage = plyr::mapvalues(pheno.stage, c("b", "f", "s"), c("Bud", "Flower", "Seed"))) %>%
  mutate(pheno.stage = factor(pheno.stage, levels = c("Bud", "Flower", "Seed"))) %>% 
  mutate(pheno.unit = plyr::mapvalues(pheno.unit, c("doy", "o.snowmelt", "oCumTemp"), c("DOY", "Days since SM", "Temp since SM"))) %>%
  mutate(pheno.unit = factor(pheno.unit, levels = c("DOY", "Days since SM", "Temp since SM"))) %>%
  group_by(pheno.unit, pheno.var, pheno.stage) %>% 
  mutate_each(funs(as.factor), species, newTT) %>% 
  do(GlmerModelAndPredictOrigin(.))

### GGPLOT
OriginSummaryPlot <-  ggplot() +
  geom_point(data = OrigDat, aes(x = mean, y = newTT, shape = pheno.var, color = pheno.var), size = 3) +
  ylab(paste("")) + xlab(paste("Mean value")) +
  scale_shape_manual(name = "", values = c(16,17,15)) +
  scale_colour_manual(name = "", values = c(col.red, col.dblue, col.orange)) +
  #geom_errorbarh(aes(xmin=mean-sd, xmax=mean+sd), height=0.1, color = "gray") +
  facet_grid(~pheno.stage ~ pheno.unit, scales = "free")

OriginSummaryPlot + theme_grey(base_size = 20) + theme(legend.title=element_blank()) 


### New Plot with Temp and Prec levels
LevelPlot <- OrigDat %>% 
  filter(pheno.unit == "DOY", pheno.stage == "Flower") %>%
  filter(Prec_value != "0") %>% 
  mutate(Temp_value = plyr::mapvalues(Temp_value, c(0, 0.6), c("alpine", "intermediate"))) %>%
  mutate(Temp_value = factor(Temp_value, levels = c("alpine", "intermediate"))) %>%
  mutate(Prec_value = plyr::mapvalues(Prec_value, c(0.2857143, 0.6666667, 1), c("dry", "intermediate", "wet"))) %>%
  mutate(Prec_value = factor(Prec_value, levels = c("dry", "intermediate", "wet"))) %>%
  ggplot() +
  geom_point(aes(x = mean, y = newTT, shape = pheno.var, color = pheno.var), size = 3) +
  ylab(paste("")) + xlab(paste("Mean value")) +
  ggtitle("Flowering in DOY") +
  scale_shape_manual(name = "", values = c(16,17,15)) +
  scale_colour_manual(name = "", values = c(col.red, col.dblue, col.orange)) +
  #geom_errorbarh(aes(xmin=mean-sd, xmax=mean+sd), height=0.1, color = "gray") +
  facet_grid(~Temp_value ~ Prec_value) +
  coord_fixed(ratio = 18)
LevelPlot + theme_grey(base_size = 20) + theme(legend.title=element_blank())




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
  do(GlmerModelAndPredictDurationOrigin(.))

OriginDurationPlot <- ggplot() +
  geom_point(data = OrigDatDuration, aes(x = mean, y = newTT), size = 3) +
  ylab(paste("")) + xlab(paste("Estimated duration in days")) +
  ggtitle("DOY") +
  facet_grid(~pheno.stage) +
  coord_fixed(ratio = 5)

OriginDurationPlot + theme_grey(base_size = 20) + theme(legend.title=element_blank()) 

GlmerModelAndPredictDurationOrigin <- function(dd){
  dd$newFactor <- rnorm(nrow(dd))
  modelfit <- glmer(value ~ Temp_value * Prec_value + newTT + (1|species) + (1|newFactor), data = dd, family = "poisson")
  newdat <- expand.grid(
    Temp_value=c(0, 0.6)
    , Prec_value=c(0, 0.2857143, 0.6666667, 1)
    , newTT=c("Control", "Warm", "Wet", "Warm & wet")
    , value = 0
  )
  mm <- model.matrix(terms(modelfit), newdat)
  newdat$value <- predict(modelfit,newdat,re.form=NA, type="response")  # re.form=NA without random part; re.form=NULL with random part
  
  means <- newdat %>% 
    group_by(newTT) %>% 
    summarize(mean = mean(value), sd = sd(value))
  return(means)
}


# test GlmerModelAndPredict function
pheno <- pheno.long %>% filter(pheno.stage =="f", pheno.var == "first", pheno.unit == "doy")
GlmerModelAndPredict(pheno)


# Model and Predict Data by Hand
pheno <- pheno.long %>% filter(pheno.stage =="f", pheno.var == "first", pheno.unit == "doy")
fit01 <- glmer((value) ~ Temp_value * Prec_value + From1To2Temp + From2To3Prec + From3To4Prec + (1|species), pheno, family = "poisson")
disp(fit01, pheno)
summary(fit01)
pheno$newFactor <- rnorm(nrow(pheno))
fit01 <- glmer((value) ~ Temp_value + Prec_value + newTT + (1|species) + (1|newFactor), pheno, family = "poisson")
summary(fit01)
newdat <- expand.grid(
  Temp_value=c(0, 0.6)
  , Prec_value=c(0, 0.2857143, 0.6666667, 1)
  , From1To2Temp=c(0, 1)
  , From2To3Prec=c(0, 1)
  , From3To4Prec=c(0, 1)
  , value = 0
)
mm <- model.matrix(terms(fit01), newdat)
newdat$value <- predict(fit01,newdat,re.form=NA, type="response")

newdat %>% 
  group_by(From1To2Temp, From2To3Prec, From3To4Prec) %>% 
  summarize(mean = mean(value), sd = sd(value)) %>% 
  ggplot() +
  geom_point(aes(x= mean, y = newTT)) + 
  geom_errorbarh(aes(x = mean, xmin=-sd, xmax=sd)) +
  facet_wrap(~pheno.unit)




### DESTINATION
DestDat <- pheno.long %>% 
  filter(pheno.unit %in% c("doy", "d.snowmelt", "dCumTemp")) %>% # for destination
  filter(pheno.stage != "r") %>%
  filter(pheno.var != "duration") %>% 
  mutate(pheno.var = factor(pheno.var, levels = c("first", "peak", "end"))) %>% 
  mutate(newTT = plyr::mapvalues(newTT, c("control", "TT2", "TT3", "TT4"), c("Control", "Warm", "Wet", "Warm & wet"))) %>%
  mutate(newTT = factor(newTT, levels = c("Control", "Warm", "Wet", "Warm & wet"))) %>% 
  mutate(pheno.stage = plyr::mapvalues(pheno.stage, c("b", "f", "s"), c("Bud", "Flower", "Seed"))) %>%
  mutate(pheno.stage = factor(pheno.stage, levels = c("Bud", "Flower", "Seed"))) %>% 
  mutate(pheno.unit = plyr::mapvalues(pheno.unit, c("doy", "d.snowmelt", "dCumTemp"), c("DOY", "DaySSM", "TempSSM"))) %>%
  mutate(pheno.unit = factor(pheno.unit, levels = c("DOY", "DaySSM", "TempSSM"))) %>%
  group_by(pheno.unit, pheno.var, pheno.stage) %>% 
  mutate_each(funs(as.factor), species, newTT) %>% 
  do(GlmerModelAndPredictDestination(.))


### GGPLOT
DestSummaryPlot <- ggplot() +
  geom_point(data = DestDat, aes(x = mean, y = newTT, shape = pheno.var, color = pheno.var), size = 3) +
  ylab(paste("")) + xlab(paste("Mean value")) +
  scale_shape_manual(name = "", values = c(16,17,15)) +
  scale_colour_manual(name = "", values = c(col.red, col.dblue, col.orange)) +
  #geom_errorbarh(aes(xmin=mean-sd, xmax=mean+sd), height=0.1, color = "gray") +
  facet_grid(~pheno.stage ~ pheno.unit, scales = "free")

DestSummaryPlot + theme_grey(base_size = 18) + theme(legend.title=element_blank())


# Function to perform Model and predict new values
GlmerModelAndPredictDestination <- function(dd){
  modelfit <- glmer(value ~ destTemp_value * destPrec_value + newTT + (1|species), data = dd, family = "poisson")
  #print(summary(modelfit))
  newdat <- expand.grid(
    destTemp_value=c(0, 0.6)
    , destPrec_value=c(0.2857143, 0.6666667, 1)
    , newTT=c("Control", "Warm", "Wet", "Warm & wet")
    , value = 0
  )
  mm <- model.matrix(terms(modelfit), newdat)
  newdat$value <- predict(modelfit,newdat,re.form=NA, type="response")  # re.form=NA without random part; re.form=NULL with random part
  
  means <- newdat %>% 
    group_by(newTT) %>% 
    summarize(mean = mean(value), sd = sd(value))
  return(means)
}



DestDatDuration <- pheno.long %>% 
  filter(pheno.unit == "doy", pheno.var == "duration") %>%
  filter(pheno.stage != "r") %>%
  mutate(pheno.var = factor(pheno.var, levels = c("duration"))) %>% 
  mutate(newTT = plyr::mapvalues(newTT, c("control", "TT2", "TT3", "TT4"), c("Control", "Warm", "Wet", "Warm & wet"))) %>%
  mutate(newTT = factor(newTT, levels = c("Control", "Warm", "Wet", "Warm & wet"))) %>% 
  mutate(pheno.stage = plyr::mapvalues(pheno.stage, c("b", "f", "s"), c("Bud", "Flower", "Seed"))) %>%
  mutate(pheno.stage = factor(pheno.stage, levels = c("Bud", "Flower", "Seed"))) %>% 
  mutate(pheno.unit = plyr::mapvalues(pheno.unit, c("doy"), c("DOY"))) %>%
  mutate(pheno.unit = factor(pheno.unit, levels = c("DOY"))) %>%
  group_by(pheno.stage) %>% 
  mutate_each(funs(as.factor), species, newTT) %>% 
  do(GlmerModelAndPredictDurationDestination(.))

DestDurationPlot <- ggplot() +
  geom_point(data = DestDatDuration, aes(x = mean, y = newTT), size = 3) +
  ylab(paste("")) + xlab(paste("Estimated duration in days")) +
  ggtitle("DOY") +
  facet_grid(~pheno.stage) +
  coord_fixed(ratio = 1.6)

DestDurationPlot + theme_grey(base_size = 20) + theme(legend.title=element_blank()) 


GlmerModelAndPredictDurationDestination <- function(dd){
  dd$newFactor <- rnorm(nrow(dd))
  modelfit <- glmer(value ~ destTemp_value * destPrec_value + newTT + (1|species) + (1|newFactor), data = dd, family = "poisson")
  newdat <- expand.grid(
    destTemp_value=c(0, 0.6)
    , destPrec_value=c(0.2857143, 0.6666667, 1)
    , newTT=c("Control", "Warm", "Wet", "Warm & wet")
    , value = 0
  )
  mm <- model.matrix(terms(modelfit), newdat)
  newdat$value <- predict(modelfit,newdat,re.form=NA, type="response")  # re.form=NA without random part; re.form=NULL with random part
  
  means <- newdat %>% 
    group_by(newTT) %>% 
    summarize(mean = mean(value), sd = sd(value))
  return(means)
}
