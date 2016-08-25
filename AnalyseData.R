#### ANALYSE DATA ####

#### LIBRARIES ####
library("lme4")
library("tidyr")
library("dplyr")
library("lubridate")
library("ggplot2")

#### IMPORT PHENO.LONG ####
load("PhenoLong.RData", verbose = TRUE)

### Create ORIGIN Data (doy, o.snowmelt, oCumTemp)
OrigDat <- pheno.long %>% 
  filter(pheno.unit %in% c("doy", "o.snowmelt", "oCumTemp")) %>% # for origin
  filter(pheno.stage != "r") %>%
  filter(pheno.var != "duration") %>% 
  mutate(pheno.var = factor(pheno.var, levels = c("first", "peak", "end"))) %>% 
  mutate(newTT = plyr::mapvalues(newTT, c("control", "TT2", "TT3", "TT4"), c("control", "Warm", "Wet", "Warm & wet"))) %>%
  mutate(newTT = factor(newTT, levels = c("control", "Warm", "Wet", "Warm & wet"))) %>% 
  mutate(pheno.stage = plyr::mapvalues(pheno.stage, c("b", "f", "s"), c("Bud", "Flower", "Seed"))) %>%
  mutate(pheno.stage = factor(pheno.stage, levels = c("Bud", "Flower", "Seed"))) %>% 
  mutate(pheno.unit = plyr::mapvalues(pheno.unit, c("doy", "o.snowmelt", "oCumTemp"), c("DOY", "DaySSM", "TempSSM"))) %>%
  mutate(pheno.unit = factor(pheno.unit, levels = c("DOY", "DaySSM", "TempSSM"))) %>%
  group_by(pheno.unit, pheno.var, pheno.stage) %>% 
  mutate_each(funs(as.factor), species, newTT) %>% 
  do(GlmerModelAndPredictOrigin(.))

### GGPLOT
OriginSummaryPlot <- ggplot(OrigDat, aes(x = mean, y = newTT, shape = pheno.var, color = pheno.var)) +
  geom_point() +
  ylab(paste("")) + xlab(paste("Mean value")) +
  #geom_errorbarh(aes(xmin=mean-sd, xmax=mean+sd), height=0.1, color = "gray") +
  facet_grid(~pheno.stage ~ pheno.unit, scales = "free")

OriginSummaryPlot + theme_grey(base_size = 20) + theme(legend.title=element_blank()) 


# Function to perform Model and predict new values
GlmerModelAndPredictOrigin <- function(dd){
  modelfit <- glmer(value ~ Temp_value * Prec_value + newTT + (1|species), data = dd, family = "poisson")
  #print(summary(modelfit))
  newdat <- expand.grid(
    Temp_value=c(0, 0.6)
    , Prec_value=c(0, 0.2857143, 0.6666667, 1)
    , newTT=c("control", "Warm", "Wet", "Warm & wet")
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
pheno <- pheno.long %>% filter(pheno.stage =="s", pheno.var == "first", pheno.unit == "doy")
fit01 <- glmer((value) ~ Temp_value * Prec_value + newTT + (1|species), pheno, family = "poisson")
disp(fit01, pheno)
summary(fit01)
pheno$newFactor <- rnorm(nrow(pheno))
fit01 <- glmer((value) ~ Temp_value + Prec_value + newTT + (1|species) + (1|newFactor), pheno, family = "poisson")
summary(fit01)
newdat <- expand.grid(
  Temp_value=c(0, 0.6)
  , Prec_value=c(0, 0.2857143, 0.6666667, 1)
  , newTT=c("control", "TT2", "TT3", "TT4")
  , value = 0
)
mm <- model.matrix(terms(fit01), newdat)
newdat$value <- predict(fit01,newdat,re.form=NA, type="response")

newdat %>% 
  group_by(newTT) %>% 
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
  mutate(newTT = factor(newTT, levels = c("TT4", "TT3", "TT2", "control"))) %>% 
  mutate(pheno.stage = plyr::mapvalues(pheno.stage, c("b", "f", "s"), c("Bud", "Flower", "Seed"))) %>%
  mutate(pheno.stage = factor(pheno.stage, levels = c("Bud", "Flower", "Seed"))) %>% 
  mutate(pheno.unit = plyr::mapvalues(pheno.unit, c("doy", "d.snowmelt", "dCumTemp"), c("DOY", "DaySSM", "TempSSM"))) %>%
  mutate(pheno.unit = factor(pheno.unit, levels = c("DOY", "DaySSM", "TempSSM"))) %>%
  group_by(pheno.unit, pheno.var, pheno.stage) %>% 
  mutate_each(funs(as.factor), species, newTT) %>% 
  do(GlmerModelAndPredictDestination(.))


### GGPLOT
DestSummaryPlot <- ggplot(DestDat, aes(x = mean, y = newTT, shape = pheno.var, color = pheno.var)) +
  geom_point() +
  ylab(paste("")) + xlab(paste("Mean value")) +
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
    , newTT=c("control", "TT2", "TT3", "TT4")
    , value = 0
  )
  mm <- model.matrix(terms(modelfit), newdat)
  newdat$value <- predict(modelfit,newdat,re.form=NA, type="response")  # re.form=NA without random part; re.form=NULL with random part
  
  means <- newdat %>% 
    group_by(newTT) %>% 
    summarize(mean = mean(value), sd = sd(value))
  return(means)
}