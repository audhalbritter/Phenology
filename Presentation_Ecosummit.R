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
  modelfit <- glmer(value ~ Temp_value * Prec_value + newTT + (1|species), data = dd, family = "poisson")
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
    group_by(newTT) %>% 
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
  mutate(pheno.unit = plyr::mapvalues(pheno.unit, c("doy", "o.snowmelt", "oCumTemp"), c("DOY", "DaySSM", "TempSSM"))) %>%
  mutate(pheno.unit = factor(pheno.unit, levels = c("DOY", "DaySSM", "TempSSM"))) %>%
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

