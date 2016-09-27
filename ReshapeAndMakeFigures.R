# Phenology data 2015

#### LIBRARIES ####
library("lme4")
library("tidyr")
library("dplyr")
library("lubridate")
library("ggplot2")

#### IMPORT PHENO.LONG ####
load("PhenoLong.RData", verbose = TRUE)

#### RENAME VARIABLES ####
Phenology <- pheno.long %>% 
  filter(Precipitation_level != 1) %>% #remove turfs transplanted from Ulv and Alr, because they have no control
  mutate(pheno.var = factor(pheno.var, levels = c("first", "peak", "end", "duration"))) %>% 
  mutate(newTT = plyr::mapvalues(newTT, c("control", "TT2", "TT3", "TT4"), c("Control", "Warm", "Wet", "Warm & wet"))) %>%
  mutate(newTT = factor(newTT, levels = c("Control", "Warm", "Wet", "Warm & wet"))) %>% 
  mutate(pheno.stage = plyr::mapvalues(pheno.stage, c("b", "f", "s", "r"), c("Bud", "Flower", "Seed", "Ripe seed"))) %>%
  mutate(pheno.stage = factor(pheno.stage, levels = c("Bud", "Flower", "Seed", "Ripe seed"))) %>% 
  mutate(pheno.unit = plyr::mapvalues(pheno.unit, c("doy", "o.snowmelt", "oCumTemp", "d.snowmelt", "dCumTemp"), c("DOY", "Days since SM", "Temp since SM", "Days since SM dest", "Temp since SM dest"))) %>%
  mutate(pheno.unit = factor(pheno.unit, levels = c("DOY", "Days since SM", "Temp since SM", "Days since SM dest", "Temp since SM dest"))) %>% 
  mutate_each(funs(as.factor), species, flowering.time, functionalGroup, occurrence.2)


#### SUBSET FOR PLASTIC AND ADAPT
# PLASTICITY
plasticity <- Phenology %>% 
  filter(pheno.unit %in% c("DOY", "Days since SM", "Temp since SM")) %>% # for origin
  filter(pheno.stage != "Ripe seed") %>%
  filter(!(Precipitation_level == 4 & Temperature_level == 2)) # remove controls in Veskre, not used for plasticity

# ADAPTATION
adaptation <- Phenology %>% 
  filter(pheno.unit %in% c("DOY", "Days since SM dest", "Temp since SM dest")) %>% # for destination
  filter(pheno.stage != "Ripe seed") %>%
  filter(!(destP_level == 2 & destT_level == 1)) # remove controls in Gudmedalen, not used for adaptation


#### RESHAPE DATA TO MAKE PLOTS ####
# destination
pheno.treat.dest <- ReshapeToMakeFigure(pheno.long, "destination", "C1")
head(pheno.treat.dest)

# origin
pheno.treat.orig <- ReshapeToMakeFigure(pheno.long, "origin", "C1")
head(pheno.treat.orig)



library("cowplot")
MakePlot2(pheno.treat.dest, "destination", "first", "b", "dCumTemp", "DOY")
MakePlot2(pheno.treat.orig, "origin", "first", "f", "oCumTemp", "DOY")


# Figures for Presentation
Plot1Plasticity <- MakePlot2(pheno.treat.orig, "origin", "first", "f", "doy", "DOY") + theme_grey(base_size = 24)+theme(legend.position= "top")


### Plot for functionalGroup
MakePlotFuncGroup(pheno.treat.orig, "origin", "duration", "f", "doy", "DOY")