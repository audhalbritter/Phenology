# Phenology data 2015

#### LIBRARIES ####
library("lme4")
library("tidyr")
library("dplyr")
library("lubridate")
library("ggplot2")

#### IMPORT PHENO.LONG ####
load("PhenoLong.RData", verbose = TRUE)


#### SUBSET FOR PLASTIC AND ADAPT
# PLASTICITY
plasticity <- Phenology %>% 
  filter(pheno.unit %in% c("DOY", "Days","DaysSinceSM", "TempSinceSM")) %>% # for origin
  filter(pheno.stage != "RipeSeed") %>%
  filter(!(Precipitation_level == 4 & Temperature_level == 2)) # remove controls in Veskre, not used for plasticity

# ADAPTATION
adaptation <- Phenology %>% 
  filter(pheno.unit %in% c("DOY", "Days", "DaysSinceSMDest", "TempSinceSMDest")) %>% # for destination
  filter(pheno.stage != "RipeSeed") %>%
  filter(!(destP_level == 2 & destT_level == 1)) # remove controls in Gudmedalen, not used for adaptation





### NOT USED AT THE MOMENT !!!!!
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