# Phenology data 2015

#### LIBRARIES ####
library("lme4")
library("tidyr")
library("dplyr")
library("lubridate")
library("ggplot2")

#### IMPORT PHENO.LONG ####
load("PhenoLong.RData", verbose = TRUE)


#### RESHAPE DATA TO MAKE PLOTS ####
# destination
pheno.treat.dest <- ReshapeToMakeFigure(pheno.long, "destination", "C1")
head(pheno.treat.dest)

# origin
pheno.treat.orig <- ReshapeToMakeFigure(pheno.long, "origin", "C1")
head(pheno.treat.orig)



library("cowplot")
MakePlot2(pheno.treat.dest, "destination", "first", "f", "dCumTemp", "DOY")
MakePlot2(pheno.treat.orig, "origin", "duration", "s", "doy", "DOY")


# Figures for Presentation
Plot1Plasticity <- MakePlot2(pheno.treat.orig, "origin", "first", "f", "oCumTemp", "DOY") + theme_grey(base_size = 24)+theme(legend.position= "top")


### Plot for functionalGroup
MakePlotFuncGroup(pheno.treat.orig, "origin", "duration", "f", "doy", "DOY")