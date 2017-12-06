#------------------------------------------------------------------------------
# LOAD LIBRARIES


graphics.off() # This closes all of R's graphics windows.
rm(list=ls())  # Careful! This clears all of R's memory!

# load libraries
library("tidyverse")
library("rjags")

#------------------------------------------------------------------------------
# LOAD DATA
load(file = "PhenoLong.RData", verbose = TRUE)

myData <- Phenology %>% 
  # subset
  filter(pheno.stage == "Flower", pheno.var == "peak", pheno.unit == "DaysSinceSM") %>% 
  select(value, newTT, species, siteID, destSiteID, blockID, destBlockID) %>% 
  rename(treatment = newTT, origSiteID = siteID, origBlockID = blockID) %>% 
  mutate(treatment = as.numeric(treatment), origSiteID = as.numeric(origSiteID), species = as.numeric(factor(species)), origSiteID = as.numeric(origSiteID), destSiteID = as.numeric(factor(destSiteID)), origBlockID = as.numeric(factor(origBlockID)), destBlockID = as.numeric(factor(destBlockID)))

myData <- as.data.frame(myData)

y = myData$value

# Making a data list
dataList <- list(y = myData$value, 
                 treatment = myData$treatment, 
                 origSite = myData$origSiteID,
                 destSite = myData$destSiteID,
                 species = myData$species, 
                 origBlock = myData$origBlockID, 
                 destBlock = myData$destBlockID, 
                 Ntotal = length(y), 
                 NtreatmentLvl = nlevels(factor(myData$treatment)), 
                 NorigSiteLvl = nlevels(factor(myData$origSiteID)),
                 NdestSiteLvl = nlevels(factor(myData$destSiteID)),
                 NSPLvl = nlevels(factor(myData$species)),
                 NorigBlockLvl = nlevels(factor(myData$origBlockID)),
                 NdestBlockLvl = nlevels(factor(myData$destBlockID))
)


#------------------------------------------------------------------------------
# LOAD THE MODEL

### FLOWERING ~ TREATMENT + ORIGINSITE + (1|SPECIES) + (1|Block) ###
#source("NegativeBinomial.R")
source("MixedEffectModel_Normal.R")


#------------------------------------------------------------------------------
# SPECIFY PARAMETERS
# Specify parameters for which posterior samples are saved
para.names <- c("alpha", paste("treatmentCoeff[", 2:4, "]", sep = ""), paste("origBlockCoeff[", 1:30, "]", sep = ""), "tau")
#para.names <- c("alpha", paste("newTTCoeff[", 2:4, "]", sep = ""), paste("siteCoeff[", 1:2, "]", sep = ""), paste("spCoeff[", 1:20, "]", sep = ""), "tau")

#------------------------------------------------------------------------------
# RUN ANALYSIS

jagsModel <- jags.model(file = "TEMPmodel.txt", data = dataList, n.chains = 3, n.adapt = 10000)

# Continue the MCMC runs with sampling
Samples <- coda.samples(jagsModel , variable.names = para.names, n.iter = 10000)
summary(Samples)
res <- summary(Samples)

png(file = "Gelmanplots%d.png", width = 1000, height = 1000)
gelman.plot(Samples)
dev.off()

res$statistics
dd <- as.data.frame(res$quantiles)
colnames(dd) <- c("twofive", "one_quanter", "median", "three_quarter", "ninetyseven")

dd %>% 
  rownames_to_column(var = "variable") %>% 
  filter(grepl("newTT", variable)) %>% 
  mutate(variable = plyr::mapvalues(variable, c("newTTCoeff[2]", "newTTCoeff[3]", "newTTCoeff[4]"), c("OTC", "Transplant warm", "Transplant cold"))) %>% 
  mutate(variable = factor(variable, levels = c("OTC", "Transplant warm", "Transplant cold"))) %>% 
  ggplot(aes(x = variable, y =median, ymin = twofive, ymax = ninetyseven)) +
  geom_point() +
  geom_errorbar(width = 0) +
  geom_hline(yintercept = 0, color = "grey", linetype = "dashed") +
  labs(x = "", y = "Credible interval")


png(file = "Traceplots%d.png", width = 1000, height = 1000)
plot(Samples)
dev.off()

gelman.diag(Samples)

library(jagstools)
res <- jagsresults(x=Samples, params=c('alpha', 'newTTCoeff', 'siteCoeff'))
res <- data.frame(as.matrix(res))
res$variable <- row.names(res)