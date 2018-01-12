#------------------------------------------------------------------------------
# LOAD LIBRARIES


graphics.off() # This closes all of R's graphics windows.
rm(list=ls())  # Careful! This clears all of R's memory!

# load libraries
library("tidyverse")
library("rjags")
library("R2jags")

#------------------------------------------------------------------------------
# LOAD DATA
load(file = "PhenoLong.RData", verbose = TRUE)

myData <- Phenology %>% 
  # subset
  filter(pheno.stage == "Flower", pheno.var == "peak", pheno.unit == "DaysSinceSM") %>% 
  select(value, newTT, species, siteID, destSiteID, blockID, destBlockID) %>% 
  rename(treatment = newTT, origSiteID = siteID, origBlockID = blockID) %>% 
  mutate(treatment = factor(treatment, levels = c("Control", "Warmer", "LaterSM", "WarmLate"))) %>% 
  mutate(treatment = as.numeric(treatment), origSiteID = as.numeric(origSiteID), species = as.numeric(factor(species)), origSiteID = as.numeric(origSiteID), destSiteID = as.numeric(factor(destSiteID)), origBlockID = as.numeric(factor(origBlockID)), destBlockID = as.numeric(factor(destBlockID)))

myData <- as.data.frame(myData)

head(myData)
table(myData$origSiteID, myData$destSiteID)

Data2 <- Phenology %>% 
  filter(pheno.stage == "Flower", pheno.var == "peak", pheno.unit == "DaysSinceSM") %>% 
  select(value, newTT, species, siteID, destSiteID, blockID, destBlockID) %>% 
  rename(treatment = newTT, origSiteID = siteID, origBlockID = blockID) 
Data2 <- as.data.frame(Data2)

head(Data2)
table(Data2$treatment, Data2$destSiteID)
table(Data2$treatment, as.numeric(Data2$treatment))


# This may seem a little redundant, but I like having all the data as objects in the workspace so you can work with them later
        y = myData$value
        treatment <- myData$treatment 
        origSite <- myData$origSiteID
        destSite <- myData$destSiteID
        species <- myData$species 
        origBlock <- myData$origBlockID 
        destBlock <- myData$destBlockID 
        Ntotal <- length(y) 
        NtreatmentLvl <- nlevels(factor(myData$treatment)) 
        NorigSiteLvl <- nlevels(factor(myData$origSiteID))
        NdestSiteLvl <- nlevels(factor(myData$destSiteID))
        NSPLvl <- nlevels(factor(myData$species))
        NorigBlockLvl <- nlevels(factor(myData$origBlockID))
        NdestBlockLvl <- nlevels(factor(myData$destBlockID))

# Making a data list
dataList.mod1 <- list(y = y, 
                 treatment = treatment, 
                 # origSite = origSite,
                 # destSite = destSite,
                 species = species, 
#                 origBlock = origBlock, 
                 destBlock = destBlock, 
                 Ntotal = Ntotal, 
                 NtreatmentLvl = NtreatmentLvl, 
                 # NorigSiteLvl = NorigSiteLvl,
                 # NdestSiteLvl = NdestSiteLvl,
                 NSPLvl = NSPLvl,
                 NBlockLvl = NdestBlockLvl   # was origBlock  - should RE be for orig block or for destination block?  Not sure i understand why orig
#                 ,NdestBlockLvl = nlevels(factor(destBlockID))
)



mod1.inits<-function(){
  list(
    tau = 1
  ,BlockPrec=1
  ,tau.slope = rep(1,NSPLvl)
#    ,alpha = matrix(0,N_sp, N_plots)
#    ,alpha = rep(0,NSPLvl)
  )
}

n.iterations <- 5000      ## draws from posterior
n.burn <- 2000      ## draws to discard as burn-in
thin.rate <- 5    	## thinning rate
nc <- 3			## number of chains

# SPECIFY PARAMETERS - these were somehow making a problem; I have never tried specifying like this; might be possible, but not sure
#para.names <- c("alpha", paste("treatmentCoeff[", 2:4, "]", sep = ""), paste("spCoeff[", 1:66, "]", sep = ""), paste("origBlockCoeff[", 1:30, "]", sep = ""), "tau")

para.names2 <- c("treatmentCoeff","blockCoeff") # "alpha"
para.names1 <- c("treatment.contrast","treatmentCoeff","blockCoeff") # "alpha"
                
## Run model
mod1 <-jags(dataList.mod1, 
                 mod1.inits, 
                 para.names1, 
                 n.thin=thin.rate, 
                 n.chains=nc, n.burnin=n.burn, n.iter=n.iterations,
                 model.file="Models/model1.R")


mod1
mod1.mcmc <- as.mcmc(mod1)

pdf(file="mod1.JAGS.diagnostic.pdf", width = 12, height = 10)
par(mar=c(4,2,2,1))
plot(mod1)
plot(mod1.mcmc)
dev.off()



#------------------------------------------------------------------------------
# RUN ANALYSIS

jagsModel <- jags.model(file = "TEMPmodel.R", data = dataList, n.chains = 3, n.adapt = 10000)

# Continue the MCMC runs with sampling
Samples <- coda.samples(jagsModel , variable.names = para.names, n.iter = 10000)
summary(Samples)


#------------------------------------------------------------------------------
# MODEL CHECK
png(file = "Gelmanplots%d.png", width = 1000, height = 1000)
gelman.plot(Samples)
dev.off()


png(file = "Traceplots%d.png", width = 1000, height = 1000)
plot(Samples)
dev.off()

gelman.diag(Samples)



#------------------------------------------------------------------------------
# MODEL OUTPUT
res <- summary(Samples)
res$statistics

dd <- as.data.frame(res$quantiles)
colnames(dd) <- c("Min","oneQuarter", "median", "threeQuarter", "Max")

dd %>% 
  rownames_to_column(var = "variable") %>% 
  filter(grepl("treatment", variable)) %>% 
  mutate(variable = plyr::mapvalues(variable, c("treatmentCoeff[2]", "treatmentCoeff[3]", "treatmentCoeff[4]"), c("Warmer", "LateSM", "WarmLateSM"))) %>% 
  mutate(variable = factor(variable, levels = c("Warmer", "LateSM", "WarmLateSM"))) %>% 
  as_tibble() %>% 
  ggplot(aes(x = variable, y = median, ymin = Min, ymax = Max)) +
  geom_point() +
  geom_errorbar(width = 0) +
  geom_hline(yintercept = 0, color = "grey", linetype = "dashed") +
  labs(x = "", y = "Credible interval")








#####################################################################################
#### RUN MODEL AUTOMATIC ####
#------------------------------------------------------------------------------
# PREPARE AND LOAD LIBRARIES AND DATA

graphics.off() # This closes all of R's graphics windows.
rm(list=ls())  # Careful! This clears all of R's memory!

# load libraries
library("tidyverse")
library("rjags")
library("R2jags")
library("lme4")

load(file = "180111_PhenoLong.RData")
source("FunctionRunBayesianModel.R")

#------------------------------------------------------------------------------
# RUN ANALYSIS

RunBayesianAnalysis(# Data input
                    dat = Phenology, 
                    phenostage = "Flower", 
                    phenovar = "peak", 
                    phenounit = "DaysSinceSM",
  
                    # Running analysis
                    niter = 10000, 
                    nburn = 5000, 
                    nthin = 5,
                    nchain = 3, 
                    mod = "Models/model4.R")


#------------------------------------------------------------------------------
# OUTPUT

load(file = "ModelOutput/modFlowerpeakDaysSinceSM.RData", verbose = TRUE)
dd <- res %>% 
  rownames_to_column(var = "variable") %>% 
  filter(grepl("treatmentCoeff", var))

  
 
res %>% 
  rownames_to_column(var = "variable") %>% 
  filter(grepl("treatmentCoeff", variable)) %>% 
  bind_cols(meta) %>% 
  inner_join(dd, by = c("treatment", "species")) %>% 
  group_by(treatment) %>% 
  summarise(mean = mean(mean))

dd <- Phenology %>% 
  filter(pheno.stage == "Flower", pheno.var == "peak", pheno.unit == "DaysSinceSM") %>% 
  rename(treatment = newTT) %>% 
  distinct(treatment, species)


# meta data
meta <- expand.grid(species = unique(dd$species),
                    treatment = unique(dd$treatment))
meta <- meta %>% 
  arrange(treatment, species)


