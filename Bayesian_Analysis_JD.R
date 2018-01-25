#--- --- --- --- --- --- --- --- --- --- --- --- --- --- 
# LOAD LIBRARIES ----


graphics.off() # This closes all of R's graphics windows.
rm(list=ls())  # Careful! This clears all of R's memory!

# load libraries
source('~/Dropbox/Research2/Stat1/R/scripts/Synced.Rprofile.R')
library("tidyverse")
library("rjags")
library("R2jags")

#--- --- --- --- --- --- --- --- --- --- --- --- --- --- 
options(max.print=1000, dplyr.print_max = 1000)

# LOAD DATA----
load(file = "PhenoLong.RData", verbose = TRUE)
load(file = "~/Dropbox/Research/Collaborations1/Aud phenology/180111_PhenoLong.RData", verbose = TRUE)

myData <- Phenology %>% 
  # subset
  filter(pheno.stage == "Flower", pheno.var == "peak", pheno.unit == "DaysSinceSM") %>% 
  select(value, newTT, species, siteID, destSiteID, blockID, destBlockID) %>% 
  rename(treatment = newTT, origSiteID = siteID, origBlockID = blockID) %>% 
  mutate(treatment = factor(treatment, levels = c("Control", "Warmer", "LaterSM", "WarmLate"))) %>% 
  mutate(treatment = as.numeric(treatment), origSiteID = as.numeric(origSiteID), species = as.numeric(factor(species)), origSiteID = as.numeric(origSiteID), destSiteID = as.numeric(factor(destSiteID)), origBlockID = as.numeric(factor(origBlockID)), destBlockID = as.numeric(factor(destBlockID))) %>%
  as.data.frame()

head(myData); dim(myData)
table(myData$origSiteID, myData$destSiteID)
length(unique(myData$species))


## get a dataset that averages values of blocks with multiple control turfs
temp <- myData %>%
  group_by(species,treatment,origSiteID,destSiteID,origBlockID,destBlockID) %>%
  summarise(num.vals = length(value)
  ) %>%
  as.data.frame()


## exploring data structure - multiple values per species-treatment-block?
Pheno.Flr <- filter(Phenology, pheno.stage == "Flower", pheno.var == "peak", pheno.unit == "DaysSinceSM") 
  temp <- Pheno.Flr %>%
  group_by(species,blockID,destBlockID,newTT) %>%
  summarise(num.turfs=length(unique(turfID))
            ,num.unique.values=length(unique(value))
            ,num.values=length(value)
            ,num.years=length(unique(Year))
  )
  temp[temp$newTT!="Control",]
  temp[temp$newTT=="Control",]
  with(Pheno.Flr, Pheno.Flr[species=="Vio.bif" & blockID=="Ram6" & destBlockID=="Ram6",])
  with(Pheno.Flr, Pheno.Flr[species=="Vio.bif"  & destBlockID=="Ram8",])
  
# want to calculate how much observed phenology varied among the different control turfs
# temp2 <- Pheno.Flr %>%
#   filter(newTT=='Control') %>%
#   group_by(species,destBlockID) %>%
#   summarise(num.turfs=length(unique(turfID))
#             ,num.unique.values=length(unique(value))
#             ,num.values=length(value)
#             ,num.years=length(unique(Year))
#             ,value.dif = (max(value)-min(value))
#   )
# temp2
# hist(temp2$value.dif, 40, xlab='Difference in phenol value between control plots\n(within species)')


## A version with real site names
Data2 <- Phenology %>% 
  filter(pheno.stage == "Flower", pheno.var == "peak", pheno.unit == "DaysSinceSM") %>% 
  select(value, newTT, species, siteID, destSiteID, blockID, destBlockID) %>% 
  rename(treatment = newTT, origSiteID = siteID, origBlockID = blockID) %>%
  as.data.frame()

head(Data2); dim(Data2)
table(Data2$treatment, Data2$destSiteID)
table(Data2$treatment, as.numeric(Data2$treatment))


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


## Check this Aud ----
myData2 <- myData %>%
#  group_by(species,destBlockID) %>%
  group_by(species,destSiteID) %>%
  summarise(w2 = ifelse(length(value[treatment==2])>0 & length(value[treatment==1])>0, 1,0)
            ,warm.contrast = ifelse(w2==1, mean(value[treatment==2]) - mean(value[treatment==1]), NA)
            ,w3 = ifelse(length(value[treatment==3])>0 & length(value[treatment==1])>0, 1,0)
            ,late.contrast = ifelse(w3==1, mean(value[treatment==3]) - mean(value[treatment==1]), NA)
            ,w4 = ifelse(length(value[treatment==4])>0 & length(value[treatment==1])>0, 1,0)
            ,warmlate.contrast = ifelse(w4==1, mean(value[treatment==4]) - mean(value[treatment==1]), NA)
  ) %>%
  as.data.frame()
head(myData2)
#

## Checking here which species-site combinations have each contrast
warm.contrasts <- filter(myData2,is.na(warm.contrast)==FALSE)
late.contrasts <- filter(myData2,is.na(late.contrast)==FALSE)
warmlate.contrasts <- filter(myData2,is.na(warmlate.contrast)==FALSE)
# Seems there are very few species to compare flowering times across treatments within the same sites
# We decided to make comparisons at the site level instead of blocks; blocks are replicates within sites
# Is this assessment correct??
length(unique(myData$species)) # 77 species
length(unique(warm.contrasts$species)) # 13 species have warm treatment contrast
length(unique(late.contrasts$species)) # 18 species
length(unique(warmlate.contrasts$species)) # 13 species



myData3 <- myData %>%
  group_by(species, destSiteID, treatment) %>%
  summarise(mean = mean(value)
  ) %>%
  as.data.frame()
head(myData3); dim(myData3)
myData3[myData3$species==1,]





## Assemble data for JAGS model ----

# data list
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


# Initial values
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
                
# Run model ----
mod1 <-jags(dataList.mod1, 
                 mod1.inits, 
                 para.names1, 
                 n.thin=thin.rate, 
                 n.chains=nc, n.burnin=n.burn, n.iter=n.iterations,
                 model.file="model1.R")


mod1
mod1.mcmc <- as.mcmc(mod1)

pdf(file="mod1.JAGS.diagnostic.pdf", width = 12, height = 10)
par(mar=c(4,2,2,1))
plot(mod1)
plot(mod1.mcmc)
dev.off()


# jagsModel <- jags.model(file = "TEMPmodel.R", data = dataList, n.chains = 3, n.adapt = 10000)
# # Continue the MCMC runs with sampling
# Samples <- coda.samples(jagsModel , variable.names = para.names, n.iter = 10000)
# summary(Samples)



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

