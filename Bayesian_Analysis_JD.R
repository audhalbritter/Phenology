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
options(max.print=10000, dplyr.print_max = 10000)

# LOAD DATA----
#load(file = "PhenoLong.RData", verbose = TRUE)
load(file = "~/Dropbox/Research/Collaborations1/Aud phenology/180111_PhenoLong.RData", verbose = TRUE)

myData0 <- Phenology %>% 
  # subset
  filter(pheno.stage == "Flower", pheno.var == "peak", pheno.unit == "DaysSinceSM") %>% 
  select(value, newTT, species, siteID, destSiteID, blockID, destBlockID) %>% 
  rename(treatment=newTT, origSite=siteID, origBlock=blockID, destBlock=destBlockID, destSite=destSiteID) %>% 
  mutate(treatment = factor(treatment, levels = c("Control", "Warmer", "LaterSM", "WarmLate"))) %>% 
  mutate(treatmentID = as.numeric(as.factor(treatment)), origSiteID = as.numeric(as.factor(origSite)), speciesID = as.numeric(factor(species)), origSiteID = as.numeric(as.factor(origSite)), destSiteID=as.numeric(as.factor(destSite)), origBlockID = as.numeric(as.factor(origBlock)), destBlockID=as.numeric(as.factor(destBlock))
         ) %>%
  as.data.frame()

head(myData0); dim(myData0)
table(myData0$origSiteID, myData0$destSiteID)
length(unique(myData0$species))


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




## Can average the control plots within a certain destBlock for a species
cc <- colnames(myData0) ; cc<- cc[-grep("value",cc)]
  
myData <- myData0 %>%
  group_by(.dots=names(myData0)[-grep("value", names(myData0))]) %>%   # group by all but one variable
  summarise(value = mean(value)
  ) %>%
  as.data.frame()
head(myData); dim(myData)


## Checking data ----
## exploring data structure - multiple values per species-treatment-block?
check1 <- filter(Phenology, pheno.stage == "Flower", pheno.var == "peak", pheno.unit == "DaysSinceSM") 
temp <- check1 %>%
  group_by(species,blockID,destBlockID,newTT) %>%
  summarise(num.turfs=length(unique(turfID))
            ,num.unique.values=length(unique(value))
            ,num.values=length(value)
            ,num.years=length(unique(Year))
  )
temp[temp$newTT!="Control",]
temp[temp$newTT=="Control",]
with(check1, check1[species=="Vio.bif" & blockID=="Ram6" & destBlockID=="Ram6",])
with(check1, check1[species=="Vio.bif"  & destBlockID=="Ram8",])


temp <- myData %>%
  group_by(species,origBlock, destBlock, treatment) %>%
  summarise(num.values=length(value)
            )
temp[temp$treatment!="Control",]
temp[temp$treatment=="Control",]
with(temp, temp[species=="Vio.bif" & origBlock=="Ram6" & destBlock=="Ram6",])
with(temp, temp[species=="Vio.bif"  & destBlock=="Ram8",])


# need to get those species-site combinations that have both control and treatment
myData.check <- myData %>%
  #  group_by(species,destBlockID) %>%
  group_by(species, origSite) %>%  # was destSite
  summarise(w2 = ifelse(length(value[treatmentID==2])>0 & length(value[treatmentID==1])>0, 1,0)
            ,warm.contrast = ifelse(w2==1, mean(value[treatmentID==2]) - mean(value[treatmentID==1]), NA)
            ,w3 = ifelse(length(value[treatmentID==3])>0 & length(value[treatmentID==1])>0, 1,0)
            ,late.contrast = ifelse(w3==1, mean(value[treatmentID==3]) - mean(value[treatmentID==1]), NA)
            ,w4 = ifelse(length(value[treatmentID==4])>0 & length(value[treatmentID==1])>0, 1,0)
            ,warmlate.contrast = ifelse(w4==1, mean(value[treatmentID==4]) - mean(value[treatmentID==1]), NA)
  ) %>%
  as.data.frame()
head(myData.check); dim(myData.check)
#

## Checking here which species-site combinations have each contrast
warm.contrasts <- filter(myData.check, is.na(warm.contrast)==FALSE)
late.contrasts <- filter(myData.check, is.na(late.contrast)==FALSE)
warmlate.contrasts <- filter(myData.check,is.na(warmlate.contrast)==FALSE)
# Seems there are very few species to compare flowering times across treatments within the same sites
# We decided to make comparisons at the site level instead of blocks; blocks are replicates within sites
# Is this assessment correct??
length(unique(myData$species)) # 77 species
length(unique(warm.contrasts$species)) # 13 species have warm treatment contrast
length(unique(late.contrasts$species)) # 18 species
length(unique(warmlate.contrasts$species)) # 13 species

# remove species for which there are not comparisons for each model
warm.keep <- paste(warm.contrasts$species, warm.contrasts$origSite,sep='.')
late.keep <- paste(late.contrasts$species, late.contrasts$origSite,sep='.')
warmlate.keep <- paste(warmlate.contrasts$species, warmlate.contrasts$origSite,sep='.')


## Assemble data for JAGS model ----

myData$sp.site <- paste(myData$species,myData$origSite,sep='.')

data.sub <- filter(myData, sp.site %in% warm.keep, treatment != "LaterSM",treatment != "WarmLate" )
## Have to reset the IDs given the subset
head(data.sub); dim(data.sub)
data.sub$treatmentID <- as.numeric(as.factor(data.sub$treatment))
data.sub$origSiteID <- as.numeric(as.factor(data.sub$origSite))
data.sub$origBlockID <- as.numeric(as.factor(data.sub$origBlock))
data.sub$destBlockID <- as.numeric(as.factor(data.sub$destBlock))
data.sub$destSiteID <- as.numeric(as.factor(data.sub$destSite))
data.sub$speciesID <- as.numeric(as.factor(data.sub$species))

# [treatment, species, origSiteID]
temp <- data.sub %>%
  group_by(species, destSite, treatment) %>%
  summarise(num.values=length(value)
  )
temp
table(temp$num.values)

# species.table.plasticity.warm
sptab1.sites <- data.sub %>%
  group_by(species, origSite) %>%
  summarise(mean.warm=mean(value[treatment=="Warmer"])
            ,mean.control=mean(value[treatment=="Control"])
            ,warm.contrast=mean.warm-mean.control
) %>%
  as.data.frame()
sptab1.sites; dim(sptab1.sites)  # 

sptab1 <- data.sub %>%
  group_by(species) %>%
  summarise(num.sites = length(unique(origSite))
            ,mean.warm=mean(value[treatment=="Warmer"])
            ,mean.control=mean(value[treatment=="Control"])
            ,warm.contrast=mean.warm-mean.control
  ) %>%
  as.data.frame()
sptab1; dim(sptab1)  # 


y = data.sub$value

Ntotal <- length(y) 
NtreatmentLvl <- nlevels(factor(data.sub$treatment)) 
NorigSiteLvl <- nlevels(factor(data.sub$origSiteID))
NdestSiteLvl <- nlevels(factor(data.sub$destSiteID))
NSPLvl <- nlevels(factor(data.sub$species))
NorigBlockLvl <- nlevels(factor(data.sub$origBlockID))
NdestBlockLvl <- nlevels(factor(data.sub$destBlockID))

## Model 1 ----
# data list
mod1.data <- list(y = y, 
                  speciesID = data.sub$speciesID, 
                  NorigSiteLvl = NorigSiteLvl,
                  Ntotal = Ntotal, 
                  NSPLvl = NSPLvl,
                  NBlockLvl = NdestBlockLvl,   # was origBlock  - should RE be for orig block or for destination block?  Not sure i understand why orig
                  treatmentID = data.sub$treatmentID 
                  ,origSiteID = data.sub$origSiteID
#                  ,destBlockID = data.sub$destBlockID 
                  # destSite = destSite,
#                 origBlock = origBlock, 
#                 NtreatmentLvl = NtreatmentLvl, 
                 # NdestSiteLvl = NdestSiteLvl,
#                 ,NdestBlockLvl = nlevels(factor(destBlockID))
)


# Initial values
mod1.inits<-function(){
  list(
    tau = rep(1,NSPLvl)
  ,BlockPrec=1
  ,tau.slope = rep(1,NSPLvl)
#    ,alpha = matrix(0,N_sp, N_plots)
#    ,alpha = rep(0,NSPLvl)
  )
}

n.iterations <- 50000      ## draws from posterior
n.burn <- 10000      ## draws to discard as burn-in
thin.rate <- 10    	## thinning rate
nc <- 3			## number of chains

# SPECIFY PARAMETERS - these were somehow making a problem; I have never tried specifying like this; might be possible, but not sure
#para.names <- c("alpha", paste("treatmentCoeff[", 2:4, "]", sep = ""), paste("spCoeff[", 1:66, "]", sep = ""), paste("origBlockCoeff[", 1:30, "]", sep = ""), "tau")

para.names <- c("warm.treatment","treatmentCoeff")#,"blockCoeff") # "alpha"
                
# Run model ----
mod1 <-jags(mod1.data, 
                 mod1.inits, 
                 para.names, 
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


# jagsModel <- jags.model(file = "TEMPmodel.R", data = dataList, n.chains = 3, n.adapt = 10000)
# # Continue the MCMC runs with sampling
# Samples <- coda.samples(jagsModel , variable.names = para.names, n.iter = 10000)
# summary(Samples)




## Model 2 ----
mod2.data <- list(y = y, 
                  speciesID = data.sub$speciesID, 
                  NorigSiteLvl = NorigSiteLvl,
                  Ntotal = Ntotal, 
                  NSPLvl = NSPLvl,
#                  NBlockLvl = NdestBlockLvl,   # was origBlock  - should RE be for orig block or for destination block?  Not sure i understand why orig
                  treatmentID = data.sub$treatmentID 
                  ,origSiteID = data.sub$origSiteID
)


# Initial values
mod2.inits<-function(){
  list(
    tau.sp=1
    ,tau.blocks = rep(1,NSPLvl)
    ,tau.sites = rep(1,NSPLvl)
    )
}

n.iterations <- 50000      ## draws from posterior
n.burn <- 10000      ## draws to discard as burn-in
thin.rate <- 10    	## thinning rate
nc <- 3			## number of chains

param2 <- c("warm.overall","warm.treatment","warm.site.treat")

# Run model ----
mod2 <-jags(mod2.data, 
            mod2.inits, 
            param2, 
            n.thin=thin.rate, 
            n.chains=nc, n.burnin=n.burn, n.iter=n.iterations,
            model.file="Models/model2.R")


mod2
mod2.mcmc <- as.mcmc(mod2)

pdf(file="mod2.JAGS.diagnostic.pdf", width = 12, height = 10)
par(mar=c(4,2,2,1))
plot(mod2)
plot(mod2.mcmc)
dev.off()


options(scipen=999)
s.table <- data.frame(signif(mod2$BUGSoutput$summary, 3))
p.temp <- pnorm(0,s.table$mean,s.table$sd)
s.table$p <- round(sapply(p.temp, function(x) min(x, 1-x)), 5)
mod2.params <- select(s.table,-c(X25.,X50.,X75.,Rhat,n.eff))
mod2.warm <- mod2.params[grep("warm.treatment",rownames(mod2.params)),]
mod2.warm.overall <- mod2.params[grep("warm.overall",rownames(mod2.params)),]
mod2.warm.site <- mod2.params[grep("warm.site",rownames(mod2.params)),]

write.table(sub.table.growth, "sub.table.growth.csv", sep=',', row.names=TRUE)

## connect model output to species table ----

table(data.sub$species, data.sub$speciesID)
sptab1 <- cbind(sptab1, mod2.warm)
sptab1; dim(sptab1)

plot(sptab1$mean, sptab1$warm.contrast)




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

