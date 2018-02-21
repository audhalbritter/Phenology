#--- --- --- --- --- --- --- --- --- --- --- --- --- --- 
# LOAD LIBRARIES ----


graphics.off() # This closes all of R's graphics windows.
rm(list=ls())  # Careful! This clears all of R's memory!

# load libraries
source('~/Google Drive/Research2/Stat1/R/scripts/Synced.Rprofile.R')
library("tidyverse")
library("rjags")
library("R2jags")

#--- --- --- --- --- --- --- --- --- --- --- --- --- --- 
options(max.print=10000, dplyr.print_max = 10000)

# LOAD DATA----
#load(file = "PhenoLong.RData", verbose = TRUE)
load(file = "~/Google Drive/Projects1/Aud phenology/180111_PhenoLong.RData", verbose = TRUE)

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


myData$sp.site <- paste(myData$species,myData$origSite,sep='.')


## Assemble data for JAGS WARM model ---- 

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
sptab2.sites <- data.sub %>%
  group_by(species, origSite) %>%
  summarise(mean.warm=mean(value[treatment=="Warmer"])
            ,mean.control=mean(value[treatment=="Control"])
            ,warm.contrast=mean.warm-mean.control
) %>%
  as.data.frame()
sptab2.sites; dim(sptab2.sites)  # 

sptab2 <- data.sub %>%
  group_by(species) %>%
  summarise(num.sites = length(unique(origSite))
            ,mean.warm=mean(value[treatment=="Warmer"])
            ,mean.control=mean(value[treatment=="Control"])
            ,warm.contrast=mean.warm-mean.control
  ) %>%
  as.data.frame()
sptab2; dim(sptab2)  # 


y = data.sub$value

Ntotal <- length(y) 
NtreatmentLvl <- nlevels(factor(data.sub$treatment)) 
NorigSiteLvl <- nlevels(factor(data.sub$origSiteID))
NdestSiteLvl <- nlevels(factor(data.sub$destSiteID))
NSPLvl <- nlevels(factor(data.sub$species))
NorigBlockLvl <- nlevels(factor(data.sub$origBlockID))
NdestBlockLvl <- nlevels(factor(data.sub$destBlockID))

  # Model 1 is also a warm model but does not have nested effects, so can't pull out the contrasts at different levels like the below models
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

n.iterations <- 10000      ## draws from posterior
n.burn <- 2000      ## draws to discard as burn-in
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
gelman.plot(mod2.mcmc)
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

## connect model output to species table ----

sptable2 <- cbind(sptab2, mod2.warm)
sptable2; dim(sptable2)
plot(sptable2$mean, sptable2$warm.contrast)
abline(0,1, lty=2)
write.table(sptable2, "model output/sptab1.warm.plasticity.csv", sep=',', row.names=TRUE)

source("print.parameters.R")
pdf(file="model output/mod2.warm.param.pdf", width = 6, height = 10)
print.parameters(sptable1, mod2.warm.overall, "days shift in peak flowering\ndue to warming", title='Plasticity model')
dev.off()




## Assemble data for JAGS LATE model ---- 

data.sub <- droplevels(filter(myData, sp.site %in% late.keep, treatment != "Warmer",treatment != "WarmLate" ))
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

# species.table.plasticity.late
sptab3.sites <- data.sub %>%
  group_by(species, origSite) %>%
  summarise(mean.late=mean(value[treatment=="LaterSM"])
            ,mean.control=mean(value[treatment=="Control"])
            ,late.contrast=mean.late-mean.control
  ) %>%
  as.data.frame()
sptab3.sites; dim(sptab3.sites)  # 

sptab3 <- data.sub %>%
  group_by(species) %>%
  summarise(num.sites = length(unique(origSite))
            ,mean.late=mean(value[treatment=="LaterSM"])
            ,mean.control=mean(value[treatment=="Control"])
            ,late.contrast=mean.late-mean.control
  ) %>%
  as.data.frame()
sptab3; dim(sptab3)  # 


y = data.sub$value

Ntotal <- length(y) 
NtreatmentLvl <- nlevels(factor(data.sub$treatment)) 
NorigSiteLvl <- nlevels(factor(data.sub$origSiteID))
NdestSiteLvl <- nlevels(factor(data.sub$destSiteID))
NSPLvl <- nlevels(factor(data.sub$species))
NorigBlockLvl <- nlevels(factor(data.sub$origBlockID))
NdestBlockLvl <- nlevels(factor(data.sub$destBlockID))


## Model 3 ----
mod3.data <- list(y = y, 
                  speciesID = data.sub$speciesID, 
                  NorigSiteLvl = NorigSiteLvl,
                  Ntotal = Ntotal, 
                  NSPLvl = NSPLvl,
                  #                  NBlockLvl = NdestBlockLvl,   # was origBlock  - should RE be for orig block or for destination block?  Not sure i understand why orig
                  treatmentID = data.sub$treatmentID 
                  ,origSiteID = data.sub$origSiteID
)


# Initial values
mod3.inits<-function(){
  list(
    tau.sp=1
    ,tau.blocks = rep(1,NSPLvl)
    ,tau.sites = rep(1,NSPLvl)
  )
}

n.iterations <- 10000      ## draws from posterior
n.burn <- 2000      ## draws to discard as burn-in
thin.rate <- 10    	## thinning rate
nc <- 3			## number of chains

param3 <- c("late.overall","late.treatment","late.site.treat")

# Run model ----
mod3 <- jags(mod3.data, 
            mod3.inits, 
            param3, 
            n.thin=thin.rate, 
            n.chains=nc, n.burnin=n.burn, n.iter=n.iterations,
            model.file="Models/model3.R")


mod3
mod3.mcmc <- as.mcmc(mod3)

pdf(file="mod3.JAGS.diagnostic.pdf", width = 12, height = 10)
par(mar=c(4,2,2,1))
plot(mod3)
gelman.plot(mod3.mcmc)
plot(mod3.mcmc)
dev.off()


options(scipen=999)
s.table <- data.frame(signif(mod3$BUGSoutput$summary, 3))
p.temp <- pnorm(0,s.table$mean,s.table$sd)
s.table$p <- round(sapply(p.temp, function(x) min(x, 1-x)), 5)
mod3.params <- select(s.table,-c(X25.,X50.,X75.,Rhat,n.eff))
mod3.late <- mod3.params[grep("late.treatment",rownames(mod3.params)),]
mod3.late.overall <- mod3.params[grep("late.overall",rownames(mod3.params)),]
mod3.late.site <- mod3.params[grep("late.site",rownames(mod3.params)),]

## connect model output to species table ----

sptable3 <- cbind(sptab3, mod3.late)
sptable3; dim(sptable3)
plot(sptable3$mean, sptable3$late.contrast); abline(0, 1, lty=2)

write.table(sptable3, "model output/sptab3.late.plasticity.csv", sep=',', row.names=TRUE)

source("print.parameters.R")
pdf(file="model output/mod3.late.param.pdf", width = 6, height = 10)
print.parameters(sptable3, mod3.late.overall, "days shift in peak flowering\ndue to later season", title='Plasticity model')
dev.off()




## Assemble data for JAGS Warm-LATE model ---- 

data.sub <- droplevels(filter(myData, sp.site %in% warmlate.keep, treatment != "Warmer",treatment != "LaterSM" ))
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

# species.table.plasticity.late
sptab4.sites <- data.sub %>%
  group_by(species, origSite) %>%
  summarise(mean.warmlate=mean(value[treatment=="WarmLate"])
            ,mean.control=mean(value[treatment=="Control"])
            ,warmlate.contrast=mean.warmlate-mean.control
  ) %>%
  as.data.frame()
sptab4.sites; dim(sptab4.sites)  # 

sptab4 <- data.sub %>%
  group_by(species) %>%
  summarise(num.sites = length(unique(origSite))
            ,mean.warmlate=mean(value[treatment=="WarmLate"])
            ,mean.control=mean(value[treatment=="Control"])
            ,warmlate.contrast=mean.warmlate-mean.control
  ) %>%
  as.data.frame()
sptab4; dim(sptab4)  # 


y = data.sub$value

Ntotal <- length(y) 
NtreatmentLvl <- nlevels(factor(data.sub$treatment)) 
NorigSiteLvl <- nlevels(factor(data.sub$origSiteID))
NdestSiteLvl <- nlevels(factor(data.sub$destSiteID))
NSPLvl <- nlevels(factor(data.sub$species))
NorigBlockLvl <- nlevels(factor(data.sub$origBlockID))
NdestBlockLvl <- nlevels(factor(data.sub$destBlockID))

## Model 4 ----
mod4.data <- list(y = y, 
                  speciesID = data.sub$speciesID, 
                  NorigSiteLvl = NorigSiteLvl,
                  Ntotal = Ntotal, 
                  NSPLvl = NSPLvl,
                  #                  NBlockLvl = NdestBlockLvl,   # was origBlock  - should RE be for orig block or for destination block?  Not sure i understand why orig
                  treatmentID = data.sub$treatmentID 
                  ,origSiteID = data.sub$origSiteID
)


# Initial values
mod4.inits<-function(){
  list(
    tau.sp=1
    ,tau.blocks = rep(1,NSPLvl)
    ,tau.sites = rep(1,NSPLvl)
  )
}

n.iterations <- 20000      ## draws from posterior
n.burn <- 5000      ## draws to discard as burn-in
thin.rate <- 10    	## thinning rate
nc <- 3			## number of chains

param4 <- c("warmlate.overall","warmlate.treatment","warmlate.site.treat")

# Run model ----
mod4 <- jags(mod4.data, 
             mod4.inits, 
             param4, 
             n.thin=thin.rate, 
             n.chains=nc, n.burnin=n.burn, n.iter=n.iterations,
             model.file="Models/model4.R")


mod4
mod4.mcmc <- as.mcmc(mod4)

pdf(file="mod4.JAGS.diagnostic.pdf", width = 12, height = 10)
par(mar=c(4,2,2,1))
plot(mod4)
gelman.plot(mod4.mcmc)
#gelman.diag(mod4.mcmc)
plot(mod4.mcmc)
dev.off()


options(scipen=999)
s.table <- data.frame(signif(mod4$BUGSoutput$summary, 3))
p.temp <- pnorm(0,s.table$mean,s.table$sd)
s.table$p <- round(sapply(p.temp, function(x) min(x, 1-x)), 5)
mod4.params <- select(s.table,-c(X25.,X50.,X75.,Rhat,n.eff))
mod4.warmlate <- mod4.params[grep("warmlate.treatment",rownames(mod4.params)),]
mod4.warmlate.overall <- mod4.params[grep("warmlate.overall",rownames(mod4.params)),]
mod4.warmlate.site <- mod4.params[grep("warmlate.site",rownames(mod4.params)),]

## connect model output to species table ----

sptable4 <- cbind(sptab4, mod4.warmlate)
sptable4; dim(sptable4)
plot(sptable4$mean, sptable4$warmlate.contrast); abline(0,1,lty=2)

write.table(sptab4, "model output/sptab4.warmlate.plasticity.csv", sep=',', row.names=TRUE)

source("print.parameters.R")
pdf(file="model output/mod4.warmlate.param.pdf", width = 6, height = 10)
print.parameters(sptable4, mod4.warmlate.overall, "days shift in peak flowering\ndue to warmlater season", title='Plasticity model')
dev.off()



pdf(file="model output/mod.compare.pdf", width = 9, height = 3)
par(mfrow=c(1,3))
plot(sptable2$warm.contrast,sptable2$mean, bty='l', xlab='Empirical Warm Contrast', ylab='Modeled Warm Contrast'); abline(0,1,lty=2)
plot(sptable3$late.contrast,sptable3$mean, bty='l', xlab='Empirical Late Contrast', ylab='Modeled Late Contrast'); abline(0,1,lty=2)
plot(sptable4$warmlate.contrast,sptable4$mean, bty='l', xlab='Empirical WarmLate Contrast', ylab='Modeled WarmLate Contrast'); abline(0,1,lty=2)
dev.off()






## Genetic models ----

## Plasticity
# [treatment, species, origSiteID]

## Genetics
# [treatment, species, destSiteID]



# need to get those species-site combinations that have both control and treatment
myData.check <- myData %>%
  group_by(species, destSite) %>%  # was origSite in Plasticity model
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
warm.keep <- paste(warm.contrasts$species, warm.contrasts$destSite,sep='.')  # changed from orig to dest for Genetic models
late.keep <- paste(late.contrasts$species, late.contrasts$destSite,sep='.')
warmlate.keep <- paste(warmlate.contrasts$species, warmlate.contrasts$destSite,sep='.')

myData$sp.site <- paste(myData$species,myData$destSite,sep='.')

## Assemble data for JAGS WARM model ---- 

data.sub <- filter(myData, sp.site %in% warm.keep, treatment != "LaterSM",treatment != "WarmLate" )
## Have to reset the IDs given the subset
head(data.sub); dim(data.sub)
data.sub$treatmentID <- as.numeric(as.factor(data.sub$treatment))
data.sub$origSiteID <- as.numeric(as.factor(data.sub$origSite))
data.sub$origBlockID <- as.numeric(as.factor(data.sub$origBlock))
data.sub$destBlockID <- as.numeric(as.factor(data.sub$destBlock))
data.sub$destSiteID <- as.numeric(as.factor(data.sub$destSite))
data.sub$speciesID <- as.numeric(as.factor(data.sub$species))

# [treatment, species, destSiteID]
temp <- data.sub %>%
  group_by(species, destSite, treatment) %>%
  summarise(num.values=length(value)
  )
temp
table(temp$num.values)

# species.table.GENETICS.warm
sptab2.GEN.sites <- data.sub %>%
  group_by(species,destSite) %>%
  summarise(mean.warm=mean(value[treatment=="Warmer"])
            ,mean.control=mean(value[treatment=="Control"])
            ,warm.contrast=mean.warm-mean.control
  ) %>%
  as.data.frame()
sptab2.sites; dim(sptab2.sites)  # 

sptab2.GEN <- data.sub %>%
  group_by(species) %>%
  summarise(num.sites = length(unique(destSite)) # changed to dest
            ,mean.warm=mean(value[treatment=="Warmer"])
            ,mean.control=mean(value[treatment=="Control"])
            ,warm.contrast=mean.warm-mean.control
  ) %>%
  as.data.frame()
sptab2.GEN; dim(sptab2.GEN)  # 


y = data.sub$value

Ntotal <- length(y) 
NtreatmentLvl <- nlevels(factor(data.sub$treatment)) 
NorigSiteLvl <- nlevels(factor(data.sub$origSiteID))
NdestSiteLvl <- nlevels(factor(data.sub$destSiteID))
NSPLvl <- nlevels(factor(data.sub$species))
NorigBlockLvl <- nlevels(factor(data.sub$origBlockID))
NdestBlockLvl <- nlevels(factor(data.sub$destBlockID))

## Model 2 GENETIC ----
mod2Gen.data <- list(y = y, 
                  speciesID = data.sub$speciesID, 
                  NdestSiteLvl = NdestSiteLvl, #NorigSiteLvl = NorigSiteLvl,   # changed frp, orig to dest
                  Ntotal = Ntotal, 
                  NSPLvl = NSPLvl,
                  #                  NBlockLvl = NdestBlockLvl,   # was origBlock  - should RE be for orig block or for destination block?  Not sure i understand why orig
                  treatmentID = data.sub$treatmentID 
                  ,destSiteID = data.sub$destSiteID # changed from orig to dest
)


# Initial values
mod2Gen.inits<-function(){
  list(
    tau.sp=1
    ,tau.blocks = rep(1,NSPLvl)
    ,tau.sites = rep(1,NSPLvl)
  )
}

n.iterations <- 10000      ## draws from posterior
n.burn <- 5000      ## draws to discard as burn-in
thin.rate <- 10    	## thinning rate
nc <- 3			## number of chains

param2 <- c("warm.overall","warm.treatment","warm.site.treat")

# Run mod2Gen model ----
mod2Gen <-jags(mod2Gen.data, 
            mod2Gen.inits, 
            param2, 
            n.thin=thin.rate, 
            n.chains=nc, n.burnin=n.burn, n.iter=n.iterations,
            model.file="Models/model2Gen.R")


mod2Gen
mod2Gen.mcmc <- as.mcmc(mod2Gen)


pdf(file="mod2Gen.JAGS.diagnostic.pdf", width = 12, height = 10)
par(mar=c(4,2,2,1))
plot(mod2Gen)
gelman.plot(mod2Gen.mcmc)
plot(mod2Gen.mcmc)
dev.off()


options(scipen=999)
s.table <- data.frame(signif(mod2Gen$BUGSoutput$summary, 3))
p.temp <- pnorm(0,s.table$mean,s.table$sd)
s.table$p <- round(sapply(p.temp, function(x) min(x, 1-x)), 5)
mod2Gen.params <- select(s.table,-c(X25.,X50.,X75.,Rhat,n.eff))
mod2Gen.warm <- mod2Gen.params[grep("warm.treatment",rownames(mod2Gen.params)),]
mod2Gen.warm.overall <- mod2Gen.params[grep("warm.overall",rownames(mod2Gen.params)),]
mod2Gen.warm.site <- mod2Gen.params[grep("warm.site",rownames(mod2Gen.params)),]

## connect model output to species table ----

sptable2.GEN <- cbind(sptab2.GEN, mod2Gen.warm)
sptable2.GEN; dim(sptable2.GEN)
plot(sptable2.GEN$mean, sptable2.GEN$warm.contrast); abline(0,1, lty=2)
write.table(sptable2.GEN, "model output/sptable2.GEN.csv", sep=',', row.names=TRUE)

source("print.parameters.R")
pdf(file="model output/mod2Gen.warm.param.pdf", width = 6, height = 10)
print.parameters(sptable2.GEN, mod2Gen.warm.overall, "days shift in peak flowering\ndue to warming", title='Genetic model')
dev.off()



## Assemble data for JAGS LATE model GENETICS---- 

data.sub <- droplevels(filter(myData, sp.site %in% late.keep, treatment != "Warmer",treatment != "WarmLate" ))
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

# species.table.plasticity.late
sptab3.GEN.sites <- data.sub %>%
  group_by(species, destSite) %>%
  summarise(mean.late=mean(value[treatment=="LaterSM"])
            ,mean.control=mean(value[treatment=="Control"])
            ,late.contrast=mean.late-mean.control
  ) %>%
  as.data.frame()
sptab3.GEN.sites; dim(sptab3.GEN.sites)  # 

sptab3.GEN <- data.sub %>%
  group_by(species) %>%
  summarise(num.sites = length(unique(origSite))
            ,mean.late=mean(value[treatment=="LaterSM"])
            ,mean.control=mean(value[treatment=="Control"])
            ,late.contrast=mean.late-mean.control
  ) %>%
  as.data.frame()
sptab3.GEN; dim(sptab3.GEN)  # 


y = data.sub$value

Ntotal <- length(y) 
NtreatmentLvl <- nlevels(factor(data.sub$treatment)) 
NorigSiteLvl <- nlevels(factor(data.sub$origSiteID))
NdestSiteLvl <- nlevels(factor(data.sub$destSiteID))
NSPLvl <- nlevels(factor(data.sub$species))
NorigBlockLvl <- nlevels(factor(data.sub$origBlockID))
NdestBlockLvl <- nlevels(factor(data.sub$destBlockID))


## Model 3 ----
mod3Gen.data <- list(y = y, 
                  speciesID = data.sub$speciesID, 
                  NdestSiteLvl = NdestSiteLvl, #NorigSiteLvl = NorigSiteLvl,
                  Ntotal = Ntotal, 
                  NSPLvl = NSPLvl,
                  #                  NBlockLvl = NdestBlockLvl,   # was origBlock  - should RE be for orig block or for destination block?  Not sure i understand why orig
                  treatmentID = data.sub$treatmentID 
                  ,destSiteID = data.sub$destSiteID
)


# Initial values
mod3Gen.inits<-function(){
  list(
    tau.sp=1
    ,tau.blocks = rep(1,NSPLvl)
    ,tau.sites = rep(1,NSPLvl)
  )
}

n.iterations <- 10000      ## draws from posterior
n.burn <- 5000      ## draws to discard as burn-in
thin.rate <- 10    	## thinning rate
nc <- 3			## number of chains

param3 <- c("late.overall","late.treatment","late.site.treat")

# Run model ----
mod3Gen <- jags(mod3Gen.data, 
             mod3Gen.inits, 
             param3, 
             n.thin=thin.rate, 
             n.chains=nc, n.burnin=n.burn, n.iter=n.iterations,
             model.file="Models/model3Gen.R")


mod3Gen
mod3Gen.mcmc <- as.mcmc(mod3Gen)

pdf(file="mod3Gen.JAGS.diagnostic.pdf", width = 12, height = 10)
par(mar=c(4,2,2,1))
plot(mod3Gen)
gelman.plot(mod3Gen.mcmc)
plot(mod3Gen.mcmc)
dev.off()


options(scipen=999)
s.table <- data.frame(signif(mod3Gen$BUGSoutput$summary, 3))
p.temp <- pnorm(0,s.table$mean,s.table$sd)
s.table$p <- round(sapply(p.temp, function(x) min(x, 1-x)), 5)
mod3Gen.params <- select(s.table,-c(X25.,X50.,X75.,Rhat,n.eff))
mod3Gen.late <- mod3Gen.params[grep("late.treatment",rownames(mod3Gen.params)),]
mod3Gen.late.overall <- mod3Gen.params[grep("late.overall",rownames(mod3Gen.params)),]
mod3Gen.late.site <- mod3Gen.params[grep("late.site",rownames(mod3Gen.params)),]

## connect model output to species table ----

sptable3.GEN <- cbind(sptab3.GEN, mod3Gen.late)
sptable3.GEN; dim(sptable3.GEN)
plot(sptable3.GEN$mean, sptable3.GEN$late.contrast); abline(0, 1, lty=2)

write.table(sptable3, "model output/sptab3.late.plasticity.csv", sep=',', row.names=TRUE)

source("print.parameters.R")
pdf(file="model output/mod3Gen.late.param.pdf", width = 6, height = 10)
print.parameters(sptable3.GEN, mod3Gen.late.overall, "days shift in peak flowering\ndue to later season", title='Genetics model')
dev.off()




## Assemble data for Mod4 GENETICS Warm-LATE model ---- 

data.sub <- droplevels(filter(myData, sp.site %in% warmlate.keep, treatment != "Warmer",treatment != "LaterSM" ))
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

# species.table.plasticity.late
sptab4.GEN.sites <- data.sub %>%
  group_by(species, destSite) %>%
  summarise(mean.warmlate=mean(value[treatment=="WarmLate"])
            ,mean.control=mean(value[treatment=="Control"])
            ,warmlate.contrast=mean.warmlate-mean.control
  ) %>%
  as.data.frame()
sptab4.GEN.sites; dim(sptab4.GEN.sites)  # 

sptab4.GEN <- data.sub %>%
  group_by(species) %>%
  summarise(num.sites = length(unique(origSite))
            ,mean.warmlate=mean(value[treatment=="WarmLate"])
            ,mean.control=mean(value[treatment=="Control"])
            ,warmlate.contrast=mean.warmlate-mean.control
  ) %>%
  as.data.frame()
sptab4.GEN; dim(sptab4.GEN)  # 


y = data.sub$value

Ntotal <- length(y) 
NtreatmentLvl <- nlevels(factor(data.sub$treatment)) 
NorigSiteLvl <- nlevels(factor(data.sub$origSiteID))
NdestSiteLvl <- nlevels(factor(data.sub$destSiteID))
NSPLvl <- nlevels(factor(data.sub$species))
NorigBlockLvl <- nlevels(factor(data.sub$origBlockID))
NdestBlockLvl <- nlevels(factor(data.sub$destBlockID))

## Model 4 ----
mod4Gen.data <- list(y = y, 
                  speciesID = data.sub$speciesID, 
                  NdestSiteLvl = NdestSiteLvl, #NorigSiteLvl = NorigSiteLvl,
                  Ntotal = Ntotal, 
                  NSPLvl = NSPLvl,
                  #                  NBlockLvl = NdestBlockLvl,   # was origBlock  - should RE be for orig block or for destination block?  Not sure i understand why orig
                  treatmentID = data.sub$treatmentID 
                  ,destSiteID = data.sub$destSiteID
)


# Initial values
mod4Gen.inits<-function(){
  list(
    tau.sp=1
    ,tau.blocks = rep(1,NSPLvl)
    ,tau.sites = rep(1,NSPLvl)
  )
}

n.iterations <- 20000      ## draws from posterior
n.burn <- 5000      ## draws to discard as burn-in
thin.rate <- 10    	## thinning rate
nc <- 3			## number of chains

param4Gen <- c("warmlate.overall","warmlate.treatment","warmlate.site.treat")

# Run model ----
mod4Gen <- jags(mod4Gen.data, 
             mod4Gen.inits, 
             param4Gen, 
             n.thin=thin.rate, 
             n.chains=nc, n.burnin=n.burn, n.iter=n.iterations,
             model.file="Models/model4Gen.R")


mod4Gen
mod4Gen.mcmc <- as.mcmc(mod4Gen)

pdf(file="mod4Gen.JAGS.diagnostic.pdf", width = 12, height = 10)
par(mar=c(4,2,2,1))
plot(mod4Gen)
gelman.plot(mod4Gen.mcmc)
#gelman.diag(mod4Gen.mcmc)
plot(mod4Gen.mcmc)
dev.off()


options(scipen=999)
s.table <- data.frame(signif(mod4Gen$BUGSoutput$summary, 3))
p.temp <- pnorm(0,s.table$mean,s.table$sd)
s.table$p <- round(sapply(p.temp, function(x) min(x, 1-x)), 5)
mod4Gen.params <- select(s.table,-c(X25.,X50.,X75.,Rhat,n.eff))
mod4Gen.warmlate <- mod4Gen.params[grep("warmlate.treatment",rownames(mod4Gen.params)),]
mod4Gen.warmlate.overall <- mod4Gen.params[grep("warmlate.overall",rownames(mod4Gen.params)),]
mod4Gen.warmlate.site <- mod4Gen.params[grep("warmlate.site",rownames(mod4Gen.params)),]

## connect model output to species table ----

sptable4.GEN <- cbind(sptab4.GEN, mod4Gen.warmlate)
sptable4.GEN; dim(sptable4.GEN)
plot(sptable4.GEN$mean, sptable4.GEN$warmlate.contrast); abline(0,1,lty=2)

write.table(sptab4.GEN, "model output/sptab4.warmlate.GEN.csv", sep=',', row.names=TRUE)

source("print.parameters.R")
pdf(file="model output/mod4Gen.warmlate.param.pdf", width = 6, height = 10)
print.parameters(sptable4.GEN, mod4Gen.warmlate.overall, "days shift in peak flowering\ndue to warmlater season", title='Genetics model')
dev.off()



pdf(file="model output/mod.compare.pdf", width = 9, height = 3)
par(mfrow=c(1,3))
plot(sptable2$warm.contrast,sptable2$mean, bty='l', xlab='Empirical Warm Contrast', ylab='Modeled Warm Contrast'); abline(0,1,lty=2)
plot(sptable3$late.contrast,sptable3$mean, bty='l', xlab='Empirical Late Contrast', ylab='Modeled Late Contrast'); abline(0,1,lty=2)
plot(sptable4$warmlate.contrast,sptable4$mean, bty='l', xlab='Empirical WarmLate Contrast', ylab='Modeled WarmLate Contrast'); abline(0,1,lty=2)
dev.off()



