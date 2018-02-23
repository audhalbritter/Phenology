#------------------------------------------------------------------------------
# LOAD LIBRARIES


graphics.off() # This closes all of R's graphics windows.
rm(list=ls())  # Careful! This clears all of R's memory!

# load libraries
library("tidyverse")
library("rjags")
library("R2jags")

pn <- . %>% print(n = Inf)

#------------------------------------------------------------------------------
# LOAD DATA
load(file = "PhenoLong.RData", verbose = TRUE)

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



## Average over the control plots within a certain destBlock for a species
myData <- myData0 %>%
  group_by(.dots = names(myData0)[-grep("value", names(myData0))]) %>% # group by all columns but value
  summarise(value = mean(value)) %>%
  as.data.frame()
head(myData); dim(myData)


length(myData$value[myData$treatmentID == 2]) > 0

# Calculate mean treatment - mean control within species and site 
# need to get those species-site combinations that have both control and treatment
myData.check <- myData %>%
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

# remove species for which there are not comparisons for each model
warm.keep <- paste(warm.contrasts$species, warm.contrasts$origSite,sep='.')
late.keep <- paste(late.contrasts$species, late.contrasts$origSite,sep='.')
warmlate.keep <- paste(warmlate.contrasts$species, warmlate.contrasts$origSite,sep='.')

# Paste speciea and origSite
myData$sp.site <- paste(myData$species, myData$origSite,sep='.')


## Assemble data for JAGS WARM model ---- 

data.sub <- myData %>% 
  filter(sp.site %in% warm.keep, treatment != "LaterSM",treatment != "WarmLate" )

## Have to reset the IDs given the subset
head(data.sub); dim(data.sub)
data.sub$treatmentID <- as.numeric(as.factor(data.sub$treatment))
data.sub$origSiteID <- as.numeric(as.factor(data.sub$origSite))
data.sub$origBlockID <- as.numeric(as.factor(data.sub$origBlock))
data.sub$destBlockID <- as.numeric(as.factor(data.sub$destBlock))
data.sub$destSiteID <- as.numeric(as.factor(data.sub$destSite))
data.sub$speciesID <- as.numeric(as.factor(data.sub$species))


y = data.sub$value

# Load objects in the workspace
Ntotal <- length(y) 
NtreatmentLvl <- nlevels(factor(data.sub$treatment)) 
NorigSiteLvl <- nlevels(factor(data.sub$origSiteID))
NdestSiteLvl <- nlevels(factor(data.sub$destSiteID))
NSPLvl <- nlevels(factor(data.sub$species))
NorigBlockLvl <- nlevels(factor(data.sub$origBlockID))
NdestBlockLvl <- nlevels(factor(data.sub$destBlockID))


# Making a data list
mod2.data <- list(y = y, 
                  speciesID = data.sub$speciesID, 
                  NorigSiteLvl = NorigSiteLvl,
                  Ntotal = Ntotal, 
                  NSPLvl = NSPLvl,
                  # NBlockLvl = NdestBlockLvl,   
                  # was origBlock  - should RE be for orig block or for destination block?  Not sure i understand why orig
                  treatmentID = data.sub$treatmentID, 
                  origSiteID = data.sub$origSiteID
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




# SPECIFY PARAMETERS
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

## connect model output to species table ----
table(data.sub$species, data.sub$speciesID)
sptab1 <- cbind(sptab1, mod2.warm)
sptab1; dim(sptab1)
plot(sptab1$mean, sptab1$warm.contrast)

write.table(sptab1, "model output/sptab1.warm.plasticity.csv", sep=',', row.names=TRUE)

source("print.parameters.R")
pdf(file="ModelOutput/mod2.warm.param.pdf", width = 6, height = 10)
print.parameters(sptab1, mod2.warm.overall, "days shift in peak flowering\ndue to warming", title='Plasticity model')
dev.off()

sp.table <- data.sub %>% 
  select(species, speciesID) %>% 
  distinct()

origSite.table <- data.sub %>% 
  select(origSite, origSiteID) %>% 
  distinct()

results <- data.frame(signif(mod2$BUGSoutput$summary, 3))
results %>% 
  rownames_to_column(var = "variable") %>% 
  select(-c(X25.,X50.,X75.,Rhat,n.eff)) %>% 
  filter(variable != "deviance") %>% 
  mutate(model = gsub("\\[\\d*,\\d\\]|\\[\\d*]", "", variable)) %>% 
  # extract speciesID
  mutate(speciesID = ifelse(variable != "warm.overall", sub(".*?(\\d{1,2}).*", "\\1", variable), "NA")) %>% 
  mutate(speciesID = as.numeric(speciesID)) %>% 
  left_join(sp.table, by = c("speciesID")) %>% 
  # extract origSiteID
  mutate(origSiteID = ifelse(model == "warm.site.treat", substr(variable, nchar(variable)-1, nchar(variable)-1), "NA")) %>% 
  mutate(origSiteID = as.numeric(origSiteID)) %>% 
  left_join(origSite.table, by = c("origSiteID")) %>% 
  filter(model == "warm.site.treat") %>% 
  group_by(species, origSiteID) %>% 
  mutate(p.temp = pnorm(0, mean, sd), p1 = min(p.temp, 1-p.temp)) %>% 
  mutate(signif.symbol = ifelse(p1 < 0.15, "*", " ")) %>% 
  mutate(signif.symbol = ifelse(p1 < 0.10, "**", signif.symbol)) %>% 
  mutate(signif.symbol = ifelse(p1 < 0.05, "***", signif.symbol)) %>% 
  ggplot(aes(x = mean, y = species, xmin = X2.5., xmax = X97.5.)) +
  geom_point() +
  geom_errorbarh() +
  annotate(signif.symbol, x = mean, y = species) +
  facet_wrap(~ origSite)

