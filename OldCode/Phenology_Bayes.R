####################################
# Bayesian Phenology Model
####################################
#install.packages(c("rjags", "coda", "INLA", "rgdal"), repos = c(getOption("repos"), "http://www.math.ntnu.no/inla/R/stable")) 
# drop levels and convert factors in integer

######################
## DESTINATION SITE
######################

pheno.dat <- pheno # rename data set

# Temperature
pheno.dat$destT_level <- as.character(pheno.dat$destT_level)
pheno.dat$destTemp <- factor(pheno.dat$destT_level, levels=c("1", "2"))
pheno.dat$destTempIndex <- as.integer(pheno.dat$destTemp)

# Precipitation
pheno.dat$destP_level <- as.character(pheno.dat$destP_level)
pheno.dat$destPrec <- factor(pheno.dat$destP_level, levels = c("2", "3", "4"))
pheno.dat$destPrecIndex <- as.integer(pheno.dat$destPrec)

# numeric
pheno.dat$From1To2Temp <- as.numeric(pheno.dat$From1To2Temp)
pheno.dat$From2To3Temp <- as.numeric(pheno.dat$From2To3Temp)
pheno.dat$From1To2Prec <- as.numeric(pheno.dat$From1To2Prec)
pheno.dat$From2To4Prec <- as.numeric(pheno.dat$From2To3Prec)
pheno.dat$From3To4Prec <- as.numeric(pheno.dat$From3To4Prec)


# Species
pheno.dat$species <- as.character(pheno.dat$species)
sp <- unique(pheno.dat$species)
pheno.dat$species <- factor(pheno.dat$species, levels = sp)
pheno.dat$specisIndex <- as.integer(pheno.dat$species)



#rm(list=ls())
library(rjags)

### First flowering ~ T * P + (1|Site) + (1|species)
sink('Phenology.txt')
cat("
  model {

    ### EFFECT PRIORS 
    # Random effects
    # Site random effects
    invSiteEffectVar ~ dgamma(0.0001, 0.0001)
    for(siteIter in 1:numSite){
      siteEffect[siteIter] ~ dnorm(0, invSiteEffectVar)
    }
    # Species random effects
    invSpeciesEffectVar ~ dgamma(0.0001, 0.0001)
    for(speciesIter in 1:numSP){
    speciesEffect[speciesIter] ~ dnorm(0, invSpeciesEffectVar)
    }

    # Fixed Effects
    # Intercept coefficient
    intercept ~ dnorm(0,0.0001)
    # Ordinal coefficients for temperature levels
    tempCoeffs[1] <- 0
    for(tempLevelIter in 2:numTemps){
      tempCoeffs[tempLevelIter] ~ dnorm(0, 0.0001)
    }
    # Ordinal coefficients for precipitation levels
    precCoeffs[1] <- 0
    for(precLevelIter in 2:numPrecs){
      precCoeffs[precLevelIter] ~ dnorm(0,0.0001)
    }

    ### LIKELIHOOD
    for(dataIter in 1:numSamples) {
      log(mu[dataIter]) <- intercept + sum(tempCoeffs[1:destTemp[dataIter]]) + sum(precCoeffs[1:destPrec[dataIter]]) +
        siteEffect[siteIndex[dataIter]] + speciesEffect[speciesIndex[dataIter]]
      pheno.var[dataIter] ~ dpois(mu[dataIter])
    }
  }
 ", fill = TRUE)
sink()

# 2) Set up a list that contains all the necessary data (here, including parameters of the prior distribution)
Data = list(pheno.var = pheno.dat$value, numSamples = length(pheno.dat$value), numTemps = nlevels(pheno.dat$destTemp), numPrecs = nlevels(pheno.dat$destPrec), numSite = nlevels(pheno.dat$destSiteID), numSP = nlevels(pheno.dat$species), destTemp = pheno.dat$destTemp, destPrec = pheno.dat$destPrec, siteIndex = pheno.dat$destSiteIDIndex, speciesIndex = pheno.dat$specisIndex)

# 3) Specify a function to generate inital values for the parameters
inits.fn <- function() list(intercept = rnorm(1), invSpeciesEffectVar = exp(rnorm(1)), invSiteEffectVar = exp(rnorm(1)))

# Compile the model and run the MCMC for an adaptation (burn-in) phase
jagsModel <- jags.model(file= "Phenology.txt", data=Data, init = inits.fn, n.chains = 3, n.adapt= 5000)
# Specify parameters for which posterior samples are saved
para.names <- c("intercept","tempCoeffs","precCoeffs")
# Continue the MCMC runs with sampling
Samples <- coda.samples(jagsModel, variable.names = para.names, n.iter = 5000)
plot(Samples)


# convergence check
gelman.diag(Samples, multivariate = FALSE)
gelman.plot(Samples)

# Statistical summaries of the posterior sample for p => compare to MLE
summary(Samples)






######################
## ORIGIN SITE
######################

pheno.dat <- pheno.long # rename data set

# Temperature
pheno.dat$Temperature_level <- as.character(pheno.dat$Temperature_level)
pheno.dat$destTemp <- factor(pheno.dat$Temperature_level, levels=c("1", "2"))
pheno.dat$destTempIndex <- as.integer(pheno.dat$destTemp)

# Precipitation
pheno.dat$Precipitation_level <- as.character(pheno.dat$Precipitation_level)
pheno.dat$destPrec <- factor(pheno.dat$Precipitation_level, levels = c("1", "2", "3", "4"))
pheno.dat$destPrecIndex <- as.integer(pheno.dat$destPrec)

# Site
pheno.dat$siteID <- as.character(pheno.dat$siteID)
pheno.dat$siteID <- factor(pheno.dat$siteID, levels=c("Alrust", "Hogsete", "Rambera", "Veskre", "Ulvhaugen", "Lavisdalen", "Gudmedalen" , "Skjellingahaugen"))
pheno.dat$destSiteIDIndex <- as.integer(pheno.dat$siteID)


# Species
pheno.dat$species <- as.character(pheno.dat$variable)
sp <- unique(pheno.dat$species)
pheno.dat$species <- factor(pheno.dat$species, levels = sp)
pheno.dat$specisIndex <- as.integer(pheno.dat$species)



#rm(list=ls())
library(rjags)

sink('Phenology.txt')
cat("
    model {
    
    ### EFFECT PRIORS 
    # Random effects
    # Site random effects
    invSiteEffectVar ~ dgamma(0.0001, 0.0001)
    for(siteIter in 1:numSite){
    siteEffect[siteIter] ~ dnorm(0, invSiteEffectVar)
    }
    # Species random effects
    invSpeciesEffectVar ~ dgamma(0.0001, 0.0001)
    for(speciesIter in 1:numSP){
    speciesEffect[speciesIter] ~ dnorm(0, invSpeciesEffectVar)
    }
    
    # Fixed Effects
    # Intercept coefficient
    intercept ~ dnorm(0,0.0001)
    # Ordinal coefficients for temperature levels
    tempCoeffs[1] <- 0
    for(tempLevelIter in 2:numTemps){
    tempCoeffs[tempLevelIter] ~ dnorm(0, 0.0001)
    }
    # Ordinal coefficients for precipitation levels
    precCoeffs[1] <- 0
    for(precLevelIter in 2:numPrecs){
    precCoeffs[precLevelIter] ~ dnorm(0,0.0001)
    }
    
    ### LIKELIHOOD
    for(dataIter in 1:numSamples) {
    log(mu[dataIter]) <- intercept + sum(tempCoeffs[1:destTemp[dataIter]]) + sum(precCoeffs[1:destPrec[dataIter]]) +
    siteEffect[siteIndex[dataIter]] + speciesEffect[speciesIndex[dataIter]]
    pheno.var[dataIter] ~ dpois(mu[dataIter])
    }
    }
    ", fill = TRUE)
sink()

# 2) Set up a list that contains all the necessary data (here, including parameters of the prior distribution)
Data = list(pheno.var = pheno.dat$value, numSamples = length(pheno.dat$value), numTemps = nlevels(pheno.dat$destTemp), numPrecs = nlevels(pheno.dat$destPrec), numSite = nlevels(pheno.dat$siteID), numSP = nlevels(pheno.dat$species), destTemp = pheno.dat$destTemp, destPrec = pheno.dat$destPrec, siteIndex = pheno.dat$destSiteIDIndex, speciesIndex = pheno.dat$specisIndex)

# 3) Specify a function to generate inital values for the parameters
inits.fn <- function() list(intercept = rnorm(1), invSpeciesEffectVar = exp(rnorm(1)), invSiteEffectVar = exp(rnorm(1)))
# Compile the model and run the MCMC for an adaptation (burn-in) phase
jagsModel <- jags.model(file= "Phenology.txt", data=Data, init = inits.fn, n.chains = 3, n.adapt= 5000)
# Specify parameters for which posterior samples are saved
para.names <- c("intercept","tempCoeffs","precCoeffs")
# Continue the MCMC runs with sampling
Samples <- coda.samples(jagsModel, variable.names = para.names, n.iter = 5000)
plot(Samples)


# convergence check
gelman.diag(Samples, multivariate = FALSE)
gelman.plot(Samples)

# Statistical summaries of the posterior sample for p => compare to MLE
summary(Samples)