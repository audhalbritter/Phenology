####################################
# Bayesian Phenology Model
####################################
#install.packages(c("rjags", "coda", "INLA", "rgdal"), repos = c(getOption("repos"), "http://www.math.ntnu.no/inla/R/stable")) 
# drop levels and convert factors in integer

pheno.dat <- pheno.long # rename data set

# Temperature
pheno.dat$destT_level <- as.character(pheno.dat$destT_level)
pheno.dat$destTemp <- factor(pheno.dat$destT_level, levels=c("1", "2"))
pheno.dat$destTempIndex <- as.integer(pheno.dat$destTemp)

# Precipitation
pheno.dat$destP_level <- as.character(pheno.dat$destP_level)
pheno.dat$destPrec <- factor(pheno.dat$destP_level, levels = c("2", "3", "4"))
pheno.dat$destPrecIndex <- as.integer(pheno.dat$destPrec)

# Species
pheno.dat$species <- as.character(pheno.dat$species)
sp <- unique(pheno.dat$species)
pheno.dat$species <- factor(pheno.dat$species, levels = sp)
pheno.dat$speciesIndex <- as.integer(pheno.dat$species)
pheno <- pheno.dat

# Transform variables
pheno <- pheno.long %>% 
  mutate_each(funs(as.numeric), From1To2Temp, From2To3Temp, From1To2Prec, From2To3Prec, From3To4Prec) %>% # need to be numeric
  mutate_each(funs(as.character), destT_level, destP_level, species) %>%  # make a character
  mutate(destTemp = factor(destT_level, levels = c("1", "2"))) %>%
  mutate(destPrec = factor(destP_level, levels = c("2", "3", "4"))) %>%
  mutate(species = factor(species)) %>% 
  mutate(destTempIndex = as.integer(destTemp)) %>% 
  mutate(destPrecIndex = as.integer(destPrec)) %>% 
  mutate(speciesIndex = as.integer(species)) %>% 
  mutate_each(funs(as.character), Temperature_level, Precipitation_level, species) %>%  # make a character
  mutate(Temp = factor(Temperature_level, levels = c("1", "2"))) %>%
  mutate(Prec = factor(Precipitation_level, levels = c("1", "2", "3", "4"))) %>%
  mutate(TempIndex = as.integer(Temp)) %>% 
  mutate(PrecIndex = as.integer(Prec))

# Subset Data
pheno <- pheno %>% filter(pheno.stage =="f", pheno.var == "peak", pheno.unit == "doy")

#rm(list=ls())
library(rjags)

### First flowering ~ T * P + 1To2T + 2To3P + 3To4P + (1|species)
sink('GLMME_PhenologyDestination.txt')
cat("
    model {
    
    ### EFFECT PRIORS 
    # Random effects

    # Species random effects
    invSpeciesEffectVar ~ dgamma(0.0001, 0.0001)
    for(speciesIter in 1:numSP){
    speciesEffect[speciesIter] ~ dnorm(0, invSpeciesEffectVar)
    }
    
    # Fixed Effects
    # Intercept coefficient
    intercept ~ dnorm(0,0.0001)
    From1To2TempCoef ~ dnorm(0, 0.0001)
    From2To3PrecCoef ~ dnorm(0, 0.0001)
    From3To4PrecCoef ~ dnorm(0, 0.0001)

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
    log(mu[dataIter]) <- intercept + sum(tempCoeffs[1:destTemp[dataIter]]) + sum(precCoeffs[1:destPrec[dataIter]]) + From1To2Temp[dataIter]*From1To2TempCoef + From2To3Prec[dataIter]*From2To3PrecCoef + From3To4Prec[dataIter]*From3To4PrecCoef + speciesEffect[speciesIndex[dataIter]]
    pheno.var[dataIter] ~ dpois(mu[dataIter])
    }
    }
    ", fill = TRUE)
sink()

# 2) Set up a list that contains all the necessary data (here, including parameters of the prior distribution)
Data = list(pheno.var = pheno$value, numSamples = length(pheno$value), numTemps = nlevels(pheno$destTemp), numPrecs = nlevels(pheno$destPrec), numSP = nlevels(pheno$species), destTemp = pheno$destTemp, destPrec = pheno$destPrec, From1To2Temp = pheno$From1To2Temp, From2To3Prec = pheno$From2To3Prec, From3To4Prec = pheno$From3To4Prec, speciesIndex = pheno$speciesIndex)

# 3) Specify a function to generate inital values for the parameters
#inits.fn <- function() list(intercept = rnorm(1), invSpeciesEffectVar = exp(rnorm(1)))

# Compile the model and run the MCMC for an adaptation (burn-in) phase
jagsModel <- jags.model(file= "GLMME_PhenologyDestination.txt", data=Data, n.chains = 3, n.adapt= 5000)
#init = inits.fn, 
# Specify parameters for which posterior samples are saved
para.names <- c("intercept","From1To2TempCoef","From2To3PrecCoef", "From3To4PrecCoef")
# Continue the MCMC runs with sampling
Samples <- coda.samples(jagsModel, variable.names = para.names, n.iter = 5000)
plot(Samples)

# convergence check
gelman.diag(Samples, multivariate = FALSE)
gelman.plot(Samples)

# Statistical summaries of the posterior sample for p => compare to MLE
summary(Samples)





sink('GLMME_PhenologyOrigin.txt')
cat("
    model {
    
    ### EFFECT PRIORS 
    # Random effects
    
    # Species random effects
    invSpeciesEffectVar ~ dgamma(0.0001, 0.0001)
    for(speciesIter in 1:numSP){
    speciesEffect[speciesIter] ~ dnorm(0, invSpeciesEffectVar)
    }
    
    # Fixed Effects
    # Intercept coefficient
    intercept ~ dnorm(0,0.0001)
    From1To2TempCoef ~ dnorm(0, 0.0001)
    From2To3PrecCoef ~ dnorm(0, 0.0001)
    From3To4PrecCoef ~ dnorm(0, 0.0001)
    
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
    log(mu[dataIter]) <- intercept + sum(tempCoeffs[1:Temp[dataIter]]) + sum(precCoeffs[1:Prec[dataIter]]) + From1To2Temp[dataIter]*From1To2TempCoef + From2To3Prec[dataIter]*From2To3PrecCoef + From3To4Prec[dataIter]*From3To4PrecCoef + speciesEffect[speciesIndex[dataIter]]
    pheno.var[dataIter] ~ dpois(mu[dataIter])
    }
    }
    ", fill = TRUE)
sink()

# 2) Set up a list that contains all the necessary data (here, including parameters of the prior distribution)
Data = list(pheno.var = pheno$value, numSamples = length(pheno$value), numTemps = nlevels(pheno$Temp), numPrecs = nlevels(pheno$Prec), numSP = nlevels(pheno$species), Temp = pheno$Temp, Prec = pheno$Prec, From1To2Temp = pheno$From1To2Temp, From2To3Prec = pheno$From2To3Prec, From3To4Prec = pheno$From3To4Prec, speciesIndex = pheno$speciesIndex)

# Compile the model and run the MCMC for an adaptation (burn-in) phase
jagsModel <- jags.model(file= "GLMME_PhenologyOrigin.txt", data=Data, n.chains = 3, n.adapt= 5000)
