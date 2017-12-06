sink('TestModel.txt')
cat("
    model {
    
    ### EFFECT PRIORS 
    # Intercept coefficient
    intercept ~ dnorm(0,0.0001)
    # Temp
    From1To2TempCoef ~ dnorm(0, 0.0001)
    From2To3PrecCoef ~ dnorm(0, 0.0001)
    From3To4PrecCoef ~ dnorm(0, 0.0001)
    
    ### LIKELIHOOD
    for(dataIter in 1:numSamples) {
    log(mu[dataIter]) <- intercept + From1To2Temp[dataIter]*From1To2TempCoef + From2To3Prec[dataIter]*From2To3Prec + From3To4Prec[dataIter]*From3To4Prec
    pheno.var[dataIter] ~ dpois(mu[dataIter])
    }
    }
    ", fill = TRUE)
sink()

# 2) Set up a list that contains all the necessary data (here, including parameters of the prior distribution)
pheno <- pheno.long %>% filter(pheno.stage =="f", pheno.var == "first", pheno.unit == "doy") %>% 
  mutate_each(funs(as.numeric), From1To2Temp, From2To3Temp, From1To2Prec, From2To3Prec, From3To4Prec) %>% 

Data = list(pheno.var = pheno$value, numSamples = length(pheno$value), From1To2Temp = pheno$From1To2Temp, From2To3Prec = pheno$From2To3Prec, From3To4Prec = pheno$From3To4Prec)

# Compile the model and run the MCMC for an adaptation (burn-in) phase
jagsModel <- jags.model(file= "TestModel.txt", data=Data, n.chains = 3, n.adapt= 5000)
#init = inits.fn, 
# Specify parameters for which posterior samples are saved
para.names <- c("intercept","tempCoeffs","precCoeffs")
# Continue the MCMC runs with sampling
Samples <- coda.samples(jagsModel, variable.names = para.names, n.iter = 5000)
plot(Samples)

