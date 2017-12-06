# read in data
head(pheno.long)

### Model

# simple model with temperature

### First flowering ~ T
sink('Phenology.txt')
cat("
    model {
    
    # Likelihood
    for (i in 1:i.max) {
    value[i] ~ dnorm(mu[i],tau)
    mu[i] <- b.T + a.T * destT_level[i]
    }
    
    # Effect priors 
    a.T ~ dnorm(0,0.0001)
    b.T ~ dnorm(0,0.0001)
    tau ~ dgamma(0.0001, 0.0001)
    #tau <- 1/(sigma*sigma)
    #sigma ~ dunif(0,100)
    
    }
    ", fill = TRUE)
sink()



# 2) Set up a list that contains all the necessary data (here, including parameters of the prior distribution)
Data = list(value = pheno.long$value.log, destT_level = pheno.long$destT_level, i.max = length(pheno.long$value))

# 3) Specify a function to generate inital values for the parameters
inits.fn <- function() list(a.T = rnorm(1), b.T = rnorm(1))

# Compile the model and run the MCMC for an adaptation (burn-in) phase
jagsModel <- jags.model(file= "Phenology.txt", data=Data, init = inits.fn, n.chains = 3, n.adapt= 5000)
# Specify parameters for which posterior samples are saved
para.names <- c("a.T","b.T","sigma")
# Continue the MCMC runs with sampling
Samples <- coda.samples(jagsModel, variable.names = para.names, n.iter = 5000)
plot(Samples)

# convergence check
gelman.diag(Samples)
gelman.plot(Samples)

# Statistical summaries of the posterior sample for p => compare to MLE
summary(Samples)