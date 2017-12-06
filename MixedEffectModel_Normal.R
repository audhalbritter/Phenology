### FLOWERING ~ TREATMENT + ORIGINSITE + DESTSITE + (TREATMENT + SITE|SPECIES) + (1|Block) ###

sink('TEMPmodel.txt')
cat("
    model{
    ### LIKELIHOOD
    for(i in 1:Ntotal){
    
    ### NORMAL DISTRIBUTION
    y[i] ~ dnorm(mu[i], tau)
    
    ### LINEAR PREDICTOR, SPECIES SPECIFIC
    
    # which of these two options is correct? Or what is the difference
    mu[i] <- alpha[species[i]] + treatmentCoeff[species[i], treatment[i]] + origBlockCoeff[origBlock[i]]
    #mu[i] <- alpha[species[i]] + treatmentCoeff[species[i]] * treatment[i] + origBlockCoeff[origBlock[i]]

    # Model including Site; do we need to add a random factor for origin and destination block?
    #mu[i] <- alpha[species[i]] + treatmentCoeff[species[i], treatment[i], origSite[i], destSite[i]] + blockCoeff[block[i]]

    }
    
    ### PRIORS ###

    origBlockPrec ~ dgamma(0.001, 0.001)
    
    tau ~ dgamma(0.001, 0.001)
    
    for(i in 1:(NSPLvl-1)){    
      alpha[i] ~ dunif(0, 360) #  random intercept for each sp
      
      for(j in 2:NtreatmentLvl){

      treatmentCoeff[i, j] ~ dnorm(mean.treatment[i, j], tau.slope) # random slope

      }
    }

    treatmentCoeff[1] <- 0    
  
    tau.slope <– dgamma(0.001, 0.001)
    mean.treatment <- dnorm(0, 0.001)


    ### PRIOR FOR RANDOM EFFECTS
    for(i in 1:(NorigBlockLvl-1)){
    origBlockCoeff[i] ~ dnorm(0, origBlockPrec)
    }
    origBlockCoeff[NorigBlockLvl] <- 0
    }

    ", fill = TRUE)
sink()



for(i in 1:(NdestSiteLvl-1)){
  destSiteCoeff[i] ~ dnorm(0, 1/10^2)
}
destSiteCoeff[NdestSiteLvl] <- 0

for(i in 1:(NorigSiteLvl-1)){
  origSiteCoeff[i] ~ dnorm(0, 1/10^2)
}
origSiteiteCoeff[NorigSiteLvl] <- 0

  
