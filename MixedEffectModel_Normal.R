### FLOWERING ~ TREATMENT + ORIGINSITE + DESTSITE + (TREATMENT + SITE|SPECIES) + (1|Block) ###

sink('TEMPmodel.txt')
cat("
    model{
    ### LIKELIHOOD
    for(i in 1:Ntotal){
    
    ### NORMAL DISTRIBUTION
    y[i] ~ dnorm(mu[i], tau)
    
    ### LINEAR PREDICTOR, SPECIES SPECIFIC
    
    mu[i] <- alpha + treatmentCoeff[treatment[i], species[i]] + origBlockCoeff[origBlock[i]]

    }
    
    ### PRIORS ###
    origBlockPrec ~ dgamma(0.001, 0.001)
    tau ~ dgamma(0.001, 0.001)
    
    ### PRIOR FOR FIXED EFFECTS
    for(i in 2:NtreatmentLvl){
      for(j in 1:(NSPLvl-1)){
      alpha[i, j] ~ dunif(0, 360) # Intercept
      treatmentCoeff[i, j] ~ dnorm(0, 1/10^2)
      }
    }  
    treatmentCoeff[1] <- 0    
    spCoeff[NSPLvl] <- 0 



    ### PRIOR FOR RANDOM EFFECTS
    for(i in 1:(NorigBlockLvl-1)){
    origBlockCoeff[i] ~ dnorm(0, origBlockPrec)
    }
    origBlockCoeff[NorigBlockLvl] <- 0
    }

    ", fill = TRUE)
sink()


#mu[i] <- alpha[species[i]] + treatmentCoeff[species[i], treatment[i], origSite[i], destSite[i]] + blockCoeff[block[i]]





for(i in 1:(NdestSiteLvl-1)){
  destSiteCoeff[i] ~ dnorm(0, 1/10^2)
}
destSiteCoeff[NdestSiteLvl] <- 0

for(i in 1:(NorigSiteLvl-1)){
  origSiteCoeff[i] ~ dnorm(0, 1/10^2)
}
origSiteiteCoeff[NorigSiteLvl] <- 0

  
