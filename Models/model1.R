### model1: Plasticity model
### FLOWERING ~ TREATMENT + ORIGINSITE + Species + (1|Block)

## Plasticity
# [treatment, species, origSiteID]

model{
  ### LIKELIHOOD
  for(i in 1:Ntotal){
    
    ### NORMAL DISTRIBUTION
    y[i] ~ dnorm(mu[i], tau[speciesID[i]])
    mu[i] <- treatmentCoeff[treatmentID[i], speciesID[i], origSiteID[i]] #+ blockCoeff[destBlockID[i]]    # took out block random effect
  }
  
  ### PRIORS ###
  
  for(sp in 1:NSPLvl){  # need to loop through all species  
    for(t in 1:2){
      for(s1 in 1:NorigSiteLvl){
        #    for(s2 in 1:NdestSiteLvl){
        
        treatmentCoeff[t,sp,s1] ~ dnorm(mean.treatment[t,sp,s1], tau.slope[sp]) 
        mean.treatment[t,sp,s1] ~ dnorm(0, 0.001) 
      }}}
  
  ### PRIOR FOR RANDOM EFFECTS
  for(i in 1:NBlockLvl){
    blockCoeff[i] ~ dnorm(0, BlockPrec)
  }
  
  ## Precision / Variance priors
  
  BlockPrec ~ dgamma(0.001, 0.001)
  for(sp in 1:(NSPLvl)){  
    tau[sp] ~ dgamma(0.001, 0.001)
    tau.slope[sp] ~ dgamma(0.001, 0.001)  
  }
  
## Contrasts
  for(s1 in 1:NorigSiteLvl){
    for(sp in 1:NSPLvl){
      warm.treatment[sp,s1] <- mean.treatment[2,sp,s1] - mean.treatment[1,sp,s1]
      # warm.treatment[sp,s1] ~ dnorm(warm1[sp,s1], tau.warm1) 
      # warm1[sp,s1] <- mean.treatment[2,sp,s1] - mean.treatment[1,sp,s1]
    }
#    warm2[s1] ~ dnorm(warm1[sp,s1], tau.warm2)
  }
  
  
} # end model
