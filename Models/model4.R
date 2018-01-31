### model3: Plasticity model, of WARM-warmlate
### FLOWERING ~ TREATMENT + ORIGINSITE + Species 

## Plasticity
# [treatment, species, origSiteID]

model{
  ### LIKELIHOOD
  for(i in 1:Ntotal){
    
    ### NORMAL DISTRIBUTION
    y[i] ~ dnorm(mu[i], tau.blocks[speciesID[i]])
    mu[i] <- treatmentCoeff[treatmentID[i], speciesID[i], origSiteID[i]] #+ blockCoeff[destBlockID[i]]    # took out block random effect
  }
  
  ### PRIORS ###
  
  for(sp in 1:NSPLvl){  # need to loop through all species  
    for(t in 1:2){
      for(s1 in 1:NorigSiteLvl){
        treatmentCoeff[t,sp,s1] ~ dnorm(mean.treatment0[t,sp], tau.sites[sp]) 
      }
      mean.treatment0[t,sp] <- mean.treatment[t,sp] # + ... could put site level covariates here
      mean.treatment[t,sp] ~ dnorm(warmlate.treat[t], tau.sp) 
    }}
  
  for(t in 1:2){
    warmlate.treat[t] ~ dnorm(0, .0001)
  }
  
  ## Precision / Variance priors
  
  for(sp in 1:(NSPLvl)){  
    tau.blocks[sp] ~ dgamma(0.001, 0.001)
    tau.sites[sp] ~ dgamma(0.001, 0.001)  
  }
  tau.sp ~ dgamma(0.001, 0.001)  
  
  ## Contrasts
  warmlate.overall <- warmlate.treat[2] - warmlate.treat[1]
  for(sp in 1:NSPLvl){
    warmlate.treatment[sp] <- mean.treatment[2,sp] - mean.treatment[1,sp]
  }
  
  for(sp in 1:NSPLvl){
    for(s1 in 1:NorigSiteLvl){
      warmlate.site.treat[sp,s1] <- treatmentCoeff[2,sp,s1] - treatmentCoeff[1,sp,s1]
    }}
  
} # end model
