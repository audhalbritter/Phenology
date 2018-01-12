### FLOWERING ~ TREATMENT + ORIGINSITE + DESTSITE + (TREATMENT + SITE|SPECIES) + (1|Block) ###

# JD: A few general changes that were necessary:
# took out some of the data that are not used. In JAGS, all variables entered in the data have to be used somehow in the model
# distributions need tilde instead of arrow
# dimensions of parameters always need to match other places the variable is used.   e.g. treatmentCoeff[1] <- 0   said that treatmentCoeff is a vector

model{
  ### LIKELIHOOD
  for(i in 1:Ntotal){
    
    ### NORMAL DISTRIBUTION
    y[i] ~ dnorm(mu[i], tau)
    
    ### LINEAR PREDICTOR, SPECIES SPECIFIC
    
    #    mu[i] <- treatmentCoeff[species[i], treatment[i], origSite[i], destSite[i]] + blockCoeff[destBlock[i]]
    mu[i] <- treatmentCoeff[species[i], treatment[i], origSite[i], destSite[i]] + blockCoeff[destBlock[i]]
    
  }
  
  ### PRIORS ###
  
  for(sp in 1:NSPLvl){  # need to loop through all species  
    for(t in 1:NtreatmentLvl){
      for(so in 1:NorigSiteLvl){
        for(sd in 1:NdestSiteLvl){
      
      treatmentCoeff[sp,t, so, sd] ~ dnorm(mean.treatment[sp,t, so, sd], tau.slope[sp]) 
      mean.treatment[sp,t, so, sd] ~ dnorm(0, 0.001) 
      
    }}}}
  
  
  ### PRIOR FOR RANDOM EFFECTS
  for(i in 1:NBlockLvl){
    blockCoeff[i] ~ dnorm(0, BlockPrec)
  }
  
  ## Precision / Variance priors
  
  BlockPrec ~ dgamma(0.001, 0.001)
  tau ~ dgamma(0.001, 0.001)
  for(sp in 1:(NSPLvl)){  
    tau.slope[sp] ~ dgamma(0.001, 0.001)  
  }
  
  ## Treatment contrasts
  for(sp in 1:NSPLvl){  
    for(t1 in 1:NtreatmentLvl){
      for(t2 in 1:NtreatmentLvl){
        treatment.contrast[sp,t1,t2] <- mean.treatment[sp,t1] - mean.treatment[sp,t2]
      }
      # treatment.contrast[sp,t1] <- mean.treatment[sp,t1] - mean.treatment[sp,1] # Or could just get contrasts with control
    } }
  
} # end model
