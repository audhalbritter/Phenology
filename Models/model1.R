### FLOWERING ~ TREATMENT + ORIGINSITE + DESTSITE + (TREATMENT + SITE|SPECIES) + (1|Block) ###


model{
  ### LIKELIHOOD
  for(i in 1:Ntotal){
    
    ### NORMAL DISTRIBUTION
    y[i] ~ dnorm(mu[i], tau)
    
    ### LINEAR PREDICTOR, SPECIES SPECIFIC
    
#    mu[i] <- treatmentCoeff[species[i], treatment[i], origSite[i], destSite[i]] + blockCoeff[destBlock[i]]
    mu[i] <- treatmentCoeff[species[i], treatment[i]] + blockCoeff[destBlock[i]]

  }
  
  ### PRIORS ###
  
  for(sp in 1:NSPLvl){  # need to loop through all species  
    for(t in 1:NtreatmentLvl){
    # for(s1 in 1:NorigSiteLvl){
    # for(s2 in 1:NdestSiteLvl){
      
      treatmentCoeff[sp,t] ~ dnorm(mean.treatment[sp,t], tau.slope[sp]) 
      mean.treatment[sp,t] ~ dnorm(0, 0.001) 

    }}#}}
  

### PRIOR FOR RANDOM EFFECTS
  for(i in 1:NBlockLvl){
    blockCoeff[i] ~ dnorm(0, BlockPrec)
  }
  #origBlockCoeff[NorigBlockLvl] <- 0

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
