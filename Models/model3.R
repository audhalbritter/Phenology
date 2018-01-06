### FLOWERING ~ TREATMENT + ORIGINSITE + DESTSITE + (TREATMENT + SITE|SPECIES) + (1|Block) ###

# JD: A few general changes that were necessary:
# took out some of the data that are not used. In JAGS, all variables entered in the data have to be used somehow in the model
# distributions need tilde instead of arrow

model{
  ### LIKELIHOOD
  for(i in 1:Ntotal){
    
    ### NORMAL DISTRIBUTION
    y[i] ~ dnorm(mu[i], tau)
    
    ### LINEAR PREDICTOR, SPECIES SPECIFIC
    
    mu[i] <- treatmentCoeff[species[i], treatment[i], origSite[i], destSite[i]] + blockCoeff[destBlock[i]]

        # which of these two options is correct? Or what is the difference
    #mu[i] <- alpha[species[i]] + treatmentCoeff[species[i]] * treatment[i] + origBlockCoeff[origBlock[i]]
    
    # Model including Site; do we need to add a random factor for origin and destination block?
    #mu[i] <- alpha[species[i]] + treatmentCoeff[species[i], treatment[i], origSite[i], destSite[i]] + blockCoeff[block[i]]
    
  }
  
  ### PRIORS ###
  
  for(sp in 1:NSPLvl){  # need to loop through all species  
#    alpha[i] ~ dunif(0, 360) #  random intercept for each sp
    for(t in 1:NtreatmentLvl){
    for(s1 in 1:NorigSiteLvl){
    for(s2 in 1:NdestSiteLvl){
      
      treatmentCoeff[sp,t,s1,s2] ~ dnorm(mean.treatment[sp,t,s1,s2], tau.slope[sp]) # random slope
      mean.treatment[sp,t,s1,s2] ~ dnorm(0, 0.001) 

#      treatmentCoeff[,1] <- 0  # added 
#      treatmentCoeff[i, j] ~ dnorm(mean.treatment[i, j], tau.slope) # random slope
#      mean.treatment[i,j] ~ dnorm(0, 0.001) # moved this here - needs same dimensions
    }}}}
  
# treatmentCoeff[1] <- 0    # JD: this says that treatmentCoeff is a vector; dimensions need to match other places the variable is used 

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
  }
