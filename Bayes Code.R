### FLOWERING ~ TREATMENT + ORIGINSITE + (1|SPECIES) + (1|Block) ###

sink('TEMPmodel.txt')
cat("
    model{
    # Likelihood
    for(i in 1:Ntotal){
    
    # normal distribution
    y[i] ~ dnorm(mu[i], tau)
    
    # linear predictor
    mu[i] <- alpha + newTTCoeff[newTT[i]] + siteCoeff[origSite[i]] + spCoeff[species[i]] + blockCoeff[block[i]]
    # make model completely species specific
    mu[i] <- alpha[species[i]] + newTTCoeff[species[i], newTT[i]] + siteCoeff[species[i], origSite[i]] + blockCoeff[block[i]]

    # or estimate treatment effect for each sp and site   
    mu[i] <- alpha[species[i]] + newTTCoeff[species[i], newTT[i], origSite[i]] + blockCoeff[block[i]]

    # species effect for each treatment
    mu[i] <- alpha[species[i]] + newTTCoeff[species[i], newTT[i]] + tempCoeff*Temperature[origSite[i]]  + precipCoeff*Precipitation[origSite[i]] + blockCoeff[block[i]]
    }
    
    ### PRIORS
    alpha ~ dunif(0, 360) # Intercept
    spPrec ~ dgamma(0.001, 0.001)
    blockPrec ~ dgamma(0.001, 0.001)
    
    #sigma ~ dunif(0, 100)
    #tau <- 1 / (sigma * sigma)
    tau ~ dgamma(0.001, 0.001)
    
    # Prior for Fixed Effects
    for(i in 2:NnewTTLvl){
    newTTCoeff[i] ~ dnorm(0, 1/10^2)
    }  
    newTTCoeff[1] <- 0    
    
    for(i in 1:(NsiteLvl-1)){
    siteCoeff[i] ~ dnorm(0, 1/10^2)
    }
    siteCoeff[NsiteLvl] <- 0
    
    
    # Prior for Random Effects
    for(i in 1:(NSPLvl-1)){
    spCoeff[i] ~ dnorm(0, spPrec)
    }
    spCoeff[NSPLvl] <- 0   
    
    for(i in 1:(NBlockLvl-1)){
    blockCoeff[i] ~ dnorm(0, blockPrec)
    }
    blockCoeff[NBlockLvl] <- 0 
    
    }
    ", fill = TRUE)
sink()







mu[i] <- alpha[species[i]] + newTTCoeff[species[i], newTT[i], origSite[i], destSite[i]] + blockCoeff[block[i]] 

} 

for(sp in 1:Num.sp){
  for(t in 1:Num.treatments){
    for(s in 1:Num.sites){
      newTTCoeff[sp,t,s] ~ dnorm(mean.tr[sp,t,s], tau)
      mean.tr[sp,t,s] <- alpha2[sp,t] + precip.effect*Precip[s] + temp.effect*Temp[s]
      
    }}}


# Contrasts
for(sp in 1:Num.sp){
  for(s in 1:Num.sites){
    contrast1[sp,s]  <- newTTCoeff[species[i], 2, origSite[i]] - newTTCoeff[species[i], 1, origSite[i]] # contrast between treatment 2 and treatment 1
  }}



# linear model saying if the effect of the treatment contrast is affected by temp and prec at site and species level
for(sp in 1:Num.sp){
  for(s in 1:Num.sites){
    contrast1[sp,s] ~ dnorm(mean.tr[sp,s], tau)
    mean.tr[sp,s]  <- alpha2[species[s]]  + precip.effect*Precip[s] + temp.effect*Temp[s]
  }}




mu[i] <- alpha[species[i]] + newTTCoeff[species[i], newTT[i], origSite[i], destSite[i]] + blockCoeff[block[i]] 
}

for(sp in 1:Num.sp){
  for(t in 1:Num.treatments){
    for(orig in 1:Num.sites){
      for(dest in 1:Num.sites){
        newTTCoeff[sp,t,orig,dest] ~ dnorm(mean.tr[sp,t,orig,dest], tau)
        # prior on mean.tr
      }}}
  
  for(sp in 1:Num.sp){
    for(s in 1:Num.sites){
      contrast1[sp,s]  <- newTTCoeff[sp, 2, s,s] - newTTCoeff[sp, 1, s,s] # contrast between treatment 2 and treatment 1 at a particular site
    }}

