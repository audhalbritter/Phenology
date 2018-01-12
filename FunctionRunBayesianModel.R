### FUNCTION TO RUN BAYESIAN ANALYSIS
# dat = data
# Year = year
# phenostage = pheno.stage
# phenovar = pheno.var
# niter = number of iterations
# nburn = burnin phase
# nthin = thining
# nchain = number of chains
# mod = model

RunBayesianAnalysis <- function(dat, phenostage, phenovar, phenounit, niter, nburn, nthin, nchain, mod){
  
  #------------------------------------------------------------------------------
  # LOAD DATA
  
  
  myData <- dat %>% 
    # subset
    filter(pheno.stage == phenostage, pheno.var == phenovar, pheno.unit == phenounit) %>% 
    select(value, newTT, species, siteID, destSiteID, blockID, destBlockID) %>% 
    select(value, newTT, species, siteID, destSiteID, blockID, destBlockID) %>% 
    rename(treatment = newTT, origSiteID = siteID, origBlockID = blockID) %>% 
    mutate(treatment = factor(treatment, levels = c("Control", "Warmer", "LaterSM", "WarmLate"))) %>% 
    mutate(treatment = as.numeric(treatment), origSiteID = as.numeric(origSiteID), species = as.numeric(factor(species)), origSiteID = as.numeric(origSiteID), destSiteID = as.numeric(factor(destSiteID)), origBlockID = as.numeric(factor(origBlockID)), destBlockID = as.numeric(factor(destBlockID)))
  
  myData <- as.data.frame(myData)
  
  # Making a data list
  y = myData$value
  treatment <- myData$treatment 
  origSite <- myData$origSiteID
  destSite <- myData$destSiteID
  species <- myData$species 
  origBlock <- myData$origBlockID 
  destBlock <- myData$destBlockID 
  Ntotal <- length(y) 
  NtreatmentLvl <- nlevels(factor(myData$treatment)) 
  NorigSiteLvl <- nlevels(factor(myData$origSiteID))
  NdestSiteLvl <- nlevels(factor(myData$destSiteID))
  NSPLvl <- nlevels(factor(myData$species))
  NorigBlockLvl <- nlevels(factor(myData$origBlockID))
  NdestBlockLvl <- nlevels(factor(myData$destBlockID))
  
  # Making a data list
  dataList.mod1 <- list(y = y, 
                        treatment = treatment, 
                        # origSite = origSite,
                        # destSite = destSite,
                        species = species, 
                        # origBlock = origBlock, 
                        destBlock = destBlock, 
                        Ntotal = Ntotal, 
                        NtreatmentLvl = NtreatmentLvl, 
                        # NorigSiteLvl = NorigSiteLvl,
                        # NdestSiteLvl = NdestSiteLvl,
                        NSPLvl = NSPLvl,
                        NBlockLvl = NdestBlockLvl   
                        # was origBlock  - should RE be for orig block or for destination block?  Not sure i understand why orig
                        #NdestBlockLvl = nlevels(factor(destBlockID))
                        )
  
  # unique name
  modname = paste("mod", phenostage, phenovar, sep = "")
  
  #------------------------------------------------------------------------------
  # SPECIFY PARAMETERS
  
  # initial values
  mod1.inits<-function(){
    list(tau = 1
      ,BlockPrec=1
      ,tau.slope = rep(1,NSPLvl)
      #,alpha = matrix(0,N_sp, N_plots)
      #,alpha = rep(0,NSPLvl)
    )
  }
  
  n.iterations <- niter      ## draws from posterior
  n.burn <- nburn      ## draws to discard as burn-in
  thin.rate <- nthin    	## thinning rate
  nc <- nchain			## number of chains
  
  # Specify parameters for which posterior samples are saved
  para.names1 <- c("treatment.contrast","treatmentCoeff","blockCoeff") # "alpha"
  
  
  #------------------------------------------------------------------------------
  # RUN ANALYSIS
  
  ## Run model
  mod <-jags(
            # data and parameters
            data = dataList.mod1, 
             inits = mod1.inits,
             parameters.to.save = para.names1,
             
             # analysis
             n.thin = thin.rate, 
             n.chains = nc, 
             n.burnin = n.burn, 
             n.iter = n.iterations,
             model.file = "Models/model1.R")
  
  # use as.mcmmc to convert rjags object into mcmc.list
  mod.mcmc <- as.mcmc(mod)
  
  #------------------------------------------------------------------------------
  # MODEL CHECK
  
  pdf(file="ModelCheck/mod1.JAGS.diagnostic.pdf", width = 12, height = 10)
  par(mar=c(4,2,2,1))
  plot(mod)
  plot(mod.mcmc)
  dev.off()
  
  #------------------------------------------------------------------------------
  # OUTPUT
  
  res <- data.frame(mod$BUGSoutput$summary)
  
  save(res, file = paste("ModelOutput/", modname, ".RData", sep = ""))
  
}
