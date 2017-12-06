pheno <- pheno.long %>% filter(pheno.stage =="f", pheno.var == "first", pheno.unit == "doy")
fit01 <- glmer((value) ~ destTemp_value * destPrec_value + From1To2Temp + From2To3Prec + From3To4Prec + (1|species), pheno, family = "poisson")

fit01
summary(fit01)

newdat <- expand.grid(
  destTemp_value=c(0.6,0)
  , destPrec_value=c(0.2857143, 0.6666667, 1)
  , From1To2Temp=c(0,1)
  , From2To3Prec=c(0,1)
  , From3To4Prec=c(0,1)
  , species=c("Ach.mil", "Ant.odo", "Fes.ovi", "Gal.sax", "Pot.ere", "Ver.cha", "Ver.off", "Vio.riv", "Ave.fle", "Luz.mul", "Agr.cap", "Car.vag", "Gal.uli", "Car.lep", "Ste.gra", "Vio.pal", "Des.ces", "Tri.pra", "Cam.rot", "Tri.rep", "Alc.alp", "Sib.pro", "Phl.alp", "Pot.cra", "Vio.bif", "Alc.sp", "Bis.viv", "Kna.arv", "Ran.acr", "Leo.aut", "Pru.vul", "Poa.alp", "Car.pil", "Gal.sp", "Car.pal", "Fes.rub", "Nar.str", "Par.pal", "Sol.vir", "Ver.alp", "Car.light", "Sag.sp", "Ran.rep", "Pin.vul", "Tha.alp", "Tof.pus", "Tri.ces", "Car.sax", "Poa.pra", "Car.pan", "Luz.sp", "Rum.ace", "Cer.sp", "Ant.dio", "Car.cap", "Ran.aur", "Car.nig", "Hie.pil", "Jun.alp", "Kob.sim", "Pla.lan", "Ger.syl", "Car.sp2", "Car.grn", "Oma.sup", "Tar.sp", "Car.nor", "Sil.aca", "Agr.mer", "Gen.pur", "Car.big", "Car.rup", "Oma.sp", "Car.sp", "Cer.fon", "Sax.aiz", "Eri.uni", "Coe.vir")
  , value = 0
)

newdat <- expand.grid(
  destTemp_value=c(0.6,0)
  , destPrec_value=c(0.2857143, 0.6666667, 1)
  , From1To2Temp=c(0,1)
  , From2To3Prec=c(0,1)
  , From3To4Prec=c(0,1)
  , value = 0
)
mm <- model.matrix(terms(fit01), newdat)
newdat$value <- predict(fit01,newdat,re.form=NA, type="response") # re.form=NA without random part; re.form=NULL with random part

means <- newdat %>% 
  group_by(destTemp_value, destPrec_value, From1To2Temp, From2To3Prec, From3To4Prec) %>% 
  summarize(mean = mean(value))

### ADAPTATION
pheno <- pheno.long %>% filter(pheno.stage =="f", pheno.var == "first", pheno.unit == "dCumTemp")
fit01 <- glmer((value) ~ destTemp_value * destPrec_value + newTT + (1|species), pheno, family = "poisson")
disp(fit01, pheno)
pheno$newFactor <- rnorm(nrow(pheno))
fit01 <- glmer((value) ~ destTemp_value + destPrec_value + newTT + (1|species) + (1|newFactor), pheno, family = "poisson")
summary(fit01)
newdat <- expand.grid(
  destTemp_value=c(0.6,0)
  , destPrec_value=c(0.2857143, 0.6666667, 1)
  , newTT=c("control", "TT2", "TT3", "TT4")
  , value = 0
)
mm <- model.matrix(terms(fit01), newdat)
newdat$value <- predict(fit01,newdat,re.form=NA, type="response") # re.form=NA without random part; re.form=NULL with random part

newdat %>% 
  group_by(newTT) %>% 
  summarize(mean = mean(value), sd = sd(value))









# with species
newdat <- expand.grid(
  Temp_value=c(0.6,0)
  , Prec_value=c(0.2857143, 0, 0.6666667, 1)
  , newTT=c("control", "TT2", "TT3", "TT4")
  , species=c("Ach.mil", "Ant.odo", "Fes.ovi", "Gal.sax", "Pot.ere", "Ver.cha", "Ver.off", "Vio.riv", "Ave.fle", "Luz.mul", "Agr.cap", "Car.vag", "Gal.uli", "Car.lep", "Ste.gra", "Vio.pal", "Des.ces", "Tri.pra", "Cam.rot", "Tri.rep", "Alc.alp", "Sib.pro", "Phl.alp", "Pot.cra", "Vio.bif", "Alc.sp", "Bis.viv", "Kna.arv", "Ran.acr", "Leo.aut", "Pru.vul", "Poa.alp", "Car.pil", "Gal.sp", "Car.pal", "Fes.rub", "Nar.str", "Par.pal", "Sol.vir", "Ver.alp", "Car.light", "Sag.sp", "Ran.rep", "Pin.vul", "Tha.alp", "Tof.pus", "Tri.ces", "Car.sax", "Poa.pra", "Car.pan", "Luz.sp", "Rum.ace", "Cer.sp", "Ant.dio", "Car.cap", "Ran.aur", "Car.nig", "Hie.pil", "Jun.alp", "Kob.sim", "Pla.lan", "Ger.syl", "Car.sp2", "Car.grn", "Oma.sup", "Tar.sp", "Car.nor", "Sil.aca", "Agr.mer", "Gen.pur", "Car.big", "Car.rup", "Oma.sp", "Car.sp", "Cer.fon", "Sax.aiz", "Eri.uni", "Coe.vir")
  , value = 0
)
mm <- model.matrix(terms(fit01), newdat)
newdat$value <- predict(fit01,newdat,re.form=NULL, type="response") # re.form=NA without random part; re.form=NULL with random part

newdat[6:9] <- traits.15[match(newdat$species, traits.15$species, nomatch = NA), c("family", "functionalGroup", "flowering.time", "occurrence.2")]
head(newdat)


newdat %>% 
  filter(!is.na(family)) %>% 
  group_by(functionalGroup) %>% 
  summarize(mean = mean(value), sd = sd(value))



# functionalGroup etc
pheno <- pheno.long %>% filter(pheno.stage =="f", pheno.var == "peak", pheno.unit == "doy")
fit01 <- glmer((value) ~ Temp_value * Prec_value + newTT + functionalGroup + (1|species), pheno, family = "poisson")
disp(fit01, pheno)
pheno$newFactor <- rnorm(nrow(pheno))
fit01 <- glmer((value) ~ Temp_value * Prec_value + newTT + functionalGroup + (1|species) + (1|newFactor), pheno, family = "poisson")
fit01
summary(fit01)
newdat <- expand.grid(
  Temp_value=c(0, 0.6)
  , Prec_value=c(0, 0.2857143, 0.6666667, 1)
  , newTT=c("control", "TT2", "TT3", "TT4")
  , functionalGroup=c("forb", "graminoid")
  , value = 0
)
mm <- model.matrix(terms(fit01), newdat)
newdat$value <- predict(fit01,newdat,re.form=NA, type="response") # re.form=NA without random part; re.form=NULL with random part

newdat %>% 
  group_by(functionalGroup) %>% 
  summarize(mean = mean(value), sd = sd(value))




#### TEST ONLY FOR CONTROL PLOTS ####
pheno <- pheno.long %>% filter(pheno.stage =="s", pheno.var == "peak", TTtreat== c("TTC", "TT1"))
pheno$newFactor <- rnorm(nrow(pheno))
fit01 <- glmer(value ~ TTtreat + (1|species), pheno, family = "poisson")
disp(fit01, pheno)
fit01 <- glmer(value ~ TTtreat + (1|species) + (1|newFactor), pheno, family = "poisson")
disp(fit01, pheno)
summary(fit01)

newdat <- expand.grid(TTtreat=c("TT1","TTC"))
mm <- model.matrix((fit01), newdat)
newdat$value <- predict(fit01,newdat,re.form=NA, type="response")




