# Model for one species

# Species: find out from which species are most common 
ddd <- pheno.long %>%
  filter(pheno.unit == "doy") %>% 
  group_by(species, pheno.var, pheno.stage) %>% 
  summarise(n = n()) %>% 
  filter(n > 30)
unique(ddd$species)

ddd <- pheno.long %>% filter(species == "Tha.alp")
unique(ddd$newTT)

#paste(unique(dd$species), collapse = ", ")
GlmerModelAndPredictDestinationThaAlp <- function(dd){
  if(unique(dd$pheno.unit) == "Temp since SM"){
    modelfit <- lmer(value ~ destTemp_value * destPrec_value + TransWarm + TransWet + (1|species), data = dd)
  }
  else if(unique(dd$pheno.unit) != "Temp since SM"){
    modelfit <- glmer(value ~ destTemp_value * destPrec_value + TransWarm + TransWet + (1|species), data = dd, family = "poisson")
  }
  newdat <- expand.grid(
    destTemp_value=c(0, 0.6)
    , destPrec_value=c(0.2857143, 0.6666667, 1)
    , TransWarm=c(0,1)
    , TransWet=c(0,1)
    , species=c("Ant.odo", "Ave.fle", "Ran.rep", "Tri.pra", "Ach.mil", "Fes.ovi", "Fes.viv", "Pin.vul", "Tof.pus", "Tri.ces", "Ver.alp", "Hie.vul", "Ast.alp", "Agr.cap", "Cer.fon", "Gal.sp", "Car.light", "Tri.rep", "Car.cap", "Sax.aiz", "Dia.del", "Ste.gra", "Leo.aut", "Alc.alp", "Bar.alp", "Tar.sp", "Kna.arv", "Sil.aca", "Cam.rot", "Ant.dio", "Poa.alp", "Sib.pro", "Fes.rub", "Luz.mul", "Sag.sp", "Car.nig", "Hie.pil", "Luz.sp", "Phl.alp", "Alc.sp", "Car.grn", "Tha.alp", "Pot.cra", "Pru.vul", "Car.lep", "Oma.sup", "Bis.viv", "Nar.str", "Car.pal", "Pla.lan", "Ger.syl", "Poa.pra", "Car.grs", "Gen.sp", "Gen.sp2", "Hie.alp", "Oma.sp", "Rum.ace", "Gen.pur", "Sil.vul", "Car.pan", "Sol.vir", "Car.vag", "Car.pil", "Ran.acr", "Eri.uni", "Par.pal", "Agr.mer", "Pot.ere", "Gal.uli", "Kob.sim", "Car.sp", "Cer.sp", "Gal.sax", "Jun.alp", "Coe.vir", "Des.ces", "Car.big")
    , value = 0
  )
  mm <- model.matrix(terms(modelfit), newdat)
  newdat$value <- predict(modelfit,newdat,re.form=NULL, type="response")  # re.form=NA without random part; re.form=NULL with random part
  
  means <- newdat %>% 
    group_by(TransWarm, TransWet, species) %>% 
    summarize(mean = mean(value), sd = sd(value))
  return(means)
}


# Create data and run prediction
DestDatSP <- pheno.long %>% 
  filter(pheno.unit %in% c("doy", "d.snowmelt", "dCumTemp")) %>% # for origin
  filter(pheno.stage != "r") %>%
  filter(pheno.var != "duration") %>% 
  mutate(pheno.var = factor(pheno.var, levels = c("first", "peak", "end"))) %>% 
  mutate(newTT = plyr::mapvalues(newTT, c("control", "TT2", "TT3", "TT4"), c("Control", "Warm", "Wet", "Warm & wet"))) %>%
  mutate(newTT = factor(newTT, levels = c("Control", "Warm", "Wet", "Warm & wet"))) %>% 
  mutate(pheno.stage = plyr::mapvalues(pheno.stage, c("b", "f", "s"), c("Bud", "Flower", "Seed"))) %>%
  mutate(pheno.stage = factor(pheno.stage, levels = c("Bud", "Flower", "Seed"))) %>% 
  mutate(pheno.unit = plyr::mapvalues(pheno.unit, c("doy", "d.snowmelt", "dCumTemp"), c("DOY", "Days since SM", "Temp since SM"))) %>%
  mutate(pheno.unit = factor(pheno.unit, levels = c("DOY", "Days since SM", "Temp since SM"))) %>%
  group_by(pheno.unit, pheno.var, pheno.stage) %>% 
  mutate_each(funs(as.factor), species, flowering.time, functionalGroup, occurrence.2) %>% 
  mutate(TransWarm = ifelse(newTT %in% c("Warm", "Warm & wet"), 1, 0)) %>% 
  mutate(TransWet = ifelse(newTT %in% c("Wet", "Warm & wet"), 1, 0)) %>%
  mutate(NoTrans = ifelse(newTT == "control", 1, 0)) %>%
  do(GlmerModelAndPredictDestinationThaAlp(.))
dd <- DestDatSP %>% filter(pheno.unit == "DOY", pheno.stage == "Flower", pheno.var == "first")

# MAKE PLOT
# "Agr.cap" "Alc.alp" "Ant.odo" "Bis.viv" "Car.cap" "Luz.mul" "Nar.str" "Pot.ere" "Tha.alp"
DestinationPlotSP <- means %>% 
  #filter(species=="Bis.viv") %>% 
  mutate(Treatment = paste(TransWarm, TransWet, sep = "")) %>% 
  mutate(Treatment = plyr::mapvalues(Treatment, c("00", "01", "10", "11"), c("Control", "Wet", "Warm", "Warm & wet"))) %>%
  mutate(Treatment = factor(Treatment, levels = rev(c("Control", "Warm", "Wet", "Warm & wet")))) %>% 
  ggplot() +
  geom_point(aes(x = mean, y = Treatment), size = 3) +
  ylab(paste("")) + xlab(paste("Mean value")) +
  #scale_shape_manual(name = "", values = c(16,17,15)) +
  #scale_colour_manual(name = "", values = c(col.red, col.dblue, col.orange)) +
  #geom_errorbarh(aes(xmin=mean-sd, xmax=mean+sd), height=0.1, color = "gray") +
  facet_grid(~species)

DestinationPlotSP + theme_grey(base_size = 20) + theme(legend.title=element_blank())