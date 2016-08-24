# Phenology data 2015

#### LIBRARIES ####
#library("reshape2")
#library("reshape")
library("lme4")
library("tidyr")
library("dplyr")
library("lubridate")
library("ggplot2")

#### READ IN HEAD OF PHENOLOGY DATA 2015 ####
dath1 <- ReadInHeadPhenology15("DataSheet2015Hog.csv", "Hogsete")
dath2 <- ReadInHeadPhenology15("DataSheet2015Ram.csv", "Rambaera")
dath3 <- ReadInHeadPhenology15("DataSheet2015Ves.csv", "Veskre")
dath4 <- ReadInHeadPhenology15("DataSheet2015Lav.csv", "Lavisdalen")
dath5 <- ReadInHeadPhenology15("DataSheet2015Gud.csv", "Gudmedalen")
dath6 <- ReadInHeadPhenology15("DataSheet2015Skj.csv", "Skjellingahaugen")
meta.pheno <- data.frame(rbind(dath1, dath2, dath3, dath4, dath5, dath6))
meta.pheno$date<-as.Date(meta.pheno$date, format="%d.%m.%Y")
meta.pheno <- meta.pheno[-c(15,34,42,54),] # should solve this differently
#rm(dath1, dath2, dath3, dath4, dath5, dath6)

#### READ IN BODY OF PHENOLOGY DATA 2015 ####
dat1 <- ReadInBodyPhenology15("DataSheet2015Hog.csv", "Hogsete")
dat2 <- ReadInBodyPhenology15("DataSheet2015Ram.csv", "Rambaera")
dat3 <- ReadInBodyPhenology15("DataSheet2015Ves.csv", "Veskre")
dat4 <- ReadInBodyPhenology15("DataSheet2015Lav.csv", "Lavisdalen")
dat5 <- ReadInBodyPhenology15("DataSheet2015Gud.csv", "Gudmedalen")
dat6 <- ReadInBodyPhenology15("DataSheet2015Skj.csv", "Skjellingahaugen")
pheno15 <- rbind(dat1, dat2, dat3, dat4, dat5, dat6)
pheno15$week.nr <- as.numeric(pheno15$week) # needs to be outside function, that week.nr across sites is consistent
pheno15$week.nr2 <- as.numeric(substring(pheno15$week,2))
#rm(dat1, dat2, dat3, dat4, dat5, dat6)

#### CALCULATE SUM OF BUD, FLOWER, SEED AND RIPE SEEDS PER TURFID AND SPECIES ####
pheno15 <- CalcSums(pheno15)

#### READ IN TURFS 2015 ####
turfs.15 <- read.csv("turfs.csv", sep=";", header=TRUE)
# rescaling Temp and Prec values
turfs.15$Prec_value <- (turfs.15$Prec_value-min(turfs.15$Prec_value))/(max(turfs.15$Prec_value) - min(turfs.15$Prec_value))
turfs.15$Temp_value <- (turfs.15$Temp_value-min(turfs.15$Temp_value))/(max(turfs.15$Temp_value) - min(turfs.15$Temp_value))
turfs.15$destPrec_value <- (turfs.15$destPrec_value-min(turfs.15$destPrec_value))/(max(turfs.15$destPrec_value) - min(turfs.15$destPrec_value))
turfs.15$destTemp_value <- (turfs.15$destTemp_value-min(turfs.15$destTemp_value))/(max(turfs.15$destTemp_value) - min(turfs.15$destTemp_value))
turfs.15$d.date.osm <- dmy(turfs.15$d.date.osm)
turfs.15$d.dosm <- yday(turfs.15$d.date.osm)
turfs.15$o.date.osm <- dmy(turfs.15$o.date.osm)
turfs.15$o.dosm <- yday(turfs.15$o.date.osm)
head(turfs.15)
str(turfs.15)
# all variables From1To2Temp etc must be numeric


#### READ IN TRAITS ####
traits.15 <- read.csv("trait.csv", sep=";", header=TRUE)
head(traits.15)
# ???
traits.15$early<-traits.15$F.Mso==0&traits.15$F.Sso==0&traits.15$F.Ho==0
traits.15$late<-traits.15$F.Var==0&traits.15$F.Fso==0&traits.15$F.Mso==0


#### CALCULATE FIRST, PEAK, END AND DURATION ####
### MAKE LONG DATA SET ###
pheno.long <- pheno15 %>%
  select(turfID, species, doy, Site, nr.b, nr.f, nr.s, nr.r) %>%
  gather(key = pheno.stage, value = value, -turfID, -species, -Site, -doy) %>%
  group_by(turfID, species, pheno.stage) %>%  # group by turfID, species and phenological stage to calculate first, end etc for each stage
  mutate(minDoy = min(doy, na.rm = TRUE)) %>% # calculate min doy
  group_by(minDoy, add = TRUE) %>% # add variable but remember the previous groups
  filter(value > 0) %>%
  summarize(first = first(doy), end = last(doy), peak = doy[which.max(value)]) %>%
  mutate(duration = end-(first-1)) %>%            # calculate duration
  filter(first > minDoy) %>%
  ungroup() %>% 
  select(-minDoy) %>% 
  mutate_each(funs(as.numeric), first, peak, end, duration) %>% # make variables numeric (probably not necessary)
  mutate(pheno.stage = substring(pheno.stage, nchar(pheno.stage), nchar(pheno.stage))) %>%  # take last letter from pheno.stage
  gather(key = pheno.var, value = doy, -turfID, -species, -pheno.stage) %>% # create pheno.var and gather 4 variable into 1 column
  left_join(turfs.15, by = "turfID") # merge data set with turfs.15
head(pheno.variables)

#### CALCULATE EVENT IN DAYS SINCE SNOWMELT
pheno.long$d.snowmelt <- pheno.long$doy - pheno.long$d.dosm
pheno.long$o.snowmelt <- pheno.long$doy - ifelse(pheno.long$newTT == "control", pheno.long$o.dosm, pheno.long$d.dosm)


# DELETE THESE LINES
#pheno.variables$first <- as.numeric(pheno.variables$first)
#pheno.variables$end <- as.numeric(pheno.variables$end)
#pheno.variables$peak <- as.numeric(pheno.variables$peak)
#pheno.variables$duration <- as.numeric(pheno.variables$duration)
#pheno.variables$pheno.stage <- substring(pheno.variables$pheno.stage, nchar(pheno.variables$pheno.stage),nchar(pheno.variables$pheno.stage))
# Make long data set for pheno.var: first, end, peak, duration
# used for analysis
pheno.long <- pheno.variables %>%
  gather(key = pheno.var, value = doy, -turfID, -species, -pheno.stage) %>% 
  left_join(turfs.15, by = "turfID")
head(pheno.long)




### DELETE
#pheno.long$d.snowmelt <- pheno.long$doy - turfs.15$d.dosm[match(pheno.long$turfID, turfs.15$turfID)]
#pheno.long$o.snowmelt <- pheno.long$doy - turfs.15$o.dosm[match(pheno.long$turfID, turfs.15$turfID)]
# control = origin; transplant = destination snowmelt date !!!! NEEDS FIXING
pheno.long$o.snowmelt <- ifelse(grep(c("TTC" | "TT1"), pheno.long$turfID) %in% c("TTC", "TT1"), pheno.long$doy - turfs.15$o.dosm[match(pheno.long$turfID, turfs.15$turfID)], pheno.long$doy - turfs.15$d.dosm[match(pheno.long$turfID, turfs.15$turfID)])

#### GROWING DEGREE DAYS
# add day of snowmelt for each turf
pheno.long[,(ncol(pheno.long)+1):(ncol(pheno.long)+4)] <- turfs.15[match(pheno.long$turfID, turfs.15$turfID), c("d.dosm", "o.dosm", "siteID", "destSiteID")]
pheno.long$doy.site <- paste(pheno.long$doy, substr(pheno.long$siteID,1,3), sep="_")
pheno.long$doy.destsite <- paste(pheno.long$doy, substr(pheno.long$destSiteID,1,3), sep="_")
pheno.long$dosm.site <- paste(pheno.long$o.dosm, substr(pheno.long$siteID,1,3), sep="_")
pheno.long$dosm.destsite <- paste(pheno.long$d.dosm, substr(pheno.long$destSiteID,1,3), sep="_")
#climate <- daily.temp[daily.temp$year == YEAR,] # climate data
climate$site.doy <- paste(climate$doy, climate$site, sep="_")


#origin
pheno.long$oCumTempFlower <- climate$CumTemperature[match(pheno.long$doy.site, climate$site.doy)] # CumTemp until FLOWERING
pheno.long$oCumTempSnow <- climate$CumTemperature[match(pheno.long$dosm.site, climate$site.doy)] # CumTemp until SNOWMELT
pheno.long$oCumTemp <- pheno.long$oCumTempFlower - pheno.long$oCumTempSnow # Degree days

#destination
pheno.long$dCumTempFlower <- climate$CumTemperature[match(pheno.long$doy.destsite, climate$site.doy)] # CumTemp until FLOWERING
pheno.long$dCumTempSnow <- climate$CumTemperature[match(pheno.long$dosm.destsite, climate$site.doy)] # CumTemp until SNOWMELT
pheno.long$dCumTemp <- pheno.long$dCumTempFlower - pheno.long$dCumTempSnow # Degree days


# create new variable pheno.unit: doy, snowmelt and cumTemp
pheno.long <- pheno.long %>%
  select(turfID, species, pheno.stage, pheno.var, doy, d.snowmelt, o.snowmelt, oCumTemp, dCumTemp) %>% 
  gather(key = pheno.unit, value = value, -turfID, -species, -pheno.stage, -pheno.var) #%>%
  #filter(pheno.unit!="d.snowmelt" & pheno.var != "duration") %>%
  #filter(pheno.unit!="o.snowmelt" & pheno.var != "duration")
head(pheno.long)

# add metadata
pheno.long[,(ncol(pheno.long)+1):(ncol(pheno.long)+20)] <- turfs.15[match(pheno.long$turfID, turfs.15$turfID), c("siteID", "TTtreat", "Year", "blockID", "Temperature_level", "Precipitation_level", "destBlockID", "destSiteID", "newTT", "destT_level", "destP_level", "Prec_value", "Temp_value", "destTemp_value", "destPrec_value", "From1To2Temp", "From2To3Temp", "From1To2Prec", "From2To3Prec", "From3To4Prec")]
pheno.long[,(ncol(pheno.long)+1):(ncol(pheno.long)+4)] <- traits.15[match(pheno.long$species, traits.15$species), c("family", "functionalGroup", "flowering.time", "occurrence.2")]

#### SAVE PHENO.LONG ####




#### RESHAPE DATA TO MAKE PLOTS ####
# destination
pheno.treat.dest <- ReshapeToMakeFigure(pheno.long, "destination", "C1")
head(pheno.treat.dest)

# origin
pheno.treat.orig <- ReshapeToMakeFigure(pheno.long, "origin", "C1")
head(pheno.treat.orig)



#### ANALYSE DATA ####
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




### ORIGIN (doy, o.snowmelt, oCumTemp)
pheno <- pheno.long %>% filter(pheno.stage =="f", pheno.var == "peak", pheno.unit == "doy")
fit01 <- glmer((value) ~ Temp_value * Prec_value + newTT + (1|species), pheno, family = "poisson")
disp(fit01, pheno)
pheno$newFactor <- rnorm(nrow(pheno))
fit01 <- glmer((value) ~ Temp_value + Prec_value + newTT + (1|species) + (1|newFactor), pheno, family = "poisson")
summary(fit01)
newdat <- expand.grid(
  Temp_value=c(0, 0.6)
  , Prec_value=c(0, 0.2857143, 0.6666667, 1)
  , newTT=c("control", "TT2", "TT3", "TT4")
  , value = 0
)
mm <- model.matrix(terms(fit01), newdat)
newdat$value <- predict(fit01,newdat,re.form=NA, type="response") # re.form=NA without random part; re.form=NULL with random part

newdat %>% 
  group_by(newTT) %>% 
  summarize(mean = mean(value), sd = sd(value)) %>% 
  ggplot() +
  geom_point(aes(x= mean, y = newTT)) + 
  geom_errorbarh(aes(x = mean, xmin=-sd, xmax=sd)) +
  facet_wrap(~pheno.unit)




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




