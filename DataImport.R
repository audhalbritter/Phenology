##################################################
#### PHENOLOGY DATA IMPORT AND HANDLING       ####
##################################################

#### LIBRARIES ####
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
# Warning message "failed to parse" is because no measurement in w34 in some sites. Not a problem!




#### CALCULATE SUM OF BUD, FLOWER, SEED AND RIPE SEEDS PER TURFID AND SPECIES ####
pheno15 <- CalcSums(pheno15)


#### READ IN TURFS 2015 ####
turfs.15 <- read.csv("turfs.csv", sep=";", header=TRUE, stringsAsFactors=FALSE)
# rescaling Temp and Prec values
turfs.15$Prec_valueRescale <- (turfs.15$Prec_value-min(turfs.15$Prec_value))/(max(turfs.15$Prec_value) - min(turfs.15$Prec_value))
turfs.15$Temp_valueRescale <- (turfs.15$Temp_value-min(turfs.15$Temp_value))/(max(turfs.15$Temp_value) - min(turfs.15$Temp_value))
turfs.15$destPrec_valueRescale <- (turfs.15$destPrec_value-min(turfs.15$destPrec_value))/(max(turfs.15$destPrec_value) - min(turfs.15$destPrec_value))
turfs.15$destTemp_valueRescale <- (turfs.15$destTemp_value-min(turfs.15$destTemp_value))/(max(turfs.15$destTemp_value) - min(turfs.15$destTemp_value))
turfs.15$d.date.osm <- dmy(turfs.15$d.date.osm)
turfs.15$d.dosm <- yday(turfs.15$d.date.osm)
turfs.15$o.date.osm <- dmy(turfs.15$o.date.osm)
turfs.15$o.dosm <- yday(turfs.15$o.date.osm)
head(turfs.15)
str(turfs.15)
# all variables From1To2Temp etc must be numeric


#### READ IN TRAITS DATA ####
traits.15 <- read.csv("trait.csv", sep=";", header=TRUE, stringsAsFactors=FALSE)
str(traits.15)

#### IMPORT CLIMATE DATA WITH CUM TEMP ####
load(file = "climateData.Rdata", verbose = TRUE)
climate <- climateData %>% 
  filter(logger=="temp30cm", year==2015) %>% 
  select(site, doy, cumTemp) %>% 
  mutate(site.doy = paste(doy, site, sep="_"))


#### CALCULATE FIRST, PEAK, END AND DURATION ####
### MAKE LONG DATA SET ###
pheno.long <- pheno15 %>%
  select(turfID, species, doy, Site, nr.b, nr.f, nr.s, nr.r) %>%
  gather(key = pheno.stage, value = value, -turfID, -species, -Site, -doy) %>% # make variable pheno.stage
  group_by(turfID, species, pheno.stage) %>%  # group by turfID, species and phenological stage to calculate first, end etc for each stage
  mutate(minDoy = min(doy, na.rm = TRUE)) %>% # calculate min doy
  group_by(minDoy, add = TRUE) %>% # add variable but remember the previous groups
  filter(value > 0) %>%
  summarize(first = first(doy), end = last(doy), peak = doy[which.max(value)]) %>%
  filter(first > minDoy) %>% # remove if plant is flowering in the first week
  ungroup() %>% 
  select(-minDoy) %>% # remove this variable
  mutate_each(funs(as.numeric), first, peak, end) %>% # make variables numeric (probably not necessary)
  mutate(pheno.stage = substring(pheno.stage, nchar(pheno.stage), nchar(pheno.stage))) %>%  # take last letter from pheno.stage
  gather(key = pheno.var, value = value, -turfID, -species, -pheno.stage) %>% # create pheno.var and gather 4 variable into 1 column
  left_join(turfs.15, by = "turfID") # merge data set with turfs.15
head(pheno.long)


#### CALCULATE DAYS BETWEEN PHENO.STAGES IN DAYS ####
### DURATION BETWEEN FIRST AND END OF PHENO.STAGES
pheno.long <- pheno.long %>% 
  select(turfID, species, newTT, value, pheno.var, pheno.stage, d.dosm, o.dosm) %>%
  spread(key = pheno.stage, value = value) %>% 
  # in days
  mutate(d.smb = b - d.dosm) %>% 
  mutate(o.smb = b - ifelse(newTT == "control", o.dosm, d.dosm)) %>% 
  mutate(bf = f-b, fs = s-f) %>% # calculate difference in days between bud-flower and flower-seed
  gather(key = pheno.stage, value = value, b, f, r, s, d.smb, o.smb, bf, fs) %>% 
  mutate(pheno.unit = ifelse(pheno.stage %in% c("d.smb", "o.smb", "bf", "fs"), "days", "doy")) %>% # create variable pheno.unit
  # calculate duration of stages
  spread(key = pheno.var, value = value) %>% 
  mutate(duration = ifelse(pheno.unit == "doy", end-(first-1), NA)) %>% # calculate duration
  gather(key = pheno.var, value = value, end, first, peak, duration) %>% 
  mutate(pheno.unit = replace(pheno.unit, pheno.var == "duration", "days")) %>% 
  filter(!is.na(value))


#### CALCULATE EVENT IN DAYS SINCE SNOWMELT ####
pheno.long <- pheno.long %>% 
  mutate(d.snowmelt = ifelse(pheno.unit == "doy", value - d.dosm, NA)) %>% 
  mutate(o.snowmelt = ifelse(pheno.unit == "doy", value - ifelse(newTT == "control", o.dosm, d.dosm), NA))


#### CALCULATE CUMULATIVE TEMPERATURE SINCE SNOWMELT ####
pheno.long <- pheno.long %>% 
  select(-newTT, -d.dosm, -o.dosm) %>% 
  left_join(turfs.15, by = "turfID") %>% 
  mutate(doy.site = paste(value, siteID, sep="_")) %>% 
  mutate(doy.destsite = paste(value, destSiteID, sep="_")) %>% 
  mutate(dosm.site = paste(o.dosm, siteID, sep="_")) %>% 
  mutate(dosm.destsite = paste(d.dosm, destSiteID, sep="_"))

# destination
pheno.long$dCumTempPhenoEvent <- climate$cumTemp[match(pheno.long$doy.destsite, climate$site.doy)] # CumTemp until PHENO EVENT
pheno.long$dCumTempSnow <- climate$cumTemp[match(pheno.long$dosm.destsite, climate$site.doy)] # CumTemp until SNOWMELT
pheno.long$dCumTemp <- ifelse(pheno.long$pheno.unit == "doy", pheno.long$dCumTempPhenoEvent - pheno.long$dCumTempSnow, NA) # Degree days

#origin
pheno.long$oCumTempPhenoEvent <- ifelse(pheno.long$newTT == "control", climate$cumTemp[match(pheno.long$doy.site, climate$site.doy)], climate$cumTemp[match(pheno.long$doy.destsite, climate$site.doy)]) # CumTemp until FLOWERING
pheno.long$oCumTempSnow <- ifelse(pheno.long$newTT == "control", climate$cumTemp[match(pheno.long$dosm.site, climate$site.doy)], climate$cumTemp[match(pheno.long$dosm.destsite, climate$site.doy)]) # CumTemp until SNOWMELT
pheno.long$oCumTemp <- ifelse(pheno.long$pheno.unit == "doy", pheno.long$oCumTempPhenoEvent - pheno.long$oCumTempSnow, NA) # Degree days


# create new variable pheno.unit: doy, snowmelt and cumTemp, days
pheno.long <- pheno.long %>%
  select(turfID, species, pheno.unit, pheno.var, pheno.stage, value, d.snowmelt, o.snowmelt, dCumTemp, oCumTemp) %>% 
  spread(key = pheno.unit, value = value) %>% 
  gather(key = pheno.unit, value = value, -pheno.var, -turfID, -species, -pheno.stage) %>%
  filter(!is.na(value)) %>% 
  left_join(turfs.15, by = "turfID") %>% # add metadata
  left_join(traits.15, by = "species")
head(pheno.long)


#### RENAME VARIABLES ####
Phenology <- pheno.long %>% 
  filter(Precipitation_level != 1) %>% #remove turfs transplanted from Ulv and Alr, because they have no control
  mutate(pheno.var = factor(pheno.var, levels = c("first", "peak", "end", "duration"))) %>% 
  mutate(newTT = plyr::mapvalues(newTT, c("control", "TT2", "TT3", "TT4"), c("Control", "Warm", "Wet", "WarmWet"))) %>%
  mutate(newTT = factor(newTT, levels = c("Control", "Warm", "Wet", "WarmWet"))) %>% 
  mutate(pheno.stage = plyr::mapvalues(pheno.stage, c("b", "f", "s", "r", "o.smb", "d.smb", "bf", "fs"), c("Bud", "Flower", "Seed", "RipeSeed", "SMBud", "SMBudDest", "BudFlower", "FlowerSeed"))) %>%
  mutate(pheno.stage = factor(pheno.stage, levels = c("Bud", "Flower", "Seed", "RipeSeed", "SMBud", "SMBudDest", "BudFlower", "FlowerSeed"))) %>% 
  mutate(pheno.unit = plyr::mapvalues(pheno.unit, c("doy", "days", "o.snowmelt", "oCumTemp", "d.snowmelt", "dCumTemp"), c("DOY", "Days", "DaysSinceSM", "TempSinceSM", "DaysSinceSMDest", "TempSinceSMDest"))) %>%
  mutate(pheno.unit = factor(pheno.unit, levels = c("DOY", "Days", "DaysSinceSM", "TempSinceSM", "DaysSinceSMDest", "TempSinceSMDest"))) %>% 
  mutate_each(funs(as.factor), species, flowering.time, functionalGroup, occurrence.2)

#### SAVE PHENO.LONG ####
save(Phenology, file = "PhenoLong.RData")
