##################################################
#### PHENOLOGY DATA IMPORT AND HANDLING       ####
##################################################

#### LIBRARIES ####
library("lme4")
library("tidyverse")
library("lubridate")
library("readr")

  

#### READ IN HEAD OF PHENOLOGY DATA 2015 ####
dath1 <- ReadInHeadPhenology15("DataSheet2015Hog.csv", "Hogsete")
dath2 <- ReadInHeadPhenology15("DataSheet2015Ram.csv", "Rambaera")
dath3 <- ReadInHeadPhenology15("DataSheet2015Ves.csv", "Veskre")
dath4 <- ReadInHeadPhenology15("DataSheet2015Lav.csv", "Lavisdalen")
dath5 <- ReadInHeadPhenology15("DataSheet2015Gud.csv", "Gudmedalen")
dath6 <- ReadInHeadPhenology15("DataSheet2015Skj.csv", "Skjellingahaugen")
meta.pheno <- data.frame(rbind(dath1, dath2, dath3, dath4, dath5, dath6))
meta.pheno$date<-as.Date(meta.pheno$date, format="%d.%m.%Y")
meta.pheno <- meta.pheno %>% filter(!is.na(date))
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
#### SAVE PHENO.15 ####
save(pheno15, file = "pheno15.RData")


#### READ IN TURFS 2015 ####
turfs.15 <- read_csv2(file = "turfs.csv")
# rescaling Temp and Prec values

turfs.15 <- turfs.15 %>% 
  mutate(Prec_valueRescale = (Prec_value - min(Prec_value))/(max(Prec_value) - min(Prec_value))) %>% 
  mutate(Temp_valueRescale = (Temp_value - min(Temp_value))/(max(Temp_value) - min(Temp_value))) %>%
  mutate(destPrec_valueRescale = (destPrec_value - min(destPrec_value))/(max(destPrec_value) - min(destPrec_value))) %>% 
  mutate(destTemp_valueRescale = (destTemp_value - min(destTemp_value))/(max(destTemp_value) - min(destTemp_value))) %>%
  # calculate day of year of sm
  mutate(d.date.osm = dmy(d.date.osm)) %>% 
  mutate(d.dosm = yday(d.date.osm)) %>% 
  mutate(o.date.osm = dmy(o.date.osm)) %>% 
  mutate(o.dosm = yday(o.date.osm)) %>% 
  # calculate difference in SM between destination and origin site
  mutate(SMDiff = d.dosm - o.dosm)

head(turfs.15)
str(turfs.15)
# all variables From1To2Temp etc must be numeric


#### READ IN TRAITS DATA ####
traits.15 <- read.csv("trait.csv", sep=";", header=TRUE, stringsAsFactors=FALSE)
str(traits.15)


#### CREATE CLIMATE CONTEXT DATA ####
ClimateContext <- data_frame(
  siteID = c("Lavisdalen", "Gudmedalen", "Skjellingahaugen", "Lavisdalen", "Gudmedalen", "Hogsete", "Rambera", "Lavisdalen", "Gudmedalen"),
  Treatment = c(rep("Warmer", 3), rep("LaterSM", 4), rep("WarmLate", 2)),
  Shape = c("dry", "intermediate", "wet", "alpine-intermediate", "alpine-wet", "subalpine-intermediate", "subalpine-wet", "dry", "intermediate"),
  Alpha = c("dry", "intermediate", "wet", "alpine-intermediate", "alpine-wet", "subalpine-intermediate", "subalpine-wet", "dry", "intermediate")
)


#### IMPORT CLIMATE DATA WITH CUM TEMP ####
load(file = "climateData.Rdata", verbose = TRUE)
climate <- climateData %>% 
  filter(logger=="temp30cm", year==2015) %>% 
  select(site, doy, cumTemp) %>% 
  mutate(doy.site = paste(doy, site, sep="_")) %>% 
  ungroup() %>% 
  select(-year, -logger, -site, -doy)


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


#### CALCULATE EVENT IN DAYS SINCE SNOWMELT ####
pheno.long <- pheno.long %>% 
  select(turfID, blockID, destSiteID, destBlockID, species, newTT, value, pheno.var, pheno.stage, d.dosm, SMDiff) %>%
  mutate(dssm = value - d.dosm) %>% 
  rename(doy = value)



#### CALCULATE CUMULATIVE TEMPERATURE SINCE SNOWMELT ####
pheno.long <- pheno.long %>% 
  # paste event in doy and destSite
  mutate(event.destsite = paste(doy, destSiteID, sep="_")) %>% 
  # paste dosm and destSite
  mutate(dosm.destsite = paste(d.dosm, destSiteID, sep="_")) %>% 
  left_join(climate, by = c("event.destsite" = "doy.site")) %>% 
  rename(CumTempPhenoEvent = cumTemp) %>% 
  left_join(climate, by = c("dosm.destsite" = "doy.site")) %>% 
  rename(CumTempSM = cumTemp) %>% 
  # only calculate for doy, not dssm
  mutate(cumtemp = CumTempPhenoEvent - CumTempSM) %>% 
  select(-event.destsite, -dosm.destsite, -CumTempPhenoEvent, -CumTempSM) %>% 
  # pheno.unit
  gather(key = pheno.unit, value = value, -turfID, -blockID, -destSiteID, -destBlockID, -species, -newTT, -pheno.var, -pheno.stage, -d.dosm) %>% 
  # add metadata and traits
  left_join(turfs.15, by = c("turfID", "blockID", "destBlockID", "newTT", "d.dosm")) %>%
  left_join(traits.15, by = "species")




####### **************************************************************************
### NEEDS TO BE CHECKED !!!####
#### CALCULATE INTERVAL IN DAYS BETWEEN PEAK OF PHENO.STAGES ####
### DURATION BETWEEN FIRST AND END OF PHENO.STAGES
pheno.long <- pheno.long %>% 
  select(turfID, species, newTT, value, pheno.var, pheno.stage, d.dosm, o.dosm) %>%
  spread(key = pheno.stage, value = value) %>% 
  # Intervall bud - SM
  mutate(d.smb = ifelse(pheno.var == "peak", b - d.dosm, NA)) %>% 
  mutate(o.smb = ifelse(pheno.var == "peak", b - ifelse(newTT == "control", o.dosm, d.dosm), NA)) %>% 
  # Intervall between bud-flower and flower-seed
  mutate(bf = ifelse(pheno.var == "peak", f - b, NA), fs = ifelse(pheno.var == "peak", s - f, NA)) %>% 
  gather(key = pheno.stage, value = value, b, f, r, s, d.smb, o.smb, bf, fs) %>% 
  mutate(pheno.unit = ifelse(pheno.stage %in% c("d.smb", "o.smb", "bf", "fs"), "days", "doy")) %>% # create variable pheno.unit
  # calculate duration of stages
  spread(key = pheno.var, value = value) %>% 
  mutate(duration = ifelse(pheno.unit == "doy", end-(first-1), NA)) %>% # calculate duration
  gather(key = pheno.var, value = value, end, first, peak, duration) %>% 
  mutate(pheno.unit = replace(pheno.unit, pheno.var == "duration", "days")) %>% 
  filter(!is.na(value))

# create new variable pheno.unit: doy, snowmelt and cumTemp, days
pheno.long <- pheno.long %>%
  select(turfID, species, pheno.unit, pheno.var, pheno.stage, value, d.snowmelt, o.snowmelt, dCumTemp, oCumTemp) %>% 
  spread(key = pheno.unit, value = value) %>% 
  gather(key = pheno.unit, value = value, -pheno.var, -turfID, -species, -pheno.stage) %>%
  filter(!is.na(value)) %>% 
  left_join(turfs.15, by = "turfID") %>% # add metadata
  left_join(traits.15, by = "species") %>% 
head(pheno.long)
####### **************************************************************************


#### RENAME VARIABLES ####
Phenology <- pheno.long %>% 
  filter(Precipitation_level != 1) %>% #remove turfs transplanted from Ulv and Alr, because they have no control
  mutate(siteID = factor(siteID, levels = c("Lavisdalen", "Gudmedalen", "Skjellingahaugen", "Hogsete", "Rambera", "Veskre"))) %>% 
  mutate(pheno.var = factor(pheno.var, levels = c("first", "peak", "end", "duration"))) %>% 
  #mutate(newTT = plyr::mapvalues(newTT, c("control", "TT2", "TT3", "TT4"), c("Control", "Warm", "Wet", "WarmWet"))) %>%
  mutate(newTT = plyr::mapvalues(newTT, c("control", "TT2", "TT3", "TT4"), c("Control", "Warmer", "LaterSM", "WarmLate"))) %>%
  mutate(newTT = factor(newTT, levels = c("Control", "Warmer", "LaterSM", "WarmLate"))) %>% 
  mutate(pheno.stage = plyr::mapvalues(pheno.stage, c("b", "f", "s", "r"), c("Bud", "Flower", "Seed", "RipeSeed"))) %>%
  mutate(pheno.stage = factor(pheno.stage, levels = c("Bud", "Flower", "Seed", "RipeSeed"))) %>% 
  mutate(pheno.unit = plyr::mapvalues(pheno.unit, c("doy", "dssm", "cumtemp"), c("DOY", "DaysSinceSM", "CumTempSinceSM"))) %>%
  mutate(pheno.unit = factor(pheno.unit, levels = c("DOY", "DaysSinceSM", "CumTempSinceSM"))) %>% 
  mutate_each(funs(as.factor), species, flowering.time, functionalGroup, occurrence.2)

# pheno.stage intervalls: "o.smb", "d.smb", "bf", "fs" = "SMBud", "SMBudDest", "BudFlower", "FlowerSeed"


#### CREATE METADATA ####
MetaData <- turfs.15 %>% 
  select(siteID, blockID, newTT, SMDiff) %>% 
  mutate(Treatment = plyr::mapvalues(newTT, c("control", "TT2", "TT3", "TT4"), c("Control", "Warmer", "LaterSM", "WarmLate"))) %>%
  left_join(ClimateContext, by = c("siteID", "Treatment")) %>% 
  mutate(Treatment = factor(Treatment, levels = c("Control", "Warmer", "LaterSM", "WarmLate")))


#### SAVE PHENO.LONG ####
save(Phenology, file = "PhenoLong.RData")
