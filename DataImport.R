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


#### READ IN TRAITS DATA ####
traits.15 <- read.csv("trait.csv", sep=";", header=TRUE)
str(traits.15)


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
  filter(first > minDoy) %>% # remove if plant is flowering in the first week
  ungroup() %>% 
  select(-minDoy) %>% # remove this variable
  mutate_each(funs(as.numeric), first, peak, end, duration) %>% # make variables numeric (probably not necessary)
  mutate(pheno.stage = substring(pheno.stage, nchar(pheno.stage), nchar(pheno.stage))) %>%  # take last letter from pheno.stage
  gather(key = pheno.var, value = doy, -turfID, -species, -pheno.stage) %>% # create pheno.var and gather 4 variable into 1 column
  left_join(turfs.15, by = "turfID") # merge data set with turfs.15
head(pheno.long)


#### CALCULATE EVENT IN DAYS SINCE SNOWMELT
pheno.long$d.snowmelt <- pheno.long$doy - pheno.long$d.dosm
pheno.long$o.snowmelt <- pheno.long$doy - ifelse(pheno.long$newTT == "control", pheno.long$o.dosm, pheno.long$d.dosm)

#### CALCULATE CUMULATIVE TEMPERATURE SINCE SNOWMELT ####
climate <- climateData %>% 
  filter(logger=="temp30cm", year==2015) %>% 
  select(site, doy, cumTemp) %>% 
  mutate(site.doy = paste(doy, site, sep="_"))

pheno.long$doy.site <- paste(pheno.long$doy, substr(pheno.long$siteID,1,3), sep="_")
pheno.long$doy.destsite <- paste(pheno.long$doy, substr(pheno.long$destSiteID,1,3), sep="_")
pheno.long$dosm.site <- paste(pheno.long$o.dosm, substr(pheno.long$siteID,1,3), sep="_")
pheno.long$dosm.destsite <- paste(pheno.long$d.dosm, substr(pheno.long$destSiteID,1,3), sep="_")

#destination
pheno.long$dCumTempFlower <- climate$cumTemp[match(pheno.long$doy.destsite, climate$site.doy)] # CumTemp until FLOWERING
pheno.long$dCumTempSnow <- climate$cumTemp[match(pheno.long$dosm.destsite, climate$site.doy)] # CumTemp until SNOWMELT
pheno.long$dCumTemp <- pheno.long$dCumTempFlower - pheno.long$dCumTempSnow # Degree days

#origin
pheno.long$oCumTempFlower <- climate$cumTemp[match(pheno.long$doy.site, climate$site.doy)] # CumTemp until FLOWERING
pheno.long$oCumTempSnow <- ifelse(pheno.long$newTT == "control", climate$cumTemp[match(pheno.long$dosm.site, climate$site.doy)], climate$cumTemp[match(pheno.long$dosm.destsite, climate$site.doy)]) # CumTemp until SNOWMELT
pheno.long$oCumTemp <- pheno.long$oCumTempFlower - pheno.long$oCumTempSnow # Degree days


# create new variable pheno.unit: doy, snowmelt and cumTemp
pheno.long <- pheno.long %>%
  select(turfID, species, pheno.stage, pheno.var, doy, d.snowmelt, o.snowmelt, oCumTemp, dCumTemp) %>% 
  gather(key = pheno.unit, value = value, -turfID, -species, -pheno.stage, -pheno.var) %>%
  left_join(turfs.15, by = "turfID") %>% # add metadata
  left_join(traits.15, by = "species")
#filter(pheno.unit!="d.snowmelt" & pheno.var != "duration") %>%
#filter(pheno.unit!="o.snowmelt" & pheno.var != "duration")
head(pheno.long)


#### SAVE PHENO.LONG ####
save(pheno.long, file = "PhenoLong.RData")