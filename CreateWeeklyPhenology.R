### Make a weekly data set

WeeklyPhenology <- pheno15 %>% 
  select(turfID, species, week, doy, Site, week.nr2, nr.b, nr.f, nr.s) %>% 
  left_join(turfs.15, by = c("turfID")) %>% 
  select(turfID, siteID, blockID, destSiteID, destBlockID, TTtreat, newTT, species, week, week.nr2, nr.b, nr.f, nr.s, Temperature_level, Precipitation_level, destT_level, destP_level) %>% 
  rename(origSite = siteID, origBlock = blockID, destSite = destSiteID, destBlock = destBlockID, Treatment = newTT, weekNumber = week.nr2, Bud = nr.b, Flower = nr.f, Seed = nr.s) %>% 
  gather(key = pheno.stage, value = value, Bud, Flower, Seed) %>% 
  as.tibble() %>% 
  mutate(presence = ifelse(is.na(value), 0, 1))
  
save(WeeklyPhenology, file = "WeeklyPhenology.Rdata")

