pheno15 %>%
  select(turfID, species, doy, Site, nr.b, nr.f, nr.s, nr.r) %>%
  gather(key = pheno.stage, value = value, -turfID, -species, -Site, -doy) %>% # make variable pheno.stage
  group_by(turfID, species, pheno.stage) %>%  # group by turfID, species and phenological stage to calculate first, end etc for each stage
  mutate(minDoy = min(doy, na.rm = TRUE)) %>% # calculate min doy
  group_by(minDoy, add = TRUE) %>% # add variable but remember the previous groups
  filter(value > 0) %>% 
  left_join(turfs.15, by = "turfID") %>% 
  filter(species == "Ant.odo", pheno.stage == "nr.f") %>% 
  ggplot(aes(x = doy, y = value, color = newTT)) +
  geom_point() +
  facet_grid(Temperature_level ~ Precipitation_level)
