#### CALCULATE CUMSUM FOR EACH LOGGER ####

# Load daily temperature data
load("DailyTemperature.Rdata")

# Rearange temperature data
dailyT <- dailyTemperature %>% 
  mutate(site = factor(site, levels = c("Skj", "Gud", "Lav", "Ulv", "Ves", "Ram", "Hog", "Alr", "Ovs", "Arh", "Vik", "Fau"))) %>% 
  mutate(site = plyr::mapvalues(site, from = c("Skj", "Gud", "Lav", "Ulv", "Ves", "Ram", "Hog", "Alr", "Ovs", "Arh", "Vik", "Fau"), to = c("Skjellingahaugen", "Gudmedalen", "Lavisdalen", "Ulvehaugen", "Veskre", "Rambera", "Hogsete", "Alrust", "Ovstedalen", "Arhelleren", "Vikesland", "Fauske"))) %>% 
  arrange(logger, site) %>% 
  mutate(doy = yday(date), year = year(date))


#### calcualte cumsum #### 
climateData <- dailyT %>% 
  mutate(temp = replace(value, is.na(value), 0)) %>%  # replace NA by zero
  mutate(temp = replace(value, value < 5, 0)) %>%
  group_by(site, year, logger) %>% 
  mutate(cumTemp = cumsum(temp))

save(climateData, file = "climateData.Rdata")


# PLOT DATA
dailyTemperature %>%
  filter(site %in% c("Skj", "Lav", "Gud", "Ves", "Ram", "Hog"), logger %in% c("temp30cm")) %>%
  ggplot(aes(x = date, y = value, color = logger)) +
  geom_line() +
  facet_wrap(~site)



# calculate cumsum
dailyTemperature %>%
  filter(date > (ymd("2014.1.1")), date < (ymd("2015.12.31"))) %>%
  filter (site %in% c("Skj", "Lav", "Gud", "Ves", "Ram", "Hog")) %>% 
  filter(logger %in% c("temp30cm", "tempabove")) %>%
  ggplot(aes(x = date, y = CumTemperature, color = logger)) + 
  geom_line() +
  facet_wrap(~site)

dailyTemperature %>%
  filter(date > (ymd("2014.1.1")), date < (ymd("2015.12.31"))) %>%
  filter (site %in% c("Skj", "Lav", "Gud", "Ves", "Ram", "Hog")) %>% 
  filter(logger %in% c("temp30cm", "tempabove")) %>%
  ggplot(aes(x = date, y = value, color = logger)) + 
  geom_line() +
  facet_wrap(~site)
