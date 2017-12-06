#### SEEDCLIM GRIDDED DATA ####

library("ggplot2")
library("tidyr")
library("plyr")
library("dplyr")
library("lubridate")

load("~/Dropbox/Bergen/SeedClim Climate/SeedClim-Climate-Data/Daily.Temp_GriddedData_2010-2015.RData", verbose = TRUE)
head(daily.temp)

daily.temp$site <- factor(daily.temp$site, levels=c("Skjellingahaugen", "Gudmedalen", "Lavisdalen", "Ulvhaugen", "Veskre", "Rambera", "Hogsete", "Alrust", "Ovstedal", "Arhelleren", "Vikesland", "Fauske"))

col.red <- "#0072B2"

daily.temp %>% 
    filter(date > as.POSIXct(ymd("2014.1.1")), date < as.POSIXct(ymd("2015.12.31"))) %>%
    filter (site %in% c("Skjellingahaugen", "Gudmedalen", "Lavisdalen", "Veskre", "Rambera", "Hogsete")) %>% 
    ggplot(aes(x = date, y = temperature, color = col.red)) + 
    geom_line() +
    scale_x_datetime(date_breaks = "3 month", date_labels = "%B %Y") +
    theme(axis.text.x = element_text(angle = 90, hjust = 0.5, vjust = 0)) +
    facet_wrap(~site) +
    xlab("") + ylab("Temperature in °C") +
    theme(legend.position="none")

# add week nr
daily.temp$week.nr2 <- week(daily.temp$date)
daily.temp$doy <- yday(daily.temp$date)
daily.temp$year <- year(daily.temp$date)
CumT <- ddply(daily.temp, .(site, year), summarize, CumTemperature = cumsum(temperature > 0))
daily.temp <- cbind(daily.temp, "CumTemperature" = CumT[,3] )
head(daily.temp)

# calculate cumsum
daily.temp %>%
  filter(date > as.POSIXct(ymd("2014.1.1")), date < as.POSIXct(ymd("2015.12.31"))) %>%
  filter (site %in% c("Skjellingahaugen", "Gudmedalen", "Lavisdalen", "Veskre", "Rambera", "Hogsete")) %>% 
  group_by(year) %>%
  ggplot(aes(x = date, y = CumTemperature, color = col.red)) + 
  geom_line() +
  facet_wrap(~site)




