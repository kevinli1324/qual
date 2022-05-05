#### Mass regressions ####
fdat <- readRDS("final_dat.rds")
fdat <- mutate(fdat, month = as.factor(month), year= as.factor(year),
               ddate = ymd(paste(year, month, "1", sep = "-")))

fdat <- mutate(fdat,
               ddiff = round(as.numeric((ddate - min(ddate)))/30, 1), 
               reform = ifelse(ddate >= ymd("2013-09-01"), 1, 0),
               piece1 = ddiff, 
               piece2 = ddiff*reform)

m_group <- group_by(fdat, month, station) %>% summarise(
  CO = mean(max_CO8), 
  O3 = mean(max_O38), 
  PM2.5 = mean(maxPM2.524), 
  NO2 = mean(maxNO2), 
  PM10 = mean(maxPM1024), 
  SO2 = mean(maxS02_3)
)


long_pivot <- pivot_longer(m_group, cols = c(CO, O3, PM2.5, NO2, PM10, SO2))


ggplot(long_pivot, aes(x = as.numeric(month), y = value, color = station)) + geom_point() + 
  facet_wrap(~name, scales = "free") + xlab("Month") + ylab("Mean Measurement (ug/m^3)") + 
  ggtitle("Average Pollutant Measurement By Month")



s_group <- group_by(fdat, month, station) %>% summarise(
  CO = mean(max_CO8), 
  O3 = mean(max_O38), 
  PM2.5 = mean(maxPM2.524), 
  NO2 = mean(maxNO2), 
  PM10 = mean(maxPM1024), 
  SO2 = mean(maxS02_3)
)


long_pivot <- pivot_longer(s_group, cols = c(CO, O3, PM2.5, NO2, PM10, SO2))


ggplot(long_pivot, aes(x = station, y = value)) + geom_boxplot() + 
  facet_wrap(~name, scales = "free") + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + 
  ylab("Mean Measurement (ug/m^3)") +  xlab("Station") + 
  ggtitle("Average Pollutant Measurement By Station")



y_group <- group_by(fdat, month, year) %>% summarise(
  CO = mean(max_CO8), 
  O3 = mean(max_O38), 
  PM2.5 = mean(maxPM2.524), 
  NO2 = mean(maxNO2), 
  PM10 = mean(maxPM1024), 
  SO2 = mean(maxS02_3)
)

long_pivot <- pivot_longer(y_group, cols = c(CO, O3, PM2.5, NO2, PM10, SO2))
ggplot(data = long_pivot, aes(x = year, y = value)) + geom_point() + facet_wrap(month~name, scales = "free_y")

x
## temp interaction ###
colnames(fdat_lag)[5:10] <- c("CO", "O3", "PM2.5", "PM10", "NO2", "SO2")
long_pivot <- pivot_longer(fdat_lag, cols = c(CO, O3, PM2.5, NO2, PM10, SO2))

dp <- filter(long_pivot, name == "PM10")
ggplot(data = dp, aes(x = diff_east, y = value))+ geom_point() + facet_wrap(~month, scales = "free") + 
  xlab("Mean East Difference Angle (Degrees)") + ylab("Mean PM10 Measurement (ug/m^3)")+ ggtitle("Mean East Difference Angle vs PM10 Measurements")

dp <- filter(long_pivot, name == "PM10")
ggplot(data = dp, aes(x = TEMP, y = value))+ geom_point() + facet_wrap(~month, scales = "free") + 
  xlab("Mean Temperature (C)") + ylab("Mean PM10 Measurement (ug/m^3)")+ ggtitle("Mean Temperature vs PM10 Measurements")

woot <- posterior_samples(fit)


dp <- filter(long_pivot, name == "PM10")
ggplot(data = dp, aes(x = diff_east, y = value)) + geom_point()

