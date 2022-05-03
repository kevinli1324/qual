library(tidyr)
library(dplyr)
library(ggplot2)
library(mice)
library(zoo)
library(lubridate)
library(brms)
library(lme4)
compass_dict <- list("N" = 0, "NE" = 45, "E" = 90, "NW" = 315, "WNW" = 292, "ENE" = 65, "E" = 90, 
                     "W" = 270, "SSW" = 202, "WSW" = 248, "SE" = 136, "SSE" = 158, 
                     "ESE" = 112, "S" = 180, "SW" = 226, "NNW" = 338, "NNE"  = 24)

dat <- read.csv("beijing.csv")
dat <- mutate(dat, time_string = paste(paste(year, month, day, sep = "-"), paste0(hour, ".00")))
dat <- mutate(dat, th = ymd_hm(time_string), dt = ymd(paste(year, month, day, sep = "-")))
dat <- mutate(dat, hr_diff = as.numeric((dat$th - min(dat$th)))/3600)

compass_vec <- rep(NA, nrow(dat))
for(i in 1:nrow(dat)) {
  if(!is.na(dat$wd[i])) {
    compass_vec[i] <- compass_dict[[dat$wd[i]]]
  }
}

dat$wind_compass <- compass_vec
dat <- mutate(dat, diff_east = abs(wind_compass - 90), na.rm = TRUE)

dat_vars <- select(dat, dt, year, month, station, TEMP, PRES, DEWP, RAIN,diff_east, WSPM )
dat_vars <- group_by(dat_vars, year, month, station) %>%
  summarise(TEMP = mean(TEMP, na.rm = TRUE), PRES = mean(PRES, na.rm = TRUE),
           DEWP = mean(DEWP, na.rm = TRUE), 
           RAIN = sum(RAIN, na.rm = TRUE), 
           diff_east = mean(diff_east, na.rm = TRUE),
           WSPM = mean(WSPM, na.rm = TRUE)) %>% ungroup()

#imp_dat <- mice(dat,m=5,maxit=50,meth='pmm',seed=500)
#saveRDS(imp_dat, "imputed_data.rds")

test_dat <- select(dat, station, dt, year, month, day, hour, hr_diff, O3, PM2.5, PM10, SO2, NO2, CO)


rolling_dat <- arrange(test_dat, station, hr_diff) %>% group_by(station) %>% 
  mutate(O3_8 = rollapply(O3, width = 8, mean, na.rm = TRUE, fill = NA), 
         CO_8 = rollapply(CO, width = 8, mean, na.rm = TRUE, fill = NA), 
         PM2.5_24  = rollapply(PM2.5, width = 24, mean, na.rm = TRUE, fill = NA), 
         PM10_24 = rollapply(PM10, width = 24, mean, na.rm = TRUE, fill = NA), 
         SO2_3 = rollapply(SO2, width = 3, mean, na.rm = TRUE, fill = NA),
  ) %>% ungroup()


complete_roll <- rolling_dat[complete.cases(rolling_dat), ]

grouped_day <- group_by(rolling_dat, station, year, month) %>% summarise(
  max_CO8 = sum(all(is.na(CO8))), 
  max_O38 = sum(is.na(CO8)), 
  maxPM2.524 = sum(is.na(CO8)), 
  maxPM1024 = sum(is.na(CO8)), 
  maxNO2 = max(NO2, na.rm = TRUE), 
  maxS02_3 = max(SO2_3, na.rm = TRUE)
) %>% ungroup()

sum(complete.cases(dat))

grouped_day <- group_by(rolling_dat, station, year, month) %>% summarise(
  max_CO = max(CO, na.rm = TRUE), 
  max_CO8 = max(CO_8, na.rm = TRUE), 
  max_O38 = max(O3_8, na.rm = TRUE), 
  maxPM2.524 = max(PM2.5_24, na.rm = TRUE), 
  maxPM1024 = max(PM10_24, na.rm = TRUE), 
  maxNO2 = max(NO2, na.rm = TRUE), 
  maxS02_3 = max(SO2_3, na.rm = TRUE)
) %>% ungroup()
 
finite_final <- grouped_day %>% 
  filter_all(all_vars(!is.infinite(.)))

finite_final_vars <- finite_final %>% left_join(dat_vars)

dep_var <- select(finite_final_vars, maxPM2.524,maxPM1024, maxNO2, maxS02_3, max_CO8, max_O38, 
                  RAIN, TEMP, PRES, diff_east, WSPM)
pairs(dep_var)

colnames(dep_var) <- c("PM2.5", "PM10", "NO2", "SO2", "CO8", "O3", "Rain", "Temp", "Pressure", "EastDiff", 
                       "WS")

library(GGally)
ggpairs(dep_var) + ggtitle("Pairs Plot Covariates and Response")
saveRDS(finite_final_vars, "final_dat.rds")

### do plots eda ###
long_dat <- pivot_longer(finite_final_vars, cols = c("max_CO", "max_CO8", "max_O38", "maxPM2.524",
                                                "maxPM1024" , "maxNO2", "maxS02_3"), names_to = "chem") 

ggplot(data = filter(long_dat, 
                     station == "Aotizhongxin", chem == "maxPM2.524", month == 3)) + geom_line(aes(x = day, y = value)) +
  facet_wrap(~year)
  


stat_chem <- filter(long_dat, 
       station == "Aotizhongxin", chem == "maxS02_3")


ggplot(data  = long_dat, aes(x = RAIN, y = value)) + geom_point() + 
  facet_wrap(~chem)

plot_dat <- group_by(long_dat, chem) %>% mutate(value = scale(value))
ggplot(data  = long_dat, aes(x = RAIN, y = value)) + geom_point() + 
  facet_wrap(~chem)

ggplot(data  = plot_dat, aes(x = RAIN, y = value)) + geom_point() + 
  facet_wrap(~chem)

ggplot(data  = plot_dat, aes(x = RAIN, y = value)) + geom_point() + 
  facet_wrap(~chem)

ggplot(data  = plot_dat, aes(x = TEMP, y = value)) + geom_point() + 
  facet_wrap(~chem)

ggplot(data  = plot_dat, aes(x = PRES, y = value)) + geom_point() + 
  facet_wrap(~chem)

ggplot(data  = plot_dat, aes(x = diff_east, y = value)) + geom_point() + 
  facet_wrap(~chem)

ggplot(data  = plot_dat, aes(x = diff_east, y = value)) + geom_point() + 
  facet_wrap(~chem)





