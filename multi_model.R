library(brms)
library(ggplot2)
library(tidyr)
library(dplyr)
library(lme4)
library(lubridate)
#### Mass regressions ####
fdat <- readRDS("final_dat.rds")
fdat <- mutate(fdat, month = as.factor(month), year= as.factor(year),
               ddate = ymd(paste(year, month, "1", sep = "-")))

fdat <- mutate(fdat,
               ddiff = round(as.numeric((ddate - min(ddate)))/30, 1), 
               reform = ifelse(ddate >= ymd("2013-09-01"), 1, 0),
               piece1 = ddiff, 
               piece2 = ddiff*reform)


fdat <- fdat %>% arrange(year, month) %>%
  group_by(station) %>%
  mutate(max_CO8_lag1 = dplyr::lag(max_CO8, n = 1, default = NA), 
         max_O38_lag1 = dplyr::lag(max_O38, n = 1, default = NA), 
         maxPM2.524_lag1 = dplyr::lag(maxPM2.524, n = 1, default = NA), 
         maxPM1024_lag1 = dplyr::lag(maxPM1024, n = 1, default = NA), 
         maxNO2_lag1 = dplyr::lag(maxNO2, n = 1, default = NA), 
         maxS02_lag1 = dplyr::lag(maxS02_3, n = 1, default = NA)) %>% ungroup()

fdat_lag <- fdat[complete.cases(fdat),]

fdat_lag <- mutate(fdat_lag, PRES = PRES - mean(PRES), RAINT = 1/(RAIN + .01))

foo <- lm(maxPM2.524 ~ station + piece1 + piece2 +  month + diff_east + max_CO8_lag1 +
            max_O38_lag1 + maxPM2.524_lag1  + maxPM1024_lag1 + maxNO2_lag1 + 
            maxS02_lag1 + reform + TEMP + PRES + RAINT + WSPM + WSPM*diff_east, data = fdat_lag) %>% summary()

bform1 <- 
  bf(mvbind(max_CO8, max_O38, maxPM2.524, maxPM1024, maxNO2, maxS02_3 ) ~ 
       -1 + (1|station) + month + reform + diff_east + piece1 + 
       piece2 + TEMP + PRES + RAINT + WSPM + WSPM*diff_east + 
       month*RAINT + month*TEMP)


#### create priors ####


responses <- c("maxCO8", "maxO38", "maxPM2524", "maxPM1024", "maxNO2", "maxS023")

og_vec <- c()
for(i in 1:length(responses)) {
  t_prior <- c(set_prior("normal(0, 10)", class = "b", resp = responses[i]), 
               set_prior("cauchy(0, 1)", class = "sd",group = "station", resp = responses[i]), 
               set_prior("cauchy(0, 1)", class = "sd",coef = "Intercept",
                         group = "station",
                         resp = responses[i]),
               set_prior("cauchy(0, 1)", class = "sigma", resp = responses[i])
               )
  og_vec <- append(og_vec, t_prior)
}

c08_scale <- c(mean(fdat$max_CO8), sd(fdat$max_CO8))
O38_scale <-  c(mean(fdat$max_O38), sd(fdat$max_O38))
PM25_scale <- c(mean(fdat$maxPM2.524), sd(fdat$maxPM2.524))
PM10_scale <- c(mean(fdat$maxPM1024), sd(fdat$maxPM1024))
NO2_scale <- c(mean(fdat$maxNO2), sd(fdat$maxNO2))
S02_scale <- c(mean(fdat$maxS02_3), sd(fdat$maxS02_3))

fdat_scale <- mutate(fdat_lag, 
  max_CO8 = (max_CO8 - c08_scale[1])/c08_scale[2],
  max_O38 = (max_O38 - O38_scale[1])/O38_scale[2],
  maxPM2.524 = (maxPM2.524 - PM25_scale[1])/PM25_scale[2],
  maxPM1024 = (maxPM1024 - PM10_scale[1])/PM10_scale[2],
  maxNO2 = (maxNO2 - NO2_scale[1])/NO2_scale[2],
  maxS02_3 = (maxS02_3 - S02_scale[1])/S02_scale[2],
)

fdat_scale <- mutate(fdat_scale, RAINT = RAINT - mean(RAINT))

fit1 <- brm(bform1, data = fdat_scale, chains = 1, cores = 4, refresh = 20, iter = 25000,
            prior = og_vec, control=list(max_treedepth=10))
saveRDS(fit1, file = "og_run.rds")


toof <- get_prior(bform1, data = fdat_lag, prior = bprior)

wee <- posterior_samples(fit1)
woo <- as_draws_list(fit1)

plot(woo$`1`$sigma_maxCO8, type = "l")

