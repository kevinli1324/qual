

foo <- lmer(maxPM2.524 ~ (1|station)  + month +  piece1 + piece2 + diff_east + 
              reform + TEMP + PRES + RAINT + WSPM + WSPM*diff_east + RAINT*month + TEMP*month, data = fdat_lag)

fdat_test <- mutate(fdat_lag %>% ungroup(), residual = maxPM2.524 - predict(foo))



ggplot(data = fdat_test, aes(x = month, y = residual)) + geom_boxplot()

ggplot(data = fdat_test, aes(x = station, y = residual)) + geom_boxplot()

ggplot(data = fdat_test, aes(x = RAINT, y = residual)) + geom_point()

ggplot(data = fdat_test, aes(x = RAINT, y = residual)) + geom_point()


ggplot(data = fdat_test, aes(sample = (residual - mean(residual))/sd(residual))) + geom_qq()  + geom_qq_line() + 
  facet_wrap(~month) + xlab("Theoretical Normal Quantile") + ylab("Standardized Residuals")


plot(fdat_test$TEMP, fdat_test$maxPM2.524)

w <- lm(maxPM2.524 ~ RAIN, data = fdat_lag)

library(MASS)
woop <- boxcox(maxPM2.524 ~ RAIN, data = fdat_lag)
lam <- woop$x[which(woop$x == max(woop$y))]