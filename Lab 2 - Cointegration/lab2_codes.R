# Load Packages
pacman::p_load(here, readxl, dplyr, janitor, Quandl, xts, lubridate, urca, forecast, tidyverse, vars)

# Load data
data <- read_csv(here("data", "data_lecture2.csv"))
head(data)

data <- data %>%
  mutate(date = mdy(date), 
         lnbio = log(biodiesel),
         lnsoy = log(soyoil),
         lndiesel = log(diesel), 
         lncrude = log(crude))

soydiesel <- xts(data[,c("biodiesel", "soyoil", "diesel", "lnbio", "lnsoy", "lndiesel", "lncrude")], order.by = data$date)
head(soydiesel)

# Dickey Fuller (ADF)test
plot(soydiesel$biodiesel, grid.col = F, yaxis.right = F)
plot(soydiesel$soyoil, grid.col = F, yaxis.right = F)

summary(ur.df(soydiesel$biodiesel, type = c("none"), lags = 0))
summary(ur.df(soydiesel$soyoil, type = c("none"), lags = 0))

# Lag length selection - Partial autocorrelation tests
Pacf(soydiesel$biodiesel)
Pacf(soydiesel$soyoil)

# Lag length selection - selectlags specification
summary(ur.df(soydiesel$biodiesel, type = c("none"), lags = 4, selectlags = c("AIC")))
summary(ur.df(soydiesel$soyoil, type = c("none"), lags = 2, selectlags = c("AIC")))

# Lag length selection - VARselect function of vars package
VARselect(soydiesel$biodiesel, lag.max = 5) # 4 lags
VARselect(soydiesel$soyoil, lag.max = 5) # 1 lag

# ADF test - levels
summary(ur.df(soydiesel$biodiesel, type = c("trend"), lags = 4))
summary(ur.df(soydiesel$biodiesel, type = c("drift"), lags = 4))
summary(ur.df(soydiesel$biodiesel, type = c("none"), lags = 4))

summary(ur.df(soydiesel$soyoil, type = c("none"), lags = 1))
summary(ur.df(soydiesel$soyoil, type = c("drift"), lags = 1))
summary(ur.df(soydiesel$soyoil, type = c("trend"), lags = 1))

# ADF test - first difference
VARselect(diff.xts(soydiesel$biodiesel, na.pad = F), lag.max = 5) # lags
summary(ur.df(diff.xts(soydiesel$biodiesel, na.pad = F), type = c("trend"), lags = 3))

Pacf(diff.xts(soydiesel$soyoil))
VARselect(diff.xts(soydiesel$soyoil, na.pad = F), lag.max = 5)
summary(ur.df(diff.xts(soydiesel$soyoil, na.pad = F), type = c("trend"), lags = 0))

# Engle Granger Method
# Step 1
reg_biodieselsoy <- lm(biodiesel ~ soyoil, data = soydiesel)
summary(reg_biodieselsoy)

# access coefficients
reg_biodieselsoy$coefficients[2] * 100 / 7.35

# Step 2 
resid_soydiesel <- resid(reg_biodieselsoy) # save residuals
resid_ts <- xts(resid_soydiesel, order.by = index(soydiesel)) # convert to xts
soydieselr <- merge.xts(soydiesel, resid_ts) # merge to soydiesel dataframe
head(soydieselr)

VARselect(soydieselr$resid_ts, lag.max = 5) # check lags
summary(ur.df(soydieselr$resid_ts, type = c("none"), lags = 3)) #ADF test

# function that gives us the correct critical value of ADF test on residuals
englegranger <- function(var, trend, n){
  if (var == 2 & trend == 0){
    print(list(crit1 = -3.9001-10.534/n-30.03/n^2,
               crit5 = -3.3377-5.967/n-8.98/n^2,
               crit10 = -3.0462-4.069/n-5.73/n^2))
  } else if (var == 2 & trend == 1){
    print(list(crit1 = -4.326-15.531/n-34.03/n^2,
               crit5 = -3.7809-9.421/n-15.06/n^2,
               crit10 = -3.4959-7.203/n-4.01/n^2))
  } else if (var == 3 & trend == 0){
    print(list(crit1 = -4.2981-13.79/n-46.37/n^2,
               crit5 = -3.7429-8.352/n-13.41/n^2,
               crit10 = -3.4518-6.241/n-2.19/n^2))
  } else if (var == 3 & trend == 1){
    print(list(crit1 = -4.6676-18.492/n-18.492/n^2,
               crit5 = -4.1193-12.024/n-12.024/n^2,
               crit10 = -3.8344-9.188/n-9.188/n^2))
  } else {
    print('Beyond the scope of FRE530')
  }
}

# var = 2, trend = 0, n = 773
englegranger(2, 0, 773)

# Retest for cointegration using log prices
# Step 1
reg_lnbiodieselsoy <- lm(lnbio ~ lnsoy, data = soydiesel)
summary(reg_lnbiodieselsoy)

# Step 2 
resid_lnsoydiesel <- resid(reg_lnbiodieselsoy)
lnresid_ts <- xts(resid_lnsoydiesel, order.by = index(soydiesel))

soydieselr <- merge.xts(soydiesel, lnresid_ts)
head(soydieselr)

VARselect(soydieselr$lnresid_ts)
summary(ur.df(soydieselr$lnresid_ts, type = c("none"), lags = 3))

# Testing for cointegration with multiple prices - Johansen Test
# create dataframe
jotest <- soydiesel[,c("lncrude", "lndiesel", "lnbio", "lnsoy")]

# identify number of lags (K) to include
VARselect(jotest, lag.max = 10)

# run Johansen test
summary(ca.jo(jotest, type = c("trace"), ecdet = c("none"), K = 3, spec = c("transitory")))

