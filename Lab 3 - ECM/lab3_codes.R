##### COINTEGRATION (WEEK 2 LECTURE)

# Load Packages
pacman::p_load(here, readxl, dplyr, janitor, Quandl, xts, lubridate, urca, forecast, tidyverse, vars, modelsummary)

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

# ADF Tests - Levels (Log Prices)
VARselect(soydiesel$lnbio, lag.max = 5)$select
summary(ur.df(soydiesel$lnbio, type = c("trend"), lags = 4))
summary(ur.df(soydiesel$lnbio, type = c("drift"), lags = 4))
summary(ur.df(soydiesel$lnbio, type = c("none"), lags = 4))

VARselect(soydiesel$lnsoy, lag.max = 5)$select
summary(ur.df(soydiesel$lnsoy, type = c("trend"), lags = 1))
summary(ur.df(soydiesel$lnsoy, type = c("drift"), lags = 1))
summary(ur.df(soydiesel$lnsoy, type = c("none"), lags = 1))

# ADF Tests - first difference (Log Prices)
VARselect(diff.xts(soydiesel$lnbio, na.pad = F), lag.max = 5)$select
summary(ur.df(diff.xts(soydiesel$lnbio, na.pad = F), type = c("trend"), lags = 3))

VARselect(diff.xts(soydiesel$lnsoy, na.pad = F), lag.max = 5)$select
summary(ur.df(diff.xts(soydiesel$lnsoy, na.pad = F), type = c("trend"), lags = 1))

# Test for cointegration using log prices
# Step 1 - Run regression
reg_lnbiodieselsoy <- lm(lnbio ~ lnsoy, data = soydiesel)
summary(reg_lnbiodieselsoy)

# code to use modelsummary() function to format regression output
models <- list(
  "Biodiesel-Soy (Log)" = lm(lnbio ~ lnsoy, data = soydiesel)
)
modelsummary(models, estimate = "{estimate}{stars}")

# Step 2 - Save residuals, convert to xts, and do ADF test on residuals
resid_lnsoydiesel <- resid(reg_lnbiodieselsoy)
lnresid_ts <- xts(resid_lnsoydiesel, order.by = index(soydiesel))

soydieselr <- merge.xts(soydiesel, lnresid_ts)
head(soydieselr)

VARselect(soydieselr$lnresid_ts)$select

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

englegranger(2, 0, 773)
summary(ur.df(soydieselr$lnresid_ts, type = c("none"), lags = 3))@teststat # -3.6603 

# Testing for cointegration with multiple prices - Johansen Test
# create dataframe
jotest <- soydiesel[,c("lncrude", "lndiesel", "lnbio", "lnsoy")]

# identify number of lags (K) to include
VARselect(jotest, lag.max = 5)$select

# run Johansen test
summary(ca.jo(jotest, type = c("trace"), ecdet = c("none"), K = 3))


##### ERROR CORRECTION MODEL (WEEK 3 LECTURE)
# lag length
VARselect(soydieselr$lnbio, lag.max = 5)$select #4 lags
VARselect(soydieselr$lnsoy, lag.max = 5)$select #1 lag

ecm <- lm(diff.xts(lnbio, na.pad = T) ~ 
            lag.xts(diff.xts(lnbio, na.pad = T)) + 
            lag.xts(lag.xts(diff.xts(lnbio, na.pad = T))) + 
            lag.xts(lag.xts(lag.xts(diff.xts(lnbio, na.pad = T)))) + 
            lag.xts(lag.xts(lag.xts(lag.xts(diff.xts(lnbio, na.pad = T))))) +
            diff.xts(lnsoy, na.pad = T) + 
            lag.xts(diff.xts(lnsoy, na.pad = T)) + 
            lag.xts(lnresid_ts), data = soydieselr)
summary(ecm) 

# code to use modelsummary() function to format regression output
ecm_summary <- list(
  "ECM" = lm(diff.xts(lnbio, na.pad = T) ~ 
               lag.xts(diff.xts(lnbio, na.pad = T)) + 
               lag.xts(lag.xts(diff.xts(lnbio, na.pad = T))) + 
               lag.xts(lag.xts(lag.xts(diff.xts(lnbio, na.pad = T)))) + 
               lag.xts(lag.xts(lag.xts(lag.xts(diff.xts(lnbio, na.pad = T))))) +
               diff.xts(lnsoy, na.pad = T) + 
               lag.xts(diff.xts(lnsoy, na.pad = T)) + 
               lag.xts(lnresid_ts), data = soydieselr))

modelsummary(ecm_summary, estimate = "{estimate}{stars}")

######## ERROR CORRECTION MODEL: ETF

# Load data
data_etf <- read_csv(here("data", "stock_renewables.csv"))
head(data_etf)

# Clean data
data_etf <- data_etf %>%
  mutate(date = dmy(date),
         lnicln = log(p_icln),
         lnpbd = log(p_pbd))  %>% 
  filter(year(date) < 2022)

head(data_etf)

# Convert to xts
etf <- xts(data_etf[,c("lnicln", "lnpbd")], order.by = data_etf$date)
head(etf)
plot(etf)

# ADF Tests - Levels
VARselect(etf$lnicln, lag.max = 5)$select
summary(ur.df(etf$lnicln, type = c("trend"), lags = 1))
summary(ur.df(etf$lnicln, type = c("drift"), lags = 1))
summary(ur.df(etf$lnicln, type = c("none"), lags = 1))

VARselect(etf$lnpbd, lag.max = 5)$select
summary(ur.df(etf$lnpbd, type = c("trend"), lags = 1))
summary(ur.df(etf$lnpbd, type = c("drift"), lags = 1))
summary(ur.df(etf$lnpbd, type = c("none"), lags = 1))

# ADF Tests - first difference
VARselect(diff.xts(etf$lnicln, na.pad = F), lag.max = 5)$select
summary(ur.df(diff.xts(etf$lnicln, na.pad = F), type = c("trend"), lags = 2))

VARselect(diff.xts(etf$lnpbd, na.pad = F), lag.max = 5)$select
summary(ur.df(diff.xts(etf$lnpbd, na.pad = F), type = c("trend"), lags = 1))

# Engle-Granger Test
# Step 1 - Run regression
reg_etf <- lm(lnicln ~ lnpbd, data = etf)

# code to use modelsummary() function to format regression output
models <- list(
  "ICLN-PBD" = lm(lnicln ~ lnpbd, data = etf)
)
modelsummary(models, estimate = "{estimate}{stars}")

# Step 2 - Save residuals, convert to xts, and do ADF test on residuals
resid_etf <- resid(reg_etf)
resid_etf_ts <- xts(resid_etf, order.by = index(etf))

etfr <- merge.xts(etf, resid_etf_ts)
head(etfr)

VARselect(etfr$resid_etf_ts)$select
summary(ur.df(etfr$resid_etf_ts, type = c("none"), lags = 1))@teststat
englegranger(2,0,156)

# Estimating ECM of ETFs - 1 lag each
ecm_etf <- lm(diff.xts(lnicln, na.pad = T) ~ 
            lag.xts(diff.xts(lnicln, na.pad = T)) + 
            diff.xts(lnpbd, na.pad = T) + 
            lag.xts(diff.xts(lnpbd, na.pad = T)) + 
            lag.xts(resid_etf_ts), data = etfr)
summary(ecm_etf)
