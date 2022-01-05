pacman::p_load(tidyverse, lubridate, stringr, Quandl, urca, xts, forecast)

# sign up for an account at Quandl
# daily from 2012-2018
# May 13, 2012 - June 10, 2018

# https://docs.data.nasdaq.com/docs/r-time-series

soybeans <- Quandl("CHRIS/ICE_IBO2", api_key="4bwZJ_U8Nz7sPPrUH2Av", collapse = "weekly")

#1994 weekly 
diesel <- Quandl("EIA/PET_EMD_EPD2D_PTE_NUS_DPG_W", start_date = "2012-05-13", end_date = "2018-06-10")

# xts can do everything {zoo} does https://rc2e.com/timeseriesanalysis
soybeans$testdate <- soybeans$Date + 1 

ts_soy <- xts(soybeans$Settle, order.by = soybeans$testdate)
ts_diesel <- xts(diesel$Value, order.by = diesel$Date)

soydiesel <- merge.xts(ts_soy, ts_diesel)
soydiesel <- soydiesel["2012-05-14/2018-06-04"]
colnames(soydiesel) <- c("p_soy", "p_diesel")

# Testing for Stationarity
df_soy <- ur.df(soydiesel$p_soy, type = c("trend"), lags = 0)
summary(df_soy)

df_diesel <- ur.df(soydiesel$p_diesel, type = c("trend"), lags = 0)
summary(df_diesel)

# Testing for Cointegration
df_soy_diff <- ur.df(diff.xts(soydiesel$p_soy, na.pad = F), type = c("trend"), lags = 0)
# first week (May 14, 2018) will be NA so add na.pad = F to drop it off. Or else the ur.df function will not work

df_diesel_diff <- ur.df(diff.xts(soydiesel$p_diesel, na.pad = F), type = c("trend"), lags = 0)

summary(df_soy_diff)
summary(df_diesel_diff)

# Step 1 Engle Granger Test
reg_nosubsidy <- lm(p_soy ~ p_diesel, data = soydiesel)
summary(reg_nosubsidy)

soydiesel$subsidy <- ifelse(index(soydiesel) >= "2018-01-01", 1, 0)

reg_subsidy <- lm(p_soy ~ p_diesel + subsidy, data = soydiesel)
summary(reg_subsidy)

# Step 2 Engle Granger Test

resid_nosubsidy <- residuals(reg_nosubsidy)
resid_subsidy <- residuals(reg_subsidy)

df_resid_nosubsidy <- ur.df(resid_nosubsidy, type = c("none"), lags = 0)
df_resid_subsidy <- ur.df(resid_subsidy, type = c("none"), lags = 0)

summary(df_resid_nosubsidy)
summary(df_resid_subsidy)

# ADF test on residuals
Acf(resid_nosubsidy)
Pacf(resid_nosubsidy)

Acf(resid_subsidy)
Pacf(resid_nosubsidy)

adf_resid_nosubsidy <- ur.df(resid_nosubsidy, type = c("none"), lags = 1, selectlags = c("BIC"))
summary(adf_resid_nosubsidy)

adf_resid_subsidy <- ur.df(resid_subsidy, type = c("none"), lags = 1, selectlags = c("BIC"))
summary(adf_resid_subsidy)
