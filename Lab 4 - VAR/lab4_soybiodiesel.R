# Load Packages
pacman::p_load(here, readxl, dplyr, xts, lubridate, urca, forecast, vars)

# Load data
data <- read_excel(here("Data", "data_lab4.xls"), sheet = "prices")
index <- read_excel(here("Data", "data_lab4.xls"), sheet = "index")

head(data)
head(index)

# In lab 2, we showed that biodiesel and soybean oil are NON-STATIONARY
# To do a VAR model, we must have stationary data 
# One way to make data stationary is to take the first difference
# Another way is to deflate the data by a certain index - which we will do today

# Clean data
data <- data %>% 
  mutate(date = as.Date(date, format = "%m/%d/%Y"),
         month = month(date),
         year = year(date))

dataindex <- merge(data, index, by = c("year", "month"))

# Deflate prices
dataindex <- dataindex %>%
  mutate(biodieselr = biodiesel / index_energy * 100,
         soyoilr = soyoil / index_ag * 100,
         lnbiodieselr = log(biodieselr),
         lnsoyoilr = log(soyoilr))

real <- xts(dataindex[,c("lnbiodieselr", "lnsoyoilr")], order.by = dataindex$date)
head(real)

VARselect(real$lnbiodieselr, lag.max = 12)$select # lags = 10
VARselect(real$lnsoyoilr, lag.max = 12)$select # lags = 8

summary(ur.df(real$lnbiodieselr, lags = 10, type = c("trend"))) # reject null of a unit root -> stationary
summary(ur.df(real$lnsoyoilr, lags = 8, type = c("trend"))) # fail to reject null of a unit root
summary(ur.df(real$lnsoyoilr, lags = 8, type = c("drift"))) # reject null of a unit root -> stationary

# Test for cointegration
VARselect(real, lag.max = 12)$select # lags = 10

# Estimate VAR model 
model1 <- VAR(real, p = 10, type = c("const"))
model1

plot(irf(model1))

irf_bb <- irf(model1, impulse = c("lnbiodieselr"), response = c("lnbiodieselr"), n.ahead = 10)
irf_bs <- irf(model1, impulse = c("lnsoyoilr"), response = c("lnbiodieselr"), n.ahead = 10)
irf_ss <- irf(model1, impulse = c("lnsoyoilr"), response = c("lnsoyoilr"), n.ahead = 10)
irf_sb <- irf(model1, impulse = c("lnbiodieselr"), response = c("lnsoyoilr"), n.ahead = 10)

plot(irf_bb)
plot(irf_bs)
plot(irf_ss)
plot(irf_sb)

