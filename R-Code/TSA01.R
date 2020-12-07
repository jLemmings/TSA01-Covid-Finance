# Loading required package
#install.packages("quantmod")
library(quantmod)
library(tseries)

# Downloading Prices via Yahoo Finance API
finance_data <- NULL
tickers_index <-
  c("FTSEMIB.MI", "^DJI", "DB1.DE", "^SSMI", "000001.SS")

for (Ticker in tickers_index) {
  finance_data <- cbind(
    finance_data,
    getSymbols.yahoo(
      Ticker,
      from = "2020-01-22",
      periodicity = "daily",
      auto.assign = FALSE
    )[, 6]
  )
}

# GET COVID DATA
library(jsonlite)
require(dplyr)

drops <- c("CountryCode","Province", "City", "CityCode", "Lat", "Lon")

swissCovid <-
  fromJSON("https://api.covid19api.com/total/country/switzerland/status/confirmed")
swissCovid <- swissCovid %>%
  mutate("New_Cases" = Cases - lag(Cases, default = Cases[1]))
swissCovid <- swissCovid[ , !(names(swissCovid) %in% drops)]

italyCovid <-
  fromJSON("https://api.covid19api.com/total/country/italy/status/confirmed")
italyCovid <- italyCovid %>%
  mutate("New_Cases" = Cases - lag(Cases, default = Cases[1]))
italyCovid <- italyCovid[ , !(names(italyCovid) %in% drops)]

germanyCovid <-
  fromJSON("https://api.covid19api.com/total/country/germany/status/confirmed")
germanyCovid <- germanyCovid %>%
  mutate("New_Cases" = Cases - lag(Cases, default = Cases[1]))
germanyCovid <- germanyCovid[ , !(names(germanyCovid) %in% drops)]

chinaCovid <-
  fromJSON("https://api.covid19api.com/total/country/china/status/confirmed")
chinaCovid <- chinaCovid %>%
  mutate("New_Cases" = Cases - lag(Cases, default = Cases[1]))
chinaCovid[1, "New_Cases"] <- 548
chinaCovid <- chinaCovid[ , !(names(chinaCovid) %in% drops)]

usaCovid <-
  fromJSON("https://api.covid19api.com/total/country/united-states/status/confirmed")
usaCovid <- usaCovid %>%
  mutate("New_Cases" = Cases - lag(Cases, default = Cases[1]))
usaCovid[1, "New_Cases"] <- 1
usaCovid <- usaCovid[ , !(names(usaCovid) %in% drops)]

#Create Timeseries 
chinaCovid$Date = as.Date(chinaCovid$Date, format = "%Y-%m-%d")
italyCovid$Date = as.Date(italyCovid$Date, format = "%Y-%m-%d")
swissCovid$Date = as.Date(swissCovid$Date, format = "%Y-%m-%d")
germanyCovid$Date = as.Date(germanyCovid$Date, format = "%Y-%m-%d")
usaCovid$Date = as.Date(usaCovid$Date, format = "%Y-%m-%d")

dayOfYear = as.numeric(format(chinaCovid[1,4], "%j"))
chinaCovid =  ts(chinaCovid$Cases, start = c(2020, dayOfYear), frequency = 365)
italyCovid = ts(italyCovid$Cases, start = c(2020, dayOfYear), frequency = 365)
swissCovid = ts(swissCovid$Cases, start = c(2020, dayOfYear), frequency = 365)
germanyCovid = ts(germanyCovid$Cases, start = c(2020, dayOfYear), frequency = 365)
usaCovid = ts(usaCovid$Cases, start = c(2020, dayOfYear), frequency = 365)

# WHY ARE Finance Timeseries ONLY [1:227] long and covid data [1:316]
chinaFinance = ts(finance_data$X000001.SS.Adjusted, start = c(2020, dayOfYear), frequency = 365)
swissFinance = ts(finance_data$SSMI.Adjusted, start = c(2020, dayOfYear), frequency = 365)
germanyFinance = ts(finance_data$DB1.DE.Adjusted, start = c(2020, dayOfYear), frequency = 365)
italyFinance = ts(finance_data$FTSEMIB.MI.Adjusted, start = c(2020, dayOfYear), frequency = 365)
usaFinance = ts(finance_data$DJI.Adjusted, start = c(2020, dayOfYear), frequency = 365)

# Replaces NA with 0
chinaFinance[!is.finite(chinaFinance)] <- 0
italyFinance[!is.finite(italyFinance)] <- 0
swissFinance[!is.finite(swissFinance)] <- 0
germanyFinance[!is.finite(germanyFinance)] <- 0
usaFinance[!is.finite(usaFinance)] <- 0

# Test if Timeseries are stationary
adf.test(chinaCovid)  # p-value = 0.01 -> stationary
adf.test(italyCovid) # p-value = 0.01 -> stationary
adf.test(swissCovid) # p-value = 0.99 -> non stationary
adf.test(germanyCovid) # p-value = 0.24 -> non stationary
adf.test(usaCovid) # p-value = 0.99 -> non stationary

swissCovid_return <- diff(log(swissCovid))
swissCovid_return[!is.finite(swissCovid_return)] <- 0
adf.test(swissCovid_return) # p-value = 0.03

germanyCovid_return <- diff(log(germanyCovid))
germanyCovid_return[!is.finite(germanyCovid_return)] <- 0
adf.test(germanyCovid_return) # p-value = 0.02

usaCovid_return <- diff(log(usaCovid))
usaCovid_return[!is.finite(usaCovid_return)] <- 0
adf.test(usaCovid_return) # p-value = 0.15

usaCovid_return2 <- diff(log(usaCovid_return))
usaCovid_return2[!is.finite(usaCovid_return2)] <- 0
adf.test(usaCovid_return2) # p-value = 0.01

# Remove the first 2 elements of the timeseries usacovid has been shortened by 2 elements
swissCovid = swissCovid_return[-1]
chinaCovid = chinaCovid[-1]
chinaCovid = chinaCovid[-1]
italyCovid = italyCovid[-1]
italyCovid = italyCovid[-1]
germanyCovid = germanyCovid_return[2:317]
usaCovid = usaCovid_return2



adf.test(chinaFinance) # p-value = 0.01
adf.test(italyFinance) # p-value = 0.39
adf.test(swissFinance) # p-value = 0.045
adf.test(germanyFinance) # p-value = 0.042
adf.test(usaFinance) # p-value = 0.01




## TODO:
#  - Josh: Time Series, decompose, returns  
#  - Anna: stationarity, stationarity series


