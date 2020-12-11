# Loading required package
#install.packages("quantmod")
library(quantmod)
library(tseries)
library(tidyverse)
library(jsonlite)
require(dplyr)

countries <-
  c("Switzerland", "Germany", "Italy", "China", "United States")

### Covid Data from API
drops <-
  c("CountryCode", "Province", "City", "CityCode", "Lat", "Lon")

switzerlandCovid <-
  fromJSON("https://api.covid19api.com/total/country/switzerland/status/confirmed")
switzerlandCovid <- switzerlandCovid %>%
  mutate("New_Cases" = Cases - lag(Cases, default = Cases[1]))
switzerlandCovid <- switzerlandCovid[,!(names(switzerlandCovid) %in% drops)]

italyCovid <-
  fromJSON("https://api.covid19api.com/total/country/italy/status/confirmed")
italyCovid <- italyCovid %>%
  mutate("New_Cases" = Cases - lag(Cases, default = Cases[1]))
italyCovid <- italyCovid[,!(names(italyCovid) %in% drops)]

germanyCovid <-
  fromJSON("https://api.covid19api.com/total/country/germany/status/confirmed")
germanyCovid <- germanyCovid %>%
  mutate("New_Cases" = Cases - lag(Cases, default = Cases[1]))
germanyCovid <- germanyCovid[,!(names(germanyCovid) %in% drops)]

chinaCovid <-
  fromJSON("https://api.covid19api.com/total/country/china/status/confirmed")
chinaCovid <- chinaCovid %>%
  mutate("New_Cases" = Cases - lag(Cases, default = Cases[1]))
chinaCovid[1, "New_Cases"] <- 548
chinaCovid <- chinaCovid[,!(names(chinaCovid) %in% drops)]

usaCovid <-
  fromJSON("https://api.covid19api.com/total/country/united-states/status/confirmed")
usaCovid <- usaCovid %>%
  mutate("New_Cases" = Cases - lag(Cases, default = Cases[1]))
usaCovid[1, "New_Cases"] <- 1
usaCovid <- usaCovid[,!(names(usaCovid) %in% drops)]

#Create Timeseries
chinaCovid$Date = as.Date(chinaCovid$Date, format = "%Y-%m-%d")
italyCovid$Date = as.Date(italyCovid$Date, format = "%Y-%m-%d")
switzerlandCovid$Date = as.Date(switzerlandCovid$Date, format = "%Y-%m-%d")
germanyCovid$Date = as.Date(germanyCovid$Date, format = "%Y-%m-%d")
usaCovid$Date = as.Date(usaCovid$Date, format = "%Y-%m-%d")

dayOfYear = as.numeric(format(chinaCovid[1, 4], "%j"))
chinaCovid =  ts(chinaCovid$Cases,
                 start = c(2020, dayOfYear),
                 frequency = 365)
italyCovid = ts(italyCovid$Cases,
                start = c(2020, dayOfYear),
                frequency = 365)
switzerlandCovid = ts(switzerlandCovid$Cases,
                start = c(2020, dayOfYear),
                frequency = 365)
germanyCovid = ts(germanyCovid$Cases,
                  start = c(2020, dayOfYear),
                  frequency = 365)
usaCovid = ts(usaCovid$Cases,
              start = c(2020, dayOfYear),
              frequency = 365)

# Error: time series has no or less than 2 periods -> changed time series frequency to 12 (monthly)
chinaCovidDecomposed <- decompose(chinaCovid)
italyCovidDecomposed <- decompose(italyCovid)
switzerlandCovidDecomposed <- decompose(switzerlandCovid)
germanyCovidDecomposed <- decompose(germanyCovid)
usaCovidDecomposed <- decompose(usaCovid)

plot(chinaCovidDecomposed)
plot(italyCovidDecomposed)
plot(switzerlandCovidDecomposed)
plot(germanyCovidDecomposed)
plot(usaCovidDecomposed)

### Covid Data from CSV
covid_data <- read_csv("cowid-covid-data.csv")
colToKeep <-
  c(
    "location",
    "date",
    "total_cases",
    "new_cases",
    "total_deaths",
    "new_deaths",
    "new_cases_per_million",
    "new_deaths_per_million"
  )

covid_data = subset(covid_data, select = colToKeep)
covid_data_filtered <-
  covid_data[covid_data$location %in% countries, ]

switzerlandCovidCSV <-
  covid_data_filtered[covid_data_filtered$location == "Switzerland", ]
germanyCovidCSV <-
  covid_data_filtered[covid_data_filtered$location == "Germany", ]
italyCovidCSV <-
  covid_data_filtered[covid_data_filtered$location == "Italy", ]
chinaCovidCSV <-
  covid_data_filtered[covid_data_filtered$location == "China", ]
usaCovidCSV <-
  covid_data_filtered[covid_data_filtered$location == "United States", ]

# For some reason China and USA have recorded data one day before the others
chinaCovidCSV = chinaCovidCSV[-1, ]
usaCovidCSV = usaCovidCSV[-1, ]



#Create Timeseries
chinaCovidCSV$date = as.Date(chinaCovidCSV$date, format = "%Y-%m-%d")
italyCovidCSV$date = as.Date(italyCovidCSV$date, format = "%Y-%m-%d")
switzerlandCovidCSV$date = as.Date(switzerlandCovidCSV$date, format = "%Y-%m-%d")
germanyCovidCSV$date = as.Date(germanyCovidCSV$date, format = "%Y-%m-%d")
usaCovidCSV$date = as.Date(usaCovidCSV$date, format = "%Y-%m-%d")

dayOfYear = as.numeric(format(chinaCovidCSV[1, 4], "%j"))
chinaCovidCSVCases =  ts(chinaCovidCSV$total_cases,
                         start = c(2020, chinaCovidCSV$date),
                         frequency = 365)
italyCovidCSVCases = ts(
  italyCovidCSV$total_cases,
  start = c(2020, italyCovidCSV$date),
  frequency = 365
)
switzerlandCovidCSVCases = ts(
  switzerlandCovidCSV$total_cases,
  start = c(2020, switzerlandCovidCSV$date),
  frequency = 365
)
germanyCovidCSVCases = ts(
  germanyCovidCSV$total_cases,
  start = c(2020, germanyCovidCSV$date),
  frequency = 365
)
usaCovidCSVCases = ts(usaCovidCSV$total_cases,
                      start = c(2020, usaCovidCSV$date),
                      frequency = 365)
italyCovidCSVDeath = ts(
  italyCovidCSV$total_deaths,
  start = c(2020, italyCovidCSV$date),
  frequency = 365
)
switzerlandCovidCSVDeath = ts(
  switzerlandCovidCSV$total_deaths,
  start = c(2020, switzerlandCovidCSV$date),
  frequency = 365
)
germanyCovidCSVDeath = ts(
  germanyCovidCSV$total_deaths,
  start = c(2020, germanyCovidCSV$date),
  frequency = 365
)
usaCovidCSVDeath = ts(usaCovidCSV$total_deaths,
                      start = c(2020, usaCovidCSV$date),
                      frequency = 365)

switzerlandCovidCSVCases[!is.finite(switzerlandCovidCSVCases)] <- 0
germanyCovidCSVCases[!is.finite(germanyCovidCSVCases)] <- 0
italyCovidCSVCases[!is.finite(italyCovidCSVCases)] <- 0
chinaCovidCSVCases[!is.finite(chinaCovidCSVCases)] <- 0
usaCovidCSVCases[!is.finite(usaCovidCSVCases)] <- 0

switzerlandCovidCSVDeath[!is.finite(switzerlandCovidCSVDeath)] <- 0
germanyCovidCSVDeath[!is.finite(germanyCovidCSVDeath)] <- 0
italyCovidCSVDeath[!is.finite(italyCovidCSVDeath)] <- 0
chinaCovidCSVDeath[!is.finite(chinaCovidCSVDeath)] <- 0
usaCovidCSVDeath[!is.finite(usaCovidCSVDeath)] <- 0

# Error: time series has no or less than 2 periods -> changed time series frequency to 12 (monthly)
chinaCovidCSVDecomposed <- decompose(chinaCovidCSVCases)
italyCovidCSVDecomposed <- decompose(italyCovidCSVCases)
switzerlandCovidCSVDecomposed <- decompose(switzerlandCovidCSVCases)
germanyCovidCSVDecomposed <- decompose(germanyCovidCSVCases)
usaCovidCSVDecomposed <- decompose(usaCovidCSVCases)

plot(chinaCovidCSVDecomposed)
plot(italyCovidCSVDecomposed)
plot(switzerlandCovidCSVDecomposed)
plot(germanyCovidCSVDecomposed)
plot(usaCovidCSVDecomposed)


### FINANCE PART
# Downloading Prices via Yahoo Finance API
finance_data <- NULL
tickers_index <-
  c("FTSEMIB.MI", "^DJI", "DB1.DE", "^SSMI", "000001.SS")

for (Ticker in tickers_index) {
  finance_data <- cbind(
    finance_data,
    getSymbols.yahoo(
      Ticker,
      from = "2020-01-23",
      to = "2020-12-06",
      periodicity = "daily",
      auto.assign = FALSE
    )[, 6]
  )
}

date <-
  seq(as.Date("2020-01-23"), as.Date("2020-12-06"), by = "days")
date_sequence <- data.frame(date)

finance_data <-
  data.frame(date = index(finance_data), coredata(finance_data))

# Merge finance date on date sequence
finance_data <-
  merge(
    date_sequence,
    finance_data,
    by.x = "date",
    by.y = "date",
    all.x = TRUE
  )

# Replace each NA value with the value from the day before
for (ticker in 1:ncol(finance_data)) {
  for (row in 1:nrow(finance_data)) {
    if (is.na(finance_data[row, ticker])) {
      finance_data[row, ticker] <- finance_data[row - 1, ticker]
    }
  }
}


# WHY ARE Finance Timeseries ONLY [1:227] long and covid data [1:316]
chinaFinance = ts(
  finance_data$X000001.SS.Adjusted,
  start = c(2020, dayOfYear),
  frequency = 365
)
switzerlandFinance = ts(
  finance_data$SSMI.Adjusted,
  start = c(2020, dayOfYear),
  frequency = 365
)
germanyFinance = ts(
  finance_data$DB1.DE.Adjusted,
  start = c(2020, dayOfYear),
  frequency = 365
)
italyFinance = ts(
  finance_data$FTSEMIB.MI.Adjusted,
  start = c(2020, dayOfYear),
  frequency = 365
)
usaFinance = ts(
  finance_data$DJI.Adjusted,
  start = c(2020, dayOfYear),
  frequency = 365
)

# Error: time series has no or less than 2 periods -> changed time series frequency to 12 (monthly)
chinaFinanceDecomposed <- decompose(chinaFinance)
switzerlandFinanceDecomposed <- decompose(switzerlandFinance)
germanyFinanceDecomposed <- decompose(germanyFinance)
italyFinanceDecomposed <- decompose(italyFinance)
usaFinanceDecomposed <- decompose(usaFinance)


# Test if Timeseries are stationary
adf.test(chinaCovidCSVCases)  # p-value = 0.01 -> stationary
adf.test(italyCovidCSVCases) # p-value = 0.01 -> stationary
adf.test(switzerlandCovidCSVCases) # p-value = 0.99 -> non stationary
adf.test(germanyCovidCSVCases) # p-value = 0.9649 -> non stationary
adf.test(usaCovidCSVCases) # p-value = 0.99 -> non stationary

switzerlandCovid_return <- diff(switzerlandCovid)
adf.test(switzerlandCovid_return) # p-value = 0.9533

switzerlandCovid_return2 <- diff(switzerlandCovid_return)
adf.test(switzerlandCovid_return2) # p-value = 0.01

germanyCovid_return <- diff(germanyCovid)
adf.test(germanyCovid_return) # p-value = 0.99

germanyCovid_return2 <- diff(germanyCovid_return)
adf.test(germanyCovid_return2) # p-value = 0.01

usaCovid_return <- diff(usaCovid)
adf.test(usaCovid_return) # p-value = 0.99

usaCovid_return2 <- diff(usaCovid_return)
adf.test(usaCovid_return2) # p-value = 0.01

# Remove the first 2 elements of the timeseries usacovid has been shortened by 2 elements
switzerlandCovid = switzerlandCovid_return2
chinaCovid = chinaCovid[-1]
chinaCovid = chinaCovid[-1]
italyCovid = italyCovid[-1]
italyCovid = italyCovid[-1]
germanyCovid = germanyCovid_return2
usaCovid = usaCovid_return2

adf.test(chinaFinance) # p-value = 0.01
adf.test(italyFinance) # p-value = 0.39
adf.test(switzerlandFinance) # p-value = 0.045
adf.test(germanyFinance) # p-value = 0.042
adf.test(usaFinance) # p-value = 0.01




## TODO:
#  - Josh: Time Series, decompose, returns
#  - Anna: stationarity, stationarity series
