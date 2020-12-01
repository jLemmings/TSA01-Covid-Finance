# Loading required package
#install.packages("quantmod")
library(quantmod)

# Downloading Prices via Yahoo Finance API
finance_data <- NULL
tickers_index <-
  c("FTSEMIB.MI", "^DJI", "DB1.DE", "^SSMI", "000001.SS")

for (Ticker in tickers_index) {
  finance_data <- cbind(
    finance_data,
    getSymbols.yahoo(
      Ticker,
      from = "2018-01-01",
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
  mutate("New Cases" = Cases - lag(Cases, default = Cases[1]))
swissCovid <- swissCovid[ , !(names(swissCovid) %in% drops)]

italyCovid <-
  fromJSON("https://api.covid19api.com/total/country/italy/status/confirmed")
italyCovid <- italyCovid %>%
  mutate("New Cases" = Cases - lag(Cases, default = Cases[1]))
italyCovid <- italyCovid[ , !(names(italyCovid) %in% drops)]

germanyCovid <-
  fromJSON("https://api.covid19api.com/total/country/germany/status/confirmed")
germanyCovid <- germanyCovid %>%
  mutate("New Cases" = Cases - lag(Cases, default = Cases[1]))
germanyCovid <- germanyCovid[ , !(names(germanyCovid) %in% drops)]

chinaCovid <-
  fromJSON("https://api.covid19api.com/total/country/china/status/confirmed")
chinaCovid <- chinaCovid %>%
  mutate("New Cases" = Cases - lag(Cases, default = Cases[1]))
chinaCovid[1, "New Cases"] <- 548
chinaCovid <- chinaCovid[ , !(names(chinaCovid) %in% drops)]

usaCovid <-
  fromJSON("https://api.covid19api.com/total/country/united-states/status/confirmed")
usaCovid <- usaCovid %>%
  mutate("New Cases" = Cases - lag(Cases, default = Cases[1]))
usaCovid[1, "New Cases"] <- 1
usaCovid <- usaCovid[ , !(names(usaCovid) %in% drops)]


