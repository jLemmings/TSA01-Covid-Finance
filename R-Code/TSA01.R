# Loading required package
library(quantmod)
library(tseries)
library(tidyverse)
library(jsonlite)
require(dplyr)
library(vars)
library(ggplot2)
countries <-
  c("Switzerland", "Germany", "Italy", "China", "United States")


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


chinaCovidCases =  ts(chinaCovidCSV$total_cases,
                      start = c(2020, 23),
                      frequency = 365)
italyCovidCases = ts(italyCovidCSV$total_cases,
                     start = c(2020, 23),
                     frequency = 365)
switzerlandCovidCases = ts(switzerlandCovidCSV$total_cases,
                           start = c(2020, 23),
                           frequency = 365)
germanyCovidCases = ts(germanyCovidCSV$total_cases,
                       start = c(2020, 23),
                       frequency = 365)
usaCovidCases = ts(usaCovidCSV$total_cases,
                   start = c(2020, 23),
                   frequency = 365)
italyCovidDeath = ts(italyCovidCSV$total_deaths,
                     start = c(2020, 23),
                     frequency = 365)
switzerlandCovidDeath = ts(
  switzerlandCovidCSV$total_deaths,
  start = c(2020, 23),
  frequency = 365
)
germanyCovidDeath = ts(germanyCovidCSV$total_deaths,
                       start = c(2020, 23),
                       frequency = 365)
usaCovidDeath = ts(usaCovidCSV$total_deaths,
                   start = c(2020, 23),
                   frequency = 365)
chinaCovidDeath = ts(chinaCovidCSV$total_deaths,
                     start = c(2020, 23),
                     frequency = 365)

switzerlandCovidCases[!is.finite(switzerlandCovidCases)] <- 0
germanyCovidCases[!is.finite(germanyCovidCases)] <- 0
italyCovidCases[!is.finite(italyCovidCases)] <- 0
chinaCovidCases[!is.finite(chinaCovidCases)] <- 0
usaCovidCases[!is.finite(usaCovidCases)] <- 0

switzerlandCovidDeath[!is.finite(switzerlandCovidDeath)] <- 0
germanyCovidDeath[!is.finite(germanyCovidDeath)] <- 0
italyCovidDeath[!is.finite(italyCovidDeath)] <- 0
chinaCovidDeath[!is.finite(chinaCovidDeath)] <- 0
usaCovidDeath[!is.finite(usaCovidDeath)] <- 0

# Error: time series has no or less than 2 periods -> changed time series frequency to 12 (monthly)
chinaCovidDecomposed <- decompose(chinaCovidCases)
italyCovidDecomposed <- decompose(italyCovidCases)
switzerlandCovidDecomposed <- decompose(switzerlandCovidCases)
germanyCovidDecomposed <- decompose(germanyCovidCases)
usaCovidDecomposed <- decompose(usaCovidCases)

plot(chinaCovidDecomposed)
plot(italyCovidDecomposed)
plot(switzerlandCovidDecomposed)
plot(germanyCovidDecomposed)
plot(usaCovidDecomposed)



# Test if Timeseries are stationary
adf.test(chinaCovidCases)  # p-value = 0.01 -> stationary
adf.test(italyCovidCases) # p-value = 0.01 -> stationary
adf.test(switzerlandCovidCases) # p-value = 0.99 -> non stationary
adf.test(germanyCovidCases) # p-value = 0.9649 -> non stationary
adf.test(usaCovidCases) # p-value = 0.99 -> non stationary

switzerlandCovid_return <- diff(switzerlandCovidCases)
adf.test(switzerlandCovid_return) # p-value = 0.9489

switzerlandCovid_return2 <- diff(switzerlandCovid_return)
adf.test(switzerlandCovid_return2) # p-value = 0.01

germanyCovid_return <- diff(germanyCovidCases)
adf.test(germanyCovid_return) # p-value = 0.99

germanyCovid_return2 <- diff(germanyCovid_return)
adf.test(germanyCovid_return2) # p-value = 0.01

usaCovid_return <- diff(usaCovidCases)
adf.test(usaCovid_return) # p-value = 0.99

usaCovid_return2 <- diff(usaCovid_return)
adf.test(usaCovid_return2) # p-value = 0.01

# Remove the first 2 elements of the timeseries usacovid has been shortened by 2 elements
switzerlandCovidCases = switzerlandCovid_return2
chinaCovidCases = chinaCovidCases[-1]
chinaCovidCases = chinaCovidCases[-1]
italyCovidCases = italyCovidCases[-1]
italyCovidCases = italyCovidCases[-1]
germanyCovidCases = germanyCovid_return2
usaCovidCases = usaCovid_return2


adf.test(chinaCovidDeath)  # p-value = 0.01191 -> stationary
adf.test(italyCovidDeath) # p-value = 0.01 -> stationary
adf.test(switzerlandCovidDeath) # p-value = 0.99 -> non stationary
adf.test(germanyCovidDeath) # p-value = 0.2751 -> non stationary
adf.test(usaCovidDeath) # p-value = 0.01 -> stationary

switzerlandCovid_diff <- diff(switzerlandCovidDeath)
adf.test(switzerlandCovid_diff) # p-value = 0.99

switzerlandCovid_diff2 <- diff(switzerlandCovid_diff)
adf.test(switzerlandCovid_diff2) # p-value = 0.01

germanyCovidDeath_diff <- diff(germanyCovidDeath)
adf.test(germanyCovidDeath_diff) # p-value = 0.99

germanyCovidDeath_diff2 <- diff(germanyCovidDeath_diff)
adf.test(germanyCovidDeath_diff2) # p-value = 0.01

switzerlandCovidDeath = switzerlandCovid_diff2
chinaCovidDeath = chinaCovidDeath[-1]
chinaCovidDeath = chinaCovidDeath[-1]
italyCovidDeath = italyCovidDeath[-1]
italyCovidDeath = italyCovidDeath[-1]
germanyCovidDeath = germanyCovidDeath_diff2
usaCovidDeath = usaCovidDeath[-1]
usaCovidDeath = usaCovidDeath[-1]


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
  start = c(2020, 23),
  frequency = 365
)
switzerlandFinance = ts(finance_data$SSMI.Adjusted,
                        start = c(2020, 23),
                        frequency = 365)
germanyFinance = ts(finance_data$DB1.DE.Adjusted,
                    start = c(2020, 23),
                    frequency = 365)
italyFinance = ts(
  finance_data$FTSEMIB.MI.Adjusted,
  start = c(2020, 23),
  frequency = 365
)
usaFinance = ts(finance_data$DJI.Adjusted,
                start = c(2020, 23),
                frequency = 365)

# Error: time series has no or less than 2 periods -> changed time series frequency to 12 (monthly)
chinaFinanceDecomposed <- decompose(chinaFinance)
switzerlandFinanceDecomposed <- decompose(switzerlandFinance)
germanyFinanceDecomposed <- decompose(germanyFinance)
italyFinanceDecomposed <- decompose(italyFinance)
usaFinanceDecomposed <- decompose(usaFinance)

### Plots to visually interpret finance data
# China
dates_vlineChina <- as.Date(c("2020-01-23"))
dates_vlineChina <- which(finance_data$date %in% dates_vlineChina)
chinaFinancePlot <-
  ggplot(finance_data, aes(date, X000001.SS.Adjusted)) +
  geom_line() +
  geom_vline(
    xintercept = as.numeric(finance_data$date[dates_vlineChina]),
    linetype = 1,
    col = "red"
  ) + ggtitle("China stock market and first confirmed case") + xlab("Date") + ylab("Stock Market")

# Switzerland
dates_vlineSwitzerland <- as.Date(c("2020-02-25"))
dates_vlineSwitzerland <-
  which(finance_data$date %in% dates_vlineSwitzerland)
switzerlandFinancePlot <-
  ggplot(finance_data, aes(date, SSMI.Adjusted)) +
  geom_line() +
  geom_vline(
    xintercept = as.numeric(finance_data$date[dates_vlineSwitzerland]),
    linetype = 1,
    col = "red"
  ) + ggtitle("Switzerland stock market and first confirmed case") + xlab("Date") + ylab("Stock Market")


# Germany
dates_vlineGermany <- as.Date(c("2020-01-27"))
dates_vlineGermany <-
  which(finance_data$date %in% dates_vlineGermany)
germanyFinancePlot <-
  ggplot(finance_data, aes(date, DB1.DE.Adjusted)) +
  geom_line() +
  geom_vline(
    xintercept = as.numeric(finance_data$date[dates_vlineGermany]),
    linetype = 1,
    col = "red"
  ) + ggtitle("Germany stock market and first confirmed case") + xlab("Date") + ylab("Stock Market")

# Italy
dates_vlineItaly <- as.Date(c("2020-01-31"))
dates_vlineItaly <- which(finance_data$date %in% dates_vlineItaly)
italyFinancePlot <-
  ggplot(finance_data, aes(date, FTSEMIB.MI.Adjusted)) +
  geom_line() +
  geom_vline(
    xintercept = as.numeric(finance_data$date[dates_vlineItaly]),
    linetype = 1,
    col = "red"
  ) + ggtitle("Italy stock market and first confirmed case") + xlab("Date") + ylab("Stock Market")

# USA
dates_vlineUSA <- as.Date(c("2020-01-27"))
dates_vlineUSA <- which(finance_data$date %in% dates_vlineUSA)
usaFinancePlot <-
  ggplot(finance_data, aes(date, DJI.Adjusted)) +
  geom_line() +
  geom_vline(
    xintercept = as.numeric(finance_data$date[dates_vlineUSA]),
    linetype = 1,
    col = "red"
  ) + ggtitle("United States stock market and first confirmed case") + xlab("Date") + ylab("Stock Market")

chinaFinancePlot
switzerlandFinancePlot
germanyFinancePlot
italyFinancePlot
usaFinancePlot

# Export plots for documentation
png(filename = "plots/chinaFinance.png")
plot(chinaFinancePlot)
dev.off()

png(filename = "plots/switzerlandFinance.png")
plot(switzerlandFinancePlot)
dev.off()

png(filename = "plots/chinaFinance.png")
plot(chinaFinancePlot)
dev.off()

png(filename = "plots/chinaFinance.png")
plot(chinaFinancePlot)
dev.off()

png(filename = "plots/chinaFinance.png")
plot(chinaFinancePlot)
dev.off()

#Check if timeseries are stationary
adf.test(chinaFinance) # p-value = 0.3271
adf.test(italyFinance) # p-value = 0.409
adf.test(switzerlandFinance) # p-value = 0.2596
adf.test(germanyFinance) # p-value = 0.5447
adf.test(usaFinance) # p-value = 0.3754

adf.test(diff(log(chinaFinance))) # p-value = 0.01
adf.test(diff(log(italyFinance))) # p-value = 0.01
adf.test(diff(log(switzerlandFinance))) # p-value = 0.01
adf.test(diff(log(germanyFinance))) # p-value = 0.01
adf.test(diff(log(usaFinance)))# p-value = 0.01


chinaFinance = diff(log(chinaFinance))[-1]
italyFinance = diff(log(italyFinance))[-1]
switzerlandFinance = diff(log(switzerlandFinance))[-1]
germanyFinance = diff(log(germanyFinance))[-1]
usaFinance = diff(log(usaFinance))[-1]


###VAR Model and Causality

china_var <-
  VAR(
    cbind (chinaFinance, chinaCovidCases, chinaCovidDeath),
    type = "const",
    ic = "AIC"
  )
summary(china_var)
causality(china_var, cause = "chinaCovidCases")
causality(china_var, cause = "chinaCovidDeath")
causality(china_var, cause = "chinaFinance")

italy_var <-
  VAR(
    cbind (italyFinance, italyCovidCases, italyCovidDeath),
    type = "const",
    ic = "AIC"
  )
summary(italy_var)
causality(italy_var, cause = "italyCovidCases")
causality(italy_var, cause = "italyCovidDeath")
causality(italy_var, cause = "italyFinance")

switzerland_var <-
  VAR(
    cbind (
      switzerlandFinance,
      switzerlandCovidCases,
      switzerlandCovidDeath
    ),
    type = "const",
    ic = "AIC"
  )
summary(switzerland_var)
causality(switzerland_var, cause = "switzerlandCovidCases")
causality(switzerland_var, cause = "switzerlandCovidDeath")
causality(switzerland_var, cause = "switzerlandFinance")

germany_var <-
  VAR(
    cbind (germanyFinance, germanyCovidCases, germanyCovidDeath),
    type = "const",
    ic = "AIC"
  )
summary(germany_var)
causality(germany_var, cause = "germanyCovidCases")
causality(germany_var, cause = "germanyCovidDeath")
causality(germany_var, cause = "germanyFinance")

usa_var <-
  VAR(cbind (usaFinance, usaCovidCases, usaCovidDeath),
      type = "const",
      ic = "AIC")
summary(usa_var)
causality(usa_var, cause = "usaCovidCases")
causality(usa_var, cause = "usaCovidDeath")
causality(usa_var, cause = "usaFinance")
