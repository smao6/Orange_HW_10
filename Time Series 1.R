#Class 1
#------------------------------------#
#    Introduction to Time Series     #
#      & Time Series Structure       #
#                                    #
#           Dr Susan Simmons           #
#------------------------------------#

# Needed Libraries for Analysis #
install.packages('forecast',dependencies = T)
install.packages('tseries')
install.packages(c('expsmooth','lmtest','zoo','seasonal', 'haven'))
library(forecast)
library(haven)
library(fma)
library(expsmooth)
library(lmtest)
library(zoo)
library(seasonal)

# Saving File Locations and Uploading SAS File #
file.dir <- "/Users/Garrett/Desktop/MSA Fall/Fall 1/Time Series/Data/"
input.file1 <- "usairlines.sas7bdat"
input.file2 <- "ar2.sas7bdat"

USAirlines <- read_sas(paste(file.dir, input.file1,sep = ""))
AR2 <- read_sas(paste(file.dir, input.file2, sep = ""))

# Creation of Time Series Data Object #
#Using 'frequency=' option determines seasonality. If you are not sure if you have seasonality, put frequency=1#
#If you do not include a seasonality, then it will not do a times series decomposition#
Passenger <- ts(USAirlines$Passengers, start = 1990, frequency =12)

# Time Series Decomposition ...STL#
#Want s.window to be odd and not less than 7#
decomp_stl <- stl(Passenger, s.window = 7)
plot(decomp_stl)

plot(Passenger, col = "grey", main = "US Airline Passengers - Trend/Cycle", xlab = "", ylab = "Number of Passengers (Thousands)", lwd = 2)
lines(decomp_stl$time.series[,2], col = "red", lwd = 2)

seas_pass=Passenger-decomp_stl$time.series[,1]
plot(Passenger, col = "grey", main = "US Airline Passengers - Seasonally Adjusted", xlab = "", ylab = "Number of Passengers (Thousands)", lwd = 2)
lines(seas_pass, col = "red", lwd = 2)

monthplot(decomp_stl$time.series[,"seasonal"], main = "US Airline Passengers - Monthly Effects", ylab = "Seasonal Sub Series", xlab = "Seasons (Months)", lwd = 2)


#####For fun....X13
decomp_x13=seas(Passenger)
summary(decomp_x13)
install.packages('seasonalview')
library(seasonalview)
view(decomp_x13)

#Class 2
#------------------------------------#
#        Exponential Smoothing       #
#               Models               #
#                                    #
#           Dr Susan Simmons         #
#------------------------------------#

# Needed Libraries for Analysis #
library(haven)
library(forecast)
library(fma)
library(tseries)
library(expsmooth)
library(lmtest)
library(zoo)

# Saving File Locations and Uploading SAS File #
file.dir <- "/Users/Garrett/Desktop/MSA Fall/Fall 1/Time Series/Data/"
input.file1 <- "usairlines.sas7bdat"
input.file2 <- "steel.sas7bdat"

USAirlines <- read_sas(paste(file.dir, input.file1, sep = ""))
Steel <- read_sas(paste(file.dir, input.file2, sep = ""))

# Creating Time Series Data Objects #
Passenger <- ts(USAirlines$Passengers, start = 1990, frequency = 12)

SteelShp <- ts(Steel$steelshp, start = 1984, frequency = 12)

# Building a Single Exponential Smoothing Model - Steel Data #
SES.Steel <- ses(SteelShp, initial = "optimal", h = 24) #This tells to predict out 24 observations
summary(SES.Steel)
#Know that you may get slightly different predicted values in SAS and R#

plot(SES.Steel, main = "US Steel Shipments with Simple ESM Forecast", xlab = "Date", ylab = "Shipments (Thousands of Net Tons)")
abline(v = 1992, col = "red", lty = "dashed")
round(accuracy(SES.Steel),2)
library(ggplot2)
autoplot(SES.Steel)+
  autolayer(fitted(SES.Steel),series="Fitted")+ylab("US Steel Shipments with Simple ESM Forecast")

# Building a Linear Exponential Smoothing Model - Steel Data #
LES.Steel <- holt(SteelShp, initial = "optimal", h = 24)
summary(LES.Steel)

plot(LES.Steel, main = "US Steel Shipments with Linear ESM Forecast", xlab = "Date", ylab = "Shipments (Thousands of Net Tons)")
abline(v = 1992, col = "red", lty = "dashed")

autoplot(LES.Steel)+
  autolayer(fitted(LES.Steel),series="Fitted")+ylab("US Steel Shipments with Holt ESM Forecast")

LDES.Steel <- holt(SteelShp, initial = "optimal", h = 24, damped = TRUE)
summary(LDES.Steel)

plot(LDES.Steel, main = "US Steel Shipments with Linear Damped ESM Forecast", xlab = "Date", ylab = "Shipments (Thousands of Net Tons)")
abline(v = 1992, col = "red", lty = "dashed")

# Building a Linear Exponential Smoothing Model - US Airlines Data #
LES.USAir <- holt(Passenger, initial = "optimal", h = 24)
summary(LES.USAir)

plot(LES.USAir, main = "US Airline Passengers with Linear ESM Forecast", xlab = "Date", ylab = "Passengers (Thousands)")
abline(v = 2008.25, col = "red", lty = "dashed")

LDES.USAir <- holt(Passenger, initial = "optimal", h = 24, damped = TRUE)
summary(LDES.USAir)

plot(LDES.USAir, main = "US Airline Passengers with Linear Damped ESM Forecast", xlab = "Date", ylab = "Passengers (Thousands)")
abline(v = 2008.25, col = "red", lty = "dashed")

# Building a Holt-Winters ESM - US Airlines Data #
HWES.USAir <- hw(Passenger, seasonal = "additive")
summary(HWES.USAir)

plot(HWES.USAir, main = "US Airline Passengers with Holt-Winters ESM Forecast", xlab = "Date", ylab = "Passengers (Thousands)")
abline(v = 2008.25, col = "red", lty = "dashed")

HWES.USAir <- hw(Passenger, seasonal = "multiplicative")
summary(HWES.USAir)

plot(HWES.USAir, main = "US Airline Passengers with Holt-Winters ESM Forecast", xlab = "Date", ylab = "Passengers (Thousands)")
abline(v = 2008.25, col = "red", lty = "dashed")

#####Using a holdout data set
training=subset(Passenger,end=length(Passenger)-12)
test=subset(Passenger,start=length(Passenger)-11)
HWES.USAir.train <- hw(training, seasonal = "multiplicative",initial='optimal')
test.results=forecast(HWES.USAir.train,h=12)

error=test-test.results$mean
MAE=mean(abs(error))
MAPE=mean(abs(error)/abs(test))


