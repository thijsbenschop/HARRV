## Add forecasts for RVw and RVm

## Realized volatility

## Clear history
rm(list = ls(all = TRUE))
graphics.off()

## Install and load packages
libraries = c("e1071", "stats", "forecast", "tseries", "quantmod", "expm", "hydroGOF", "arfima", "fracdiff", "xtable")
lapply(libraries, function(x) if (!(x %in% installed.packages())) {
  install.packages(x)
})
lapply(libraries, library, quietly = TRUE, character.only = TRUE)

# Set working directory
#setwd("")
#setwd("C:/Users/thijs.benschop/DropBox/Realized volatility CO2/Data")  # Lenovo
#setwd("/Users/thijsbenschop/Dropbox/Realized volatility CO2/Data")    # Mac
#setwd("C:/Users/thijs.benschop/Documents/GitHub/HAR-RV")  # Lenovo
setwd("/Users/thijsbenschop/GitHub/HAR-RV")  # Mac

# 1 #### Read-in data ####
getwd()
data15 <- read.csv("co2_futures15min.csv")
dim(data15)
names(data15)

# 2 #### Format and clean data ####
# Convert date to date format
Sys.setlocale("LC_TIME", "C") # set location for language of month
data15$Date.G. <- as.Date(data15[,'Date.G.'], "%d-%B-%Y")

# Create vector with data for each year
# CFI2YZ8: 21.05.07 - 29.09.08
data1 <- data15[data15$X.RIC == "CFI2YZ8" & data15$Date.G >= as.Date("2007-05-21") & data15$Date.G <= as.Date("2008-09-29"), ]
dim(data1)
data1$Last <- (data1$Last/100)
# CFI2Z8: 30.09.08 - 30.11.08
data2 <- data15[data15$X.RIC == "CFI2Z8" & data15$Date.G >= as.Date("2008-09-30") & data15$Date.G <= as.Date("2008-11-30"), ]
dim(data2)
data2$Last <- (data2$Last/100)
# CFI2Z9: 01.12.08 - 30.11.09
data3 <- data15[data15$X.RIC == "CFI2Z9" & data15$Date.G >= as.Date("2008-12-01") & data15$Date.G <= as.Date("2009-11-30"), ]
dim(data3)
data3[data3$Date.G. <= 14330,'Last'] <- data3[data3$Date.G. <= 14330,'Last']/100
# CFI2Z0: 01.12.09 - 30.11.10
data4 <- data15[data15$X.RIC == "CFI2Z0" & data15$Date.G >= as.Date("2009-12-01") & data15$Date.G <= as.Date("2010-11-30"), ]
dim(data4)
# CFI2Z1: 01.12.10 - 30.11.11
data5 <- data15[data15$X.RIC == "CFI2Z1" & data15$Date.G >= as.Date("2010-12-01") & data15$Date.G <= as.Date("2011-11-30"), ]
dim(data5)
# CFI2Z2: 01.12.11 - 13.04.12
data6 <- data15[data15$X.RIC == "CFI2Z2" & data15$Date.G >= as.Date("2011-12-01") & data15$Date.G <= as.Date("2012-04-12"), ]
dim(data6)

# Merge 6 years of data (21.05.07 - 13.04.12)
dataBind <- rbind(data1, data2, data3, data4, data5, data6)
dim(dataBind)

# Drop observations outside trading hours (6 am - 4 pm)
timeLevels      <- levels(dataBind[,'Time.G.'])
TradeTimeLevels <- timeLevels[25:64]

dataBind        <- dataBind[dataBind$Time.G. %in% TradeTimeLevels,]
dim(dataBind)

# Only keep Date, Time and Last, drop other variables
dataBind <- dataBind[,c('Date.G.', 'Time.G.', 'Last')]
dim(dataBind)

# Drop days without any trading (holidays, weekends)
dataBind            <- cbind(dataBind, weekdays(dataBind$Date))  # Add weekdays in extra column
colnames(dataBind)  <- c('Date', 'Time', 'Last', 'Weekday')      # Rename columns

dataBind <- dataBind[!(dataBind$Weekday %in% c('Saturday', 'Sunday')), ] # drop Saturdays and Sundays

datesWithoutTrading <- as.Date(c('28.05.07', '25.12.08', '26.12.08', '01.01.09', '10.04.09',
                                 '13.04.09', '25.12.09', '28.12.09', ' 01.01.10', '02.04.10',
                                 '05.04.10', '31.05.10', '27.12.10', '28.12.10', '03.01.11', '22.04.11', 
                                 '25.04.11', '30.05.11', '26.12.11', '02.01.2012', '06.04.12', '09.04.12'), format='%d.%m.%y')
dataBind <- dataBind[!(dataBind$Date %in% datesWithoutTrading), ] #drop days without trading

# Data (49,280 row, 4 columns (Date, Time, Last and Weekday))
data <- dataBind[2:nrow(dataBind),] # exclude first row (missing last)

# Interpolate missing values
#library(zoo)
data[, 'Last'] <- na.locf(data[, 'Last']) # replaces NA with last non-missing value
#data[, 'Last'] <- na.spline(data[, 'Last']) # replaces NA with interpolated value (spline)
#detach("package:zoo", unload=TRUE)

# (Log) returns (quarter of an hour) (p_t - p_t-1) / (ln(p_t / p_t-1)) (this is across days)
data[, 'return']      <- c(NA, data[2:length(data[,'Last']),'Last'] - data[1:(length(data[,'Last']) -1),'Last']) # return (difference)
#data[, 'return']     <- c(NA, log(data[2:length(data[,'Last']),'Last'] / data[1:(length(data[,'Last']) -1),'Last'])) # log return

data <- data[2:nrow(data),] # Remove first row (no difference/return computed)

# Check (and remove) outliers
head(sort(data[,"return"]), n = 10)
which(data[,"return"] %in% head(sort(data[,"return"]), n = 2))
tail(sort(data[,"return"]), n = 10)

# plot(data[,'return'] ~ as.Date(data[,'Date'], origin="1970-1-1"), type='l', xlab="time", ylab="absolute return (15 min)")

# Plot log returns(15 minutes) (red vertical lines are roll-over points to next futures contract)
png(file = "logreturn.png", width = 16000, height = 8000, res = 900)
par(mar=c(3.1,5.1,1.1,1.1))
plot(data[,'return'] ~ as.Date(data[,'Date'], origin="1970-1-1"), type='l', xlab="", ylab="log return (15 min)", cex.lab = 1.5, cex.axis = 1.5)
# Add vertical lines at roll-over moments
abline(v = as.numeric(as.Date("2008-09-29", origin="1970-1-1")), col = "red")
abline(v = as.numeric(as.Date("2008-11-30", origin="1970-1-1")), col = "red")
abline(v = as.numeric(as.Date("2009-11-30", origin="1970-1-1")), col = "red")
abline(v = as.numeric(as.Date("2010-11-30", origin="1970-1-1")), col = "red")
abline(v = as.numeric(as.Date("2011-11-30", origin="1970-1-1")), col = "red")
par(mar=c(5.1,4.1,4.1,2.1))
dev.off()

summary(data[,'return'])
kurtosis(data[,'return'], method = "moment")
sd(data[,'return'])
skewness(data[,'return'])

# Plot density of log returns and normal distibution
plot(density(data[,'return']), xlim=c(-0.05, 0.05),  ylim = c(0, 16), xlab = "Returns", main = "", lwd = 2)
#par(new= TRUE)
#plot(dnorm(seq(-0.05, 0.05, 0.0001), mean = mean(data[,'return']), sd = sd(data[,'return'])),ylim = c(0, 16), xlab = "Log-returns", main = "", lwd = 2, col = "red", type = 'l', ylab = "", xaxt ='n')

# Plot prices
png(file = "prices.png", width = 12000, height = 4000, res = 900)
par(mar=c(3.1,5.1,1.1,1.1))
plot(data[,'Last'] ~ as.Date(data[,'Date'], origin="1970-1-1"), type='l', xlab="time", 
     ylab="futures price", cex.lab = 1.5, cex.axis = 1.5, lwd = 2)
# Add vertical lines at roll-over moments
abline(v = as.numeric(as.Date("2008-09-30", origin="1970-1-1")), col = "red", lwd = 2)
abline(v = as.numeric(as.Date("2008-11-30", origin="1970-1-1")), col = "red", lwd = 2)
abline(v = as.numeric(as.Date("2009-11-30", origin="1970-1-1")), col = "red", lwd = 2)
abline(v = as.numeric(as.Date("2010-11-30", origin="1970-1-1")), col = "red", lwd = 2)
abline(v = as.numeric(as.Date("2011-11-30", origin="1970-1-1")), col = "red", lwd = 2)
par(mar=c(5.1,4.1,4.1,2.1))
dev.off()

summary(data[,'Last'])
kurtosis(data[,'Last'], method = "moment")
sd(data[,'Last'])
skewness(data[,'Last'])

# # Plot all 6 contracts in one plot
# data1full <- data15[data15$X.RIC == "CFI2YZ8", ]
# plot(data1full[,'Last'] ~ as.Date(data1full[,'Date.G.'], origin="1970-1-1"), type='l', xlab="time", ylab="Futures price", col = "red", lwd = 2, ylim = c(0,3000), xlim = c(as.Date("2007-01-01"), as.Date("2012-12-31")))
# par(new=TRUE)
# data2full <- data15[data15$X.RIC == "CFI2Z8", ]
# plot(data2full[,'Last'] ~ as.Date(data2full[,'Date.G.'], origin="1970-1-1"), type='l', xlab="time", ylab="Futures price", col = "red", lwd = 2, ylim = c(0,3000), xlim = c(as.Date("2007-01-01"), as.Date("2012-12-31")))
# par(new=TRUE)
# data3full <- data15[data15$X.RIC == "CFI2Z9", ]
# plot(data3full[,'Last'] ~ as.Date(data3full[,'Date.G.'], origin="1970-1-1"), type='l', xlab="time", ylab="Futures price", col = "red", lwd = 2, ylim = c(0,30000), xlim = c(as.Date("2007-01-01"), as.Date("2012-12-31")))
# par(new=TRUE)
# data4full <- data15[data15$X.RIC == "CFI2Z0", ]
# plot(data4full[,'Last'] ~ as.Date(data4full[,'Date.G.'], origin="1970-1-1"), type='l', xlab="time", ylab="Futures price", col = "red", lwd = 2, ylim = c(0,30000), xlim = c(as.Date("2007-01-01"), as.Date("2012-12-31")))
# par(new=TRUE)
# data5full <- data15[data15$X.RIC == "CFI2Z1", ]
# plot(data5full[,'Last'] ~ as.Date(data5full[,'Date.G.'], origin="1970-1-1"), type='l', xlab="time", ylab="Futures price", col = "red", lwd = 2, ylim = c(0,30000), xlim = c(as.Date("2007-01-01"), as.Date("2012-12-31")))
# par(new=TRUE)
# data6full <- data15[data15$X.RIC == "CFI2Z2", ]
# plot(data6full[,'Last'] ~ as.Date(data6full[,'Date.G.'], origin="1970-1-1"), type='l', xlab="time", ylab="Futures price", col = "red", lwd = 2, ylim = c(0,30000), xlim = c(as.Date("2007-01-01"), as.Date("2012-12-31")))

# 3 #### Compute realized volatility #####
# Calculate daily realized volatility (see p.177 in Corsi)
obsPerDay               <- table(data$Date) # number of observations per day
realVol                 <- matrix(NA, length(obsPerDay), 6) # matrix, each row is one day
colnames(realVol)       <- c('Date', 'obsPerDay', 'RVd', 'RVw', 'RV2w', 'RVm') #day, month, year
realVol[, 'Date']       <- as.Date(names(obsPerDay)) # dates
realVol[, 'obsPerDay']  <- obsPerDay # number of observations per day

# For realized quarticity
realQuar                <- realVol
colnames(realQuar)      <- c('Date', 'obsPerDay', 'RQd', 'RQw', 'RQ2w', 'RQm') #day, month, year

# For realized semivariance
realSemiVar             <- realVol[, 1:4]
colnames(realSemiVar)      <- c('Date', 'obsPerDay', 'RSpos', 'RSneg') # daily positive and negative semi-variance

## Daily realized volatility (computed by using the difference across days)
#lastValuePrevDay <- data[39,'return'] # last value of previous day, initialized with last value on first day
#for (i in 2:length(obsPerDay)) {   # for all trading days
#  # Realized volatility (day), Corsi, equation (3)
#   dayLast            <- data[data$Date == realVol[i, 'Date'],'return']  # all last prices with 15 minutes interval of that day
#   realVol[i, 'RVd']  <- sqrt(sum((diff(dayLast))^2 + (dayLast[1]-lastValuePrevDay)^2)) # squared differences 
#   lastValuePrevDay   <- dayLast[length(dayLast)] # last value of day for next day
#} 
#

# Daily realized volatility (computed within days) (should we take the log here?)
for (i in 1:length(obsPerDay)) {   # for all trading days
  # Realized volatility (day), Corsi, equation (3)
  dayLast            <- data[data$Date == realVol[i, 'Date'],'return']  # returns for each 15 minute interval of that day
  dayLast            <- dayLast[2:length(dayLast)]                      # remove first return, since calculated across days
  #realVol[i, 'RVd'] <- sqrt(sum(diff(dayLast), na.rm = T)^2)           # squared differences, should we take the square root?
  
  realVol[i, 'RVd']  <- sum(dayLast^2, na.rm = T)           # squared returns, should we take the square root?
} 

# Compute realized quarticity (M/3)sum r^4
for (i in 1:length(obsPerDay)) {   # for all trading days
  # Realized quarticity (day), Bollerslev, Patton, Quaedvlieg, equation 4
  dayLast            <- data[data$Date == realVol[i, 'Date'],'return']  # all last prices with 15 minutes interval of that day
  dayLast            <- dayLast[2:length(dayLast)] # remove first return, since calculated across days
  
  realQuar[i, 'RQd']  <- (length(dayLast) / 3 ) * sum(dayLast^4, na.rm = T) # sum of returns to the power 4
} 

# Compute realized semivariance (Barndorff-Nielsen et al. 2010)
for (i in 1:length(obsPerDay)) {   # for all trading days
  # Realized semivariance (Barndorff-Nielsen et al. 2010)
  dayLast            <- data[data$Date == realVol[i, 'Date'],'return']  # all last prices with 15 minutes interval of that day
  dayLast            <- dayLast[2:length(dayLast)] # remove first return, since calculated across days
  
  realSemiVar[i, 'RSpos']  <- sum(( (dayLast > 0) * dayLast) ^ 2, na.rm = T) # sum of squared postive returns
  realSemiVar[i, 'RSneg']  <- sum(( (dayLast < 0) * dayLast) ^ 2, na.rm = T) # sum of squared negative returns
}

# Annualization of daily volatility
realVol[,'RVd'] <- realVol[,'RVd'] * sqrt(252)
#realVol[,'RVd'] <- sqrt(realVol[,'RVd']) * sqrt(252)
#realVol[,'RVd'] <- log(sqrt(realVol[,'RVd']) * sqrt(252))

# Weekly, biweekly and monthly realized volatility (average)
realVol[,'RVw']  <- filter(realVol[,'RVd'], rep(1/5, 5), sides=1)
realVol[,'RV2w'] <- filter(realVol[,'RVd'], rep(1/10, 10), sides=1)
realVol[,'RVm']  <- filter(realVol[,'RVd'], rep(1/21, 21), sides=1) # this should be 22?

# Weekly, biweekly and monthly realized quarticity (average)
realQuar[,'RQw']  <- filter(realQuar[,'RQd'], rep(1/5, 5), sides=1)
realQuar[,'RQ2w'] <- filter(realQuar[,'RQd'], rep(1/10, 10), sides=1)
realQuar[,'RQm']  <- filter(realQuar[,'RQd'], rep(1/21, 21), sides=1) # this should be 22?

# Realized daily returns
#realDailyReturn <-  rep(NA, length(obsPerDay))
#for (i in 1:length(obsPerDay)) {   # for all trading days
#  dayLast             <- data[data$Date == realVol[i, 'Date'],'return']  # all last prices with 15 minutes interval of that day
#  realDailyReturn[i]  <- sqrt(sum((diff(dayLast))^2) + (dayLast[1]-lastValuePrevDay)^2) # squared differences 
#  lastValuePrevDay    <- dayLast[length(dayLast)] # last value of day for next day
#}

# Plots
png(file = "realvolNew.png", width = 12000, height = 4000, res = 900)
par(mar=c(2.1,4.1,1.1,1.1))
ylim1 <- c(0, 70) # c(0,1)
plot(realVol[,'RVd'] ~ as.Date(realVol[,'Date'], origin="1970-1-1"), type='l', col='black', ylim=ylim1, xlim=as.Date(c('2007-05-21', '2012-04-15')), ylab="", xlab="", lwd = 2)
par(new=T)
plot(realVol[,'RVw'] ~ as.Date(realVol[,'Date'], origin="1970-1-1"), type='l', col='red', ylim=ylim1, xlim=as.Date(c('2007-05-21', '2012-04-15')), ylab="", xlab="", lwd = 2)
par(new=T)
plot(realVol[,'RVm'] ~ as.Date(realVol[,'Date'], origin="1970-1-1"), type='l', col='green', ylim=ylim1, xlim=as.Date(c('2007-05-21', '2012-04-15')), ylab="realized volatility", xlab="", lwd = 2)
par(mar=c(5.1,4.1,4.1,2.1))
dev.off()

#log scale (black/white)
png(file = "realvolNewlogscale.png", width = 12000, height = 4000, res = 900)
par(mar=c(2.1,4.1,1.1,1.1))
ylim1 <- c(0.1, 70) # c(0,1)
plot(realVol[,'RVd'] ~ as.Date(realVol[,'Date'], origin="1970-1-1"), type='l', col='black', ylim=ylim1, xlim=as.Date(c('2007-05-21', '2012-04-15')), ylab="", xlab="", log = "y", cex.lab = 1.5, cex.axis = 1.5, lwd = 2)
par(new=T)
plot(realVol[,'RVw'] ~ as.Date(realVol[,'Date'], origin="1970-1-1"), type='l', col='red', ylim=ylim1, xlim=as.Date(c('2007-05-21', '2012-04-15')), ylab="", xlab="", log = "y", cex.lab = 1.5, cex.axis = 1.5, lwd = 2)
par(new=T)
plot(realVol[,'RVm'] ~ as.Date(realVol[,'Date'], origin="1970-1-1"), type='l', col='green', ylim=ylim1, xlim=as.Date(c('2007-05-21', '2012-04-15')), ylab="realized volatility (log scale)", xlab="", log = "y", cex.lab = 1.5, cex.axis = 1.5, lwd = 2)
par(mar=c(5.1,4.1,4.1,2.1))
dev.off()

#log scale
png(file = "realvolNewlogscale.png", width = 8000, height = 4000, res = 450)
par(mar=c(2.1,4.1,1.1,1.1))
ylim1 <- c(0.1, 70) # c(0,1)
plot(realVol[,'RVd'] ~ as.Date(realVol[,'Date'], origin="1970-1-1"), type='l', lty = 3, lwd = 3, col='black', ylim=ylim1, xlim=as.Date(c('2007-05-21', '2012-04-15')), ylab="", xlab="", log = "y", cex.lab = 1.5, cex.axis = 1.5)
par(new=T)
plot(realVol[,'RVw'] ~ as.Date(realVol[,'Date'], origin="1970-1-1"), type='l', lty = 2, lwd = 3, col='red', ylim=ylim1, xlim=as.Date(c('2007-05-21', '2012-04-15')), ylab="", xlab="", log = "y", cex.lab = 1.5, cex.axis = 1.5)
par(new=T)
plot(realVol[,'RVm'] ~ as.Date(realVol[,'Date'], origin="1970-1-1"), type='l', lty = 1, lwd = 3, col='darkgreen', ylim=ylim1, xlim=as.Date(c('2007-05-21', '2012-04-15')), ylab="realized volatility (log scale)", xlab="", log = "y", cex.lab = 1.5, cex.axis = 1.5)
par(mar=c(5.1,4.1,4.1,2.1))
dev.off()

#Log of realized volatilities
#png(file = "realvolNew.png", width = 8000, height = 4000, res = 450)
par(mar=c(2.1,4.1,1.1,1.1))
ylim1 <- c(-2, 5) # c(0,1)
plot(log(realVol[,'RVd']) ~ as.Date(realVol[,'Date'], origin="1970-1-1"), type='l', col='black', ylim=ylim1, xlim=as.Date(c('2007-05-21', '2012-04-15')), ylab="", xlab="")
par(new=T)
plot(log(realVol[,'RVw']) ~ as.Date(realVol[,'Date'], origin="1970-1-1"), type='l', col='red', ylim=ylim1, xlim=as.Date(c('2007-05-21', '2012-04-15')), ylab="", xlab="")
par(new=T)
plot(log(realVol[,'RVm']) ~ as.Date(realVol[,'Date'], origin="1970-1-1"), type='l', col='green', ylim=ylim1, xlim=as.Date(c('2007-05-21', '2012-04-15')), ylab="realized volatility", xlab="")
par(mar=c(5.1,4.1,4.1,2.1))
#dev.off()

# Plot for presentation (icon)
par(mar=c(0.1,0.1,0.1,0.1))
ylim1 <- c(0, 65) # c(0,1)
plot(realVol[,'RVd'] ~ as.Date(realVol[,'Date'], origin="1970-1-1"), type='l', col='black', ylim=ylim1, xlim=as.Date(c('2007-05-21', '2012-04-15')), ylab="", xlab="", yaxt = 'n', xaxt = 'n', lwd = 2)
par(new=T)
plot(realVol[,'RVw'] ~ as.Date(realVol[,'Date'], origin="1970-1-1"), type='l', col='red', ylim=ylim1, xlim=as.Date(c('2007-05-21', '2012-04-15')), ylab="", xlab="", yaxt = 'n', xaxt = 'n', lwd=2)
par(new=T)
plot(realVol[,'RVm'] ~ as.Date(realVol[,'Date'], origin="1970-1-1"), type='l', col='green', ylim=ylim1, xlim=as.Date(c('2007-05-21', '2012-04-15')), ylab="", xlab="", yaxt = 'n', xaxt = 'n', lwd=2)
par(mar=c(5.1,4.1,4.1,2.1))

# Plot prices and relaized volatility together in one plot
# # Plot prices
# plot(data[,'Last'] ~ as.Date(data[,'Date'], origin="1970-1-1"), type='l', xlab="time", ylab="futures price")
# # Add vertical lines at roll-over moments
# abline(v = as.numeric(as.Date("2008-09-30", origin="1970-1-1")), col = "red")
# abline(v = as.numeric(as.Date("2008-11-30", origin="1970-1-1")), col = "red")
# abline(v = as.numeric(as.Date("2009-11-30", origin="1970-1-1")), col = "red")
# abline(v = as.numeric(as.Date("2010-11-30", origin="1970-1-1")), col = "red")
# abline(v = as.numeric(as.Date("2011-11-30", origin="1970-1-1")), col = "red")

# Kernel density plots (normal kernel)
xlim2 <- c(0, 15) #c(0,0.05) 
ylim2 <- c(0, 0.42) #c(0,120)
plot(density(realVol[,'RVd'], from = 0, adjust = 0.5, kernel = "gaussian",  na.rm=T), main="", xlab="", xlim=xlim2, ylim=ylim2, lwd=2)
par(new=T)
plot(density(realVol[,'RVw'], from = 0, adjust = 0.5, kernel = "gaussian", na.rm=T), main="", xlab="", xlim=xlim2, ylim=ylim2, lwd=2, lty=2)
par(new=T)
plot(density(realVol[,'RV2w'], from = 0, adjust = 0.5, kernel = "gaussian", na.rm=T), main="", xlab="", xlim=xlim2, ylim=ylim2, lwd=2, lty=2)
par(new=T)
plot(density(realVol[,'RVm'], from = 0, adjust = 0.5, kernel = "gaussian", na.rm=T), main="", xlab="", xlim=xlim2, ylim=ylim2,lwd=2, lty=3)

par(mar=c(2.1,4.1,1.1,1.1))
xlim3 <- c(0, 15) #c(0,0.05) 
ylim3 <- c(0, 0.27) #c(0,120)
plot(density(realVol[,'RVd'], from = 0, adjust = 2, kernel = "gaussian",  na.rm=T), main="", xlab="", xlim=xlim3, ylim=ylim3, lwd=2)
par(new=T)
plot(density(realVol[,'RVw'], from = 0, adjust = 2, kernel = "gaussian", na.rm=T), main="", xlab="", xlim=xlim3, ylim=ylim3, lwd=2, lty=2)
par(new=T)
plot(density(realVol[,'RV2w'], from = 0, adjust = 2, kernel = "gaussian", na.rm=T), main="", xlab="", xlim=xlim3, ylim=ylim3, lwd=2, lty=2)
par(new=T)
plot(density(realVol[,'RVm'], from = 0, adjust = 2, kernel = "gaussian", na.rm=T), main="", xlab="", xlim=xlim3, ylim=ylim3,lwd=2, lty=3)
par(mar=c(5.1,4.1,4.1,2.1))

##### Estimate models #####
# Models are estimated based on obs 21-1249 to allow for comparison across models
##### Estimate HAR(3)-RV #####
dataHAR <- cbind(realVol, rep(NA, nrow(realVol)), rep(NA, nrow(realVol)), rep(NA, nrow(realVol)))
colnames(dataHAR) <- c("Date", "obsPerDay", "RVd", "RVw", "RV2w", "RVm", "RVd1", "RVw1", "RVm1")
dataHAR[1 : (nrow(realVol) - 1), 'RVd1'] <- realVol[2 : nrow(realVol), 'RVd']
dataHAR[1 : (nrow(realVol) - 1), 'RVw1'] <- realVol[2 : nrow(realVol), 'RVw']
dataHAR[1 : (nrow(realVol) - 1), 'RVm1'] <- realVol[2 : nrow(realVol), 'RVm']

# take 2 weeks or 1 week? / use LASSO to select the lag structure
sumHAR <- summary(lm(RVd1 ~ RVd + RV2w + RVm, data = as.data.frame(dataHAR[21:1249,])))
names(sumHAR)
sumHAR$coefficients
sumHAR$r.squared # why is there a difference?
1 - sum(sumHAR$residuals^2, na.rm=T) / sum((realVol[,'RVd'] - mean(realVol[,'RVd']))^2) #r-squared

# # Information criteria
# AIC(lm(RVd1 ~ RVd + RV2w + RVm, data=as.data.frame(dataHAR[21:1249,])))
# BIC(lm(RVd1 ~ RVd + RV2w + RVm, data=as.data.frame(dataHAR[21:1249,])))

# Stationarity tests
adf.test(realVol[,'RVd']) # null of non-stationarity is rejected
kpss.test(realVol[,'RVd'])
Box.test(realVol[,'RVd'])

adf.test(diff(realVol[,'RVd']))
kpss.test(diff(realVol[,'RVd']))
Box.test(diff(realVol[,'RVd']))

plot(diff(realVol[,'RVd']), type="l") # plot difference of daily realized volatility

# acf and pacf
acf(realVol[,'RVd'], lag.max = 100, ci = 0.95, ci.type = 'ma', main = "")
#acf(diff(realVol[,'RVd']), lag.max = 100)
pacf(realVol[,'RVd'], lag.max = 100, main = "")
#pacf(diff(realVol[,'RVd']), lag.max = 100)

# long memory test


xtable(sumHAR$coefficients, digits = 4)

##### Estimate HARQ-RV and HARQ-F models according to Bollerslev, Patton and Quaedvlieg #####
dataHARQ            <- as.data.frame(cbind(realVol, realQuar[,3:6], rep(NA, nrow(realVol)), rep(NA, nrow(realVol)), rep(NA, nrow(realVol))))
colnames(dataHARQ)  <- c("Date", "obsPerDay", "RVd", "RVw", "RV2w", "RVm", "RQd", "RQw", "RQ2w", "RQm", "RVd1", "RVw1", "RVm1")
dataHARQ$RQRVd      <- (sqrt(dataHARQ$RQd)  * dataHARQ$RVd)
dataHARQ$RQRVw      <- (sqrt(dataHARQ$RQw)  * dataHARQ$RVw)
dataHARQ$RQRV2w     <- (sqrt(dataHARQ$RQ2w) * dataHARQ$RV2w)
dataHARQ$RQRVm      <- (sqrt(dataHARQ$RQm)  * dataHARQ$RVm)
dataHARQ[1 : (nrow(realVol) - 1), 'RVd1'] <- realVol[2 : nrow(realVol), 'RVd'] # realized volatility of next day
dataHARQ[1 : (nrow(realVol) - 1), 'RVw1'] <- realVol[2 : nrow(realVol), 'RVw'] # realized volatility of next day
dataHARQ[1 : (nrow(realVol) - 1), 'RVm1'] <- realVol[2 : nrow(realVol), 'RVm'] # realized volatility of next day

# 1 day, 1 week, 1 month
sumHARQ <- summary(lm(RVd1 ~ RVd + RQRVd + RVw + RVm, data = dataHARQ[21:1249,]))
names(sumHARQ)
sumHARQ$coefficients
sumHARQ$r.squared # why is there a difference?
1 - sum(sumHARQ$residuals^2, na.rm=T) / sum((realVol[,'RVd'] - mean(realVol[,'RVd']))^2) #r-squared

# 1 day, 1 week, 1 month
sumHARQF <- summary(lm(RVd1 ~ RVd + RQRVd + RVw + RQRVw + RVm + RQRVm, data = dataHARQ[21:1249,]))
names(sumHARQ)
sumHARQF$coefficients
sumHARQF$r.squared # why is there a difference?
1 - sum(sumHARQ$residuals^2, na.rm=T) / sum((realVol[,'RVd'] - mean(realVol[,'RVd']))^2) #r-squared

##### Estimate SHAR model according to Patton and Shephard 2015 #####
dataSHAR            <- as.data.frame(cbind(realVol, realSemiVar[,3:4], rep(NA, nrow(realVol)), rep(NA, nrow(realVol)), rep(NA, nrow(realVol))))
colnames(dataSHAR)  <- c("Date", "obsPerDay", "RVd", "RVw", "RV2w", "RVm", "RSpos", "RSneg", "RVd1", "RVw1", "RVm1")
dataSHAR[1 : (nrow(realVol) - 1), 'RVd1'] <- realVol[2 : nrow(realVol), 'RVd'] # realized volatility of next day
dataSHAR[1 : (nrow(realVol) - 1), 'RVw1'] <- realVol[2 : nrow(realVol), 'RVw'] # realized volatility of next day
dataSHAR[1 : (nrow(realVol) - 1), 'RVm1'] <- realVol[2 : nrow(realVol), 'RVm'] # realized volatility of next day

# 1 day, 1 week, 1 month
sumSHAR <- summary(lm(RVd1 ~ RSpos + RSneg + RVw + RVm, data = dataSHAR[21:1249,]))
names(sumSHAR)
sumSHAR$coefficients
sumSHAR$r.squared # why is there a difference?
1 - sum(sumSHAR$residuals^2, na.rm=T) / sum((realVol[,'RVd'] - mean(realVol[,'RVd']))^2) #r-squared

##### Estimate AR(1)  ####
#dataAR <- diff(realVol[,'RVd']) 
sumAR1 <- Arima(realVol[21:1249,'RVd'], order = c(1, 0, 0))
#sumAR1 <- Arima(dataAR, order = c(1, 0, 0))
sumAR1

1 - sum(sumAR1$residuals^2, na.rm=T) / sum((realVol[,'RVd'] - mean(realVol[,'RVd']))^2) # r-squared

##### Estimate AR(3) #####
#dataAR <- realVol[,'RVd'] #diff(realVol[,'RVd']) #
sumAR3 <- Arima(realVol[21:1249,'RVd'], order=c(3,0,0))
sumAR3
1 - sum(sumAR3$residuals^2, na.rm=T) / sum((realVol[,'RVd'] - mean(realVol[,'RVd']))^2) #r-squared

##### Estimate AR(5) #####
#dataAR <- realVol[,'RVd'] #diff(realVol[,'RVd']) #
sumAR5 <- Arima(realVol[21:1249,'RVd'], order=c(5,0,0))
sumAR5
1 - sum(sumAR5$residuals^2, na.rm=T) / sum((realVol[,'RVd'] - mean(realVol[,'RVd']))^2) #r-squared

##### Estimate AR(10) #####
#dataAR <- realVol[,'RVd'] #diff(realVol[,'RVd']) #
sumAR10 <- Arima(realVol[21:1249,'RVd'], order=c(10,0,0))
sumAR10
1 - sum(sumAR10$residuals^2, na.rm=T) / sum((realVol[,'RVd'] - mean(realVol[,'RVd']))^2) #r-squared

##### Estimate AR(22) #####
#dataAR <- realVol[,'RVd']
sumAR22 <- Arima(realVol[21:1249,'RVd'], order=c(22,0,0))
sumAR22
1 - sum(sumAR22$residuals^2, na.rm=T) / sum((realVol[,'RVd'] - mean(realVol[,'RVd']))^2) #r-squared

# Test restrictions on AR(22) on HAR-RV with F test
# 19 restrictions (J), R2 restricted is 0.2189129, R2 restricted is 0.2486714
# note different lengths of residuals
((sum(sumHAR$residuals^2) - sum(sumAR22$residuals^2)) / 19) / (sum(sumAR22$residuals^2)/(1229 - 22 -1))

#((0.2189129 - 0.2021814) / 19) / ((1-0.2189129)/(1229 - 22 -1)) # F-test

pf(1.420004, 19, (1229 - 22 - 1), lower.tail = F) #p-value 0.1375911
qf(0.95, 19, (1229 - 22 - 1), lower.tail = T, log.p = FALSE) # 5 % critical value

##### Estimate ARFIMA #####
dataARFIMA <- realVol[21:1249,'RVd']
#estimate d with GPH parameter
d <- fdGPH(dataARFIMA, bandw.exp = 0.5)
d <- fdSperio(dataARFIMA)

#sumARFIMA <- arfima(dataARFIMA, max.p = 5, max.q = 0) # forecast package
#sumARFIMA <- arfima(dataARFIMA, order = c(5,0,0)) # arfima package
sumARFIMA <- fracdiff(dataARFIMA, nar= 5, nma = 0) #, drange = c(0.3,0.5))

AIC(sumARFIMA)

names(sumARFIMA)
print(sumARFIMA)
summary(sumARFIMA)
sumARFIMA$ar # coefficients
1 - sum(sumARFIMA$residuals^2, na.rm=T) / sum((realVol[,'RVd'] - mean(realVol[,'RVd']))^2) #r-squared

### Compare model fit
# Fit evaluation (5 models, 7 measures)
comparisonFit <- matrix(rep(NA, 24), c(8,3))
colnames(comparisonFit) <- c("AIC", "BIC", "Rsquared")
rownames(comparisonFit) <- c("HAR-RV", "HARQ", "HARQF", "SHAR", "AR1", "AR3", "AR22", "ARFIMA")

# AIC, BIC
comparisonFit["HAR-RV", "AIC"] <- AIC(lm(RVd1 ~ RVd + RV2w + RVm, data = as.data.frame(dataHAR[21:1249,]))) #HAR
comparisonFit["HAR-RV", "BIC"] <- BIC(lm(RVd1 ~ RVd + RV2w + RVm, data = as.data.frame(dataHAR[21:1249,]))) #HAR
comparisonFit["HARQ", "AIC"]   <- AIC(lm(RVd1 ~ RVd + RQRVd + RVw + RVm, data = dataHARQ[21:1249,])) #HARQ
comparisonFit["HARQ", "BIC"]   <- BIC(lm(RVd1 ~ RVd + RQRVd + RVw + RVm, data = dataHARQ[21:1249,])) #HARQ
comparisonFit["HARQF", "AIC"]  <- AIC(lm(RVd1 ~ RVd + RQRVd + RVw + RQRVw + RVm + RQRVm , data = dataHARQ[21:1249,])) #HARQF
comparisonFit["HARQF", "BIC"]  <- BIC(lm(RVd1 ~ RVd + RQRVd + RVw + RQRVw + RVm + RQRVm , data = dataHARQ[21:1249,])) #HARQF
comparisonFit["SHAR", "AIC"]   <- AIC(lm(RVd1 ~ RVd + RQRVd + RVw + RQRVw + RVm + RQRVm , data = dataHARQ[21:1249,])) #SHAR
comparisonFit["SHAR", "BIC"]   <- BIC(lm(RVd1 ~ RVd + RQRVd + RVw + RQRVw + RVm + RQRVm , data = dataHARQ[21:1249,])) #SHAR
comparisonFit["AR1", "AIC"]    <- AIC(sumAR1)
comparisonFit["AR1", "BIC"]    <- - 2*sumAR1$loglik + 3 * log(sumAR1$nobs)# BIC(sumAR1)
comparisonFit["AR3", "AIC"]    <- AIC(sumAR3)
comparisonFit["AR3", "BIC"]    <- - 2*sumAR3$loglik + 3 * log(sumAR3$nobs)#; BIC(sumAR3)

AIC(sumAR5)
- 2*sumAR5$loglik + 3 * log(sumAR5$nobs)#; BIC(sumAR5)
AIC(sumAR10)
- 2*sumAR10$loglik + 3 * log(sumAR10$nobs)#; BIC(sumAR10)

comparisonFit["AR22", "AIC"]   <- AIC(sumAR22)
comparisonFit["AR22", "BIC"]   <- - 2*sumAR22$loglik + 3 * log(sumAR22$nobs)#; BIC(sumAR22)
comparisonFit["ARFIMA", "AIC"] <- AIC(sumARFIMA)
comparisonFit["ARFIMA", "BIC"] <- BIC(sumARFIMA)

# R squared
comparisonFit["HAR-RV", "Rsquared"] <- sumHAR$adj.r.squared #sumHAR$r.squared
comparisonFit["HARQ", "Rsquared"]   <- sumHARQ$adj.r.squared #sumHARQ$r.squared
comparisonFit["HARQF", "Rsquared"]  <- sumHARQF$adj.r.squared #sumHARQF$r.squared
comparisonFit["SHAR", "Rsquared"]   <- sumSHAR$adj.r.squared #sumSHAR$r.squared
comparisonFit["AR1", "Rsquared"]    <- 1-((1-cor(fitted(sumAR1), realVol[21:1249,'RVd'])^2)*(1229-1)/(1229-2-1))
comparisonFit["AR3", "Rsquared"]    <- 1-((1-cor(fitted(sumAR3), realVol[21:1249,'RVd'])^2)*(1229-1)/(1229-4-1))
comparisonFit["AR22", "Rsquared"]   <- 1-((1-cor(fitted(sumAR22), realVol[21:1249,'RVd'])^2)*(1229-1)/(1229-23-1))
comparisonFit["ARFIMA", "Rsquared"] <- 1-((1-cor(fitted(sumARFIMA), realVol[21:1249,'RVd'])^2)*(1229-1)/(1229-6-1))

comparisonFit
xtable(comparisonFit[,c(1,3)], digits = 3)

#### Check normality of the residuals ##### 
# HAR(3)-RV
jarque.bera.test(sumHAR$residuals)
qqnorm(sumHAR$residuals)
# HARQ
jarque.bera.test(sumHARQ$residuals)
qqnorm(sumHARQ$residuals)
# HARQF
jarque.bera.test(sumHARQF$residuals)
qqnorm(sumHARQF$residuals)
# SHAR
jarque.bera.test(sumSHAR$residuals)
qqnorm(sumSHAR$residuals)
# AR(1)
jarque.bera.test(sumAR1$residuals)
qqnorm(sumAR1$residuals)
# AR(3)
jarque.bera.test(sumAR3$residuals)
qqnorm(sumAR3$residuals)
# AR(22)
jarque.bera.test(sumAR22$residuals)
qqnorm(sumAR22$residuals)
# ARFIMA
#jarque.bera.test(sumARFIMA$residuals)

plot(density(sumHAR$residuals))#, xlim = c(-0.02, 0.06), ylim = c(0,90))
plot(density(sumHARQ$residuals))#, xlim = c(-0.02, 0.06), ylim = c(0,90))
plot(density(sumHARQF$residuals))#, xlim = c(-0.02, 0.06), ylim = c(0,90))
#par(new=T)
#plot(dnorm((0:100)/100 - 0.5, mean(sumHAR$residuals), sd(sumHAR$residuals)), type = 'l'), xlim = c(-0.02, 0.06))

# #### Simulation ####
# # Simulation 100000 observations, 40 per day , 2500 days
# # burn-in period 1250 days, starting values: estimated coefficents of HAR-RV model and 1 month of data (22*40 = 880 obs.)
# # HAR(3)-RV
# set.seed(123456)
# simVol            <- as.data.frame(matrix(NA, 2500, 4))    # for each day
# simReturn         <- as.data.frame(matrix(NA, 100000, 8))  # for each 15 minutes
# 
# colnames(simVol)     <- c("day", "RVd", "RVw", "RVm")
# colnames(simReturn)  <- c("day", "RVd", "RVw", "RVm", "omega", "vol", "norm", "ret")
# 
# simVol$day    <- c(1:2500)                  # 2500 days
# simReturn$day <- c(rep(1:2500, each = 40))  # Repeat day numbers 40 times (for each 15 min)
# 
# # Initialize simReturn for first 22 days with actual data
# simReturn[1:880, "ret"] <- data[279:1158,"return"] 
# 
# # Compute daily returns based on simReturn
# for(i in 2:23){
#   simVol[i, "RVd"] <- sqrt(sum(diff(simReturn[((i-2)*40):((i-1)*40),"ret"])^2))
# }
# 
# simVol[1:23,'RVw'] <- filter(simVol[1:23,'RVd'], rep(1/5, 5), sides=1)
# simVol[1:23,'RVm'] <- filter(simVol[1:23,'RVd'], rep(1/22, 22), sides=1)
# 
# # Check data
# head(simVol, n=25)
# 
# # Simulate returns for each day (2500 days) and compute daily, mweekly and monthly data
# c <- sumHAR$coefficients[1]; betad <- sumHAR$coefficients[2]; betaw <- sumHAR$coefficients[3]; betam <- sumHAR$coefficients[4]
# for(i in 24:2500){
#   # Simulated daily volatility
#   simDailyVol <- c + betad * simVol[i-1, "RVd"] + betaw * simVol[i-1, "RVw"] + betam * simVol[i-1, "RVm"] + rnorm(1, 0, sumHAR$sigma)
#   print(simDailyVol)
#   # Simulated returns (distribution of epsilon?)
#   simReturn[which(simReturn$day == i),"ret"] <- rnorm(40, 0, 0.1) * simDailyVol # replace with 1 or with sd of residuals
#   # Compute daily, weekly and monthly volatility
#   # Daily
#   if(i==24){
#     simVol[i, "RVd"] <- sqrt(sum(diff(simReturn[(which(simReturn$day == i)[1]):(which(simReturn$day == i)[1] + 39),"ret"])^2))
#   }else{
#     simVol[i, "RVd"] <- sqrt(sum(diff(simReturn[(which(simReturn$day == i)[1] - 1):(which(simReturn$day == i)[1] + 39),"ret"])^2))
#   }
#   # Weekly
#   simVol[i, "RVw"] <- mean(simVol[(i-4) : i, "RVd"])
#   # Monthly
#   simVol[i, "RVm"] <- mean(simVol[(i-21) : i, "RVd"])
# }
# 
# # Compare plots
# plot(realVol[,"RVd"], type = "l")
# plot(simVol[1251:2500,"RVd"], type = "l")
# 
# # Density plots to compare densities
# plot(density(realVol[,"RVd"]), xlim = c(0,3), ylim = c(0,2))
# par(new=T)
# plot(density(simVol[1251:2500,"RVd"]), xlim = c(0,3), ylim = c(0,2))
# 
# plot(density(realVol[,"RVw"], na.rm = T), xlim = c(0,3), ylim = c(0,2))
# par(new=T)
# plot(density(simVol[1251:2500,"RVw"]), xlim = c(0,3), ylim = c(0,2))
# 
# plot(density(realVol[,"RVm"], na.rm = T), xlim = c(0,2), ylim = c(0,4))
# par(new=T)
# plot(density(simVol[1251:2500,"RVm"]), xlim = c(0,2), ylim = c(0,4))
# 
# # Kurtosis of daily, weekly and monthly returns
# kurtosis(sim [2501:5000,"RVd"], na.rm=T)
# kurtosis(realVol[,"RVd"], na.rm=T)
# kurtosis(sim[2501:5000,"RVw"], na.rm=T)
# kurtosis(realVol[,"RVw"], na.rm=T)
# kurtosis(sim[2501:5000,"RVm"], na.rm=T)
# kurtosis(realVol[,"RVm"], na.rm=T)
# 
# ## Plot acf
# #acf(realVol[,'RVd'], )
# #acf(diff(realVol[,'RVd']), )

###### In-sample forecast (one-day ahead) #####
# In-sample evaluation (8 models, 7 measures)
comparisonInSample <- matrix(rep(NA, 40), c(8,5))
colnames(comparisonInSample) <- c("SSE", "RMSE", "MAE", "Rsquared", "QLIKE")
rownames(comparisonInSample) <- c("HAR-RV", "HARQ", "HARQF", "SHAR", "AR1", "AR3", "AR22", "ARFIMA")

##### In sample HAR(3)-RV ####
sumHAR$coefficients

forecastDataHAR3RV <- as.data.frame(cbind(realVol[21:1249,'Date'], realVol[21:1249,'RVd'], realVol[21:1249,'RVw'], realVol[21:1249,'RVm']))

colnames(forecastDataHAR3RV) <- c('Date', 'RVd', 'RVw', 'RVm')
forecastDataHAR3RV$forecast1d  <- sumHAR$coefficients[1] + sumHAR$coefficients[2] * Lag(x = forecastDataHAR3RV[,'RVd'], k = 1) +
  sumHAR$coefficients[3] * Lag(x = forecastDataHAR3RV[,'RVw'], k = 1) + 
  sumHAR$coefficients[4] * Lag(x = forecastDataHAR3RV[,'RVm'], k = 1)
colnames(forecastDataHAR3RV) <- c('Date', 'RVd', 'RVw', 'RVm', 'forecast')

# Plot estimates and residuals
plot(forecastDataHAR3RV[,'RVd'], type= 'l', ylim=c(0, 0.08), lty = 3)
par(new=T)
plot(forecastDataHAR3RV[,'forecast'], type='l',ylim=c(0, 0.08), col = 'red')

plot(forecastDataHAR3RV[,'RVd'] - forecastDataHAR3RV[,'forecast'], type='l', ylim=c(-0.02,0.08)) # residuals  

comparisonInSample["HAR-RV", "SSE"]   <- sum((forecastDataHAR3RV[30:1229,'RVd'] - forecastDataHAR3RV[30:1229,'forecast'])^2, na.rm=T) # sum of squared residuals
comparisonInSample["HAR-RV", "RMSE"]  <- sqrt(mean((forecastDataHAR3RV[30:1229,'RVd'] - forecastDataHAR3RV[30:1229,'forecast'])^2, na.rm=T))# RMSE
comparisonInSample["HAR-RV", "MAE"]   <- mean(abs((forecastDataHAR3RV[30:1229,'RVd'] - forecastDataHAR3RV[30:1229,'forecast'])), na.rm=T)# MAE
comparisonInSample["HAR-RV", "QLIKE"] <- mean(log(forecastDataHAR3RV[30:1229,'forecast'] + (forecastDataHAR3RV[30:1229,'RVd'] / forecastDataHAR3RV[30:1229,'forecast']))) # QLIKE

# R2
dataHARforR2 <- as.data.frame(cbind(forecastDataHAR3RV[,'forecast'], forecastDataHAR3RV[,'RVd']))
colnames(dataHARforR2) <- c("fore", "real")
comparisonInSample["HAR-RV", "Rsquared"] <- summary(lm('real ~ fore', dataHARforR2))$r.squared #R2 of Mincer-Zarnowitz regression

# Plot comparison actual and forecast
plot(forecastDataHAR3RV[,'forecast'] , type = 'l', lwd = 2, xlim = c(1,200), ylim = c(0,0.7), xlab ="", ylab = "")
par(new=TRUE)
plot(forecastDataHAR3RV[,'RVd'], type = 'l', xlim = c(1,200), ylim = c(0,0.7), lty = 3, xlab ="", ylab = "", main = "HAR-RV")

##### In sample HARQ ####
sumHARQ$coefficients

forecastDataHARQ <- as.data.frame(cbind(realVol[21:1249,'Date'], realVol[21:1249,'RVd'], realVol[21:1249,'RVw'], realVol[21:1249,'RVm'], realQuar[21:1249,'RQd']))

colnames(forecastDataHARQ) <- c('Date', 'RVd', 'RVw', 'RVm', 'RQd')
forecastDataHARQ$forecast1d  <- sumHARQ$coefficients[1] + 
                                sumHARQ$coefficients[2] * Lag(x = forecastDataHARQ[,'RVd'], k = 1) +
                                sumHARQ$coefficients[3] * Lag(x = forecastDataHARQ[,'RQd'], k = 1) * Lag(x = forecastDataHARQ[,'RVd'], k = 1) +
                                sumHARQ$coefficients[4] * Lag(x = forecastDataHARQ[,'RVw'], k = 1) + 
                                sumHARQ$coefficients[5] * Lag(x = forecastDataHARQ[,'RVm'], k = 1)
colnames(forecastDataHARQ) <- c('Date', 'RVd', 'RVw', 'RVm', 'RQd', 'forecast')

# Plot estimates and residuals
plot(forecastDataHARQ[,'RVd'], type= 'l', ylim=c(0, 20), lty = 3)
par(new=T)
plot(forecastDataHARQ[,'forecast'], type='l', ylim=c(0, 20), col = 'red')

plot(forecastDataHARQ[,'RVd'] - forecastDataHARQ[,'forecast'], type='l', ylim = c(-6, 10)) # residuals  

comparisonInSample["HARQ", "SSE"]  <- sum((forecastDataHARQ[30:1229,'RVd']       - forecastDataHARQ[30:1229,'forecast'])^2, na.rm=T) # sum of squared residuals
comparisonInSample["HARQ", "RMSE"] <- sqrt(mean((forecastDataHARQ[30:1229,'RVd'] - forecastDataHARQ[30:1229,'forecast'])^2, na.rm=T))# RMSE
comparisonInSample["HARQ", "MAE"]  <- mean(abs((forecastDataHARQ[30:1229,'RVd']  - forecastDataHARQ[30:1229,'forecast'])), na.rm=T)# MAE
comparisonInSample["HARQ", "QLIKE"] <- mean(log(forecastDataHARQ[c(30:642, 644:1229),'forecast'] + (forecastDataHARQ[c(30:642, 644:1229),'RVd'] / forecastDataHARQ[c(30:642, 644:1229),'forecast']))) # QLIKE

# R2
dataHARQforR2 <- as.data.frame(cbind(forecastDataHARQ[3:1229,'forecast'], forecastDataHARQ[3:1229,'RVd']))
colnames(dataHARQforR2) <- c("fore", "real")
comparisonInSample["HARQ", "Rsquared"] <- summary(lm('real ~ fore', dataHARQforR2))$r.squared #R2 of Mincer-Zarnowitz regression

# Plot comparison actual and forecast
plot(forecastDataHARQ[,'forecast'] , type = 'l', lwd = 2, xlim = c(1,200), ylim = c(0,20), xlab ="", ylab = "")
par(new=TRUE)
plot(forecastDataHARQ[,'RVd'], type = 'l', xlim = c(1,200), ylim = c(0,20), lty = 3, xlab ="", ylab = "", main = "HAR-RV")

##### In sample HARQF ####
sumHARQF$coefficients

forecastDataHARQF <- as.data.frame(cbind(realVol[21:1249,'Date'], realVol[21:1249,'RVd'], realVol[21:1249,'RVw'], realVol[21:1249,'RVm'],
                                         realQuar[21:1249,'RQd'], realQuar[21:1249,'RQw'], realQuar[21:1249,'RQm']))

colnames(forecastDataHARQF) <- c('Date', 'RVd', 'RVw', 'RVm', 'RQd', 'RQw', 'RQm')

forecastDataHARQF$forecast1d  <- sumHARQF$coefficients[1] + 
                                 sumHARQF$coefficients[2] * Lag(x = forecastDataHARQF[,'RVd'], k = 1) +
                                 sumHARQF$coefficients[3] * Lag(x = forecastDataHARQF[,'RQd'], k = 1) * Lag(x = forecastDataHARQF[,'RVd'], k = 1) +
                                 sumHARQF$coefficients[4] * Lag(x = forecastDataHARQF[,'RVw'], k = 1) + 
                                 sumHARQF$coefficients[5] * Lag(x = forecastDataHARQF[,'RQw'], k = 1) * Lag(x = forecastDataHARQF[,'RVw'], k = 1) +
                                 sumHARQF$coefficients[6] * Lag(x = forecastDataHARQF[,'RVm'], k = 1) +
                                 sumHARQF$coefficients[7] * Lag(x = forecastDataHARQF[,'RQm'], k = 1) * Lag(x = forecastDataHARQF[,'RVm'], k = 1)

colnames(forecastDataHARQF) <- c('Date', 'RVd', 'RVw', 'RVm', 'RQd', 'RQw', 'RQm', 'forecast')

# Plot estimates and residuals
plot(forecastDataHARQF[,'RVd'], type= 'l', ylim=c(0, 20), lty = 3)
par(new=T)
plot(forecastDataHARQF[,'forecast'], type='l', ylim=c(0, 20), col = 'red')

plot(forecastDataHARQF[,'RVd'] - forecastDataHARQF[,'forecast'], type='l', ylim = c(-6, 10)) # residuals  

comparisonInSample["HARQF", "SSE"]  <- sum((forecastDataHARQF[30:1229,'RVd']       - forecastDataHARQF[30:1229,'forecast'])^2, na.rm=T) # sum of squared residuals
comparisonInSample["HARQF", "RMSE"] <- sqrt(mean((forecastDataHARQF[30:1229,'RVd'] - forecastDataHARQF[30:1229,'forecast'])^2, na.rm=T))# RMSE
comparisonInSample["HARQF", "MAE"]  <- mean(abs((forecastDataHARQF[30:1229,'RVd']  - forecastDataHARQF[30:1229,'forecast'])), na.rm=T)# MAE
comparisonInSample["HARQF", "QLIKE"] <- mean(log(forecastDataHARQF[c(30:642, 644:1229),'forecast'] + (forecastDataHARQF[c(30:642, 644:1229),'RVd'] / forecastDataHARQF[c(30:642, 644:1229),'forecast']))) # QLIKE

# R2
dataHARQFforR2 <- as.data.frame(cbind(forecastDataHARQF[3:1229,'forecast'], forecastDataHARQF[3:1229,'RVd']))
colnames(dataHARQFforR2) <- c("fore", "real")
comparisonInSample["HARQF", "Rsquared"] <- summary(lm('real ~ fore', dataHARQFforR2))$r.squared #R2 of Mincer-Zarnowitz regression

# Plot comparison actual and forecast
plot(forecastDataHARQF[,'forecast'] , type = 'l', lwd = 2, xlim = c(1,200), ylim = c(0,20), xlab ="", ylab = "")
par(new=TRUE)
plot(forecastDataHARQF[,'RVd'], type = 'l', xlim = c(1,200), ylim = c(0,20), lty = 3, xlab ="", ylab = "", main = "HAR-RV")

##### In sample SHAR ####
sumSHAR$coefficients

forecastDataSHAR <- as.data.frame(cbind(realVol[21:1249,'Date'], realVol[21:1249,'RVd'], realVol[21:1249,'RVw'],
                                        realVol[21:1249,'RVm'], realSemiVar[21:1249, 'RSpos'], realSemiVar[21:1249, 'RSneg']))

colnames(forecastDataSHAR) <- c('Date', 'RVd', 'RVw', 'RVm', 'RSpos', 'RSneg')

forecastDataSHAR$forecast1d  <- sumSHAR$coefficients[1] + 
                                sumSHAR$coefficients[2] * Lag(x = forecastDataSHAR[,'RSpos'], k = 1) +
                                sumSHAR$coefficients[3] * Lag(x = forecastDataSHAR[,'RSneg'], k = 1) +
                                sumSHAR$coefficients[4] * Lag(x = forecastDataSHAR[,'RVw'], k = 1) + 
                                sumSHAR$coefficients[5] * Lag(x = forecastDataSHAR[,'RVm'], k = 1)

colnames(forecastDataSHAR) <- c('Date', 'RVd', 'RVw', 'RVm', 'RSpos', 'RSneg', 'forecast')

# Plot estimates and residuals
plot(forecastDataSHAR[,'RVd'], type= 'l', ylim=c(0, 20), lty = 3)
par(new=T)
plot(forecastDataSHAR[,'forecast'], type='l',ylim=c(0, 20), col = 'red')

plot(forecastDataSHAR[,'RVd'] - forecastDataSHAR[,'forecast'], type='l', ylim=c(-0.02,0.08)) # residuals  

comparisonInSample["SHAR", "SSE"] <- sum((forecastDataSHAR[30:1229,'RVd'] - forecastDataSHAR[30:1229,'forecast'])^2, na.rm=T) # sum of squared residuals
comparisonInSample["SHAR", "RMSE"] <- sqrt(mean((forecastDataSHAR[30:1229,'RVd'] - forecastDataSHAR[30:1229,'forecast'])^2, na.rm=T))# RMSE
comparisonInSample["SHAR", "MAE"] <- mean(abs((forecastDataSHAR[30:1229,'RVd'] - forecastDataSHAR[30:1229,'forecast'])), na.rm=T)# MAE
comparisonInSample["SHAR", "QLIKE"] <- mean(log(forecastDataSHAR[30:1229,'forecast'] + (forecastDataSHAR[30:1229,'RVd'] / forecastDataSHAR[30:1229,'forecast'])), na.rm = T) # QLIKE

# R2
dataSHARforR2 <- as.data.frame(cbind(forecastDataSHAR[,'forecast'], forecastDataSHAR[,'RVd']))
colnames(dataSHARforR2) <- c("fore", "real")
comparisonInSample["SHAR", "Rsquared"] <- summary(lm('real ~ fore', dataSHARforR2))$r.squared #R2 of Mincer-Zarnowitz regression

# Plot comparison actual and forecast
plot(forecastDataSHAR[,'forecast'] , type = 'l', lwd = 2, xlim = c(1,200), ylim = c(0, 20), xlab ="", ylab = "")
par(new=TRUE)
plot(forecastDataSHAR[,'RVd'], type = 'l', xlim = c(1,200), ylim = c(0,20), lty = 3, xlab ="", ylab = "", main = "HAR-RV")

##### In sample AR(1) #####
sumAR1$coef

forecastDataAR1 <- as.data.frame(cbind(realVol[21:1249,'Date'], realVol[21:1249,'RVd']))
colnames(forecastDataAR1) <- c('Date', 'RVd')
# forecast 1 day
forecastDataAR1$forecast1d  <- (sumAR1$coef[1])^1 * (Lag(x = forecastDataAR1[,'RVd'], k = 1)) # + sumAR1$coef[2]  
# forecast 1 week (5 days)
#forecastDataAR1$forecast1w  <- sumAR1$coef[2] + (sumAR1$coef[1])^5 * (Lag(x = forecastDataAR1[,'RVd'], k = 5))
# forecast 2 weeks (10 days)
#forecastDataAR1$forecast2w  <- sumAR1$coef[2] + (sumAR1$coef[1])^10 * (Lag(x = forecastDataAR1[,'RVd'], k = 10))
head(forecastDataAR1, n = 15)

names(forecastDataAR1) <- c('Date', 'RVd', 'forecast')

# Plot estimates and residuals
plot(forecastDataAR1[,'RVd'], type= 'l', ylim=c(0, 0.08), lty = 3)
par(new=T)
plot(forecastDataAR1[,'forecast'], type='l',ylim=c(0, 0.08), col = 'red')

plot(forecastDataAR1[,'RVd'] - forecastDataAR1[,'forecast'], type='l', ylim=c(-0.02,0.08)) # residuals  

comparisonInSample["AR1", "SSE"] <- sum((forecastDataAR1[30:1229,'RVd'] - forecastDataAR1[30:1229,'forecast'])^2, na.rm=T) # sum of squared residuals
comparisonInSample["AR1", "RMSE"] <- sqrt(mean((forecastDataAR1[30:1229,'RVd'] - forecastDataAR1[30:1229,'forecast'])^2, na.rm=T))# RMSE
comparisonInSample["AR1", "MAE"] <- mean(abs((forecastDataAR1[30:1229,'RVd'] - forecastDataAR1[30:1229,'forecast'])), na.rm=T)# MAE
comparisonInSample["AR1", "QLIKE"] <- mean(log(forecastDataAR1[30:1229,'forecast'] + (forecastDataAR1[30:1229,'RVd'] / forecastDataAR1[30:1229,'forecast']))[-c(1127, 1131)]) # QLIKE
# Remove infinite values (log(0)) in QLIKE
#which(is.infinite(log(forecastDataAR1[30:1229,'forecast'] + (forecastDataAR1[30:1229,'RVd'] / forecastDataAR1[30:1229,'forecast']))))

# R2
dataAR1forR2 <- as.data.frame(cbind(forecastDataAR1[,'forecast'], forecastDataAR1[,'RVd']))
colnames(dataAR1forR2) <- c("fore", "real")
comparisonInSample["AR1", "Rsquared"] <- summary(lm('real ~ fore', dataAR1forR2))$r.squared #R2 of Mincer-Zarnowitz regression

#### In sample AR(3) ####
sumAR3$coef

forecastDataAR3 <- as.data.frame(cbind(realVol[21:1249,'Date'], realVol[21:1249,'RVd']))
colnames(forecastDataAR3) <- c('Date', 'RVd')
matrixAR3 <- matrix(c(sumAR3$coef[1], 1, 0, sumAR3$coef[2], 0, 1, sumAR3$coef[3], 0, 0), 3, 3)

# forecast 1 day
forecastDataAR3$forecast1d  <-  ((matrixAR3 %^% 1)[1,1])^1 * (Lag(x = forecastDataAR3[,'RVd'], k = 1)) +
  ((matrixAR3 %^% 1)[1,2])^1 * (Lag(x = forecastDataAR3[,'RVd'], k = 2)) +
  ((matrixAR3 %^% 1)[1,3])^1 * (Lag(x = forecastDataAR3[,'RVd'], k = 3)) # + sumAR3$coef[4]
## forecast 1 week (5 days)
#forecastDataAR3$forecast1d  <- sumAR3$coef[4] + ((matrixAR3 %^% 5)[1,1])^1 * (Lag(x = forecastDataAR3[,'RVd'], k = 1) - sumAR3$coef[4]) +
#                                                ((matrixAR3 %^% 5)[1,2])^1 * (Lag(x = forecastDataAR3[,'RVd'], k = 2) - sumAR3$coef[4]) +
#                                                ((matrixAR3 %^% 5)[1,3])^1 * (Lag(x = forecastDataAR3[,'RVd'], k = 3) - sumAR3$coef[4])
## forecast 2 weeks (10 days)
#forecastDataAR3$forecast1d  <- sumAR3$coef[4] + ((matrixAR3 %^% 10)[1,1])^1 * (Lag(x = forecastDataAR3[,'RVd'], k = 1) - sumAR3$coef[4]) +
#                                                ((matrixAR3 %^% 10)[1,2])^1 * (Lag(x = forecastDataAR3[,'RVd'], k = 2) - sumAR3$coef[4]) +
#                                                ((matrixAR3 %^% 10)[1,3])^1 * (Lag(x = forecastDataAR3[,'RVd'], k = 3) - sumAR3$coef[4])
head(forecastDataAR3, n = 15)
colnames(forecastDataAR3) <- c('Date', 'RVd', 'forecast')

# Plot estimates and residuals
plot(forecastDataAR3[,'RVd'], type= 'l', ylim=c(0, 0.08), lty = 3)
par(new=T)
plot(forecastDataAR3[,'forecast'], type='l',ylim=c(0, 0.08), col = 'red')

plot(forecastDataAR3[,'RVd'] - forecastDataAR3[,'forecast'], type='l', ylim=c(-0.02,0.08)) # residuals  

comparisonInSample["AR3", "SSE"] <- sum((forecastDataAR3[30:1229,'RVd'] - forecastDataAR3[30:1229,'forecast'])^2, na.rm=T) # sum of squared residuals
comparisonInSample["AR3", "RMSE"] <- sqrt(mean((forecastDataAR3[30:1229,'RVd'] - forecastDataAR3[30:1229,'forecast'])^2, na.rm=T))# RMSE
comparisonInSample["AR3", "MAE"] <- mean(abs((forecastDataAR3[30:1229,'RVd'] - forecastDataAR3[30:1229,'forecast'])), na.rm=T)# MAE
comparisonInSample["AR3", "QLIKE"] <- mean(log(forecastDataAR3[30:1229,'forecast'] + (forecastDataAR3[30:1229,'RVd'] / forecastDataAR3[30:1229,'forecast']))) # QLIKE

# R2
dataAR3forR2 <- as.data.frame(cbind(forecastDataAR3[,'forecast'], forecastDataAR3[,'RVd']))
colnames(dataAR3forR2) <- c("fore", "real")
comparisonInSample["AR3", "Rsquared"] <- summary(lm('real ~ fore', dataAR3forR2))$r.squared #R2 of Mincer-Zarnowitz regression

# Plot comparison actual and forecast
plot(forecastDataAR3[,'forecast'] , type = 'l', lwd = 2, xlim = xlimOut, ylim = ylimOut, xlab ="", ylab = "")
par(new=TRUE)
plot(forecastDataAR3[,'RVd'], type = 'l', xlim = xlimOut, ylim = ylimOut, lty = 3, xlab ="", ylab = "", main = "AR(3)")

##### In sample AR(22) ####
sumAR22$coef
forecastDataAR22 <- as.data.frame(cbind(realVol[21:1249,'Date'], realVol[21:1249,'RVd']))
colnames(forecastDataAR22) <- c('Date', 'RVd')

matrixAR22 <- matrix(sumAR22$coef[1:22], 1, 22)

# forecast 1 day
forecastDataAR22$forecast1d  <- 
  ((matrixAR22)[1,1])^1 * (Lag(x = forecastDataAR22[,'RVd'], k = 1)) +
  ((matrixAR22)[1,2])^1 * (Lag(x = forecastDataAR22[,'RVd'], k = 2)) +
  ((matrixAR22)[1,3])^1 * (Lag(x = forecastDataAR22[,'RVd'], k = 3)) + 
  ((matrixAR22)[1,4])^1 * (Lag(x = forecastDataAR22[,'RVd'], k = 4)) +
  ((matrixAR22)[1,5])^1 * (Lag(x = forecastDataAR22[,'RVd'], k = 5)) +
  ((matrixAR22)[1,6])^1 * (Lag(x = forecastDataAR22[,'RVd'], k = 6)) +
  ((matrixAR22)[1,7])^1 * (Lag(x = forecastDataAR22[,'RVd'], k = 7)) +
  ((matrixAR22)[1,8])^1 * (Lag(x = forecastDataAR22[,'RVd'], k = 8)) +
  ((matrixAR22)[1,9])^1 * (Lag(x = forecastDataAR22[,'RVd'], k = 9)) +
  ((matrixAR22)[1,10])^1 * (Lag(x = forecastDataAR22[,'RVd'], k = 10)) +
  ((matrixAR22)[1,11])^1 * (Lag(x = forecastDataAR22[,'RVd'], k = 11)) +
  ((matrixAR22)[1,12])^1 * (Lag(x = forecastDataAR22[,'RVd'], k = 12)) +
  ((matrixAR22)[1,13])^1 * (Lag(x = forecastDataAR22[,'RVd'], k = 13)) +
  ((matrixAR22)[1,14])^1 * (Lag(x = forecastDataAR22[,'RVd'], k = 14)) +
  ((matrixAR22)[1,15])^1 * (Lag(x = forecastDataAR22[,'RVd'], k = 15)) +
  ((matrixAR22)[1,16])^1 * (Lag(x = forecastDataAR22[,'RVd'], k = 16)) +
  ((matrixAR22)[1,17])^1 * (Lag(x = forecastDataAR22[,'RVd'], k = 17)) +
  ((matrixAR22)[1,18])^1 * (Lag(x = forecastDataAR22[,'RVd'], k = 18)) +
  ((matrixAR22)[1,19])^1 * (Lag(x = forecastDataAR22[,'RVd'], k = 19)) +
  ((matrixAR22)[1,20])^1 * (Lag(x = forecastDataAR22[,'RVd'], k = 20)) +
  ((matrixAR22)[1,21])^1 * (Lag(x = forecastDataAR22[,'RVd'], k = 21)) +
  ((matrixAR22)[1,22])^1 * (Lag(x = forecastDataAR22[,'RVd'], k = 22)) # +  sumAR22$coef[23]

head(forecastDataAR22, n = 15)
colnames(forecastDataAR22) <- c('Date', 'RVd', 'forecast')

# Plot estimates and residuals
plot(forecastDataAR22[,'RVd'], type= 'l', ylim=c(0, 0.4), lty = 3)
par(new=T)
plot(forecastDataAR22[,'forecast'], type='l',ylim=c(0, 0.4), col = 'red')

plot(forecastDataAR22[,'RVd'] - forecastDataAR22[,'forecast'], type='l', ylim=c(-8,5)) # residuals  

comparisonInSample["AR22", "SSE"]   <- sum((forecastDataAR22[30:1229,'RVd'] - forecastDataAR22[30:1229,'forecast'])^2, na.rm=T) # sum of squared residuals
comparisonInSample["AR22", "RMSE"]  <- sqrt(mean((forecastDataAR22[30:1229,'RVd'] - forecastDataAR22[30:1229,'forecast'])^2, na.rm=T))# RMSE
comparisonInSample["AR22", "MAE"]   <- mean(abs((forecastDataAR22[30:1229,'RVd'] - forecastDataAR22[30:1229,'forecast'])), na.rm=T)# MAE
comparisonInSample["AR22", "QLIKE"] <- mean(log(forecastDataAR22[30:1229,'forecast'] + (forecastDataAR22[30:1229,'RVd'] / forecastDataAR22[30:1229,'forecast']))) # QLIKE

# R2
dataAR22forR2 <- as.data.frame(cbind(forecastDataAR22[,'forecast'], forecastDataAR22[,'RVd']))
colnames(dataAR22forR2) <- c("fore", "real")
comparisonInSample["AR22", "Rsquared"] <- summary(lm('real ~ fore', dataAR22forR2))$r.squared #R2 of Mincer-Zarnowitz regression

##### In sample ARFIMA #####
#predict.arfima(sumARFIMA)
sumARFIMA$ar
forecastDataARFIMA <- as.data.frame(cbind(realVol[21:1249,'Date'], realVol[21:1249,'RVd']))
colnames(forecastDataARFIMA) <- c('Date', 'RVd')

# forecast 1 day
forecastDataARFIMA$forecast1d <- forecast.fracdiff(sumARFIMA)$fitted
colnames(forecastDataARFIMA) <- c('Date', 'RVd', 'forecast')

# Plot estimates and residuals
plot(forecastDataARFIMA[,'RVd'], type= 'l', ylim=c(0, 0.4), lty = 3)
par(new=T)
plot(forecastDataARFIMA[,'forecast'], type='l',ylim=c(0, 0.4), col = 'red')

plot(forecastDataARFIMA[,'RVd'] - forecastDataARFIMA[,'forecast'], type='l', ylim=c(-0.2,0.3)) # residuals  

comparisonInSample["ARFIMA", "SSE"] <- sum((forecastDataARFIMA[30:1229,'RVd'] - forecastDataARFIMA[30:1229,'forecast'])^2, na.rm=T)         # sum of squared residuals
comparisonInSample["ARFIMA", "RMSE"] <- sqrt(mean((forecastDataARFIMA[30:1229,'RVd'] - forecastDataARFIMA[30:1229,'forecast'])^2, na.rm=T)) # RMSE
comparisonInSample["ARFIMA", "MAE"] <- mean(abs((forecastDataARFIMA[30:1229,'RVd'] - forecastDataARFIMA[30:1229,'forecast'])), na.rm=T)     # MAE
comparisonInSample["ARFIMA", "QLIKE"] <- mean(log(forecastDataARFIMA[30:1229,'forecast'] + (forecastDataARFIMA[30:1229,'RVd'] / forecastDataARFIMA[30:1229,'forecast']))) # QLIKE

# R2
dataARFIMAforR2 <- as.data.frame(cbind(forecastDataARFIMA[,'forecast'], forecastDataARFIMA[,'RVd']))
colnames(dataARFIMAforR2) <- c("fore", "real")
comparisonInSample["ARFIMA", "Rsquared"] <- summary(lm('real ~ fore', dataARFIMAforR2))$r.squared # R2 of Mincer-Zarnowitz regression

comparisonInSample
xtable(comparisonInSample, digits = 3)

# Boxplots for comparison of in-sample forecasting (forecasting error)
par(mar = c(2.1,5.1,1.1,1.1))
boxplot(cbind((forecastDataAR1[30:1229,'RVd'] - forecastDataAR1[30:1229,'forecast']),
              (forecastDataAR3[30:1229,'RVd'] - forecastDataAR3[30:1229,'forecast']),
              (forecastDataAR22[30:1229,'RVd'] - forecastDataAR22[30:1229,'forecast']),
              (forecastDataARFIMA[30:1229,'RVd'] - forecastDataARFIMA[30:1229,'forecast']),
              (forecastDataHAR3RV[30:1229,'RVd'] - forecastDataHAR3RV[30:1229,'forecast']),
              (forecastDataHARQ[30:1229,'RVd'] - forecastDataHARQ[30:1229,'forecast']),
              (forecastDataSHAR[30:1229,'RVd'] - forecastDataSHAR[30:1229,'forecast'])),
              ylim = c(-10, 10), names = c("AR(1)", "AR(3)", "AR(22)", "ARFIMA", "HAR-RV", "HARQ", "SHAR"), horizontal = T, las=1)
par(mar=c(5.1,4.1,4.1,2.1))

# Diebold Mariano test to compare forecasting differences
dm.test(forecastDataARFIMA[,'RVd'] - forecastDataARFIMA[,'forecast'], (forecastDataARFIMA[,'RVd'] - forecastDataHAR3RV[,'forecast']), 
        alternative= "two.sided", h=1, power=2)
dm.test(forecastDataARFIMA[,'RVd'] - forecastDataARFIMA[,'forecast'], (forecastDataARFIMA[,'RVd'] - forecastDataSHAR[,'forecast']), 
        alternative= "two.sided", h=1, power=2)
dm.test(forecastDataARFIMA[,'RVd'] - forecastDataARFIMA[,'forecast'], (forecastDataARFIMA[,'RVd'] - forecastDataHARQ[,'forecast']), 
        alternative= "two.sided", h=1, power=2)
dm.test(forecastDataARFIMA[,'RVd'] - forecastDataARFIMA[,'forecast'], (forecastDataARFIMA[,'RVd'] - forecastDataHARQF[,'forecast']), 
        alternative= "two.sided", h=1, power=2)

# Plot in-sample fit
setylim <- c(0,25)
plot(forecastDataARFIMA[30:1229,'RVd'], type = 'l', ylim = setylim, ylab = "") # original
par(new = T); plot(forecastDataAR1[30:1229,'forecast'], type = 'l', ylim = setylim, col = 'red', ylab = "")
par(new = T); plot(forecastDataAR3[30:1229,'forecast'], type = 'l', ylim = setylim, col = 'green', ylab = "")
par(new = T); plot(forecastDataAR22[30:1229,'forecast'], type = 'l', ylim = setylim, col = 'blue', ylab = "")
par(new = T); plot(forecastDataARFIMA[30:1229,'forecast'], type = 'l', ylim = setylim, col = 'yellow', ylab = "")
par(new = T); plot(forecastDataHAR3RV[30:1229,'forecast'], type = 'l', ylim = setylim, col = 'brown', ylab = "")
par(new = T); plot(forecastDataHARQ[30:1229,'forecast'], type = 'l', ylim = setylim, col = 'orange', ylab = "")
par(new = T); plot(forecastDataHARQF[30:1229,'forecast'], type = 'l', ylim = setylim, col = 'grey', ylab = "Daily realized volatility")
# Add legend
legend(650,26, c("Original", "AR1", "AR3", "AR22", "ARFIMA", "HAR", "HARQ", "HARQF"), lty= rep(1, 8), lwd=rep(2.5, 8), 
       col=c('black', 'red', 'green', 'blue', 'yellow', 'brown', 'orange', 'grey'))

##### Out-of-sample forecast #####
# Out of sample evaluation (8 models, 12 measures)
comparisonOutSample <- matrix(rep(NA, 8 * 12), c(8, 12))
colnames(comparisonOutSample) <- c("RMSE1d", "MAE1d", "Rsquared1d", "QLIKE1d",
                                   "RMSE1w", "MAE1w", "Rsquared1w", "QLIKE1w",
                                   "RMSE1m", "MAE1m", "Rsquared1m", "QLIKE1m")
rownames(comparisonOutSample) <- c("HAR-RV", "HARQ", "HARQF", "SHAR", "AR1", "AR3", "AR22", "ARFIMA")

# Start value for out-of-sample forecasts
k                     <- 200  # length of window in days to estimate model, max 1230 - lengthForeOut
lengthForeOut         <- 200
startOFSF             <- 1240 - lengthForeOut +1 # start of forecasting window is last datapoint minuslengthForecastWindow
lastDayForeOut        <- 1240

foreOutHor            <- startOFSF:lastDayForeOut #(1041:1240)
estOutHor             <- (startOFSF - k):(startOFSF - 1) #(916:1040)

# Limits for plots
xlimOut <- c(1,200)
ylimOut <- c(0,30)

# Plot of window of values to be forecasted
plot(dataHAR[foreOutHor, "RVd"], type = 'l', xlim = xlimOut, ylim = ylimOut, lty = 3, xlab ="", ylab = "", 
     main = "Daily realized volatility in forecasting window")

# (parameter estimation based on previous k observations daily), from startOFSF

##### Out-of-sample HAR-RV ####
forecastOutHARRVd <- as.data.frame(matrix(NA, lengthForeOut, 10)) # Daily realized volalitiy
colnames(forecastOutHARRVd) <- c("1d", "2d", "3d", "4d", "1w", "6d", "7d", "8d", "9d", "2w") # 1-10 days ahead
forecastOutHARRVw <- as.data.frame(matrix(NA, lengthForeOut, 10)) # Weekly realized volalitiy
colnames(forecastOutHARRVw) <- c("1d", "2d", "3d", "4d", "1w", "6d", "7d", "8d", "9d", "2w") # 1-10 days ahead
forecastOutHARRVm <- as.data.frame(matrix(NA, lengthForeOut, 10)) # Monthly realized volalitiy
colnames(forecastOutHARRVm) <- c("1d", "2d", "3d", "4d", "1w", "6d", "7d", "8d", "9d", "2w") # 1-10 days ahead

dataHAR <- as.data.frame(dataHAR)
 
for (i in 1:lengthForeOut){ # repeat lengthForeOut times, reestimate each model based on previous k obs. and forecast 1d, 1w, 2w ahead
  # Daily RV
  # Reestimate model
  sumHARtemp <- summary(lm(RVd1 ~ RVd + RVw + RVm, data = as.data.frame(dataHAR[estOutHor - 1 + i,]))) # startOFSF - k until startOFSF, k obs.
  # Day 1
  forecastOutHARRVd[i, 1]    <-   sumHARtemp$coefficients[1] + 
                                  sumHARtemp$coefficients[2] * dataHAR[(startOFSF - 2 + i), "RVd"] +
                                  sumHARtemp$coefficients[3] * dataHAR[(startOFSF - 2 + i), "RVw"] + 
                                  sumHARtemp$coefficients[4] * dataHAR[(startOFSF - 2 + i), "RVm"] # daily 
  #forecastOutHARRVw[i, 1]    <-   mean(c(dataHAR[(startOFSF - 1 + i - 4):(startOFSF - 1 + i), "RVd"], forecastOutHARRVd[i, 1])) # weekly
  #forecastOutHARRVm[i, 1]    <-   mean(c(dataHAR[(startOFSF - 1 + i - 20):(startOFSF - 1 + i), "RVd"], forecastOutHARRVd[i, 1])) # monthly
  
  # Weekly RV
  # Reestimate model
  sumHARtemp <- summary(lm(RVw1 ~ RVd + RVw + RVm, data = as.data.frame(dataHAR[estOutHor - 1 + i,]))) # startOFSF - k until startOFSF, k obs.
  # Day 1
  forecastOutHARRVw[i, 1]    <-   sumHARtemp$coefficients[1] + 
    sumHARtemp$coefficients[2] * dataHAR[(startOFSF - 2 + i), "RVd"] +
    sumHARtemp$coefficients[3] * dataHAR[(startOFSF - 2 + i), "RVw"] + 
    sumHARtemp$coefficients[4] * dataHAR[(startOFSF - 2 + i), "RVm"] # daily 

  # Monthly RV
  # Reestimate model
  sumHARtemp <- summary(lm(RVm1 ~ RVd + RVw + RVm, data = as.data.frame(dataHAR[estOutHor - 1 + i,]))) # startOFSF - k until startOFSF, k obs.
  # Day 1
  forecastOutHARRVm[i, 1]    <-   sumHARtemp$coefficients[1] + 
    sumHARtemp$coefficients[2] * dataHAR[(startOFSF - 2 + i), "RVd"] +
    sumHARtemp$coefficients[3] * dataHAR[(startOFSF - 2 + i), "RVw"] + 
    sumHARtemp$coefficients[4] * dataHAR[(startOFSF - 2 + i), "RVm"] # daily 

  # Predict 1 day, 3 days, 1 week and 2 weeks ahead (1-10 days)
  # # Day 2-10
  # for(j in 2:10){ # use previous estimate to continue
  #   forecastOutHARRVd[i, j]  <-  sumHARtemp$coefficients[1] + 
  #                                sumHARtemp$coefficients[2] * forecastOutHARRVd[i, j-1] +
  #                                sumHARtemp$coefficients[3] * forecastOutHARRVw[i, j-1] + 
  #                                sumHARtemp$coefficients[4] * forecastOutHARRVm[i, j-1]
  #   if (j < 6){
  #     forecastOutHARRVw[i, j]  <-  mean(c(dataHAR[(startOFSF - 2 + i - (5-j)):(startOFSF - 2 + i), "RVd"], as.numeric(forecastOutHARRVd[i, 1:(j-1)])))
  #   }else{
  #     forecastOutHARRVw[i, j]  <-  mean(c(as.numeric(forecastOutHARRVd[i, (j-5):(j-1)])))
  #   }
  #   
  #   forecastOutHARRVm[i, j]  <-  mean(c(dataHAR[(startOFSF - 2 + i - (21-j)):(startOFSF - 2 + i), "RVd"], as.numeric(forecastOutHARRVd[i, 1:j-1])))
  # }
  
}

# Evaluate RMSE, MAE and R2
# 1 day ahead
comparisonOutSample["HAR-RV", "RMSE1d"] <- sqrt(mean((forecastOutHARRVd[,'1d'] - dataHAR[foreOutHor,"RVd"])^2))   # RMSE
comparisonOutSample["HAR-RV", "MAE1d"]  <- mean(abs(forecastOutHARRVd[,'1d']   - dataHAR[foreOutHor,"RVd"]))      # MAE

# 1 week ahead
comparisonOutSample["HAR-RV", "RMSE1w"] <- sqrt(mean((forecastOutHARRVm[,'1d'] - dataHAR[foreOutHor,"RVw"])^2))   # RMSE
comparisonOutSample["HAR-RV", "MAE1w"]  <- mean(abs(forecastOutHARRVm[,'1d']   - dataHAR[foreOutHor,"RVw"]))      # MAE

# 1 month ahead
comparisonOutSample["HAR-RV", "RMSE1m"] <- sqrt(mean((forecastOutHARRVm[,'1d'] - dataHAR[foreOutHor,"RVm"])^2))   # RMSE
comparisonOutSample["HAR-RV", "MAE1m"]  <- mean(abs(forecastOutHARRVm[,'1d']   - dataHAR[foreOutHor,"RVm"]))      # MAE

# QLIKE
comparisonOutSample["HAR-RV", "QLIKE1d"] <- mean(log(abs(forecastOutHARRVd[,'1d'] + (dataHAR[foreOutHor,"RVd"] / forecastOutHARRVd[,'1d'])))) # QLIKE
comparisonOutSample["HAR-RV", "QLIKE1w"] <- mean(log(abs(forecastOutHARRVw[,'1d'] + (dataHAR[foreOutHor,"RVw"] / forecastOutHARRVw[,'1d'])))) # QLIKE
comparisonOutSample["HAR-RV", "QLIKE1m"] <- mean(log(abs(forecastOutHARRVm[,'1d'] + (dataHAR[foreOutHor,"RVm"] / forecastOutHARRVm[,'1d'])))) # QLIKE

# R2
# 1 day ahead
dataHARforR2 <- as.data.frame(cbind(forecastOutHARRVd[,'1d'], dataHAR[foreOutHor,"RVd"],
                                    forecastOutHARRVw[,'1d'], dataHAR[foreOutHor,"RVw"],
                                    forecastOutHARRVm[,'1d'], dataHAR[foreOutHor,"RVm"]))
colnames(dataHARforR2) <- c("fore1d", "real1d", "fore1w", "real1w", "fore1m", "real1m")
comparisonOutSample["HAR-RV", "Rsquared1d"] <- summary(lm('real1d ~ fore1d', dataHARforR2))$r.squared #R2 of Mincer-Zarnowitz regression
comparisonOutSample["HAR-RV", "Rsquared1w"] <- summary(lm('real1w ~ fore1w', dataHARforR2))$r.squared #R2 of Mincer-Zarnowitz regression
comparisonOutSample["HAR-RV", "Rsquared1m"] <- summary(lm('real1m ~ fore1m', dataHARforR2))$r.squared #R2 of Mincer-Zarnowitz regression

# Plot comparison actual and forecast
plot(forecastOutHARRVd[,'1d'] , type = 'l', lwd = 2, xlim = xlimOut, ylim = ylimOut, xlab ="", ylab = "")
par(new=TRUE)
plot(dataHAR[foreOutHor,"RVd"], type = 'l', xlim = xlimOut, ylim = ylimOut, lty = 3, xlab ="", ylab = "", main = "HAR-RV - 1 day")

# plot(forecastOutHARRVd[,'1w'] , type = 'l', lwd = 2, xlim = c(1,200), ylim = c(0.0, 6.0), xlab ="", ylab = "")
# par(new=TRUE)
# plot(dataHAR[(startOFSF + 5):(startOFSF + 199 + 5),"RVd"], type = 'l', xlim = c(1,200), ylim = c(0, 6.0), lty = 3, xlab ="", ylab = "", main = "HAR-RV - 1 week")
# 
# plot(forecastOutHARRVd[,'2w'] , type = 'l', lwd = 2, xlim = c(1,200), ylim = c(0.0, 6.0), xlab ="", ylab = "")
# par(new=TRUE)
# plot(dataHAR[(startOFSF + 10):(startOFSF + 199 + 10),"RVd"], type = 'l', xlim = c(1,200), ylim = c(0, 6.0), lty = 3, xlab ="", ylab = "", main = "HAR-RV - 2 weeks")

##### Out-of-sample HARQ ####
forecastOutHARQRVd <- as.data.frame(matrix(NA, 200, 10))
colnames(forecastOutHARQRVd) <- c("1d", "2d", "3d", "4d", "1w", "6d", "7d", "8d", "9d", "2w") # Real vol 1 day
forecastOutHARQRVw <- as.data.frame(matrix(NA, 200, 10))
colnames(forecastOutHARQRVw) <- c("1d", "2d", "3d", "4d", "1w", "6d", "7d", "8d", "9d", "2w") # Real vol 1 week
forecastOutHARQRVm <- as.data.frame(matrix(NA, 200, 10)) 
colnames(forecastOutHARQRVm) <- c("1d", "2d", "3d", "4d", "1w", "6d", "7d", "8d", "9d", "2w") # Real vol 1 month

dataHARQ <- as.data.frame(dataHARQ)

#k = 125 # max 980, length of window in days to estimate model

for (i in 1:lengthForeOut){ # repeat lengthForeOut times, reestimate each model based on previous k obs. and forecast 1d, 1w, 2w
  # Daily RV
  # Restimate model
  sumHARQtemp <- summary(lm(RVd1 ~ RVd + RQRVd + RVw + RVm, data = as.data.frame(dataHARQ[estOutHor - 1 + i,]))) # startOFSF - k until startOFSF, k obs.
  
  # Day 1
  forecastOutHARQRVd[i, 1]  <- sumHARQtemp$coefficients[1] + 
    sumHARQtemp$coefficients[2] * dataHARQ[(startOFSF - 2 + i),'RVd'] +
    sumHARQtemp$coefficients[3] * dataHARQ[(startOFSF - 2 + i),'RQRVd'] +
    sumHARQtemp$coefficients[4] * dataHARQ[(startOFSF - 2 + i),'RVw'] + 
    sumHARQtemp$coefficients[5] * dataHARQ[(startOFSF - 2 + i),'RVm']
  #forecastOutHARQRVw[i, 1]    <-   mean(c(dataHAR[(startOFSF - 1 + i - 4):(startOFSF - 1 + i), "RVd"], forecastOutHARRVd[i, 1])) # weekly
  #forecastOutHARQRVm[i, 1]    <-   mean(c(dataHAR[(startOFSF - 1 + i - 20):(startOFSF - 1 + i), "RVd"], forecastOutHARRVd[i, 1])) # monthly
  
  # # Weekly RV
  # # Restimate model
  # sumHARQtemp <- summary(lm(RVw1 ~ RVd + RQRVd + RVw + RVm, data = as.data.frame(dataHARQ[estOutHor - 1 + i,]))) # startOFSF - k until startOFSF, k obs.
  # 
  # # Day 1
  # forecastOutHARQRVw[i, 1]  <- sumHARQtemp$coefficients[1] + 
  #   sumHARQtemp$coefficients[2] * dataHARQ[(startOFSF - 2 + i),'RVd'] +
  #   sumHARQtemp$coefficients[3] * dataHARQ[(startOFSF - 2 + i),'RQRVd'] + #* dataHARQ[(startOFSF - 2 + i),'RVd'] +
  #   sumHARQtemp$coefficients[4] * dataHARQ[(startOFSF - 2 + i),'RVw'] + 
  #   sumHARQtemp$coefficients[5] * dataHARQ[(startOFSF - 2 + i),'RVm']
  # 
  # # Monthly RV
  # # Restimate model
  # sumHARQtemp <- summary(lm(RVm1 ~ RVd + RQRVd + RVw + RVm, data = as.data.frame(dataHARQ[estOutHor - 1 + i,]))) # startOFSF - k until startOFSF, k obs.
  # 
  # # Day 1
  # forecastOutHARQRVm[i, 1]  <- sumHARQtemp$coefficients[1] + 
  #   sumHARQtemp$coefficients[2] * dataHARQ[(startOFSF - 2 + i),'RVd'] +
  #   sumHARQtemp$coefficients[3] * dataHARQ[(startOFSF - 2 + i),'RQRVd'] + #* dataHARQ[(startOFSF - 2 + i),'RVd'] +
  #   sumHARQtemp$coefficients[4] * dataHARQ[(startOFSF - 2 + i),'RVw'] + 
  #   sumHARQtemp$coefficients[5] * dataHARQ[(startOFSF - 2 + i),'RVm']
  # 
  # Option 2, HARQ-h
  # Weekly RV
  # Restimate model
  sumHARQtemp <- summary(lm(RVw1 ~ RVd + RQRVw + RVw + RVm, data = as.data.frame(dataHARQ[estOutHor - 1 + i,]))) # startOFSF - k until startOFSF, k obs.
  
  # Day 1
  forecastOutHARQRVw[i, 1]  <- sumHARQtemp$coefficients[1] + 
    sumHARQtemp$coefficients[2] * dataHARQ[(startOFSF - 2 + i),'RVd'] +
    sumHARQtemp$coefficients[3] * dataHARQ[(startOFSF - 2 + i),'RQRVw'] + #* dataHARQ[(startOFSF - 2 + i),'RVd'] +
    sumHARQtemp$coefficients[4] * dataHARQ[(startOFSF - 2 + i),'RVw'] + 
    sumHARQtemp$coefficients[5] * dataHARQ[(startOFSF - 2 + i),'RVm']
  
  # Monthly RV
  # Restimate model
  sumHARQtemp <- summary(lm(RVm1 ~ RVd + RQRVm + RVw + RVm, data = as.data.frame(dataHARQ[estOutHor - 1 + i,]))) # startOFSF - k until startOFSF, k obs.
  
  # Day 1
  forecastOutHARQRVm[i, 1]  <- sumHARQtemp$coefficients[1] + 
    sumHARQtemp$coefficients[2] * dataHARQ[(startOFSF - 2 + i),'RVd'] +
    sumHARQtemp$coefficients[3] * dataHARQ[(startOFSF - 2 + i),'RQRVm'] + #* dataHARQ[(startOFSF - 2 + i),'RVd'] +
    sumHARQtemp$coefficients[4] * dataHARQ[(startOFSF - 2 + i),'RVw'] + 
    sumHARQtemp$coefficients[5] * dataHARQ[(startOFSF - 2 + i),'RVm']  
}

# Evaluate RMSE, MAE and R2
# 1 day ahead
comparisonOutSample["HARQ", "RMSE1d"] <- sqrt(mean((forecastOutHARQRVd[,'1d'] - dataHARQ[foreOutHor,"RVd"])^2))   # RMSE
comparisonOutSample["HARQ", "MAE1d"]  <- mean(abs(forecastOutHARQRVd[,'1d']   - dataHARQ[foreOutHor,"RVd"]))      # MAE
comparisonOutSample["HARQ", "QLIKE1d"] <- mean(log(abs(forecastOutHARQRVd[,'1d'] + (dataHAR[foreOutHor,"RVd"] / forecastOutHARQRVd[,'1d'])))) # QLIKE

# 1 week ahead
comparisonOutSample["HARQ", "RMSE1w"] <- sqrt(mean((forecastOutHARQRVw[,'1d'] - dataHARQ[foreOutHor,"RVw"])^2))   # RMSE
comparisonOutSample["HARQ", "MAE1w"]   <- mean(abs(forecastOutHARQRVw[,'1d']   - dataHARQ[foreOutHor,"RVw"]))      # MAE
comparisonOutSample["HARQ", "QLIKE1w"] <- mean(log(forecastOutHARQRVw[,'1d'] + (dataHAR[foreOutHor,"RVw"] / forecastOutHARQRVw[,'1d']))) # QLIKE

# 1 month ahead
comparisonOutSample["HARQ", "RMSE1m"]  <- sqrt(mean((forecastOutHARQRVm[,'1d'] - dataHARQ[foreOutHor,"RVm"])^2))   # RMSE
comparisonOutSample["HARQ", "MAE1m"]   <- mean(abs(forecastOutHARQRVm[,'1d']   - dataHARQ[foreOutHor,"RVm"]))      # MAE
comparisonOutSample["HARQ", "QLIKE1m"] <- mean(log(forecastOutHARQRVm[,'1d']  + (dataHAR[foreOutHor,"RVm"] / forecastOutHARQRVm[,'1d']))) # QLIKE

# R2
dataHARQforR2 <- as.data.frame(cbind(forecastOutHARQRVd[,'1d'], dataHARQ[foreOutHor,"RVd"], 
                                     forecastOutHARQRVw[,'1d'], dataHARQ[foreOutHor,"RVw"],
                                     forecastOutHARQRVm[,'1d'], dataHARQ[foreOutHor,"RVm"]))
colnames(dataHARQforR2) <- c("fore1d", "real1d", "fore1w", "real1w", "fore1m", "real1m")
comparisonOutSample["HARQ", "Rsquared1d"] <- summary(lm('real1d ~ fore1d', dataHARQforR2))$r.squared #R2 of Mincer-Zarnowitz regression
comparisonOutSample["HARQ", "Rsquared1w"] <- summary(lm('real1w ~ fore1w', dataHARQforR2))$r.squared #R2 of Mincer-Zarnowitz regression
comparisonOutSample["HARQ", "Rsquared1m"] <- summary(lm('real1m ~ fore1m', dataHARQforR2))$r.squared #R2 of Mincer-Zarnowitz regression

# Plot comparison actual and forecast
plot(forecastOutHARQRVd[,'1d'] , type = 'l', lwd = 2, xlim = xlimOut, ylim = ylimOut, xlab ="", ylab = "")
par(new=TRUE)
plot(dataHARQ[foreOutHor,"RVd"], type = 'l', xlim = xlimOut, ylim = ylimOut, lty = 3, xlab ="", ylab = "", main = "HARQ - 1 day")

# ##### Out-of-sample HARQF ####
# forecastOutHARQFRVd <- as.data.frame(matrix(NA, 200, 10))
# colnames(forecastOutHARQFRVd) <- c("1d", "2d", "3d", "4d", "1w", "6d", "7d", "8d", "9d", "2w") # Real vol 1 day
# 
# dataHARQF <- as.data.frame(dataHARQ)
# 
# #k = 125 # max 980, length of window in days to estimate model
# 
# for (i in 1:lengthForeOut){ # repeat lengthForeOut times, reestimate each model based on previous k obs. and forecast 1d, 1w, 2w
#   # Estimate model
#   sumHARQFtemp <- summary(lm(RVd1 ~ RVd + RQRVd + RVw + RQRVw + RVm + RQRVm, data = as.data.frame(dataHARQ[estOutHor - 1 + i,]))) # startOFSF - k until startOFSF, k obs.
#   
#   # Predict 1 day, 1 week and 2 weeks ahead (1-10 days)
#   # Day 1
#   forecastOutHARQFRVd[i, 1]  <- sumHARQFtemp$coefficients[1] + 
#     sumHARQFtemp$coefficients[2] * dataHARQF[(startOFSF - 2 + i),'RVd'] +
#     sumHARQFtemp$coefficients[3] * dataHARQF[(startOFSF - 2 + i),'RQd'] * dataHARQF[(startOFSF - 2 + i),'RVd'] +
#     sumHARQFtemp$coefficients[4] * dataHARQF[(startOFSF - 2 + i),'RVw'] + 
#     sumHARQFtemp$coefficients[5] * dataHARQF[(startOFSF - 2 + i),'RQw'] * dataHARQF[(startOFSF - 2 + i),'RVw'] +
#     sumHARQFtemp$coefficients[6] * dataHARQF[(startOFSF - 2 + i),'RVm'] +
#     sumHARQFtemp$coefficients[7] * dataHARQF[(startOFSF - 2 + i),'RQm'] * dataHARQF[(startOFSF - 2 + i),'RVm']
#   }
# 
# # Evaluate RMSE, MAE and R2
# # 1 day ahead
# comparisonOutSample["HARQF", "RMSE1d"] <- sqrt(mean((forecastOutHARQFRVd[,'1d'] - dataHARQF[foreOutHor,"RVd"])^2))   # RMSE
# comparisonOutSample["HARQF", "MAE1d"]  <- mean(abs(forecastOutHARQFRVd[,'1d']   - dataHARQF[foreOutHor,"RVd"]))      # MAE
# 
# # QLIKE
# comparisonOutSample["HARQF", "QLIKE1d"] <- mean(log(forecastOutHARQFRVd[,'1d'] + (dataHAR[foreOutHor,"RVd"] / forecastOutHARQFRVd[,'1d']))) # QLIKE
# 
# # R2
# # 1 day ahead
# dataHARQFforR2 <- as.data.frame(cbind(forecastOutHARQFRVd[,'1d'], dataHARQF[foreOutHor,"RVd"],
#                                       forecastOutHARQFRVd[,'3d'], dataHARQF[foreOutHor + 2,"RVd"],
#                                       forecastOutHARQFRVd[,'1w'], dataHARQF[foreOutHor + 4,"RVd"],
#                                       forecastOutHARQFRVd[,'2w'], dataHARQF[foreOutHor + 9,"RVd"]))
# colnames(dataHARQFforR2) <- c("fore1d", "real1d", "fore1w", "real1w", "fore2w", "real2w")
# comparisonOutSample["HARQF", "Rsquared1d"] <-  summary(lm('real1d ~ fore1d', dataHARQFforR2))$r.squared #R2 of Mincer-Zarnowitz regression
# 
# # Plot comparison actual and forecast
# plot(forecastOutHARQFRVd[,'1d'], type = 'l', lwd = 2, xlim = xlimOut, ylim = ylimOut, xlab ="", ylab = "")
# par(new=TRUE)
# plot(dataHARQ[foreOutHor,"RVd"], type = 'l', xlim = xlimOut, ylim = ylimOut, lty = 3, xlab ="", ylab = "", main = "HARQF - 1 day")

##### Out-of-sample SHAR ####
forecastOutSHARRVd <- as.data.frame(matrix(NA, 200, 10))
colnames(forecastOutSHARRVd) <- c("1d", "2d", "3d", "4d", "1w", "6d", "7d", "8d", "9d", "2w") # Real vol 1 day
forecastOutSHARRVw <- as.data.frame(matrix(NA, 200, 10))
colnames(forecastOutSHARRVw) <- c("1d", "2d", "3d", "4d", "1w", "6d", "7d", "8d", "9d", "2w") # Real vol 1 week
forecastOutSHARRVm <- as.data.frame(matrix(NA, 200, 10))
colnames(forecastOutSHARRVm) <- c("1d", "2d", "3d", "4d", "1w", "6d", "7d", "8d", "9d", "2w") # Real vol 1 month

dataSHAR <- as.data.frame(dataSHAR)

#k = 125 # max 980, length of window in days to estimate model

for (i in 1:lengthForeOut){ # repeat lengthForeOut times, reestimate each model based on previous k obs. and forecast 1d, 1w, 2w
  # Daily RV
  # Estimate model
  sumSHARtemp <- summary(lm(RVd1 ~ RSpos + RSneg + RVw + RVm, data = as.data.frame(dataSHAR[estOutHor - 1 + i,]))) # startOFSF - k until startOFSF, k obs.
  
  # Day 1
  forecastOutSHARRVd[i, 1] <- sumSHAR$coefficients[1] + 
    sumSHARtemp$coefficients[2] * dataSHAR[(startOFSF - 2 + i),'RSpos'] +
    sumSHARtemp$coefficients[3] * dataSHAR[(startOFSF - 2 + i),'RSneg'] +
    sumSHARtemp$coefficients[4] * dataSHAR[(startOFSF - 2 + i),'RVw'] + 
    sumSHARtemp$coefficients[5] * dataSHAR[(startOFSF - 2 + i),'RVm']
  
  # Daily RV
  # Estimate model
  sumSHARtemp <- summary(lm(RVw1 ~ RSpos + RSneg + RVw + RVm, data = as.data.frame(dataSHAR[estOutHor - 1 + i,]))) # startOFSF - k until startOFSF, k obs.
  
  # Day 1
  forecastOutSHARRVw[i, 1] <- sumSHAR$coefficients[1] + 
    sumSHARtemp$coefficients[2] * dataSHAR[(startOFSF - 2 + i),'RSpos'] +
    sumSHARtemp$coefficients[3] * dataSHAR[(startOFSF - 2 + i),'RSneg'] +
    sumSHARtemp$coefficients[4] * dataSHAR[(startOFSF - 2 + i),'RVw'] + 
    sumSHARtemp$coefficients[5] * dataSHAR[(startOFSF - 2 + i),'RVm']
  
  # Daily RV
  # Estimate model
  sumSHARtemp <- summary(lm(RVm1 ~ RSpos + RSneg + RVw + RVm, data = as.data.frame(dataSHAR[estOutHor - 1 + i,]))) # startOFSF - k until startOFSF, k obs.
  
  # Day 1
  forecastOutSHARRVm[i, 1] <- sumSHAR$coefficients[1] + 
    sumSHARtemp$coefficients[2] * dataSHAR[(startOFSF - 2 + i),'RSpos'] +
    sumSHARtemp$coefficients[3] * dataSHAR[(startOFSF - 2 + i),'RSneg'] +
    sumSHARtemp$coefficients[4] * dataSHAR[(startOFSF - 2 + i),'RVw'] + 
    sumSHARtemp$coefficients[5] * dataSHAR[(startOFSF - 2 + i),'RVm']
}

# Evaluate RMSE, MAE and R2
# 1 day ahead
comparisonOutSample["SHAR", "RMSE1d"] <- sqrt(mean((forecastOutSHARRVd[,'1d'] - dataSHAR[foreOutHor,"RVd"])^2))   # RMSE
comparisonOutSample["SHAR", "MAE1d"]  <- mean(abs(forecastOutSHARRVd[,'1d']   - dataSHAR[foreOutHor,"RVd"]))      # MAE
comparisonOutSample["SHAR", "QLIKE1d"] <- mean(log(abs(forecastOutSHARRVd[,'1d'] + (dataHAR[foreOutHor,"RVd"] / forecastOutSHARRVd[,'1d'])))) # QLIKE

# 1 week ahead
comparisonOutSample["SHAR", "RMSE1w"] <- sqrt(mean((forecastOutSHARRVw[,'1d'] - dataSHAR[foreOutHor,"RVw"])^2))   # RMSE
comparisonOutSample["SHAR", "MAE1w"]   <- mean(abs(forecastOutSHARRVw[,'1d']   - dataSHAR[foreOutHor,"RVw"]))      # MAE
comparisonOutSample["SHAR", "QLIKE1w"] <- mean(log(forecastOutSHARRVw[,'1d'] + (dataHAR[foreOutHor,"RVw"] / forecastOutSHARRVw[,'1d']))) # QLIKE

# 1 month ahead
comparisonOutSample["SHAR", "RMSE1m"]  <- sqrt(mean((forecastOutSHARRVm[,'1d'] - dataSHAR[foreOutHor,"RVm"])^2))   # RMSE
comparisonOutSample["SHAR", "MAE1m"]   <- mean(abs(forecastOutSHARRVm[,'1d']   - dataSHAR[foreOutHor,"RVm"]))      # MAE
comparisonOutSample["SHAR", "QLIKE1m"] <- mean(log(forecastOutSHARRVm[,'1d']  + (dataHAR[foreOutHor,"RVm"] / forecastOutSHARRVm[,'1d']))) # QLIKE

# R2
# 1 day ahead
dataSHARforR2 <- as.data.frame(cbind(forecastOutSHARRVd[,'1d'], dataSHAR[foreOutHor,"RVd"], 
                                     forecastOutSHARRVw[,'1d'], dataSHAR[foreOutHor,"RVw"],
                                     forecastOutSHARRVm[,'1d'], dataSHAR[foreOutHor,"RVm"]))
colnames(dataSHARforR2) <- c("fore1d", "real1d", "fore1w", "real1w", "fore1m", "real1m")
comparisonOutSample["SHAR", "Rsquared1d"] <- summary(lm('real1d ~ fore1d', dataSHARforR2))$r.squared #R2 of Mincer-Zarnowitz regression
comparisonOutSample["SHAR", "Rsquared1w"] <- summary(lm('real1w ~ fore1w', dataSHARforR2))$r.squared #R2 of Mincer-Zarnowitz regression
comparisonOutSample["SHAR", "Rsquared1m"] <- summary(lm('real1m ~ fore1m', dataSHARforR2))$r.squared #R2 of Mincer-Zarnowitz regression

# Plot comparison actual and forecast
plot(forecastOutSHARRVd[,'1d'] , type = 'l', lwd = 2, xlim = xlimOut, ylim = ylimOut, xlab ="", ylab = "")
par(new=TRUE)
plot(dataHARQ[foreOutHor,"RVd"], type = 'l', xlim = xlimOut, ylim = ylimOut, lty = 3, xlab ="", ylab = "", main = "SHAR - 1 day")

##### Out-of-sample AR(1) ####
forecastOutAR1 <- matrix(NA, 200, 3)              # to save results
colnames(forecastOutAR1)  <- c("1d", "1w", "1m")  # 1 day ahead, 3 days ahead, 1 week ahead and 1 month ahead

#k = 400 # max 980, length of window in days to estimate model

# problem with non-stationaity if i = 45 for small k

for (i in c(1:lengthForeOut)){ #1:lengthForeOut){ ##CHECK THIS: CANNOT ESTIMATE BECAUSE OF NON-STATIONARITY
  # Estimate model
  dataAR     <- realVol[estOutHor - 1 + i,'RVd'] # startOFSF - k until startOFSF, k obs.
  try(sumAR1temp <- Arima(dataAR, order = c(1, 0, 0)))

  # Predict 1 day, 1 week and 2 weeks ahead
  # 1 day
  forecastOutAR1[i, 1]  <- sumAR1$coef[2] + (sumAR1$coef[1])^1 * (last(dataAR) - sumAR1$coef[2])

  # # 1 week (5 days)
  # forecastOutAR1[i, 2]  <- sumAR1$coef[2] + (sumAR1$coef[1])^5 * (last(dataAR) - sumAR1$coef[2])
  # 
  # # 2 weeks (10 days)
  # forecastOutAR1[i, 3]  <- sumAR1$coef[2] + (sumAR1$coef[1])^10 * (last(dataAR) - sumAR1$coef[2])
  
  # Option 2
  # Weekly
  dataAR               <- realVol[estOutHor - 1 + i,'RVw'] # startOFSF - k until startOFSF, k obs.
  try(sumAR1temp       <- Arima(dataAR, order = c(1, 0, 0)))
  forecastOutAR1[i, 2] <- sumAR1$coef[2] + (sumAR1$coef[1])^1 * (last(dataAR) - sumAR1$coef[2])
  
  # Monthly
  dataAR               <- realVol[estOutHor - 1 + i,'RVm'] # startOFSF - k until startOFSF, k obs.
  try(sumAR1temp       <- Arima(dataAR, order = c(1, 0, 0)))
  forecastOutAR1[i, 3] <- sumAR1$coef[2] + (sumAR1$coef[1])^1 * (last(dataAR) - sumAR1$coef[2])
}

# Evaluate RMSE, MAE and R2
# 1 day ahead
comparisonOutSample["AR1", "RMSE1d"] <- sqrt(mean((forecastOutAR1[,'1d'] - dataHAR[foreOutHor,"RVd"])^2, na.rm = T)) # RMSE
comparisonOutSample["AR1", "MAE1d"] <- mean(abs(forecastOutAR1[,'1d'] - dataHAR[foreOutHor,"RVd"]), na.rm = T)      # MAE
comparisonOutSample["AR1", "QLIKE1d"] <- mean(log(abs(forecastOutAR1[,'1d'] + (dataHAR[foreOutHor,"RVd"] / forecastOutAR1[,'1d'])))) # QLIKE

# # 1 week ahead
# comparisonOutSample["AR1", "RMSE1w"] <- sqrt(mean((forecastOutAR1[,'1w'] - dataHAR[foreOutHor + 4,"RVd"])^2, na.rm = T)) # RMSE
# comparisonOutSample["AR1", "MAE1w"] <- mean(abs(forecastOutAR1[,'1w'] - dataHAR[foreOutHor + 4,"RVd"]), na.rm = T)      # MAE
# 
# # 2 weeks ahead
# comparisonOutSample["AR1", "RMSE2w"] <- sqrt(mean((forecastOutAR1[,'2w'] - dataHAR[foreOutHor + 9,"RVd"])^2, na.rm = T)) # RMSE
# comparisonOutSample["AR1", "MAE2w"] <- mean(abs(forecastOutAR1[,'2w'] - dataHAR[foreOutHor + 9,"RVd"]), na.rm = T)      # MAE

# Option 2
# 1 week ahead
comparisonOutSample["AR1", "RMSE1w"] <- sqrt(mean((forecastOutAR1[,'1w'] - dataHAR[foreOutHor,"RVw"])^2, na.rm = T)) # RMSE
comparisonOutSample["AR1", "MAE1w"] <- mean(abs(forecastOutAR1[,'1w'] - dataHAR[foreOutHor,"RVw"]), na.rm = T)      # MAE
comparisonOutSample["AR1", "QLIKE1w"] <- mean(log(abs(forecastOutAR1[,'1w'] + (dataHAR[foreOutHor,"RVw"] / forecastOutAR1[,'1w'])))) # QLIKE

# 2 weeks ahead
comparisonOutSample["AR1", "RMSE1m"] <- sqrt(mean((forecastOutAR1[,'1m'] - dataHAR[foreOutHor,"RVm"])^2, na.rm = T)) # RMSE
comparisonOutSample["AR1", "MAE1m"] <- mean(abs(forecastOutAR1[,'1m'] - dataHAR[foreOutHor,"RVm"]), na.rm = T)      # MAE
comparisonOutSample["AR1", "QLIKE1m"] <- mean(log(abs(forecastOutAR1[,'1m'] + (dataHAR[foreOutHor,"RVm"] / forecastOutAR1[,'1m'])))) # QLIKE

# R2
# 1 day ahead
# dataAR1forR2 <- as.data.frame(cbind(forecastOutAR1[,'1d'], dataHAR[foreOutHor,"RVd"],
#                                     forecastOutAR1[,'1w'], dataHAR[foreOutHor + 4,"RVd"],
#                                     forecastOutAR1[,'1m'], dataHAR[foreOutHor + 9,"RVd"]))
# colnames(dataAR1forR2) <- c("fore1d", "real1d", "fore1w", "real1w", "fore1m", "real1m")

# Option 2
dataAR1forR2 <- as.data.frame(cbind(forecastOutAR1[,'1d'], dataHAR[foreOutHor,"RVd"],
                                    forecastOutAR1[,'1w'], dataHAR[foreOutHor,"RVw"],
                                    forecastOutAR1[,'1m'], dataHAR[foreOutHor,"RVm"])) 
colnames(dataAR1forR2) <- c("fore1d", "real1d", "fore1w", "real1w", "fore1m", "real1m")
comparisonOutSample["AR1", "Rsquared1d"] <- summary(lm('real1d ~ fore1d', dataAR1forR2))$r.squared #R2 of Mincer-Zarnowitz regression
comparisonOutSample["AR1", "Rsquared1w"] <- summary(lm('real1w ~ fore1w', dataAR1forR2))$r.squared #R2 of Mincer-Zarnowitz regression
comparisonOutSample["AR1", "Rsquared1m"] <- summary(lm('real1m ~ fore1m', dataAR1forR2))$r.squared #R2 of Mincer-Zarnowitz regression

# Plot comparison actual and forecast
plot(forecastOutAR1[,'1d'] , type = 'l', lwd = 2, xlim = xlimOut, ylim = ylimOut, xlab ="", ylab = "")
plot(dataHAR[foreOutHor, "RVd"], type = 'l', xlim = xlimOut, ylim = ylimOut, lty = 3, xlab ="", ylab = "", main = "AR(1) - 1 day")

plot(forecastOutAR1[,'1w'] , type = 'l', lwd = 2, xlim = xlimOut, ylim = ylimOut, xlab ="", ylab = "")
par(new=TRUE)
plot(dataHAR[foreOutHor + 4, "RVd"], type = 'l', xlim = xlimOut, ylim = ylimOut, lty = 3, xlab ="", ylab = "", main = "AR(1) - 1 week")

plot(forecastOutAR1[,'2w'] , type = 'l', lwd = 2, xlim = xlimOut, ylim = ylimOut, xlab ="", ylab = "")
par(new=TRUE)
plot(dataHAR[foreOutHor + 9, "RVd"], type = 'l', xlim = xlimOut, ylim = ylimOut, lty = 3, xlab ="", ylab = "", main = "AR(1) - 2 weeks")

# Plot residuals
plot(forecastOutAR1[,'1d'] - dataHAR[foreOutHor,"RVd"] , type = 'l', lwd = 1, xlim = xlimOut, ylim = c(-8,8), xlab ="", ylab = "")
plot(forecastOutAR1[,'1w'] - dataHAR[foreOutHor + 4,"RVd"] , type = 'l', lwd = 1, xlim = xlimOut, ylim = c(-8,8), xlab ="", ylab = "")
plot(forecastOutAR1[,'2w'] - dataHAR[foreOutHor + 9,"RVd"] , type = 'l', lwd = 1, xlim = xlimOut, ylim = c(-8,8), xlab ="", ylab = "")

##### Out-of-sample AR(3) ####
forecastOutAR3 <- matrix(NA, 200, 3)
colnames(forecastOutAR3)  <- c("1d", "1w", "1m")

#k = 400 # max 980, length of window in days to estimate model

for (i in c(1:lengthForeOut)){ #1:lengthForeOut){ ##CHECK THIS: CANNOT ESTIMATE BECAUSE OF NON-STATIONARITY
  # Estimate model
  dataAR <- realVol[estOutHor - 1 + i,'RVd'] # startOFSF - k until startOFSF, k obs.
  try(sumAR3 <- Arima(dataAR, order = c(3, 0, 0), method="ML"))
  # 1 - sum(sumAR1$residuals^2, na.rm=T) / sum((realVol[,'RVd'] - mean(realVol[,'RVd']))^2) # r-squared
  
  # Predict 1 day, 1 week and 2 weeks ahead
  # 1 day
  forecastOutAR3[i, 1]  <- forecast(object = sumAR3, h = 1)$mean[1]
  
  # # 1 week (5 days)
  # forecastOutAR3[i, 2]  <- forecast(object = sumAR3, h = 5)$mean[5]
  # 
  # # 2 weeks (10 days)
  # forecastOutAR3[i, 3]  <- forecast(object = sumAR3, h = 10)$mean[10]
  
  # Option 2
  # Weekly
  dataAR               <- realVol[estOutHor - 1 + i,'RVw'] # startOFSF - k until startOFSF, k obs.
  try(sumAR3temp       <- Arima(dataAR, order = c(3, 0, 0), method="ML"))
  forecastOutAR3[i, 2] <- forecast(object = sumAR3, h = 1)$mean[1]
  
  # Monthly
  dataAR               <- realVol[estOutHor - 1 + i,'RVm'] # startOFSF - k until startOFSF, k obs.
  try(sumAR3temp       <- Arima(dataAR, order = c(3, 0, 0), method="ML"))
  forecastOutAR3[i, 3] <- forecast(object = sumAR3, h = 1)$mean[1]
}
# Evaluate RMSE, MAE and R2
# 1 day ahead
comparisonOutSample["AR3", "RMSE1d"] <- sqrt(mean((forecastOutAR3[,'1d'] - dataHAR[foreOutHor,"RVd"])^2, na.rm = T)) # RMSE
comparisonOutSample["AR3", "MAE1d"] <- mean(abs(forecastOutAR3[,'1d'] - dataHAR[foreOutHor,"RVd"]), na.rm = T)      # MAE
comparisonOutSample["AR3", "QLIKE1d"] <- mean(log(abs(forecastOutAR3[,'1d'] + (dataHAR[foreOutHor,"RVd"] / forecastOutAR3[,'1d'])))) # QLIKE

# # 1 week ahead
# comparisonOutSample["AR3", "RMSE1w"] <- sqrt(mean((forecastOutAR3[,'1w'] - dataHAR[foreOutHor + 4,"RVd"])^2, na.rm = T)) # RMSE
# comparisonOutSample["AR3", "MAE1w"] <- mean(abs(forecastOutAR3[,'1w'] - dataHAR[foreOutHor + 4,"RVd"]), na.rm = T)      # MAE
# 
# # 2 weeks ahead
# comparisonOutSample["AR3", "RMSE2w"] <- sqrt(mean((forecastOutAR3[,'2w'] - dataHAR[foreOutHor + 9,"RVd"])^2, na.rm = T)) # RMSE
# comparisonOutSample["AR3", "MAE2w"] <- mean(abs(forecastOutAR3[,'2w'] - dataHAR[foreOutHor + 9,"RVd"]), na.rm = T)      # MAE

# Option 2
# 1 week ahead
comparisonOutSample["AR3", "RMSE1w"] <- sqrt(mean((forecastOutAR3[,'1w'] - dataHAR[foreOutHor,"RVw"])^2, na.rm = T)) # RMSE
comparisonOutSample["AR3", "MAE1w"] <- mean(abs(forecastOutAR3[,'1w'] - dataHAR[foreOutHor,"RVw"]), na.rm = T)      # MAE
comparisonOutSample["AR3", "QLIKE1w"] <- mean(log(abs(forecastOutAR3[,'1w'] + (dataHAR[foreOutHor,"RVw"] / forecastOutAR3[,'1w'])))) # QLIKE

# 2 weeks ahead
comparisonOutSample["AR3", "RMSE1m"] <- sqrt(mean((forecastOutAR3[,'1m'] - dataHAR[foreOutHor,"RVm"])^2, na.rm = T)) # RMSE
comparisonOutSample["AR3", "MAE1m"] <- mean(abs(forecastOutAR3[,'1m'] - dataHAR[foreOutHor,"RVm"]), na.rm = T)      # MAE
comparisonOutSample["AR3", "QLIKE1m"] <- mean(log(abs(forecastOutAR3[,'1m'] + (dataHAR[foreOutHor,"RVm"] / forecastOutAR3[,'1m'])))) # QLIKE

# R2
# 1 day ahead
# dataAR3forR2 <- as.data.frame(cbind(forecastOutAR3[,'1d'], dataHAR[foreOutHor,"RVd"], 
#                                     forecastOutAR3[,'1w'], dataHAR[foreOutHor + 4,"RVd"],
#                                     forecastOutAR3[,'2w'], dataHAR[foreOutHor + 9,"RVd"]))
dataAR3forR2 <- as.data.frame(cbind(forecastOutAR3[,'1d'], dataHAR[foreOutHor,"RVd"], 
                                    forecastOutAR3[,'1w'], dataHAR[foreOutHor,"RVw"],
                                    forecastOutAR3[,'1m'], dataHAR[foreOutHor,"RVm"]))
colnames(dataAR3forR2) <- c("fore1d", "real1d", "fore1w", "real1w", "fore2w", "real2w")
comparisonOutSample["AR3", "Rsquared1d"] <- summary(lm('real1d ~ fore1d', dataAR3forR2))$r.squared #R2 of Mincer-Zarnowitz regression
comparisonOutSample["AR3", "Rsquared1w"] <- summary(lm('real1w ~ fore1w', dataAR3forR2))$r.squared #R2 of Mincer-Zarnowitz regression
comparisonOutSample["AR3", "Rsquared1m"] <- summary(lm('real2w ~ fore2w', dataAR3forR2))$r.squared #R2 of Mincer-Zarnowitz regression

# Plot comparison actual and forecast
plot(forecastOutAR3[,'1d'] , type = 'l', lwd = 2, xlim = xlimOut, ylim = ylimOut, xlab ="", ylab = "")
par(new=TRUE)
plot(dataHAR[foreOutHor, "RVd"], type = 'l', xlim = xlimOut, ylim = ylimOut, lty = 3, xlab ="", ylab = "", main = "AR(3) - 1 day")

plot(forecastOutAR3[,'1w'] , type = 'l', lwd = 2, xlim = xlimOut, ylim = ylimOut, xlab ="", ylab = "")
par(new=TRUE)
plot(dataHAR[foreOutHor + 4, "RVd"], type = 'l', xlim = xlimOut, ylim = ylimOut, lty = 3, xlab ="", ylab = "", main = "AR(3) - 1 week")

plot(forecastOutAR3[,'2w'] , type = 'l', lwd = 2, xlim = xlimOut, ylim = ylimOut, xlab ="", ylab = "")
par(new=TRUE)
plot(dataHAR[foreOutHor + 9, "RVd"], type = 'l', xlim = xlimOut, ylim = ylimOut, lty = 3, xlab ="", ylab = "", main = "AR(3) - 2 weeks")

# Plot residuals
plot(forecastOutAR3[,'1d'] - dataHAR[foreOutHor,"RVd"] , type = 'l', lwd = 1, xlim = xlimOut, ylim = c(-8,8), xlab ="", ylab = "")
plot(forecastOutAR3[,'1w'] - dataHAR[foreOutHor + 4,"RVd"] , type = 'l', lwd = 1, xlim = xlimOut, ylim = c(-8,8), xlab ="", ylab = "")
plot(forecastOutAR3[,'2w'] - dataHAR[foreOutHor + 9,"RVd"] , type = 'l', lwd = 1, xlim = xlimOut, ylim = c(-8,8), xlab ="", ylab = "")

##### Out-of-sample AR(22) ####
forecastOutAR22 <- matrix(NA, 200, 3)
colnames(forecastOutAR22)  <- c("1d", "1w", "1m")

#k = 125 # max 980, length of window in days to estimate model

for (i in c(1:lengthForeOut)){
  print(i)
  # Estimate model
  dataAR <- realVol[estOutHor - 1 + i,'RVd'] # startOFSF - k until startOFSF, k obs.
  try(sumAR22 <- Arima(dataAR, order = c(22, 0, 0)))
  #1 - sum(sumAR1$residuals^2, na.rm=T) / sum((realVol[,'RVd'] - mean(realVol[,'RVd']))^2) # r-squared
  
  # Predict 1 day, 1 week and 2 weeks ahead
  # 1 day
  forecastOutAR22[i, 1]  <- forecast(object = sumAR22, h = 1)$mean[1]
  
  # # 1 week (5 days)
  # forecastOutAR22[i, 3]  <- forecast(object = sumAR22, h = 5)$mean[5]
  # 
  # # 2 weeks (10 days)
  # forecastOutAR22[i, 4]  <- forecast(object = sumAR22, h = 10)$mean[10]
  
  # Option 2
  # Weekly
  dataAR               <- realVol[estOutHor - 1 + i,'RVw'] # startOFSF - k until startOFSF, k obs.
  try(sumAR22temp       <- Arima(dataAR, order = c(22, 0, 0)))
  forecastOutAR22[i, 2] <- forecast(object = sumAR22temp, h = 1)$mean[1]
  
  # Monthly
  dataAR               <- realVol[estOutHor - 1 + i,'RVm'] # startOFSF - k until startOFSF, k obs.
  try(sumAR22temp       <- Arima(dataAR, order = c(22, 0, 0)))
  forecastOutAR22[i, 3] <- forecast(object = sumAR22temp, h = 1)$mean[1]
}

# Evaluate RMSE, MAE and R2
# 1 day ahead
comparisonOutSample["AR22", "RMSE1d"] <- sqrt(mean((forecastOutAR22[,'1d'] - dataHAR[(startOFSF + 1):(startOFSF + 199 + 1),"RVd"])^2)) # RMSE
comparisonOutSample["AR22", "MAE1d"] <- mean(abs(forecastOutAR22[,'1d'] - dataHAR[(startOFSF + 1):(startOFSF + 199 + 1),"RVd"]))      # MAE
comparisonOutSample["AR22", "QLIKE1d"] <- mean(log(abs(forecastOutAR22[,'1d'] + (dataHAR[foreOutHor,"RVd"] / forecastOutAR22[,'1d'])))) # QLIKE

# # 1 week ahead
# comparisonOutSample["AR22", "RMSE1w"] <- sqrt(mean((forecastOutAR22[,'1w'] - dataHAR[(startOFSF + 5):(startOFSF + 199 + 5),"RVd"])^2)) # RMSE
# comparisonOutSample["AR22", "MAE1w"] <- mean(abs(forecastOutAR22[,'1w'] - dataHAR[(startOFSF + 5):(startOFSF + 199 + 5),"RVd"]))      # MAE
# 
# # 2 weeks ahead
# comparisonOutSample["AR22", "RMSE2w"] <- sqrt(mean((forecastOutAR22[,'2w'] - dataHAR[(startOFSF + 21):(startOFSF + 199 + 10),"RVd"])^2)) # RMSE
# comparisonOutSample["AR22", "MAE2w"] <- mean(abs(forecastOutAR22[,'2w'] - dataHAR[(startOFSF + 21):(startOFSF + 199 + 10),"RVd"]))      # MAE

# Option 2
# 1 week ahead
comparisonOutSample["AR22", "RMSE1w"] <- sqrt(mean((forecastOutAR22[,'1w'] - dataHAR[foreOutHor,"RVw"])^2, na.rm = T)) # RMSE
comparisonOutSample["AR22", "MAE1w"] <- mean(abs(forecastOutAR22[,'1w'] - dataHAR[foreOutHor,"RVw"]), na.rm = T)      # MAE
comparisonOutSample["AR22", "QLIKE1w"] <- mean(log(abs(forecastOutAR22[,'1w'] + (dataHAR[foreOutHor,"RVw"] / forecastOutAR22[,'1w'])))) # QLIKE

# 2 weeks ahead
comparisonOutSample["AR22", "RMSE1m"] <- sqrt(mean((forecastOutAR22[,'1m'] - dataHAR[foreOutHor,"RVm"])^2, na.rm = T)) # RMSE
comparisonOutSample["AR22", "MAE1m"] <- mean(abs(forecastOutAR22[,'1m'] - dataHAR[foreOutHor,"RVm"]), na.rm = T)      # MAE
comparisonOutSample["AR22", "QLIKE1m"] <- mean(log(abs(forecastOutAR22[,'1m'] + (dataHAR[foreOutHor,"RVm"] / forecastOutAR22[,'1m'])))) # QLIKE

# R2
# 1 day ahead
# dataAR22forR2 <- as.data.frame(cbind(forecastOutAR22[,'1d'], dataHAR[(startOFSF + 1):(startOFSF + 199 + 1),"RVd"], 
#                                      forecastOutAR22[,'1w'], dataHAR[(startOFSF + 5):(startOFSF + 199 + 5),"RVd"],
#                                     forecastOutAR22[,'2w'], dataHAR[(startOFSF + 10):(startOFSF + 199 + 10),"RVd"]))
dataAR22forR2 <- as.data.frame(cbind(forecastOutAR22[,'1d'], dataHAR[foreOutHor,"RVd"], 
                                     forecastOutAR22[,'1w'], dataHAR[foreOutHor,"RVw"],
                                     forecastOutAR22[,'1m'], dataHAR[foreOutHor,"RVm"]))
colnames(dataAR22forR2) <- c("fore1d", "real1d", "fore1w", "real1w", "fore1m", "real1m")
comparisonOutSample["AR22", "Rsquared1d"] <- summary(lm('real1d ~ fore1d', dataAR22forR2))$r.squared #R2 of Mincer-Zarnowitz regression
comparisonOutSample["AR22", "Rsquared1w"] <- summary(lm('real1w ~ fore1w', dataAR22forR2))$r.squared #R2 of Mincer-Zarnowitz regression
comparisonOutSample["AR22", "Rsquared1m"] <- summary(lm('real1m ~ fore1m', dataAR22forR2))$r.squared #R2 of Mincer-Zarnowitz regression

# Plot comparison actual and forecast
plot(forecastOutAR22[,'1d'] , type = 'l', lwd = 2, xlim = xlimOut, ylim = ylimOut, xlab ="", ylab = "")
par(new=TRUE)
plot(dataHAR[foreOutHor, "RVd"], type = 'l', xlim = xlimOut, ylim = ylimOut, lty = 3, xlab ="", ylab = "", main = "AR(3) - 1 day")

plot(forecastOutAR22[,'1w'] , type = 'l', lwd = 2, xlim = xlimOut, ylim = ylimOut, xlab ="", ylab = "")
par(new=TRUE)
plot(dataHAR[foreOutHor + 4, "RVd"], type = 'l', xlim = xlimOut, ylim = ylimOut, lty = 3, xlab ="", ylab = "", main = "AR(3) - 1 week")

plot(forecastOutAR22[,'2w'] , type = 'l', lwd = 2, xlim = xlimOut, ylim = ylimOut, xlab ="", ylab = "")
par(new=TRUE)
plot(dataHAR[foreOutHor + 9, "RVd"], type = 'l', xlim = xlimOut, ylim = ylimOut, lty = 3, xlab ="", ylab = "", main = "AR(3) - 2 weeks")

#### Out-of-sample ARFIMA ####
forecastOutARFIMA <- matrix(NA, 200, 3)
colnames(forecastOutARFIMA)  <- c("1d", "1w", "1m")

#k = 125 # max 980, length of window in days to estimate model

for (i in 1:lengthForeOut){
  print(i)
  # Estimate model
  dataARFIMA     <- realVol[estOutHor - 1 + i,'RVd'] # startOFSF - k until startOFSF, k obs.
  sumARFIMAtemp  <- fracdiff(dataARFIMA, nar= 5, nma = 0) 
  #1 - sum(sumAR1$residuals^2, na.rm=T) / sum((realVol[,'RVd'] - mean(realVol[,'RVd']))^2) # r-squared
  
  # Predict 1 day, 1 week and 2 weeks ahead
  # 1 day
  forecastOutARFIMA[i, 1]  <- forecast(sumARFIMAtemp)$mean[1] #forecast.fracdiff
 
  # # 1 week (5 days)
  # forecastOutARFIMA[i, 2]  <- forecast(sumARFIMAtemp)$mean[5] #forecast.fracdiff
  # 
  # # 2 weeks (10 days)
  # forecastOutARFIMA[i, 3]  <- forecast(sumARFIMAtemp)$mean[22] #forecast.fracdiff
  
  # Option 2, forecast wekly and monthly RV
  # Weekly
  dataARFIMA     <- realVol[estOutHor - 1 + i,'RVw'] # startOFSF - k until startOFSF, k obs.
  sumARFIMAtemp  <- fracdiff(dataARFIMA, nar= 5, nma = 0) 
  forecastOutARFIMA[i, 2]  <- forecast(sumARFIMAtemp)$mean[1] #forecast.fracdiff
  
  # Monthly
  dataARFIMA     <- realVol[estOutHor - 1 + i,'RVm'] # startOFSF - k until startOFSF, k obs.
  sumARFIMAtemp  <- fracdiff(dataARFIMA, nar= 5, nma = 0) 
  forecastOutARFIMA[i, 3]  <- forecast(sumARFIMAtemp)$mean[1] #forecast.fracdiff 
}

# Evaluate RMSE, MAE and R2
# 1 day ahead
comparisonOutSample["ARFIMA", "RMSE1d"] <- sqrt(mean((forecastOutARFIMA[,'1d'] - dataHAR[foreOutHor,"RVd"])^2)) # RMSE
comparisonOutSample["ARFIMA", "MAE1d"] <- mean(abs(forecastOutARFIMA[,'1d'] - dataHAR[foreOutHor,"RVd"]))      # MAE
comparisonOutSample["ARFIMA", "QLIKE1d"] <- mean(log(abs(forecastOutARFIMA[,'1d'] + (dataHAR[foreOutHor,"RVd"] / forecastOutARFIMA[,'1d'])))) # QLIKE

# # 1 week ahead
# comparisonOutSample["ARFIMA", "RMSE1w"] <- sqrt(mean((forecastOutARFIMA[,'1w'] - dataHAR[foreOutHor + 4,"RVd"])^2)) # RMSE
# comparisonOutSample["ARFIMA", "MAE1w"] <- mean(abs(forecastOutARFIMA[,'1w'] - dataHAR[foreOutHor + 4,"RVd"]))      # MAE
# 
# # 2 weeks ahead
# comparisonOutSample["ARFIMA", "RMSE1m"] <- sqrt(mean((forecastOutARFIMA[,'1m'] - dataHAR[foreOutHor + 21,"RVd"])^2)) # RMSE
# comparisonOutSample["ARFIMA", "MAE1m"] <- mean(abs(forecastOutARFIMA[,'1m'] - dataHAR[foreOutHor + 21,"RVd"]))      # MAE

# Option 2
# 1 week ahead
comparisonOutSample["ARFIMA", "RMSE1w"] <- sqrt(mean((forecastOutARFIMA[,'1w'] - dataHAR[foreOutHor,"RVw"])^2)) # RMSE
comparisonOutSample["ARFIMA", "MAE1w"] <- mean(abs(forecastOutARFIMA[,'1w'] - dataHAR[foreOutHor,"RVw"]))      # MAE
comparisonOutSample["ARFIMA", "QLIKE1w"] <- mean(log(abs(forecastOutARFIMA[,'1w'] + (dataHAR[foreOutHor,"RVw"] / forecastOutARFIMA[,'1w'])))) # QLIKE

# 2 weeks ahead
comparisonOutSample["ARFIMA", "RMSE1m"] <- sqrt(mean((forecastOutARFIMA[,'1m'] - dataHAR[foreOutHor,"RVm"])^2)) # RMSE
comparisonOutSample["ARFIMA", "MAE1m"] <- mean(abs(forecastOutARFIMA[,'1m'] - dataHAR[foreOutHor,"RVm"]))      # MAE
comparisonOutSample["ARFIMA", "QLIKE1m"] <- mean(log(abs(forecastOutARFIMA[,'1m'] + (dataHAR[foreOutHor,"RVm"] / forecastOutARFIMA[,'1m'])))) # QLIKE

# R2
# 1 day ahead
# dataARFIMAforR2 <- as.data.frame(cbind(forecastOutARFIMA[,'1d'], dataHAR[foreOutHor,"RVd"], 
#                                        forecastOutARFIMA[,'1w'], dataHAR[foreOutHor + 4,"RVd"],
#                                        forecastOutARFIMA[,'1m'], dataHAR[foreOutHor + 21,"RVd"]))
# Option 2
dataARFIMAforR2 <- as.data.frame(cbind(forecastOutARFIMA[,'1d'], dataHAR[foreOutHor,"RVd"], 
                                       forecastOutARFIMA[,'1w'], dataHAR[foreOutHor,"RVw"],
                                       forecastOutARFIMA[,'1m'], dataHAR[foreOutHor,"RVm"]))
colnames(dataARFIMAforR2) <- c("fore1d", "real1d", "fore1w", "real1w", "fore1m", "real1m")
comparisonOutSample["ARFIMA", "Rsquared1d"] <- summary(lm('real1d ~ fore1d', dataARFIMAforR2))$r.squared #R2 of Mincer-Zarnowitz regression
comparisonOutSample["ARFIMA", "Rsquared1w"] <- summary(lm('real1w ~ fore1w', dataARFIMAforR2))$r.squared #R2 of Mincer-Zarnowitz regression
comparisonOutSample["ARFIMA", "Rsquared1m"] <- summary(lm('real1m ~ fore1m', dataARFIMAforR2))$r.squared #R2 of Mincer-Zarnowitz regression

# Plot comparison actual and forecast
plot(forecastOutARFIMA[,'1d'] , type = 'l', lwd = 2, xlim = xlimOut, ylim = ylimOut, xlab ="", ylab = "")
par(new=TRUE)
plot(dataHAR[foreOutHor, "RVd"], type = 'l', xlim = xlimOut, ylim = ylimOut, lty = 3, xlab ="", ylab = "", main = "AFIMA - 1 day")

plot(forecastOutARFIMA[,'1w'] , type = 'l', lwd = 2, xlim = xlimOut, ylim = ylimOut, xlab ="", ylab = "")
par(new=TRUE)
plot(dataHAR[foreOutHor + 4, "RVd"], type = 'l', xlim = xlimOut, ylim = ylimOut, lty = 3, xlab ="", ylab = "", main = "AFIMA - 1 week")

plot(forecastOutARFIMA[,'2w'] , type = 'l', lwd = 2, xlim = xlimOut, ylim = ylimOut, xlab ="", ylab = "")
par(new=TRUE)
plot(dataHAR[foreOutHor + 9, "RVd"], type = 'l', xlim = xlimOut, ylim = ylimOut, lty = 3, xlab ="", ylab = "", main = "AFIMA- 2 weeks")

# Boxplots for comparison of out-of-sample forecasting (forecasting error)
par(mar = c(2.1,5.1,1.1,1.1))
boxplot(cbind((forecastOutAR1[,'1w'] - dataHAR[(startOFSF + 5):(startOFSF + 199 + 5),"RVd"]),
              (forecastOutAR3[,'1w'] - dataHAR[(startOFSF + 5):(startOFSF + 199 + 5),"RVd"]),
              (forecastOutAR22[,'1w'] - dataHAR[(startOFSF + 5):(startOFSF + 199 + 5),"RVd"]),
              (forecastOutHARRVd[,'1d'] - dataHAR[(startOFSF + 1):(startOFSF + 199 + 1),"RVd"]),
              (forecastOutHARQRVd[,'1d'] - dataHAR[(startOFSF + 1):(startOFSF + 199 + 1),"RVd"]),
              (forecastOutSHARRVd[,'1d'] - dataHAR[(startOFSF + 1):(startOFSF + 199 + 1),"RVd"]),
              (forecastOutARFIMA[,'1d'] - dataHAR[(startOFSF + 1):(startOFSF + 199 + 1),"RVd"])), 
        ylim = c(-10, 10), names = c("AR1", "AR3", "AR22", "HAR", "HARQ", "SHAR", "ARFIMA"), horizontal = T, las = 1)
par(mar=c(5.1,4.1,4.1,2.1))

png(file = "boxplot.png", width = 8000, height = 6000, res = 900)
par(mar = c(2.1,5.1,1.1,1.1))
boxplot(cbind((forecastOutHARRVd[,'1d'] - dataHAR[(startOFSF + 1):(startOFSF + 199 + 1),"RVd"]),
              (forecastOutHARQRVd[,'1d'] - dataHAR[(startOFSF + 1):(startOFSF + 199 + 1),"RVd"]),
              (forecastOutSHARRVd[,'1d'] - dataHAR[(startOFSF + 1):(startOFSF + 199 + 1),"RVd"]),
              (forecastOutARFIMA[,'1d'] - dataHAR[(startOFSF + 1):(startOFSF + 199 + 1),"RVd"])), 
        ylim = c(-10, 10), names = c("HAR", "HARQ", "SHAR", "ARFIMA"), horizontal = T, las = 1)
par(mar=c(5.1,4.1,4.1,2.1))
dev.off()

boxplot(cbind((forecastOutAR1[,'1w'] - dataHAR[(startOFSF + 5):(startOFSF + 199 + 5),"RVd"]),
              (forecastOutAR3[,'1w'] - dataHAR[(startOFSF + 5):(startOFSF + 199 + 5),"RVd"]),
              (forecastOutHARRVd[,'2w'] - dataHAR[(startOFSF + 5):(startOFSF + 199 + 5),"RVd"]),
              (forecastOutARFIMA[,'1w'] - dataHAR[(startOFSF + 5):(startOFSF + 199 + 5),"RVd"])))#, ylim = c(-2, 2))

boxplot(cbind((forecastOutAR1[,'2w'] - dataHAR[(startOFSF + 10):(startOFSF + 199 + 10),"RVd"]),
              (forecastOutAR3[,'2w'] - dataHAR[(startOFSF + 10):(startOFSF + 199 + 10),"RVd"]),
              (forecastOutHARRVd[,'2w'] - dataHAR[(startOFSF + 10):(startOFSF + 199 + 10),"RVd"]),
              (forecastOutARFIMA[,'2w'] - dataHAR[(startOFSF + 10):(startOFSF + 199 + 10),"RVd"])))#, ylim = c(-2, 2))

# Plot out-of-sample fit
setylim <- c(0,25)
# Plot comparison actual and forecast
plot(dataHAR[foreOutHor,"RVd"], type = 'l', lwd = 2, xlim = xlimOut, ylim = ylimOut, lty = 3, xlab ="", ylab = "")
par(new=TRUE); plot(forecastOutHARRVd[,'1d'], type = 'l', xlim = xlimOut, ylim = ylimOut, xlab ="", ylab = "", main = "", col = 'brown')
#par(new=TRUE); plot(forecastOutHARQRVd[,'1d'], type = 'l', xlim = xlimOut, ylim = ylimOut, xlab ="", ylab = "", main = "", col = '')
par(new=TRUE); plot(forecastOutHARQRVd[,'1d'], type = 'l', xlim = xlimOut, ylim = ylimOut, xlab ="", ylab = "", main = "", col = 'orange')
par(new=TRUE); plot(forecastOutHARQFRVd[,'1d'], type = 'l', xlim = xlimOut, ylim = ylimOut, xlab ="", ylab = "", main = "", col = 'grey')
par(new=TRUE); plot(forecastOutAR1[,'1d'], type = 'l', xlim = xlimOut, ylim = ylimOut, xlab ="", ylab = "", main = "", col = 'red')
par(new=TRUE); plot(forecastOutAR3[,'1d'], type = 'l', xlim = xlimOut, ylim = ylimOut, xlab ="", ylab = "", main = "", col = 'green')
par(new=TRUE); plot(forecastOutAR22[,'1d'], type = 'l', xlim = xlimOut, ylim = ylimOut, xlab ="", ylab = "", main = "", col = 'blue')
par(new=TRUE); plot(forecastOutARFIMA[,'1d'], type = 'l', xlim = xlimOut, ylim = ylimOut, xlab ="", ylab = "", main = "", col = 'blue')

comparisonOutSample
xtable(comparisonOutSample[c(1,2,4,5,6,7,8),c(1,2,3,4)], digits = 3)

xtable(comparisonOutSample[c(1,2,4,5,6,7,8), 5:12], digits = 3)

dmResults <- as.data.frame(matrix(rep(NA, 49), 7, 7))
colnames(dmResults) <- c("HAR", "HARQ", "SHAR", "AR1", "AR3", "AR22", "ARFIMA")
rownames(dmResults) <- c("HAR", "HARQ", "SHAR", "AR1", "AR3", "AR22", "ARFIMA")

# Diebold Mariano test to compare forecasting differences
dmResults["HAR","HARQ"] <- dm.test((dataHAR[foreOutHor,"RVd"] - forecastOutHARRVd[,'1d']), (dataHAR[foreOutHor,"RVd"] - forecastOutHARQRVd[,'1d']), 
        alternative= "two.sided", h=1, power=2)$statistic #HAR > HARQ
dmResults["SHAR","HARQ"] <- dm.test((dataHAR[foreOutHor,"RVd"] - forecastOutSHARRVd[,'1d']), (dataHAR[foreOutHor,"RVd"] - forecastOutHARQRVd[,'1d']), 
        alternative= "two.sided", h=1, power=2)$statistic #SHAR > HARQ
dmResults["AR1","HARQ"] <- dm.test(dataHAR[foreOutHor,"RVd"] - forecastOutAR1[,'1d'], (dataHAR[foreOutHor,"RVd"] - forecastOutHARQRVd[,'1d']), 
        alternative= "two.sided", h=1, power=2)$statistic #  AR1 > HARQ
dmResults["AR3","HARQ"] <- dm.test((dataHAR[foreOutHor,"RVd"] - forecastOutAR3[,'1d']), (dataHAR[foreOutHor,"RVd"] - forecastOutHARQRVd[,'1d']), 
        alternative= "two.sided", h=1, power=2)$statistic #  AR3 > HARQ
dmResults["AR22","HARQ"] <- dm.test(dataHAR[foreOutHor,"RVd"] - forecastOutAR22[,'1d'], (dataHAR[foreOutHor,"RVd"] - forecastOutHARQRVd[,'1d']), 
        alternative= "two.sided", h=1, power=2)$statistic #  AR22 > HARQ
dmResults["ARFIMA","HARQ"] <- dm.test(dataHAR[foreOutHor,"RVd"] - forecastOutARFIMA[,'1d'], (dataHAR[foreOutHor,"RVd"] - forecastOutHARQRVd[,'1d']), 
        alternative= "two.sided", h=1, power=2)$statistic #ARFIMA > HARQ

dmResults["HARQ","HAR"] <- dm.test((dataHAR[foreOutHor,"RVd"] - forecastOutHARQRVd[,'1d']), (dataHAR[foreOutHor,"RVd"] - forecastOutHARRVd[,'1d']), 
        alternative= "two.sided", h=1, power=2)$statistic #HARQ > HAR
dmResults["SHAR","HAR"] <- dm.test((dataHAR[foreOutHor,"RVd"] - forecastOutSHARRVd[,'1d']), (dataHAR[foreOutHor,"RVd"] - forecastOutHARRVd[,'1d']), 
        alternative= "two.sided", h=1, power=2)$statistic #SHAR > HAR
dmResults["AR1","HAR"] <- dm.test(dataHAR[foreOutHor,"RVd"] - forecastOutAR1[,'1d'], (dataHAR[foreOutHor,"RVd"] - forecastOutHARRVd[,'1d']), 
        alternative= "two.sided", h=1, power=2)$statistic #  AR1 > HAR
dmResults["AR3","HAR"] <- dm.test((dataHAR[foreOutHor,"RVd"] - forecastOutAR3[,'1d']), (dataHAR[foreOutHor,"RVd"] - forecastOutHARRVd[,'1d']), 
        alternative= "two.sided", h=1, power=2)$statistic #  AR3 > HAR
dmResults["AR22","HAR"] <- dm.test(dataHAR[foreOutHor,"RVd"] - forecastOutAR22[,'1d'], (dataHAR[foreOutHor,"RVd"] - forecastOutHARRVd[,'1d']), 
        alternative= "two.sided", h=1, power=2)$statistic #  AR22 > HAR
dmResults["ARFIMA","HAR"] <- dm.test(dataHAR[foreOutHor,"RVd"] - forecastOutARFIMA[,'1d'], (dataHAR[foreOutHor,"RVd"] - forecastOutHARRVd[,'1d']), 
        alternative= "two.sided", h=1, power=2)$statistic #ARFIMA > HAR

dmResults["HARQ","SHAR"] <- dm.test((dataHAR[foreOutHor,"RVd"] - forecastOutHARQRVd[,'1d']), (dataHAR[foreOutHor,"RVd"] - forecastOutSHARRVd[,'1d']), 
        alternative= "two.sided", h=1, power=2)$statistic #HARQ > SHAR
dmResults["HAR","SHAR"] <- dm.test((dataHAR[foreOutHor,"RVd"] - forecastOutHARRVd[,'1d']), (dataHAR[foreOutHor,"RVd"] - forecastOutSHARRVd[,'1d']), 
        alternative= "two.sided", h=1, power=2)$statistic #HARQ > SHAR
dmResults["AR1","SHAR"] <- dm.test(dataHAR[foreOutHor,"RVd"] - forecastOutAR1[,'1d'], (dataHAR[foreOutHor,"RVd"] - forecastOutSHARRVd[,'1d']), 
        alternative= "two.sided", h=1, power=2)$statistic #  AR1 > SHAR
dmResults["AR3","SHAR"] <- dm.test((dataHAR[foreOutHor,"RVd"] - forecastOutAR3[,'1d']), (dataHAR[foreOutHor,"RVd"] - forecastOutSHARRVd[,'1d']), 
        alternative= "two.sided", h=1, power=2)$statistic #  AR3 > SHAR
dmResults["AR22","SHAR"] <- dm.test(dataHAR[foreOutHor,"RVd"] - forecastOutAR22[,'1d'], (dataHAR[foreOutHor,"RVd"] - forecastOutSHARRVd[,'1d']), 
        alternative= "two.sided", h=1, power=2)$statistic #  AR22 > SHAR
dmResults["ARFIMA","SHAR"] <- dm.test(dataHAR[foreOutHor,"RVd"] - forecastOutARFIMA[,'1d'], (dataHAR[foreOutHor,"RVd"] - forecastOutSHARRVd[,'1d']), 
        alternative= "two.sided", h=1, power=2)$statistic #ARFIMA > SHAR

dmResults["HARQ","ARFIMA"] <- dm.test((dataHAR[foreOutHor,"RVd"] - forecastOutHARQRVd[,'1d']), (dataHAR[foreOutHor,"RVd"] - forecastOutARFIMA[,'1d']), 
        alternative= "two.sided", h=1, power=2)$statistic #HARQ > ARFIMA
dmResults["SHAR","ARFIMA"] <- dm.test((dataHAR[foreOutHor,"RVd"] - forecastOutSHARRVd[,'1d']), (dataHAR[foreOutHor,"RVd"] - forecastOutARFIMA[,'1d']), 
        alternative= "two.sided", h=1, power=2)$statistic #SHAR > ARFIMA
dmResults["AR1","ARFIMA"] <- dm.test(dataHAR[foreOutHor,"RVd"] - forecastOutAR1[,'1d'], (dataHAR[foreOutHor,"RVd"] - forecastOutARFIMA[,'1d']), 
        alternative= "two.sided", h=1, power=2)$statistic #  AR1 > ARFIMA
dmResults["AR3","ARFIMA"] <- dm.test((dataHAR[foreOutHor,"RVd"] - forecastOutAR3[,'1d']), (dataHAR[foreOutHor,"RVd"] - forecastOutARFIMA[,'1d']), 
        alternative= "two.sided", h=1, power=2)$statistic #  AR3 > ARFIMA
dmResults["AR22","ARFIMA"] <- dm.test(dataHAR[foreOutHor,"RVd"] - forecastOutAR22[,'1d'], (dataHAR[foreOutHor,"RVd"] - forecastOutARFIMA[,'1d']), 
        alternative= "two.sided", h=1, power=2)$statistic #  AR22 > ARFIMA
dmResults["HAR","ARFIMA"] <- dm.test((dataHAR[foreOutHor,"RVd"] - forecastOutHARRVd[,'1d']), (dataHAR[foreOutHor,"RVd"] - forecastOutARFIMA[,'1d']), 
        alternative= "two.sided", h=1, power=2)$statistic #ARFIMA > ARFIMA

dmResults["HARQ","AR1"] <- dm.test((dataHAR[foreOutHor,"RVd"] - forecastOutHARQRVd[,'1d']), dataHAR[foreOutHor,"RVd"] - forecastOutAR1[,'1d'], 
        alternative= "two.sided", h=1, power=2)$statistic #HARQ > AR1
dmResults["SHAR","AR1"] <- dm.test((dataHAR[foreOutHor,"RVd"] - forecastOutSHARRVd[,'1d']), dataHAR[foreOutHor,"RVd"] - forecastOutAR1[,'1d'], 
        alternative= "two.sided", h=1, power=2)$statistic #SHAR > AR1
dmResults["HAR","AR1"] <- dm.test((dataHAR[foreOutHor,"RVd"] - forecastOutHARRVd[,'1d']), dataHAR[foreOutHor,"RVd"] - forecastOutAR1[,'1d'], 
        alternative= "two.sided", h=1, power=2)$statistic #  HAR > AR1
dmResults["AR3","AR1"] <- dm.test((dataHAR[foreOutHor,"RVd"] - forecastOutAR3[,'1d']), dataHAR[foreOutHor,"RVd"] - forecastOutAR1[,'1d'], 
        alternative= "two.sided", h=1, power=2)$statistic #  AR3 > AR1
dmResults["AR22","AR1"] <- dm.test(dataHAR[foreOutHor,"RVd"] - forecastOutAR22[,'1d'], dataHAR[foreOutHor,"RVd"] - forecastOutAR1[,'1d'], 
        alternative= "two.sided", h=1, power=2)$statistic #  AR22 > AR1
dmResults["ARFIMA","AR1"] <- dm.test(dataHAR[foreOutHor,"RVd"] - forecastOutARFIMA[,'1d'], dataHAR[foreOutHor,"RVd"] - forecastOutAR1[,'1d'], 
        alternative= "two.sided", h=1, power=2)$statistic #ARFIMA > AR1

dmResults["HARQ","AR3"] <- dm.test((dataHAR[foreOutHor,"RVd"] - forecastOutHARQRVd[,'1d']), dataHAR[foreOutHor,"RVd"] - forecastOutAR3[,'1d'], 
                                   alternative= "two.sided", h=1, power=2)$statistic #HARQ > AR3
dmResults["SHAR","AR3"] <- dm.test((dataHAR[foreOutHor,"RVd"] - forecastOutSHARRVd[,'1d']), dataHAR[foreOutHor,"RVd"] - forecastOutAR3[,'1d'], 
                                   alternative= "two.sided", h=1, power=2)$statistic #SHAR > AR3
dmResults["HAR","AR3"] <- dm.test((dataHAR[foreOutHor,"RVd"] - forecastOutHARRVd[,'1d']), dataHAR[foreOutHor,"RVd"] - forecastOutAR3[,'1d'], 
                                  alternative= "two.sided", h=1, power=2)$statistic #  HAR > AR3
dmResults["AR1","AR3"] <- dm.test((dataHAR[foreOutHor,"RVd"] - forecastOutAR1[,'1d']), dataHAR[foreOutHor,"RVd"] - forecastOutAR3[,'1d'], 
                                  alternative= "two.sided", h=1, power=2)$statistic #  AR1 > AR3
dmResults["AR22","AR3"] <- dm.test(dataHAR[foreOutHor,"RVd"] - forecastOutAR22[,'1d'], dataHAR[foreOutHor,"RVd"] - forecastOutAR3[,'1d'], 
                                   alternative= "two.sided", h=1, power=2)$statistic #  AR22 > AR3
dmResults["ARFIMA","AR3"] <- dm.test(dataHAR[foreOutHor,"RVd"] - forecastOutARFIMA[,'1d'], dataHAR[foreOutHor,"RVd"] - forecastOutAR3[,'1d'], 
                                     alternative= "two.sided", h=1, power=2)$statistic #ARFIMA > AR3

dmResults["HARQ","AR22"] <- dm.test((dataHAR[foreOutHor,"RVd"] - forecastOutHARQRVd[,'1d']), dataHAR[foreOutHor,"RVd"] - forecastOutAR22[,'1d'], 
                                   alternative= "two.sided", h=1, power=2)$statistic #HARQ > AR22
dmResults["SHAR","AR22"] <- dm.test((dataHAR[foreOutHor,"RVd"] - forecastOutSHARRVd[,'1d']), dataHAR[foreOutHor,"RVd"] - forecastOutAR22[,'1d'], 
                                   alternative= "two.sided", h=1, power=2)$statistic #SHAR > AR22
dmResults["HAR","AR22"] <- dm.test((dataHAR[foreOutHor,"RVd"] - forecastOutHARRVd[,'1d']), dataHAR[foreOutHor,"RVd"] - forecastOutAR22[,'1d'], 
                                  alternative= "two.sided", h=1, power=2)$statistic #  HAR > AR22
dmResults["AR3","AR22"] <- dm.test((dataHAR[foreOutHor,"RVd"] - forecastOutAR3[,'1d']), dataHAR[foreOutHor,"RVd"] - forecastOutAR22[,'1d'], 
                                  alternative= "two.sided", h=1, power=2)$statistic #  AR3 > AR22
dmResults["AR1","AR22"] <- dm.test(dataHAR[foreOutHor,"RVd"] - forecastOutAR1[,'1d'], dataHAR[foreOutHor,"RVd"] - forecastOutAR22[,'1d'], 
                                   alternative= "two.sided", h=1, power=2)$statistic #  AR1 > AR22
dmResults["ARFIMA","AR22"] <- dm.test(dataHAR[foreOutHor,"RVd"] - forecastOutARFIMA[,'1d'], dataHAR[foreOutHor,"RVd"] - forecastOutAR22[,'1d'], 
                                     alternative= "two.sided", h=1, power=2)$statistic #ARFIMA > AR22

dmResultsp <- as.data.frame(matrix(rep(NA, 49), 7, 7))
colnames(dmResultsp) <- c("HAR", "HARQ", "SHAR", "AR1", "AR3", "AR22", "ARFIMA")
rownames(dmResultsp) <- c("HAR", "HARQ", "SHAR", "AR1", "AR3", "AR22", "ARFIMA")

# Diebold Mariano test to compare forecasting differences
dmResultsp["HAR","HARQ"] <- dm.test((dataHAR[foreOutHor,"RVd"] - forecastOutHARRVd[,'1d']), (dataHAR[foreOutHor,"RVd"] - forecastOutHARQRVd[,'1d']), 
                                   alternative= "two.sided", h=1, power=2)$p.value #HAR > HARQ
dmResultsp["SHAR","HARQ"] <- dm.test((dataHAR[foreOutHor,"RVd"] - forecastOutSHARRVd[,'1d']), (dataHAR[foreOutHor,"RVd"] - forecastOutHARQRVd[,'1d']), 
                                    alternative= "two.sided", h=1, power=2)$p.value #SHAR > HARQ
dmResultsp["AR1","HARQ"] <- dm.test(dataHAR[foreOutHor,"RVd"] - forecastOutAR1[,'1d'], (dataHAR[foreOutHor,"RVd"] - forecastOutHARQRVd[,'1d']), 
                                   alternative= "two.sided", h=1, power=2)$p.value #  AR1 > HARQ
dmResultsp["AR3","HARQ"] <- dm.test((dataHAR[foreOutHor,"RVd"] - forecastOutAR3[,'1d']), (dataHAR[foreOutHor,"RVd"] - forecastOutHARQRVd[,'1d']), 
                                   alternative= "two.sided", h=1, power=2)$p.value #  AR3 > HARQ
dmResultsp["AR22","HARQ"] <- dm.test(dataHAR[foreOutHor,"RVd"] - forecastOutAR22[,'1d'], (dataHAR[foreOutHor,"RVd"] - forecastOutHARQRVd[,'1d']), 
                                    alternative= "two.sided", h=1, power=2)$p.value #  AR22 > HARQ
dmResultsp["ARFIMA","HARQ"] <- dm.test(dataHAR[foreOutHor,"RVd"] - forecastOutARFIMA[,'1d'], (dataHAR[foreOutHor,"RVd"] - forecastOutHARQRVd[,'1d']), 
                                      alternative= "two.sided", h=1, power=2)$p.value #ARFIMA > HARQ

dmResultsp["HARQ","HAR"] <- dm.test((dataHAR[foreOutHor,"RVd"] - forecastOutHARQRVd[,'1d']), (dataHAR[foreOutHor,"RVd"] - forecastOutHARRVd[,'1d']), 
                                   alternative= "two.sided", h=1, power=2)$p.value #HARQ > HAR
dmResultsp["SHAR","HAR"] <- dm.test((dataHAR[foreOutHor,"RVd"] - forecastOutSHARRVd[,'1d']), (dataHAR[foreOutHor,"RVd"] - forecastOutHARRVd[,'1d']), 
                                   alternative= "two.sided", h=1, power=2)$p.value #SHAR > HAR
dmResultsp["AR1","HAR"] <- dm.test(dataHAR[foreOutHor,"RVd"] - forecastOutAR1[,'1d'], (dataHAR[foreOutHor,"RVd"] - forecastOutHARRVd[,'1d']), 
                                  alternative= "two.sided", h=1, power=2)$p.value #  AR1 > HAR
dmResultsp["AR3","HAR"] <- dm.test((dataHAR[foreOutHor,"RVd"] - forecastOutAR3[,'1d']), (dataHAR[foreOutHor,"RVd"] - forecastOutHARRVd[,'1d']), 
                                  alternative= "two.sided", h=1, power=2)$p.value #  AR3 > HAR
dmResultsp["AR22","HAR"] <- dm.test(dataHAR[foreOutHor,"RVd"] - forecastOutAR22[,'1d'], (dataHAR[foreOutHor,"RVd"] - forecastOutHARRVd[,'1d']), 
                                   alternative= "two.sided", h=1, power=2)$p.value #  AR22 > HAR
dmResultsp["ARFIMA","HAR"] <- dm.test(dataHAR[foreOutHor,"RVd"] - forecastOutARFIMA[,'1d'], (dataHAR[foreOutHor,"RVd"] - forecastOutHARRVd[,'1d']), 
                                     alternative= "two.sided", h=1, power=2)$p.value #ARFIMA > HAR

dmResultsp["HARQ","SHAR"] <- dm.test((dataHAR[foreOutHor,"RVd"] - forecastOutHARQRVd[,'1d']), (dataHAR[foreOutHor,"RVd"] - forecastOutSHARRVd[,'1d']), 
                                    alternative= "two.sided", h=1, power=2)$p.value #HARQ > SHAR
dmResultsp["HAR","SHAR"] <- dm.test((dataHAR[foreOutHor,"RVd"] - forecastOutHARRVd[,'1d']), (dataHAR[foreOutHor,"RVd"] - forecastOutSHARRVd[,'1d']), 
                                   alternative= "two.sided", h=1, power=2)$p.value #HARQ > SHAR
dmResultsp["AR1","SHAR"] <- dm.test(dataHAR[foreOutHor,"RVd"] - forecastOutAR1[,'1d'], (dataHAR[foreOutHor,"RVd"] - forecastOutSHARRVd[,'1d']), 
                                   alternative= "two.sided", h=1, power=2)$p.value #  AR1 > SHAR
dmResultsp["AR3","SHAR"] <- dm.test((dataHAR[foreOutHor,"RVd"] - forecastOutAR3[,'1d']), (dataHAR[foreOutHor,"RVd"] - forecastOutSHARRVd[,'1d']), 
                                   alternative= "two.sided", h=1, power=2)$p.value #  AR3 > SHAR
dmResultsp["AR22","SHAR"] <- dm.test(dataHAR[foreOutHor,"RVd"] - forecastOutAR22[,'1d'], (dataHAR[foreOutHor,"RVd"] - forecastOutSHARRVd[,'1d']), 
                                    alternative= "two.sided", h=1, power=2)$p.value #  AR22 > SHAR
dmResultsp["ARFIMA","SHAR"] <- dm.test(dataHAR[foreOutHor,"RVd"] - forecastOutARFIMA[,'1d'], (dataHAR[foreOutHor,"RVd"] - forecastOutSHARRVd[,'1d']), 
                                      alternative= "two.sided", h=1, power=2)$p.value #ARFIMA > SHAR

dmResultsp["HARQ","ARFIMA"] <- dm.test((dataHAR[foreOutHor,"RVd"] - forecastOutHARQRVd[,'1d']), (dataHAR[foreOutHor,"RVd"] - forecastOutARFIMA[,'1d']), 
                                      alternative= "two.sided", h=1, power=2)$p.value #HARQ > ARFIMA
dmResultsp["SHAR","ARFIMA"] <- dm.test((dataHAR[foreOutHor,"RVd"] - forecastOutSHARRVd[,'1d']), (dataHAR[foreOutHor,"RVd"] - forecastOutARFIMA[,'1d']), 
                                      alternative= "two.sided", h=1, power=2)$p.value #SHAR > ARFIMA
dmResultsp["AR1","ARFIMA"] <- dm.test(dataHAR[foreOutHor,"RVd"] - forecastOutAR1[,'1d'], (dataHAR[foreOutHor,"RVd"] - forecastOutARFIMA[,'1d']), 
                                     alternative= "two.sided", h=1, power=2)$p.value #  AR1 > ARFIMA
dmResultsp["AR3","ARFIMA"] <- dm.test((dataHAR[foreOutHor,"RVd"] - forecastOutAR3[,'1d']), (dataHAR[foreOutHor,"RVd"] - forecastOutARFIMA[,'1d']), 
                                     alternative= "two.sided", h=1, power=2)$p.value #  AR3 > ARFIMA
dmResultsp["AR22","ARFIMA"] <- dm.test(dataHAR[foreOutHor,"RVd"] - forecastOutAR22[,'1d'], (dataHAR[foreOutHor,"RVd"] - forecastOutARFIMA[,'1d']), 
                                      alternative= "two.sided", h=1, power=2)$p.value #  AR22 > ARFIMA
dmResultsp["HAR","ARFIMA"] <- dm.test((dataHAR[foreOutHor,"RVd"] - forecastOutHARRVd[,'1d']), (dataHAR[foreOutHor,"RVd"] - forecastOutARFIMA[,'1d']), 
                                     alternative= "two.sided", h=1, power=2)$p.value #ARFIMA > ARFIMA

dmResultsp["HARQ","AR1"] <- dm.test((dataHAR[foreOutHor,"RVd"] - forecastOutHARQRVd[,'1d']), dataHAR[foreOutHor,"RVd"] - forecastOutAR1[,'1d'], 
                                   alternative= "two.sided", h=1, power=2)$p.value #HARQ > AR1
dmResultsp["SHAR","AR1"] <- dm.test((dataHAR[foreOutHor,"RVd"] - forecastOutSHARRVd[,'1d']), dataHAR[foreOutHor,"RVd"] - forecastOutAR1[,'1d'], 
                                   alternative= "two.sided", h=1, power=2)$p.value #SHAR > AR1
dmResultsp["HAR","AR1"] <- dm.test((dataHAR[foreOutHor,"RVd"] - forecastOutHARRVd[,'1d']), dataHAR[foreOutHor,"RVd"] - forecastOutAR1[,'1d'], 
                                  alternative= "two.sided", h=1, power=2)$p.value #  HAR > AR1
dmResultsp["AR3","AR1"] <- dm.test((dataHAR[foreOutHor,"RVd"] - forecastOutAR3[,'1d']), dataHAR[foreOutHor,"RVd"] - forecastOutAR1[,'1d'], 
                                  alternative= "two.sided", h=1, power=2)$p.value #  AR3 > AR1
dmResultsp["AR22","AR1"] <- dm.test(dataHAR[foreOutHor,"RVd"] - forecastOutAR22[,'1d'], dataHAR[foreOutHor,"RVd"] - forecastOutAR1[,'1d'], 
                                   alternative= "two.sided", h=1, power=2)$p.value #  AR22 > AR1
dmResultsp["ARFIMA","AR1"] <- dm.test(dataHAR[foreOutHor,"RVd"] - forecastOutARFIMA[,'1d'], dataHAR[foreOutHor,"RVd"] - forecastOutAR1[,'1d'], 
                                     alternative= "two.sided", h=1, power=2)$p.value #ARFIMA > AR1

dmResultsp["HARQ","AR3"] <- dm.test((dataHAR[foreOutHor,"RVd"] - forecastOutHARQRVd[,'1d']), dataHAR[foreOutHor,"RVd"] - forecastOutAR3[,'1d'], 
                                   alternative= "two.sided", h=1, power=2)$p.value #HARQ > AR3
dmResultsp["SHAR","AR3"] <- dm.test((dataHAR[foreOutHor,"RVd"] - forecastOutSHARRVd[,'1d']), dataHAR[foreOutHor,"RVd"] - forecastOutAR3[,'1d'], 
                                   alternative= "two.sided", h=1, power=2)$p.value #SHAR > AR3
dmResultsp["HAR","AR3"] <- dm.test((dataHAR[foreOutHor,"RVd"] - forecastOutHARRVd[,'1d']), dataHAR[foreOutHor,"RVd"] - forecastOutAR3[,'1d'], 
                                  alternative= "two.sided", h=1, power=2)$p.value #  HAR > AR3
dmResultsp["AR1","AR3"] <- dm.test((dataHAR[foreOutHor,"RVd"] - forecastOutAR1[,'1d']), dataHAR[foreOutHor,"RVd"] - forecastOutAR3[,'1d'], 
                                  alternative= "two.sided", h=1, power=2)$p.value #  AR1 > AR3
dmResultsp["AR22","AR3"] <- dm.test(dataHAR[foreOutHor,"RVd"] - forecastOutAR22[,'1d'], dataHAR[foreOutHor,"RVd"] - forecastOutAR3[,'1d'], 
                                   alternative= "two.sided", h=1, power=2)$p.value #  AR22 > AR3
dmResultsp["ARFIMA","AR3"] <- dm.test(dataHAR[foreOutHor,"RVd"] - forecastOutARFIMA[,'1d'], dataHAR[foreOutHor,"RVd"] - forecastOutAR3[,'1d'], 
                                     alternative= "two.sided", h=1, power=2)$p.value #ARFIMA > AR3

dmResultsp["HARQ","AR22"] <- dm.test((dataHAR[foreOutHor,"RVd"] - forecastOutHARQRVd[,'1d']), dataHAR[foreOutHor,"RVd"] - forecastOutAR22[,'1d'], 
                                    alternative= "two.sided", h=1, power=2)$p.value #HARQ > AR22
dmResultsp["SHAR","AR22"] <- dm.test((dataHAR[foreOutHor,"RVd"] - forecastOutSHARRVd[,'1d']), dataHAR[foreOutHor,"RVd"] - forecastOutAR22[,'1d'], 
                                    alternative= "two.sided", h=1, power=2)$p.value #SHAR > AR22
dmResultsp["HAR","AR22"] <- dm.test((dataHAR[foreOutHor,"RVd"] - forecastOutHARRVd[,'1d']), dataHAR[foreOutHor,"RVd"] - forecastOutAR22[,'1d'], 
                                   alternative= "two.sided", h=1, power=2)$p.value #  HAR > AR22
dmResultsp["AR3","AR22"] <- dm.test((dataHAR[foreOutHor,"RVd"] - forecastOutAR3[,'1d']), dataHAR[foreOutHor,"RVd"] - forecastOutAR22[,'1d'], 
                                   alternative= "two.sided", h=1, power=2)$p.value #  AR3 > AR22
dmResultsp["AR1","AR22"] <- dm.test(dataHAR[foreOutHor,"RVd"] - forecastOutAR1[,'1d'], dataHAR[foreOutHor,"RVd"] - forecastOutAR22[,'1d'], 
                                   alternative= "two.sided", h=1, power=2)$p.value #  AR1 > AR22
dmResultsp["ARFIMA","AR22"] <- dm.test(dataHAR[foreOutHor,"RVd"] - forecastOutARFIMA[,'1d'], dataHAR[foreOutHor,"RVd"] - forecastOutAR22[,'1d'], 
                                      alternative= "two.sided", h=1, power=2)$p.value #ARFIMA > AR22

dmResults[c(1,2,3,7),c(1,2,3,7)]
dmResultsp[c(1,2,3,7),c(1,2,3,7)]/2

xtable(dmResults[c(1,2,3,7),c(1,2,3,7)], digits = 3)
xtable(dmResultsp[c(1,2,3,7),c(1,2,3,7)], digits = 3)

#### Choice of lag with LASSO in HAR-RV model #####
# Current lag structure 1,5,10 and 1,5,20
library(glmnet)
test <- glmnet(y=realVol[21:1249,'RVd'], x=cbind(realVol[1:1229,'RVd'], realVol[2:1230,'RVd'], realVol[3:1231,'RVd'], realVol[4:1232,'RVd'], realVol[5:1233,'RVd']))
test$beta
names(test)

library(lars)
larstest <- lars(y=realVol[21:1249,'RVd'], x = cbind(realVol[1:1229,'RVd'],  realVol[2:1230,'RVd'], 
                                         realVol[3:1231,'RVd'],  realVol[4:1232,'RVd'], 
                                         realVol[5:1233,'RVd'],  realVol[6:1234,'RVd'], 
                                         realVol[7:1235,'RVd'],  realVol[8:1236,'RVd'], 
                                         realVol[9:1237,'RVd'],  realVol[10:1238,'RVd'], 
                                         realVol[11:1239,'RVd'], realVol[12:1240,'RVd'], 
                                         realVol[13:1241,'RVd'], realVol[14:1242,'RVd'], 
                                         realVol[15:1243,'RVd'], realVol[16:1244,'RVd'], 
                                         realVol[17:1245,'RVd'], realVol[18:1246,'RVd'], 
                                         realVol[19:1247,'RVd'], realVol[20:1248,'RVd'], 
                                         realVol[21:1249,'RVd']), type = "lasso")

names(larstest)
larstest$beta
larstest$lambda

#####################################################################################
# Remarks
# Annualization
# 'Last' values before 30-03-2009 / 14333 divided by 100
# weekly: rolling or per calendar wee, same for month?

# AR(1)
#set.seed(123)
#arima.sim(sumAR1, 1250, rand.gen = rnorm, n.start = NA)
#simAR1 <- arima.sim(n = 1250, n.start = 1250, list(ar = c(sumAR1$coeff[1], sumAR1$coeff[2]),  sd = sqrt(sumAR1$sigma2)))    
#summary(simAR1) 
#summary(diff(realVol[,'RVd']))

#names(sumAR1)
#arima.sim(n = 600, list(ar = c(-0.3983, -0.0002), sd = sqrt(0.06374)))

# Test model with different lag order (always 3 lags, best fit)


