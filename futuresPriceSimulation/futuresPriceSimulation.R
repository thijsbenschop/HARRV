# HAR
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
setwd("/Users/thijsbenschop/Dropbox/Realized volatility CO2/Data")    # Mac
setwd("H:/Documents/Downloads")

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

# (Log) returns (quarter of an hour) (p_t - p_t-1) / (ln(p_t / p_t-1)) (this is across days)
data[, 'return']      <- c(NA, data[2:length(data[,'Last']),'Last'] - data[1:(length(data[,'Last']) -1),'Last']) # return (difference)

data <- data[2:nrow(data),] # Remove first row (no difference/return computed)

# Check (and remove) outliers
head(sort(data[,"return"]), n = 10)
which(data[,"return"] %in% head(sort(data[,"return"]), n = 2))
tail(sort(data[,"return"]), n = 10)

# 3 #### Compute realized volatility #####
# Calculate daily realized volatility (see p.177 in Corsi)
obsPerDay               <- table(data$Date) # number of observations per day
realVol                 <- matrix(NA, length(obsPerDay), 7) # matrix, each row is one day
colnames(realVol)       <- c('Date', 'obsPerDay', 'RVd', 'RVw', 'RV2w', 'RVm', 'Last') #day, month, year
realVol[, 'Date']       <- as.Date(names(obsPerDay)) # dates
realVol[, 'obsPerDay']  <- obsPerDay # number of observations per day

# Daily realized volatility (computed within days) (should we take the log here?)
for (i in 1:length(obsPerDay)) {   # for all trading days
  # Realized volatility (day), Corsi, equation (3)
  dayLast            <- data[data$Date == realVol[i, 'Date'],'return']  # returns for each 15 minute interval of that day
  dayLast            <- dayLast[2:length(dayLast)]                      # remove first return, since calculated across days
  #realVol[i, 'RVd'] <- sqrt(sum(diff(dayLast), na.rm = T)^2)           # squared differences, should we take the square root?
  
  realVol[i, 'RVd']  <- sum(dayLast^2, na.rm = T)           # squared returns, should we take the square root?
  realVol[i, 'Last'] <- last(data[data$Date == realVol[i, 'Date'],'Last'])
}

# Annualization of daily volatility
realVol[,'RVd'] <- realVol[,'RVd'] * sqrt(252)

##### Simulation of option prices on futures #####
# Step 1: Simulate volatility
startDate = as.numeric(as.Date("2011-04-01")) # start date (year, month, day)
#sumHAR # see 170813ImplVol.R
# > sumHAR$coefficients[, "Estimate"]
# (Intercept)         RVd        RV2w         RVm 
# 0.5333709   0.1831072   0.3975752   0.1714059
coef      <- c(0.5333709,  0.1831072, 0.3975752, 0.1714059)
startVals <- realVol[(which(realVol[,"Date"] == startDate) - 21):which(realVol[,"Date"] == startDate), "RVd"]
S0        <- realVol[which(realVol[,"Date"] == startDate), "Last"] 
meanInov  <- -0.0000000000000002899414 # mean(sumHAR$residuals)
sdInov    <- 2.16635 # sd(sumHAR$residuals)
set.seed(123524)
resSim <- c()
for(i in 1:252){ # one year
  newVal    <- coef[1] + coef[2] * startVals[21] + coef[3] * mean(tail(startVals, n = 5)) + coef[4] * mean(tail(startVals, n = 21)) + rnorm(1, meanInov, sdInov)
  resSim    <- c(resSim, newVal)
  startVals <- c(startVals[2:21], newVal)
}
plot(resSim, type = "l")

# Step 2: Simulate futures prices with stochastic volatility model and compute option prices
numSim <- 1000 # number of simulated MC paths

MCOptionPrice = function(r, gamma, resSim, k, numSim) { 
  v.sum = 0 
  for (i in 1:numSim) {
    # resSim <- c()
    # for(i in 1:252){ # one year
    #   newVal    <- coef[1] + coef[2] * startVals[21] + coef[3] * mean(tail(startVals, n = 5)) + coef[4] * mean(tail(startVals, n = 21)) + rnorm(1, meanInov, sdInov)
    #   resSim    <- c(resSim, newVal)
    #   startVals <- c(startVals[2:21], newVal)
    # }
    st    = MCPath(r, gamma, resSim) 
    s.avg = sum(st) / length(st)           # arithmetic average 
    v     = exp(-r) * pmax(0, s.avg - k)   # discounted payoff (average price) 
    v.sum = v.sum + v                      # sum for MC estimate 
  } 
  v.mc = v.sum/numSim  # MC estimate 
  return(v.mc) 
} 

# function for simulating futures price
MCPath = function(r, gamma, IV) { 
  # r,     risk free rate
  # gamma, market price of risk
  # IV,    integrated volatility estimated by relaized volatility
  st = S0 + sapply(resSim, function(x){rnorm(n = 1, mean = r + (gamma - 0.5) * abs(x), sd = abs(x))})
  return(st) 
} 

MCOptionPrice(r = 0.01, gamma =  2, resSim, k = 1, numSim =  1000)

# Step 3: Compute implied volatility for different K
# Compute implied volatility based on simOptionPrices
implVol = function(Price, S0, K, Rf, ttm){
  # Price at lower bound
  low <- 0.0001
  d1 <- (log(S0 / K) + (low / 2) * ttm) / (sqrt(low) * sqrt(ttm))
  d2 <- d1 - (sqrt(low) * sqrt(ttm))
  lowVal <- exp(-Rf * ttm) * (S0 * pnorm(d1) - K * pnorm(d2))
  
  # Price at upperbound
  up <- 1000
  d1 <- (log(S0 / K) + (up / 2) * ttm) / (sqrt(up) * sqrt(ttm))
  d2 <- d1 - (sqrt(up) * sqrt(ttm))
  upVal <- exp(-Rf * ttm) * (S0 * pnorm(d1) - K * pnorm(d2))
  
  while(up - low > 0.0001){
    #print(paste(up, low))
    new  <- (up + low) / 2
    d1   <- (log(S0 / K) + (new / 2) * ttm) / (new * sqrt(ttm))
    d2   <- d1 - (new * sqrt(ttm))
    tempVal <- exp(-Rf * ttm) * (S0 * pnorm(d1) - K * pnorm(d2)) # futures price Black formula
    #print(tempVal)
    if(tempVal < Price){
      low    <- new
      lowVal <- tempVal
    } else {
      up    <- new
      upVal <- tempVal
    }
  }
  return(up) 
}

a <- sapply(seq(from = 15, to = 20, by = 0.2), function(x){MCOptionPrice(r = 0.01, gamma = 1, resSim, k = x, numSim = 1000)})
plot(a)
a <- cbind(a, seq(from = 15, to = 20, by = 0.2))

# implied volatility for K = 15-20 with increments of 0.2
c <- c()
for(i in 1:26){
  c <- c(c, implVol(Price = a[i, 1], K = a[i, 2], Rf = 0.01, ttm = 1, S0 = S0))
  print(implVol(Price = a[i, 1], K = a[i, 2], Rf = 0.01, ttm = 1, S0 = S0))
}

# Step 4: Plot implied volatility smile
d <- cbind (seq(from = 15, to = 20, by = 0.2) / S0, c)
plot(d[,2] ~ d[,1], ylim = c(0,0.4), type = "l", xlab = "Moneyness", ylab = "Implied volatility", lwd = 2)

# Plot prices
png(file = "volSmile.png", width = 12000, height = 6000, res = 900)
plot(d[,2] ~ d[,1], ylim = c(0,0.4), type = "l", xlab = "Moneyness", ylab = "Implied volatility", lwd = 2)
dev.off()
