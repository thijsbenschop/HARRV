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
# Set working directory

setwd("/Users/vaughan/Desktop/Realiyed volatility 2014/Souhir")    # Mac
# 1 #### Read-in data ####
getwd()
data15 <- read.table("/Users/vaughan/Desktop/co2_last.txt")
dim(data15)
names(data15)

require(data.table)
temp <- as.data.table(data15)
temp[, "date":=   unlist(lapply(strsplit(as.character(datetime), split=" ", fixed = TRUE), '[[', 1))]
temp[, "time":=   unlist(lapply(strsplit(as.character(datetime), split=" ", fixed = TRUE), '[[', 2))]
temp$date <- as.Date(temp$date, "%Y-%m-%d")

data <- data.frame(Date.G.=temp$date, Time.G.=temp$time, Last=temp$Last)
dataBind <-data 
# Drop days without any trading (holidays, weekends)
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

# Weekly, biweekly and monthly realized volatility (average)
realVol[,'RVw']  <- filter(realVol[,'RVd'], rep(1/5, 5), sides=1)
realVol[,'RV2w'] <- filter(realVol[,'RVd'], rep(1/10, 10), sides=1)
realVol[,'RVm']  <- filter(realVol[,'RVd'], rep(1/21, 21), sides=1) # this should be 22?

# Estimate HAR model
dataHAR           <- cbind(realVol[,c("Date", "RVd", "RV2w", "RVm", "Last")], rep(NA, nrow(realVol)))
colnames(dataHAR) <- c("Date", "RVd", "RVw", "RVm", "Last", "RVd1")
dataHAR[1 : (nrow(realVol) - 1), 'RVd1'] <- realVol[2 : nrow(realVol), 'RVd']

# take 2 weeks or 1 week? / use LASSO to select the lag structure
sumHAR <- summary(lm(RVd1 ~ RVd + RVw + RVm, data = as.data.frame(dataHAR[21:1249,])))

##### Simulation of option prices on futures #####
# Step 1: Simulate volatility
startDate = as.numeric(as.Date("2017-12-06")) # start date (year, month, day)
coef      <- sumHAR$coefficients[, "Estimate"]
startVals <- realVol[(which(realVol[,"Date"] == startDate) - 21):which(realVol[,"Date"] == startDate), "RVd"]
S0        <- realVol[which(realVol[,"Date"] == startDate), "Last"] 
meanInov  <- mean(sumHAR$residuals)
sdInov    <- sd(sumHAR$residuals)
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

# For several dates
#as.Date(realVol[,"Date"])
Dates <- c(                              "2012-03-01", "2012-05-01", "2012-07-02", "2012-09-03", "2012-11-01",
                                         "2013-01-02", "2013-03-01", "2013-05-01", "2013-07-01", "2013-09-02", "2013-11-01",
                                         "2014-01-02", "2014-03-03", "2014-05-01", "2014-07-01", "2014-09-01", "2014-11-03",
                                         "2015-01-02", "2015-03-02", "2015-05-01", "2015-07-01", "2015-09-01", "2015-11-02",
                                         "2016-01-04", "2016-03-01", "2016-05-02", "2016-07-01", "2016-09-01", "2016-11-01",
                                         "2017-01-02", "2017-03-01", "2017-05-02", "2017-07-04", "2017-09-01", "2017-11-01",
                                         "2018-01-02", "2018-03-01", "2018-05-01", "2018-07-02", "2018-09-03", "2018-11-01")
results <- c()

set.seed(123524)
for (i in 1:length(Dates)){
  print(i)
  startDate = as.numeric(as.Date(Dates[i]))
  #print(which(realVol[,"Date"] == startDate))
  coef      <- sumHAR$coefficients[, "Estimate"]
  startVals <- realVol[(which(realVol[,"Date"] == startDate) - 21):which(realVol[,"Date"] == startDate), "RVd"]
  S0        <- realVol[which(realVol[,"Date"] == startDate), "Last"] 
  meanInov  <- mean(sumHAR$residuals)
  sdInov    <- sd(sumHAR$residuals)
  
  resSim <- c()
  for(i in 1:252){ # one year
    newVal    <- coef[1] + coef[2] * startVals[21] + coef[3] * mean(tail(startVals, n = 5)) + coef[4] * mean(tail(startVals, n = 21)) + rnorm(1, meanInov, sdInov)
    resSim    <- c(resSim, newVal)
    startVals <- c(startVals[2:21], newVal)
  }
  Kseq <- seq(from = 0.8 * S0, to = 1.2 * S0, by = (0.4 * S0) / 25)
  a <- sapply(Kseq, function(x){MCOptionPrice(r = 0.01, gamma = 1, resSim, k = x, numSim = 1000)})
  plot(a)
  a <- cbind(a, Kseq)
  
  # implied volatility for K = 15-20 with increments of 0.2
  c <- c()
  for(i in 1:26){
    c <- c(c, implVol(Price = a[i, 1], K = a[i, 2], Rf = 0.01, ttm = 1, S0 = S0))
    #print(implVol(Price = a[i, 1], K = a[i, 2], Rf = 0.01, ttm = 1, S0 = S0))
  }
  results <- cbind(results, c)
}

# Plot implied volatility smiles
d <- cbind(seq(from = 0.8, to = 1.2, by = 0.4 / 25), results)
head(d)
plot(rowMeans(d[,2:20]) ~ d[,1], ylim = c(0,0.6), type = "l", xlab = "Moneyness", ylab = "Implied volatility", lwd = 3)

for(i in 1:19){
  par(new = TRUE)
  plot(d[,i+1] ~ d[,1], ylim = c(0,0.6), type = "l", xlab = "Moneyness", ylab = "Implied volatility", lwd = 2, col = grey.colors(20)[i])
}

png(file = "multiSmile.png", width = 16000, height = 8000, res = 700)
par(mar=c(5.1, 5.1, 4.1, 2.1), mgp=c(3, 1, 0), las=0)
for(i in 1:19){
  plot(d[,i+1] ~ d[,1], ylim = c(0,0.6), type = "l", xlab = "Moneyness", ylab = "Implied volatility", lwd = 6, col = grey.colors(20)[i], cex.lab = 2.5, cex.axis = 2.5)
  par(new = TRUE)
}
plot(rowMeans(d[,2:20]) ~ d[,1], ylim = c(0,0.6), type = "l", xlab = "Moneyness", ylab = "Implied volatility", lwd = 12, cex.lab = 2.5, cex.axis = 2.5)
dev.off()
par(mar=c(5.1, 4.1, 4.1, 2.1), mgp=c(3, 1, 0), las=0)  
