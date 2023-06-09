setwd("/Users/matthias/Google Drive/_________HSLU/_Time Series in Finance/TSA_Project")
# Load libraries
library(tidyverse)
library(xts)
library(forecast)
library(fBasics)
library(ggplot2)
library(tseries)
library(lmtest)
library(quantmod)
library(vars)

# Inputs - EIKON (Thomson & Reuters)
df1 <- read.csv2(file="df1.csv", sep = ',', header = TRUE)
df2 <- read.csv2("df2.csv", sep=',', header = TRUE)

## Prepare data
colnames(df1) <- paste(sep = '_', colnames(df1), as.character(unlist(df1[1,])))
df1<-df1[-c(1:2),]

# Extracting all CLOSING prices
df1<-cbind(df1[,1],dplyr::select(df1, colnames(df1)[grepl("CLOSE", colnames(df1))]))
colnames(df1)[1]<-"Date"

# Merging datasets
EUR <- df2[, c('Date', 'CLOSE')]
colnames(EUR)[2]<-"EUR"
df<-left_join(df1, EUR, by="Date")

# df <- df[df$Date>"2019-07-31",] # Start with month for which all variables have first observations
df <- xts(df, order.by = as.POSIXct(df$Date))[,-1]
storage.mode(df) <- "numeric"

# Interpolate NA's
df <- na.approx(df, rule = 2)

# Rename columns
colnames(df)[1:5] <- c("Electricity_Price_DE", "Gas_Price_NL","Brent_Oil", "WTI_Oil_Price", "CO2_Certificate_Price")

# create subset
df <- subset(df, select = c("Electricity_Price_DE", "Gas_Price_NL","Brent_Oil", "WTI_Oil_Price", "EUR"))
colnames(df)

# Convert oil prices from USD to EUR
df$Brent_Oil <- df$Brent_Oil/df$EUR
df$WTI_Oil_Price <- df$WTI_Oil_Price/df$EUR

# remove EUR values from subset
df <- subset(df, select = c("Electricity_Price_DE", "Gas_Price_NL","Brent_Oil", "WTI_Oil_Price"))

# ------------------------------------------------------------------------------
# ----------------------------DESCRIPTIVE-STATISTICS----------------------------
# ------------------------------------------------------------------------------
## Time Series Plot
plot(df)
addLegend(legend.loc="topleft",lty = 1, text.col=1, bg="white", bty=1)

# Electricity prices DE [EUR] - Futures
electricity_Price_DE <- ggplot(df, aes(index(df), df$Electricity_Price_DE)) +
  geom_line(colour="grey50", size = .65) +
  ggtitle("German Electricity Prices [Futures]") + xlab('Year') + ylab('Prices in EUR') +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

electricity_Price_DE
png(filename = "plots/electricity_Price_DE")
plot(electricity_Price_DE)
dev.off()

# Gas prices NL [EUR] - Futures
gas_Price_NL <- ggplot(df, aes(index(df), df$Gas_Price_NL)) +
  geom_line(colour="grey50", size = .65) +
  ggtitle("Gas Prices in the Netherlands [Futures]") + xlab('Year') + ylab('Prices in EUR') +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

gas_Price_NL
png(filename = "plots/gas_Price_NL")
plot(gas_Price_NL)
dev.off()

# Brent oil prices -[EUR]  Futures
brent_Price <- ggplot(df, aes(index(df), df$Brent_Oil)) +
  geom_line(colour="grey50", size = .65) +
  ggtitle("Brent Oil Prices [Futures]") + xlab('Year') + ylab('Prices in EUR') +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

brent_Price
png(filename = "plots/brent_Price")
plot(brent_Price)
dev.off()

# WTI oil prices [EUR] - Futures
wti_Price <- ggplot(df, aes(index(df), df$WTI_Oil_Price)) +
  geom_line(colour="grey50", size = .65) +
  ggtitle("WTI OIL Prices [Futures]") + xlab('Year') + ylab('Prices in EUR') +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

wti_Price
png(filename = "plots/wti_Price")
plot(wti_Price)
dev.off()
## END Time Series Plot

# correlation between electricity prices and commodities
cor(df$Electricity_Price_DE, df$Gas_Price_NL) # correlation -> 0.9884307: strong correlation
cor(df$Electricity_Price_DE, df$Brent_Oil) # correlation -> 0.7332616: strong correlation
cor(df$Electricity_Price_DE, df$WTI_Oil) # correlation -> 0.7453263: strong correlation

# ------------------------------------------------------------------------------
# -------------------------------STATIONARITY-----------------------------------
# ------------------------------------------------------------------------------
# Test if time series are stationary
adf.test(df$Electricity_Price_DE) # p-value = 0.99 -> non stationary
adf.test(df$Gas_Price_NL) # p-value = 0.99 -> non stationary
adf.test(df$Brent_Oil) # p-value = 0.8118 -> non stationary
adf.test(df$WTI_Oil_Price) # p-value = 0.8046 -> non stationary

# Calculating continuous returns
dElectricity_Price_DE<- diff(log(df$Electricity_Price_DE))
dElectricity_Price_DE<- na.approx(dElectricity_Price_DE, rule = 2)
dGas_Price_NL <- diff(log(df$Gas_Price_NL))
dGas_Price_NL<- na.approx(dGas_Price_NL, rule = 2)
dBrent_Oil <- diff(log(df$Brent_Oil))
dBrent_Oil<- na.approx(dBrent_Oil, rule = 2)
dWTI_Oil<- diff(log(df$WTI_Oil_Price))
dWTI_Oil<- na.approx(dWTI_Oil, rule = 2)

### Plotting the continuous returns###
par(mfrow=c(3,2))
plot(dElectricity_Price_DE, main='Returns of electricity prices in Germany', ylab='In EUR', xlab='Year')
plot(dGas_Price_NL, main='Returns of gas prices in the Netherlands', ylab='In EUR', xlab='Year')
plot(dBrent_Oil, main='Returns of Brent Oil prices', ylab='In USD', xlab='Year')
plot(dWTI_Oil, main='Reutrns ofWTI Oil Prices', ylab='In USD', xlab='Year')

# plotting histogram of continuous returns
par(mfrow=c(3,2))
# Energy price returns appear to be slightly left-skewed indicating leptokurtosis
hist(dElectricity_Price_DE, breaks = 50, main = "Histogram of electricity prices returns in Germany", xlab="electricity prices returns in Germany")
hist(dGas_Price_NL, breaks = 50, main = "Histogram of gas prices returns in the Netherlands", xlab="gas prices returns in the Netherlands")
hist(dBrent_Oil, breaks = 50, main = "Histogram of Brent oil returns", xlab="brent oil returns")
hist(dWTI_Oil, breaks = 50, main = "Histogram of WTI oil returns", xlab="WTI oil returns")

# print basic stats of continuous returns
basicStats(dElectricity_Price_DE) 
basicStats(dGas_Price_NL)
basicStats(dBrent_Oil)
basicStats(dWTI_Oil) 
# Interpretation: Left-skewness and leptokurtosis are confirmed for all values. 

## Alternative visualization continuous returns: Boxplot
par(mfrow=c(3,2))
boxplot(dElectricity_Price_DE, main="Boxplot of electricity prices returns in Germany [Future]")
boxplot(dGas_Price_NL, main="Boxplot of gas prices returns in the Netherlands [Future")
boxplot(dBrent_Oil, main="Boxplot of Brent oil price [Future]")
boxplot(dWTI_Oil, main="Boxplot of WTI oil price [Future]")

# correlation between ontinuous returns from electricity prices and commodities
cor(dElectricity_Price_DE, dGas_Price_NL) # correlation -> 0.4555898: moderate correlation
cor(dElectricity_Price_DE, dBrent_Oil) # correlation -> 0.07878121: weak correlation
cor(dElectricity_Price_DE, dWTI_Oil) # correlation -> 0.01163529: weak correlation

# Create data frame with continuous returns from the time series
stationary <- data.frame(el_prices = diff(log(df$Electricity_Price_DE)),
                         gas_prices = diff(log(df$Gas_Price_NL)), 
                         brent_prices = diff(log(df$Brent_Oil)),
                         wti_prices = diff(log(df$WTI_Oil_Price))
)

stationary <- na.omit(stationary)

# Test if the continuous returns of time series are stationary
adf.test(stationary$Electricity_Price_DE) # p-value = 0.01 -> stationary
tsdisplay(stationary$Electricity_Price_DE)

adf.test(stationary$Gas_Price_NL) # p-value = 0.01 -> stationary
tsdisplay(stationary$Gas_Price_NL)

adf.test(stationary$Brent_Oil) # p-value = 0.01 -> stationary
tsdisplay(stationary$Brent_Oil)

adf.test(stationary$WTI_Oil_Price) # p-value = 0.01 -> stationary
tsdisplay(stationary$WTI_Oil_Price)

# ------------------------------------------------------------------------------
# -----------------------------MODELING VAR ------------------------------------
# ------------------------------------------------------------------------------

# Define the optimal lag
VARselect(as.ts(stationary), lag.max = 10, type = 'both')
# $selection
# AIC(n)  HQ(n)  SC(n) FPE(n) 
#     8      2      1      8 

# fitting moodel wtih p=10
fit <- VAR(as.ts(stationary), p = 8, ic="AIC")
summary(fit)

# ------------------------------------------------------------------------------
# ------------------------------RESIDUAL-ANALYSIS-------------------------------
# ------------------------------------------------------------------------------

checkresiduals(fit$varresult$Electricity_Price_DE, test="LB")
checkresiduals(fit$varresult$Gas_Price_NL, test="LB")
checkresiduals(fit$varresult$Brent_Oil, test="LB")
checkresiduals(fit$varresult$WTI_Oil_Price, test="LB")

# ------------------------------------------------------------------------------
# ------------------------------CAUSUALITY-EFFECTS------------------------------
# ------------------------------------------------------------------------------

#### bivariate causality testing
# based on https://stats.stackexchange.com/questions/187706/does-the-granger-causality-test-in-the-vars-package-make-sense
granger_bivariate <- function(varest, causal, dep){
  dtmat <- varest$datamat
  mat_target <- dtmat[, c(causal, dep)]
  other_as_exo <- dtmat[, setdiff(names(dtmat),c(causal, dep,'const',names(dtmat)[grepl(paste0('^',causal),names(dtmat)) | grepl(paste0('^',dep),names(dtmat)) ]))]
  var_target <- VAR(mat_target, p = varest$p, exogen = other_as_exo)
  gr_target <- causality(var_target, cause = causal, vcov. = vcovHC(var_target))
  g1 <- gr_target$Granger
  result <-g1$p.value
  return(result)
}

granger_bivariate(fit, causal = "Electricity_Price_DE", dep="Gas_Price_NL")
granger_bivariate(fit, causal = "Gas_Price_NL", dep="Electricity_Price_DE")

granger_bivariate(fit, causal = "Electricity_Price_DE", dep="WTI_Oil_Price")
granger_bivariate(fit, causal = "WTI_Oil_Price", dep="Electricity_Price_DE")

granger_bivariate(fit, causal = "Electricity_Price_DE", dep="Brent_Oil")
granger_bivariate(fit, causal = "Brent_Oil", dep="Electricity_Price_DE")

granger_bivariate(fit, causal = "WTI_Oil_Price", dep="Gas_Price_NL")
granger_bivariate(fit, causal = "Gas_Price_NL", dep="WTI_Oil_Price")

granger_bivariate(fit, causal = "Brent_Oil", dep="Gas_Price_NL")
granger_bivariate(fit, causal = "Gas_Price_NL", dep="Brent_Oil")

granger_bivariate(fit, causal = "WTI_Oil_Price", dep="Brent_Oil")
granger_bivariate(fit, causal = "Brent_Oil", dep="WTI_Oil_Price")

# ------------------------------------------------------------------------------
# --------------------------IMPULSE-RESPONSE-FUNCTION---------------------------
# ------------------------------------------------------------------------------

# Degression: Plotting impulse response functions
plot(irf(fit, impulse="Gas_Price_NL", response="Electricity_Price_DE", vcov. = vcovHC(fit)))
plot(irf(fit, impulse="Brent_Oil", response="Electricity_Price_DE", vcov. = vcovHC(fit)))
plot(irf(fit, impulse="WTI_Oil_Price", response="Electricity_Price_DE", vcov. = vcovHC(fit)))

# Residual test
arch.test(fit) # Test for constant variance, null hypothesis = Residuals are homoscedastic
serial.test(fit, type="BG") # Test for serial correlation, null hypothesis = Residuals are not autocorrelated
