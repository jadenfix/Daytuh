#install.packages("xts")
library(xts)
data <- read.csv("data1451.csv")


data$spdividend <- log(data$spdividend)
data$spearnings <- log(data$spearnings)
data$cpi <- log(data$cpi)
data$CPIrent <- log(data$CPIrent)
data$employeecomp <- log(data$employeecomp)
data$commercialbankcredit<- log(data$commercialbankcredit)
data$nonrevolvingconsumercredit <- log(data$nonrevolvingconsumercredit)
data$incomeonassets <- log(data$incomeonassets)
data$PCE <- log(data$PCE)
data$mktvalueprivatedebt <- log(data$mktvalueprivatedebt)
data$mktvaluegrossfeddebt <- log(data$mktvaluegrossfeddebt)
data$m1 <- log(data$m1)
data$m2 <- log(data$m2)

data$date <- as.Date(data$date, format = "%m/%d/%Y")
data <- data[301:nrow(data), ]

# pre-regression  tests 
time_series_data <- xts(data[, -1], order.by = data$date)

cor_matrix <- cor(time_series_data)
print(cor_matrix)
write.csv(cor_matrix, file = "correlation_matrix.csv")

#as ts 
library(MSwM)

olsDJ <- lm(DJ ~ spdividend + spearnings + cpi + LRIR + ffrate + 
              CPIrent + mktyield1yr + mktyield3yr + mktyield5yr + 
              mktyield10yr + mktyield20yr + employeecomp + PCE + PS + newhousingunits + 
              mktvalueprivatedebt + mktvaluegrossfeddebt + 
              BAA10yr + industrialprod + securitiesinbankcredit + commercialbankcredit + 
              pctchangeTCC + nonrevolvingconsumercredit + m1 + m2 + 
              incomeonassets, data =data)
#
olsSP <- lm(spprice ~ spdividend + spearnings + cpi + LRIR + ffrate + 
              CPIrent + mktyield1yr + mktyield3yr + mktyield5yr + 
              mktyield10yr + mktyield20yr + employeecomp + PCE + PS + newhousingunits + 
              mktvalueprivatedebt + mktvaluegrossfeddebt + 
              BAA10yr + industrialprod + securitiesinbankcredit + commercialbankcredit + 
              pctchangeTCC + nonrevolvingconsumercredit + m1 + m2 + 
              incomeonassets, data = data)
summary(olsDJ)
summary(olsSP)

#install.packages("tseries")
library(tseries)
#install.packages("sandwich")
library(sandwich)


plot(residuals(olsSP), xlab = "Time", ylab = "Residuals", main = "Residuals Plot OLS SP")
plot(residuals(olsDJ), xlab = "Time", ylab = "Residuals", main = "Residuals Plot OLS DJ")

#install.packages(car)
library(car)


vif_valuesDJ <- car::vif(olsDJ)
vif_valuesSP <- car::vif(olsSP)

print(vif_valuesDJ)
print(vif_valuesSP)
#k=2 regimes, 10 vars + one intercept =27 + 1 for volatility = 28
msSP2 <- msmFit(olsSP, k=2, sw=rep(TRUE, 28))
msDJ2 <- msmFit(olsDJ, k=2, sw=rep(TRUE, 28))
summary(msSP2)
summary(msDJ2)
#SP
AIC(msSP2)
AIC(msDJ2)
#visualization 
par(mar=c(3,3,3,3))
plotProb(msDJ2, which=1)
plotProb(msDJ2, which=2)
plotProb(msSP2, which=1)
plotProb(msSP2, which=2)

#diagnotic tests DJ
plotDiag(msDJ2, regime=1, which=1)
plotDiag(msDJ2, regime=1, which=2)
plotDiag(msDJ2, regime=2, which=1)
plotDiag(msDJ2, regime=2, which=2)
#SP
plotDiag(msSP2, regime=1, which=1)
plotDiag(msSP2, regime=1, which=2)
plotDiag(msSP2, regime=2, which=1)
plotDiag(msSP2, regime=2, which=2)
