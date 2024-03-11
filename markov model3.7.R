#install.packages("xts")
library(xts)
data <- read.csv("data145.csv")


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

data$date <- as.Date(data$date, format="%m/%d/%Y")
# last 3 rows gone

#as ts 
time_series_data <- xts(data[, -1], order.by = data$date)
library(MSwM)
olsDJ <- lm(DJ ~ spdividend + spearnings + cpi + LRIR + ffrate + CPIrent + mktyield1yr + mktyield3yr + 
                        mktyield5yr + mktyield10yr + mktyield20yr + employeecomp + PCE + PS + newhousingunits + mktvalueprivatedebt +
                        mktvaluegrossfeddebt + BAA10yr + industrialprod + securitiesinbankcredit + commercialbankcredit +
                        pctchangeTCC + nonrevolvingconsumercredit + m1 + m2 + incomeonassets, data =time_series_data)
summary(olsDJ)
olsSP <- lm(spprice ~ spdividend + spearnings + cpi + LRIR + ffrate + 
              CPIrent + mktyield1yr + mktyield3yr + mktyield5yr + 
              mktyield10yr + mktyield20yr + employeecomp + PCE + PS + newhousingunits + 
              mktvalueprivatedebt + mktvaluegrossfeddebt + 
              BAA10yr + industrialprod + securitiesinbankcredit + commercialbankcredit + 
              pctchangeTCC + nonrevolvingconsumercredit + m1 + m2 + 
              incomeonassets, data = time_series_data)
summary(olsSP)
summary(olsDJ)
#k=2 regimes, 10 vars + one intercept =27 + 1 for volatility = 28
msDJ2 <- msmFit(olsDJ, k=2, sw=rep(TRUE, 28))
msDJ3 <- msmFit(olsDJ, k=3, sw=rep(TRUE, 28))
summary(msDJ2)
summary(msDJ3)
#SP
msSP2 <- msmFit(olsSP, k=2, sw=rep(TRUE, 28))
msSP3 <- msmFit(olsSP, k=3, sw=rep(TRUE, 28))
summary(msSP2)
summary(msSP3)
AIC(msDJ2)
AIC(msDJ3)
AIC(msSP2)
AIC(msSp3)
#visualization 
par(mar=c(3,3,3,3))
plotProb(msDJ2, which=1)
plotProb(msDJ2, which=2)
plotProb(msSP2, which=1)
plotProb(msSP2, which=2)

par(mar=c(3,3,3,3))
plotProb(msDJ3, which=1)
plotProb(msDJ3, which=2)
plotProb(msSP3, which=1)
plotProb(msSP3, which=2)
#diagnotic tests 
plotDiag(ms2, regime=1, which=1)
plotDiag(ms2, regime=1, which=2)
plotDiag(ms2, regime=1, which=3)
#statistical tests:
#
#statistical tests:
#statistical tests:
#AIC indicates better or worse fit 
AIC(ols1)
AIC(ms2)
AIC(ms3)