data <- read.csv("data145.csv")
head(data)
data$date <- as.Date(data$date, format="%d-%b-%Y")
library(MSwM)
msm <- msmFit(data, k=2)
is.na(data)
str(data)
install.packages("xts")
library(xts)
time_series_data <- xts(data[, -1], order.by = data$date)

plot(time_series_data, legend.loc = "topleft")
legend("topleft", legend = colnames(time_series_data), col = 1:ncol(time_series_data), lty = 1, cex = 0.8)
stock_returns <- coredata(time_series_data)
as.numeric(coredata(time_series_data))

msm <- msmFit(stock_returns, k=2, sw= TRUE, p=1)
msm
