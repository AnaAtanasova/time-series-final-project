library(readxl)
library(tseries)
library(ffp)
library(fpp)
library(randtests)
library(astsa)
library(ggplot2)


BKNG <- read_excel("BKNG.xlsx")
View(BKNG)
BKNG_ts = ts(BKNG$`Adj Close`, start = c(2011, 1, 1), end = c(2022, 12, 1), frequency = 12)
BKNG_ts
plot(BKNG_ts)
class(BKNG_ts) #we get that our data is transformed into time series

#checking properties
mean(BKNG_ts) #1460.702
var(BKNG_ts) #333085.4

#seasonal and trend decomposition using Loess
decomp = stl(BKNG_ts, s.window = "per")
plot(decomp)

#checking if there is seasonality
acf(BKNG_ts)
nsdiffs(BKNG_ts) #0

#checking the presence of a trend
cox.stuart.test(BKNG_ts) #p-value < 2.2e-16
rank.test(BKNG_ts)       #p-value < 2.2e-16

#checking analitically if we need to stabilize the variance
x1BKNG = window(BKNG_ts, start = c(2011, 1), end = c(2016, 12))
var(x1BKNG) # var of first part 112963.7
x2BKNG = window(BKNG_ts, start = c(2017, 1))
var(x2BKNG) # var of second part 61604.91

#stablizing the variance
BKNG_log = log(BKNG_ts)
mean(BKNG_log) #7.18734
var(BKNG_log) #0.230348
plot.ts(BKNG_log)

#removing the trend by first difference
BKNG_difflog = diff(BKNG_log)
mean(BKNG_difflog)#0.01082641
var(BKNG_difflog)#0.007799019
plot.ts(BKNG_difflog) #stationary

#checking the stationary
adf.test(BKNG_difflog)#It is stationary pvalue = 0.01
kpss.test(BKNG_difflog, null = c ("Level", "Trend"), lshort = TRUE)#it is stationary pvalue = 0.1

#making plot of all graphs to compare 
plot.ts(cbind(BKNG_ts, BKNG_log, BKNG_difflog), main = "Graph comparison")

#create split point for training and testing set
split_point = floor(0.8 * length(BKNG_ts))
split_point # the split point is 115 observations 80% from 144

# Split the data into training and testing sets
training_data = BKNG_ts[1:split_point]
test_data = BKNG_ts[(split_point + 1):length(BKNG_ts)]

# Verify the sizes of the training and testing sets
print(length(training_data)) #115 ---> 80%
print(length(test_data))  #29  ---> 20%
plot.ts(training_data)

#modeling training set
BoxCox.lambda(training_data) # 0.2805343

#stabilizing variance
training_log = log(training_data)
mean(training_log) #7.070122
var(training_log)  #0.2163765
plot.ts(training_log) # we can see that it has trend

#making first difference to eliminate trend
traininglog_diff = diff(training_log)
mean(traininglog_diff) #0.01189051
var(traininglog_diff)  #0.0067788
plot.ts(traininglog_diff) # we can see we dont have trend anymore

#checking starionarity using unit root test
adf.test(traininglog_diff) #stationary 
kpss.test(traininglog_diff, null = c("Level", "Trend"), lshort = TRUE)# we reject H0, value = 0.1 so it is stationary

#ploting all graphs for training set together
plot.ts(cbind(training_data, training_log, traininglog_diff), main = "Training set modifications")

#making acf functions to find the right model
par(mfrow = c(2,1))
acf(traininglog_diff, 50)
pacf(traininglog_diff, 50)

#acf takes non null value for 1
#pacf non null for 1
#so the possible model are ARIMA(1,1,0) or ARIMA(0,1,1)


#FINAL
sarima(training_log,1,1,0,0,0,0)
sarima(training_log,0,1,1,0,0,0)
#Since tha AICc is smaller in ARIMA(1,1,0) we go ahead with the latter 

#checking if our choice is correct...and yes it is
fit=auto.arima(training_log)
fit

smodel = arima(training_log, order = c(1,1,0), seasonal = list(order = c(0,0,0), period = 12))
smodel$var.coef

#Residual analysis - Ljung-Box
residuals_model = smodel$residuals
residuals_model
mean(residuals_model) # 0.01429932
var(residuals_model)  # 0.006329626

#test to check the normality of residuals
shapiro.test(residuals_model) # NO REJECT p-value = 0.3691
ks.test(residuals_model, "pnorm", mean(residuals_model), sd(residuals_model)) # pvalue = 0.9928
hist(residuals_model, prob = TRUE)

#forecasting future observation
forecast_data = forecast(fit, h=29)
accuracy(forecast_data)
plot(forecast_data)

Box.test(forecast_data$residuals) # p-value = 0.9103

test_data_log = log(test_data)# we make log of the test set to fit our training set

#we plot on the same graph the forecast and the test set
autoplot(forecast_data)+
  autolayer(ts(test_data_log, start= length(training_data)), series="forecast")
accuracy(forecast_data, test_data_log)

#forecast with exponential smoothing methods
fit2 = ets(training_data, model = "MAN")
fcast = forecast(fit2, h=12)
plot(fcast)
accuracy(fcast)

fit_auto = ets(training_data)
summary(fit_auto)
accuracy(fit_auto)
plot(fit_auto)