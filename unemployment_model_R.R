library(TSA)
library(tseries)
library(forecast)
#data reading
data = read.csv("C:\\Users\\Massimo Camuso\\Desktop\\Academics\\Fall 2024\\Time Series\\Final Project\\clean_unemployment_data.csv")
data
#convert data to TS object
unemployment <- ts(data$overall_rate, start = c(1948,1), frequency = 12)
#original data
plot(unemployment, main = "U.S. Unemployment Rate", ylab = "Rate", xlab="Year")
#finding the suitable model:
acf(unemployment)
pacf(unemployment)
ndiffs(unemployment)
#According to ndiffs test and acf/pacf, one order of differencing is needed and 1 AR term.
#Although ACF plot did not point to the need of an MA term, AIC is smaller by around 5-6 when 1 MA term is included.
#ARIMA(1,1,1) more accurately fits the data and captures the relationships

#model
#ARIMA(1,1,1) MODEL
model <- auto.arima(unemployment)
fitted <- fitted(model)
plot(unemployment, main="FITTED ARIMA vs Actual",lwd=2)
lines(fitted, col='red')
summary(model)


#ARIMA(1,1,0) MODEL (WORSE MODEL, PUT HERE FOR COMPARISON)
model_man <- arima(unemployment,order=c(1,1,0))
AIC(model_man)
summary(model_man)

#model diagnostics
checkresiduals(model)
plot(fitted, residuals(model))
abline(h=0,col="red")

#forecasting
forecast_values <- forecast(model, h=6)

#reading the unemployment rates for june-november
dataF <- read.csv("C:\\Users\\Massimo Camuso\\Desktop\\Academics\\Fall 2024\\Time Series\\Final Project\\future_rates_to_compareto.csv")
future_unemployment <- ts(dataF$UNRATE, start=c(2024,6),frequency = 12)
plot(future_unemployment, ylim=c(3.5,4.5), main="Forecasted Unemployment Rates June-November", ylab='Forecasted VS Actual',xlab='Month (June-November)')
lines(forecast_values$mean,col='red')