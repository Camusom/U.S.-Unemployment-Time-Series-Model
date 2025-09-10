library(TSA)
library(tseries)
library(forecast)
library(ggplot2)
library(zoo)
library(lubridate)
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






#Comprehensive Forecast Graph:

historical_df <- data.frame(
  Date = as.Date(as.yearmon(time(unemployment))),
  Rate = as.numeric(unemployment)
)
forecast_df <- data.frame(
  Date = as.Date(as.yearmon(time(forecast_values$mean))),
  Point_Forecast = as.numeric(forecast_values$mean),
  Lo_80 = as.numeric(forecast_values$lower[, "80%"]),
  Hi_80 = as.numeric(forecast_values$upper[, "80%"]),
  Lo_95 = as.numeric(forecast_values$lower[, "95%"]),
  Hi_95 = as.numeric(forecast_values$upper[, "95%"])
)

# Extract the forecasted values and actual values
forecasted <- as.numeric(forecast_values$mean)
actual <- as.numeric(future_unemployment)

# Calculate MAE (Mean Absolute Error)
mae <- mean(abs(forecasted - actual))
print(paste("MAE:", round(mae, 4)))

# --- 3. Create the Plot with a Dedicated Labeling Area ---

# Define a start date to zoom in on
plot_start_date <- as.Date("2022-01-01")

# Get specific data points for placing labels
last_historical_point <- tail(subset(historical_df, Date >= plot_start_date), 1)
first_forecast_point <- head(forecast_df, 1)
last_forecast_point <- tail(forecast_df, 1) 

# Calculate the new end date for our plot's x-axis limit
plot_end_date <- max(forecast_df$Date) %m+% months(4) # Add 4 months of padding

#specific date for lables 
label_placement_date <- max(forecast_df$Date) %m+% months(2) #labels are 2 months after forecast

forecast_plot_final <- ggplot() +
  
  # --- Ribbons and Lines 
  geom_ribbon(data = forecast_df, aes(x = Date, ymin = Lo_95, ymax = Hi_95), fill = "grey85") +
  geom_ribbon(data = forecast_df, aes(x = Date, ymin = Lo_80, ymax = Hi_80), fill = "grey70") +
  geom_line(data = subset(historical_df, Date >= plot_start_date), aes(x = Date, y = Rate), color = "#0072B2", linewidth = 1) +
  geom_line(data = forecast_df, aes(x = Date, y = Point_Forecast), color = "#D55E00", linewidth = 1.2) +
  
  # --- Direct Annotations
  geom_text(data = last_historical_point, aes(x = Date, y = Rate), 
            label = "Historical", color = "#0072B2", fontface = "bold", 
            hjust = 1.1, vjust = -0.5) +
  geom_text(data = first_forecast_point, aes(x = Date, y = Point_Forecast), 
            label = "Forecast", color = "#D55E00", fontface = "bold", 
            hjust = -0.1, vjust = -0.5) +
  
 
  annotate("text", 
           x = label_placement_date, 
           y = last_forecast_point$Hi_95,
           label = "95% CI", color = "grey40", size = 3.5, vjust = 0.5, 
           hjust = 1) + 
  annotate("text", 
           x = label_placement_date, 
           y = last_forecast_point$Hi_80,
           label = "80% CI", color = "grey20", size = 3.5, vjust = 0.5, 
           hjust = 1) + 
  
  # labels 
  labs(
    title = "U.S. Unemployment Rate Forecast",
    subtitle = "ARIMA Forecast with 80% and 95% Confidence Intervals",
    x = "Year",
    y = "Unemployment Rate (%)"
  ) +
  coord_cartesian(xlim = c(plot_start_date, plot_end_date), 
                  ylim = c(min(forecast_df$Lo_95), max(forecast_df$Hi_95) + 0.1),
                  expand = FALSE, 
                  clip = "off") +
  
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 18),
    plot.subtitle = element_text(size = 12, color = "grey30"),
    panel.grid.minor = element_blank(),
    plot.margin = margin(10, 40, 10, 10) 
  )


ggsave(
  "upwork_thumbnail_unemployment_final.png", 
  plot = forecast_plot_final,
  width = 9,
  height = 6,
  dpi = 300,
  bg = "white"
)