library(astsa)
# Applied Statistical Time Series Analysis

# Loading Original Dataframe
input_data = jj

# To count the number of quarters per instance
frequency(input_data)

# Year vs Earning per Quarter - plot
plot(input_data)

# Converting dataset in the way, mean and variance becomes constant
# Log Function - Make Variance even
# Differentiation - Make mean constatn throughout series 

plot(diff(log(input_data)))

# Building model - ARIMA Model

# ARIMA - Auto Regressive Integration Moving Average

# To find value of q
# Auto corelation Function
acf(diff(log(input_data)))
q = 0

# To find value of p
# Partial Auto Corelation Function
pacf(diff(log(input_data)))
p = 0

# Building our model
# p = 0, d = 1, q = 1
# d = 1 denotes, data has to be differentiated once
arima_model = arima(log(input_data), c(0, 1, 0), seasonal = list(order = c(0, 1, 0), period = 4))

# Predicting what will be the earnings for all the four quarters for the next 10 years with our model 
test_model = predict(arima_model, n.ahead = 10*4)

# Converting log value to decimal values - as we have used log function
test_values = 2.718^test_model$pred

# Plot the input data and prediction in single plot
ts.plot(input_data,test_values, log = "y", lty=c(1,3))

# To check how our model performs 

# Original Dataset contains values for years 1960 to 1980
# To test our model, training data is going to have values between 1960 and 1979
filtered_data = ts(input_data, frequency = 4, start = c(1960,1), end = c(1979,4))

# Fit our model with the filtered data
test_arima_model = arima(log(filtered_data), c(0, 1, 0), seasonal = list(order = c(0, 1, 0), period = 4))

# Predicting for the year 1980
predict_1980 = predict(test_arima_model, n.ahead = 1 * 4)

predict_1980_values = 2.718^predict_1980$pred

# Round off to nearby integer
predict_1980_values = round(predict_1980_values, digits = 0)

# original Values
original_1980 = tail(input_data, 4)

# Plot - predicted(black) vs original(red)
ts.plot(predict_1980_values, original_1980, gpars = list(col = c("black", "red")))

#               sample Result for the year 1980 

#           Qtr1       Qtr2         Qtr3        Qtr4
#Our model 15.73732   14.52688    16.64515    11.19810
#Original  16.20      14.67       16.02       11.61
