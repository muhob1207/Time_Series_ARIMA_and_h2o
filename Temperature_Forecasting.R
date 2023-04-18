library(tidyverse)
library(data.table)
library(lubridate)
library(timetk)
library(skimr)
library(highcharter)
library(h2o)
library(tidymodels)
library(modeltime)

raw <- fread('daily-minimum-temperatures-in-me (1).csv')
raw %>% glimpse()
raw %>% skim()

#Changing the column names
colnames(raw) <- c('Date','temperature')

#Changing the data type of the date column
raw$Date <- as.Date(raw$Date, format = "%m/%d/%Y") 


#The temperature column has some values written in the wrong format at the indexes indicated below.
#The format for those values is "?123" and that's why this column is interpreted as string.
raw$temperature %>% as.numeric() %>% is.na() %>% which()

#We need to replace "?" with "". We will use gsub for that.
raw$temperature <- gsub("\\?", "", raw$temperature) 

#Changing the data type of the temperature column
raw$temperature <- raw$temperature %>% as.numeric() 

#Plotting the data.
raw %>% 
  plot_time_series(
    Date, temperature, 
    .interactive = T,
    .plotly_slider = T,
    .smooth = T)

#Seasonality plots
raw %>%
  plot_seasonal_diagnostics(
    Date, temperature, .interactive = T)

#Now we will create new features from the Date column
all_time_arg <- raw %>% tk_augment_timeseries_signature()

df <- all_time_arg %>%
  select(-contains("hour"),
         -contains("day"),
         -minute,-second,-am.pm) %>% 
  mutate_if(is.ordered, as.character) %>% 
  mutate_if(is.character,as_factor)

#1. Build h2o::automl(). For this task:
 # • prepare data using tk_augment_timeseries_signature()
#• set stopping metric to “RMSE”
#• set exclude_algos = c("DRF", "GBM","GLM",'XGBoost')

#Running AutoML
h2o.init()    
train_h2o <- df %>% filter(year < 1990) %>% as.h2o()
test_h2o <- df %>% filter(year >= 1990) %>% as.h2o()
y <- "temperature" 
x <- df %>% select(-temperature) %>% names()


model_h2o <- h2o.automl(
  x = x, y = y, 
  training_frame = train_h2o, 
  validation_frame = test_h2o,
  leaderboard_frame = test_h2o,
  stopping_metric = "RMSE",
  seed = 123, nfolds = 10,
  exclude_algos = c("DRF", "GBM","GLM",'XGBoost'),
  max_runtime_secs = 200) 

#Showing the leaderboard. StackedEnsemble is the best model and has led to an RMSE of 2.59.
model_h2o@leaderboard %>% as.data.frame() %>% view()

#Choosing the best model from autoML
automl_leader <- model_h2o@leader

#Making predictions based on the best automl model
automl_pred <- automl_leader %>% h2o.predict(test_h2o) 

mean((test_h2o$temperature - automl_pred$predict)^2)^0.5

#There is no overfitting as the model shows smaller RMSE for test data
automl_leader %>% 
  h2o.rmse(train = T,
           valid = T,
           xval = T)

error_tbl <- df %>% 
  filter(lubridate::year(Date) >= 1990) %>% 
  add_column(pred = automl_pred %>% as_tibble() %>% pull(predict))  %>% 
  rename(actual = temperature) %>% 
  select(Date,actual,pred)

#The model shows good results for test data
highchart() %>% 
  hc_xAxis(categories = error_tbl$Date) %>% 
  hc_add_series(data=error_tbl$actual, type='line', color='red', name='Actual') %>% 
  hc_add_series(data=error_tbl$pred, type='line', color='green', name='Predicted') %>% 
  hc_title(text='Predict')

#Q2. Build modeltime::arima_reg(). For this task set engine to “auto_arima”
train <- raw %>% filter(Date < "1990-01-01")
test <- raw %>% filter(Date >= "1990-01-01")

#Creating model
model_fit_arima_no_boost <- arima_reg() %>%
  set_engine(engine = "auto_arima") %>%
  fit(temperature ~ Date, train)

# calibration
calibration <- modeltime_table(
  model_fit_arima_no_boost) %>%
  modeltime_calibrate(test)

#Plotting the predictions of auto_arima
calibration %>% 
  modeltime_forecast(actual_data = df) %>%
  plot_modeltime_forecast(.interactive = T,
                          .plotly_slider = T)

#Getting the RMSE score. We got 3.73
calibration %>% modeltime_accuracy() %>% 
  table_modeltime_accuracy(.interactive = F)

#Q3. Forecast temperatures for next year with model which has lower RMSE.
#The automl model has lower RMSE and that's why we will make forecast for the next year for this model.

# New data (next 2 years) ----
new_data <- seq(as.Date("1991/01/01"), as.Date("1991/12/31"), "days") %>%
  as_tibble() %>% 
  add_column(temperature=0) %>% 
  rename(Date=value) %>% 
  tk_augment_timeseries_signature() %>%
  select(-contains("hour"),
         -contains("day"),
         -contains("week"),
         -minute,-second,-am.pm) %>% 
  mutate_if(is.ordered, as.character) %>% 
  mutate_if(is.character,as_factor)

# Creating the forecast
new_h2o <- new_data %>% as.h2o()

new_predictions <- automl_leader %>% 
  h2o.predict(new_h2o) %>% 
  as_tibble() %>%
  add_column(Date=new_data$Date) %>% 
  select(Date,predict) %>% 
  rename(temperature=predict)

#Plotting the forecast
raw %>% 
  bind_rows(new_predictions) %>% 
  mutate(colors=c(rep('Actual',3650),rep('Predicted',365))) %>% 
  hchart("line", hcaes(Date, temperature, group = colors)) %>% 
  hc_title(text='Forecast') %>% 
  hc_colors(colors = c('red','green'))

