library(rpivotTable)
library(tibble)
library(chron)
library(lubridate)
library(zoo)
library(dplyr)
library(reshape)
library(reshape2)
library(plotly)
library(stringr)
library(forecast)
library(xts)
library(numbers)
library(shiny)
library(keras)

setwd('C:/Users/a688291/Downloads/Personal/')
path = 'C:/Users/a688291/Downloads/Personal/'

data_metro <- read.csv(file= paste(path,"data.csv", sep = ""),
                       header = TRUE, 
                       sep=",")


data_metro$calendar_date <- lubridate::mdy(data_metro$calendar_date)

data_metro <- as.tbl(data_metro)

data_metro_eda <- data_metro %>%
  filter(woy<48)%>%
  select(calendar_date, 
         product_id,
         volume_sold,
         revenue,
         cost,
         stock_level,
         retail_price,
         mpdm1,
         vwm1) %>%
  arrange(product_id,as.Date(calendar_date))

data_metro_eda <- data_metro_eda %>%
  group_by(calendar_date) %>%
  summarise(
    volume_sold = sum(volume_sold),
    stock_level = sum(stock_level),
    retail_price = sum(retail_price)) %>%
  mutate(weekday = format(as.Date(calendar_date),"%u"))


data_metro_eda <- data_metro_eda %>%
  mutate(
    volume_sold = log(volume_sold+1),
    stock_level = log(dplyr::lag(stock_level, default = mean(stock_level))+1),
    retail_price = log(dplyr::lag(retail_price, default = mean(retail_price))+1)
    ) %>%
  arrange(calendar_date)


(dates_retiro <- data_metro_eda%>%
    ungroup()%>%
    distinct(calendar_date)%>%
    mutate(pivot_wk = as.numeric(format(as.Date(calendar_date),"%u"))))

week_array <- array(data = numeric(7), dim = c(7,7))
for(i in 1:7){
  week_array[i,i]=1
}

LastStartDate <- dim(dates_retiro)[1] - (1*28) -1

n = 329 - 1*28 -1

data_array <- array(data = numeric(n), dim = c(n, 28, 10))
dim(data_array)
rowN <- 0

for(k_w in 1:(LastStartDate-1)){
  #k_w <- 265
  print(k_w)
  rowN <- rowN + 1
  dq <- dates_retiro$calendar_date[k_w]
  dt_from <- as.Date(dq)
  dt_to   <- as.Date(dq) + 27
  data_point <- data_metro_eda %>%
    ungroup()%>%
    filter(calendar_date >= as.Date(dt_from) & calendar_date <= as.Date(dt_to)) %>%
    select(volume_sold, stock_level, retail_price, weekday)
  
  data_array[rowN ,  , 1   ] <- dplyr::pull(data_point,volume_sold)#28
  data_array[rowN ,  , 2:8 ] <- week_array[ as.numeric(dplyr::pull(data_point,weekday)), ]
  data_array[rowN ,  , 9   ] <- dplyr::pull(data_point,stock_level)
  data_array[rowN ,  , 10  ] <- dplyr::pull(data_point,retail_price)
}




set.seed(235)
nt <- 198#floor(dim(data_array)[1]*0.69)
train <-sample(dim(data_array)[1], nt )
#divisors(198) #6
#divisors(102) #6

x_train <- data_array[ train , 1:21,  ]
y_train <- data_array[ train , 22:28, 1]

x_test <- data_array[ -train , 1:21,  ]
y_test <- data_array[ -train, 22:28, 1]


dim(x_train)
dim(y_train)

dim(x_test)
dim(y_test)


batch_size <- 6
epochs <- 100
library(keras)
cat('Creating model:\n')
model <- keras_model_sequential()
model %>%
  layer_lstm(units = 100, input_shape = c( 21, 10), batch_size = batch_size,
             return_sequences = FALSE, stateful = FALSE) %>% 
  layer_dropout(rate = 0.5) %>%
  #layer_lstm(units = 75, return_sequences = FALSE, stateful = TRUE) %>% 
  layer_dense(units = 7)
summary(model)

rmsprop <- optimizer_rmsprop(lr=0.005)
adm <- optimizer_adam(lr=0.0005)

model %>% compile(loss = 'mean_absolute_error', 
                  optimizer = adm,
                  metrics = c('mse')
)
history <- model %>% fit(
  x_train, y_train, 
  batch_size = batch_size,
  epochs = epochs, 
  verbose = 1, 
  validation_data = list(x_test, y_test),
  shuffle = TRUE)

plot(history)






