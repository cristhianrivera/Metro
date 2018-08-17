###Load all the packages that we will use
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


# What are you going to do?
#   1. EDA:
#     a) time series (maybe for al the variables)
#     b) distribution of the instances (box plots)
#     c) verify outliers (is exist)
#     d) functions for transformations needed to apply ARIMA/ARIMAX/ LSTM
#   2. ARIMA models
#     a) models for every product_id
#   3. ARIMAX models 
#     a) models for every product_id
#   4. Linear regression model
#   4. LSTM model
#     a) unique model with one-hot for product_id
#   ----------------just if there is time-------------
#   5. Ensemble models




setwd('/Users/Cristhian/Documents/Metro/case_study/')
path = '/Users/Cristhian/Documents/Metro/case_study/'

data_metro <- read.csv(file= paste(path,"data.csv", sep = ""),
                         header = TRUE, 
                         sep=",")
View(data_metro)
distinct(data_metro,calendar_date)
data_metro$calendar_date <- lubridate::mdy(data_metro$calendar_date)


plotgg <- function(ser1, ser2){
  n1 = names(ser1)
  n2 = names(ser1)
  plotfun <- as.data.frame(
    cbind(ts = seq(1, length(ser1), by =1 ),
          ser1,
          ser2))
  plotfun <- reshape2::melt(plotfun, id="ts")
  p <- ggplot2::ggplot(data = plotfun,
              aes(x = ts, 
                  y = value, 
                  colour = variable )) +
    geom_line()+
    xlab('Time')+
    ylab('Value')+
    theme_minimal()
  
  plotly::ggplotly(p)
}
################EDA################
library(dplyr)
library(reshape)
library(reshape2)
library(plotly)

data_metro <- as.tbl(data_metro)

#distinct(data_metro,product_id) #4 product_id 

data_metro_eda <- data_metro %>%
  group_by(as.factor(product_id)) %>%
  summarise(data_points = n())

data_metro_eda <- data_metro %>%
  group_by(calendar_date) %>%
  summarise(volume_sold = sum(volume_sold),
            revenue = sum(revenue),
            cost = sum(cost),
            stock_level = sum(stock_level),
            retail_price = sum(retail_price))

p <- ggplot(data= data_metro_eda)+
  geom_boxplot(aes(x = "cost", y=cost))+
  theme_minimal()
# ggplotly(p)

p1 <- ggplot(data= data_metro_eda)+
  geom_boxplot(aes(x = "revenue", y=revenue))+
  theme_minimal()
# ggplotly(p)

p2 <- ggplot(data= data_metro_eda)+
  geom_boxplot(aes(x = "volume_sold", y=volume_sold))+
  theme_minimal()
# ggplotly(p)

p3 <- ggplot(data= data_metro_eda)+
  geom_boxplot(aes(x = "stock_level", y=stock_level))+
  theme_minimal()
# ggplotly(p)

p4 <- ggplot(data= data_metro_eda)+
  geom_boxplot(aes(x = "retail_price", y=retail_price))+
  theme_minimal()
# ggplotly(p)

plot_grid(p,p1,p2,p3,p4)

####volume sold 
p1 <- ggplot(data = data_metro_eda)+
  geom_line(aes(x = lubridate::dmy(calendar_date), y=volume_sold), group=1)+
  theme_minimal()
ggplotly(p1)  

data_metro_eda <- data_metro %>%
  group_by(calendar_date, product_id) %>%
  summarise(volume_sold = sum(volume_sold),
            revenue = sum(revenue),
            cost = sum(cost),
            stock_level = sum(stock_level),
            retail_price = sum(retail_price)) %>%
  select(calendar_date, product_id, volume_sold)

data_metro_eda <- data.frame(data_metro_eda)
plotfun <- melt(data_metro_eda, id=c("calendar_date","product_id"), variable_name = "volume_sold")

p<- ggplot(plotfun)+
  geom_line(aes(x = lubridate::dmy(calendar_date), 
                y = value, 
                colour=as.factor(product_id) ))+
  ylab(" ")+
  xlab("date")
ggplotly(p)  

#########################################################################
###Simple model by product_id 
###volume old per product id seems to be very different
#########################################################################
TSAnalysis <- function(ts, lags=20){
  ts <- na.omit(ts)
  win.graph(width=4.875,height=3,pointsize=8)
  forecast::Acf(ts,lags, main = "ACF", xlab = "")
  win.graph(width=4.875,height=3,pointsize=8)
  forecast::Pacf(ts,lags, main = "PACF")
}

# product_id
# 1      13701
# 2      26104
# 3     158105
# 4     158737




################### 1 #################################
data_metro_eda <- data_metro %>%
  group_by(calendar_date, product_id) %>%
  summarise(volume_sold = sum(volume_sold),
            revenue = sum(revenue),
            cost = sum(cost),
            stock_level = sum(stock_level),
            retail_price = sum(retail_price))%>%
  filter(product_id==13701) %>%
  arrange(lubridate::dmy(calendar_date))

p <- ggplot(data_metro_eda)+
  geom_line(aes(x = lubridate::dmy(calendar_date), y = volume_sold), group=1)+
  theme_minimal()
ggplotly(p)


myts <- ts(data_metro_eda$volume_sold) 
TSAnalysis(myts)

ser.model <- ts(data_metro_eda$volume_sold) 
#manual Arima choice
modelBaseLine <- forecast::Arima(log(ser.model+1), 
                order = c(1,1,1),
                xreg =exo.vars)
#auto Arima
modelBaseLine <- forecast::auto.arima(log(ser.model+1),xreg =exo.vars)

TSAnalysis(modelBaseLine$residuals)
summary(modelBaseLine$residuals)
plot(modelBaseLine$residuals)


ser.for <- forecast::forecast(modelBaseLine, xreg = exo.vars,h =31)
ori = exp(as.double(ser.for$x))-1
fore = exp(as.double(ser.for$fitted))-1

plotgg(ori,fore)
forecast::accuracy(ser.for)
mape(fore,ori)
#########################################################################
#########################################################################


################### 2 #################################
##################Per product_id######################

data_metro_eda <- data_metro %>%
  #mutate(calendar_date = woy) %>%
  group_by(calendar_date, product_id) %>%
  summarise(volume_sold = sum(volume_sold),
            revenue = sum(revenue),
            cost = sum(cost),
            stock_level = sum(stock_level),
            retail_price = sum(retail_price),
            mpdm1 = sum(mpdm1),
            cdm1  = sum(cdm1) )%>%
  #filter(product_id==13701, calendar_date <52) %>%
    filter(product_id==13701) %>%
  #arrange(calendar_date)
  arrange(as.Date(calendar_date))

View(data_metro_eda)

data_metro_eda <- data_metro_eda %>%
  ungroup() %>%
  mutate(stock_levelD = dplyr::lag(stock_level,
                                    order_by = as.Date(calendar_date),
                                    # order_by = calendar_date,
                                    n=7,
                                    defaul = stock_level ),
         retail_priceD = dplyr::lag(retail_price,
                                    order_by = as.Date(calendar_date),
                                    # order_by = calendar_date,
                                    n=7,
                                    defaul = retail_price )
         )

#rpivotTable::rpivotTable(data_metro_eda)
plotgg(log(data_metro_eda$volume_sold+1), log(data_metro_eda$stock_level+1) )
TSAnalysis(log(data_metro_eda$retail_price+1))

ser.model      <- ifelse(data_metro_eda$volume_sold<0,0,data_metro_eda$volume_sold)
ser.retprice   <- data_metro_eda$retail_priceD
ser.stock      <- data_metro_eda$stock_levelD
ser.maxprice   <- data_metro_eda$mpdm1
ser.cost       <- data_metro_eda$cdm1


exo.vars <- data.frame(
  cbind(price    = log(ifelse(ser.retprice<0, 0, ser.retprice)+1),
        stock    = log(ifelse(ser.stock<0   , 0, ser.stock   )+1),
        maxprice = log(ser.maxprice+1),
        cost     = log(ifelse(ser.cost<0    , 0,ser.cost     )+1)
        )
  )
data_metro_eda$mpdm1
data_metro_eda$retail_price

p<-ggplot(data_metro_eda)+
  geom_line(aes(x = as.Date(calendar_date), y = mpdm1), group=1, colour=1)+
  geom_line(aes(x = as.Date(calendar_date), y = retail_price), group=1, colour=2)+
  geom_line(aes(x = as.Date(calendar_date), y = volume_sold), group=1, colour=3)+
  theme_light()
ggplotly(p)
  


forecast::Acf((log(ser.model+1)), 50)
forecast::Pacf(log(ser.model+1), 50)

model.exo <- forecast::Arima(
                log(ser.model+1),
                #order = c(6,0,0), #works good for weekly timeset
                order = c(2,1,0),
                xreg = exo.vars,
                include.drift = TRUE)


summary(model.exo)
forecast::Acf(model.exo$residuals, lag.max = 50)
forecast::Pacf(model.exo$residuals, lag.max = 50)

summary(model.exo$residuals)
plot(model.exo$residuals)

ser.for <- forecast::forecast(model.exo, xreg = exo.vars[1:30,])
plot(ser.for)
ori = exp(as.double(ser.for$x))-1
fore = exp(as.double(ser.for$fitted))-1
plotgg(ori,fore)

forecast::accuracy(model.exo)


################### 3 #################################
##################Aggregate model######################

data_metro_eda <- data_metro %>%
  mutate(calendar_date = woy) %>%
  group_by(calendar_date) %>%
  summarise(volume_sold = sum(volume_sold),
            revenue = sum(revenue),
            cost = sum(cost),
            stock_level = sum(stock_level),
            retail_price = sum(retail_price),
            mpdm1 = sum(mpdm1),
            cdm1  = sum(cdm1) )%>%
  filter(calendar_date <52) %>%
  arrange(calendar_date)


View(data_metro_eda)

data_metro_eda <- data_metro_eda %>%
  ungroup() %>%
  mutate(stock_levelD = dplyr::lag(stock_level,
                                   order_by = calendar_date,
                                   n=7,
                                   defaul = stock_level ),
         retail_priceD = dplyr::lag(retail_price,
                                    order_by = calendar_date,
                                    n=7,
                                    defaul = retail_price )
  )

rpivotTable::rpivotTable(data_metro_eda)
plotgg(log(data_metro_eda$volume_sold+1), log(data_metro_eda$stock_level+1) )
TSAnalysis(log(data_metro_eda$retail_price+1))

ser.model      <- ifelse(data_metro_eda$volume_sold<0,0,data_metro_eda$volume_sold)
ser.retprice   <- data_metro_eda$retail_priceD
ser.stock      <- data_metro_eda$stock_levelD
ser.maxprice   <- data_metro_eda$mpdm1
ser.cost       <- data_metro_eda$cdm1


exo.vars <- data.frame(
  cbind(price    = log(ifelse(ser.retprice<0, 0, ser.retprice)+1),
        stock    = log(ifelse(ser.stock<0   , 0, ser.stock   )+1),
        maxprice = log(ifelse(ser.maxprice<0, 0, ser.maxprice)+1),
        cost     = log(ifelse(ser.cost<0    , 0,ser.cost     )+1)
  )
)


forecast::Acf((log(ser.model+1)), 50)
forecast::Pacf(log(ser.model+1), 50)

model.exo <- forecast::Arima(
  log(ser.model+1),
  order = c(3,0,0),
  xreg = exo.vars,
  include.drift = TRUE)

summary(model.exo)
forecast::Acf(model.exo$residuals, lag.max = 50)
forecast::Pacf(model.exo$residuals, lag.max = 50)

summary(model.exo$residuals)
plot(model.exo$residuals)

ser.for <- forecast::forecast(model.exo, xreg = exo.vars[1:4,])
plot(ser.for)
ori = exp(as.double(ser.for$x))-1
fore = exp(as.double(ser.for$fitted))-1
plotgg(ori,fore)

model.exo <- stats::arima(
  log(ser.model+1),
  order = c(3,0,0),
  xreg = exo.vars)

model.exo
predict(model.exo, n.ahead = 1, newxreg = exo.vars[1:4,])
forecast::accuracy(model.exo)



#################Some charts##########################


data_metro <- data_metro %>%
  arrange(product_id, calendar_date) %>%
  mutate(vdm1_me = lag(volume_sold)) %>%
  select(product_id, calendar_date, volume_sold, vdm1, vdm1_me)

data_metro_plot <- data_metro %>%
  filter(product_id == 13701)%>%
  select(-product_id, - vdm1_me)

data_metro_plot <- data.frame(data_metro_plot)

plotfun <- melt(data_metro_plot, id="calendar_date")

p <- ggplot(data = plotfun,
            aes(x = as.Date(calendar_date,"%d/%m/%Y"), 
                y = value, 
                colour = variable)) +
  geom_line()+
  xlab('Time')+
  ylab('Value')+
  theme_minimal()

ggplotly(p)


#################################################################


data_metro_pid <- data_metro %>%
  filter(product_id == 13701)%>%
  arrange(calendar_date)

x_ts <- data_metro_pid$volume_sold
x_1 <- data_metro_pid$cdm1



tsa::acf(diff(x_ts), lag.max = 20)
acf(x_ts)
pacf(x_ts)


library(TSA)
mm <- arimax(x = log(x_ts+1), 
             xreg = NULL,
             order = c(1,1,0),
             seasonal = list(order = c(0,0,0), period = NA),
             include.mean = TRUE,
             transform.pars = TRUE,
             fixed = NULL,
             init = NULL,
             n.cond = 0,
             xtransf = data.frame(x_1),
             transfer = list(c(1,0)),
             method='CSS',
             list(maxit = 1000)
             )

attributes(mm)

# prints the predicted values and their 95% prediction limits.
# (x, 
# order = c(0, 0, 0), 
# seasonal = list(order = c(0, 0, 0), period = NA), 
# xreg = NULL, 
# include.mean = TRUE, 
# transform.pars = TRUE, 
# fixed = NULL, 
# init = NULL, 
# method = c("CSS-ML", "ML", "CSS"), 
# n.cond, 
# optim.control = list(), 
# kappa = 1e+06, 
# io = NULL, 
# xtransf, 
# transfer = NULL) 
hist(mm$residuals)
mm$arma


air.m1=arimax(log(airmiles),
              order=c(0,1,1),
              seasonal=list(order=c(0,1,1),
                            period=12),
              xtransf=data.frame(I911=1*(seq(airmiles)==69),
                                 I911=1*(seq(airmiles)==69)),
              transfer=list(c(0,0),c(1,0)),
              xreg=data.frame(Dec96=1*(seq(airmiles)==12),
                              Jan97=1*(seq(airmiles)==13),
                              Dec02=1*(seq(airmiles)==84)),
              method='ML')
plot.Arima(air.m1, n=10)






arimax


air.m1=arimax(log(airmiles),order=c(0,1,1),seasonal=list(order=c(0,1,1),
                                                         period=12),
              xtransf=data.frame(I911=1*(seq(airmiles)==69),  I911=1*(seq(airmiles)==69)),
              transfer=list(c(0,0),c(1,0)),xreg=data.frame(Dec96=1*(seq(airmiles)==12),
                                                           Jan97=1*(seq(airmiles)==13),Dec02=1*(seq(airmiles)==84)),method='ML')