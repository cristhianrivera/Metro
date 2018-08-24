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
#     c) verify outliers (if exist)
#     d) functions for transformations needed to apply ARIMA/ARIMAX/ LSTM
#   2. ARIMA models
#     a) models for every product_id
#   3. ARIMAX models 
#     a) models for every product_id
#   4. LSTM model
#     a) unique model with one-hot for product_id
#   ----------------just if there is time-------------
#   5. Ensemble models




setwd('/Users/Cristhian/Documents/Metro/case_study/')
path = '/Users/Cristhian/Documents/Metro/case_study/'

data_metro <- read.csv(file= paste(path,"data.csv", sep = ""),
                         header = TRUE, 
                         sep=",")

distinct(data_metro,calendar_date)


data_metro$calendar_date <- lubridate::mdy(data_metro$calendar_date)
#data_metro$calendar_date <- as.Date(data_metro$calendar_date)


plotgg <- function(ser1, ser2){
  n1 = names(ser1)
  print(n1)
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
  group_by(calendar_date, product_id) %>%
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
install.packages('cowplot')
cowplot::plot_grid(p,p1,p2,p3,p4)

####volume sold 
p1 <- ggplot(data = data_metro_eda)+
  geom_line(aes(x = as.Date(calendar_date), y=volume_sold), group=1)+
  facet_wrap(product_id~.)+
  xlab("Time")+
  ylab("Number of units")+
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
  par(mfrow=c(2,1))
  #win.graph(width=4.875,height=3,pointsize=8)
  #forecast::Acf(ts,lags, ci.type='ma', xlab = "", main="")
  acf(ts,lags, ci.type='ma', xlab = "", main="")
  #win.graph(width=4.875,height=3,pointsize=8)
  forecast::Pacf(ts,lags, main = "")
  par(mfrow=c(1,1))
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
  filter(product_id==158105) %>%
  mutate(retail_priceLag = dplyr::lag(retail_price, default = mean(retail_price)),
         mpdm1D = lag(mpdm1),
         mpdm1FixedLag = dplyr::if_else(mpdm1 == 0 , mpdm1D, mpdm1 ),
         stock_levelLag = dplyr::lag(stock_level, default = mean(stock_level))
  ) %>%
  select(-product_id) %>%
  arrange(as.Date(calendar_date))


data_metro_eda <- data_metro_eda %>%
  mutate(calendar_date = woy) %>%
  group_by(calendar_date) %>%
  summarise(volume_sold = sum(volume_sold),
            retail_price = sum(retail_price) / n(),
            retail_priceLag = sum(retail_priceLag) / n(),
            mpdm1FixedLag = sum(mpdm1FixedLag) / n(),
            priceLag = max(mpdm1FixedLag,retail_priceLag),
            stock_level = sum(stock_level),
            stock_levelLag = sum(stock_levelLag)
            ) %>%
  arrange(calendar_date) %>%
  filter(calendar_date<48)

#data_metro_eda <- reshape2::melt(data_metro_eda, id = "calendar_date")
#rpivotTable::rpivotTable(data_metro_eda)

ser.model         <- data_metro_eda$volume_sold
ser.model         <- ifelse(ser.model<0,0,ser.model)
ser.retpriceLag   <- data_metro_eda$retail_priceLag
ser.stockLag      <- data_metro_eda$stock_levelLag
ser.maxpriceLag   <- data_metro_eda$mpdm1FixedLag
ser.price         <- data_metro_eda$priceLag
#ser.outliers      <- 1*(seq(data_metro_eda$volume_sold)==7) ###26104
ser.outliers      <- -1*(seq(data_metro_eda$volume_sold)==36) ###158737


exo.vars <- data.frame(
  cbind(#ser.retpriceLag = log(ser.retpriceLag+1),
        ser.stockLag = log(ifelse(ser.stockLag<0,0,ser.stockLag)+1)
        ,ser.price = log(ser.price+1)
        ,ser.outliers
        #,ser.maxpriceLag = log(ser.maxpriceLag+1)
        )
  )

plot(log(ser.model+1), type='l')
TSAnalysis(ser.model,50)
TSA::eacf(log(ser.model+1))

model.exo.auto <- forecast::auto.arima(log(ser.model+1),
           xreg = exo.vars)

model.exo <- forecast::Arima(
                log(ser.model+1),
                order = c(3,0,1), #works good for weekly timeset 
                #13701 (3,0,1)
                #26104 (2,1,0)
                #158105 
                #158737
                # 
                xreg = exo.vars,
                include.constant = TRUE,
                include.drift = FALSE)

summary(model.exo)
summary(model.exo.auto)

lmtest::coeftest(model.exo, level=0.95)
lmtest::coeftest(model.exo.auto, level=0.95)


TSAnalysis(model.exo$residuals, 50)
TSAnalysis(model.exo.auto$residuals, 50)

summary(model.exo$residuals)
plot(model.exo$residuals)

ser.for <- forecast::forecast(model.exo.auto, xreg = exo.vars[1:8,])
plot(ser.for)

ori = exp(as.double(ser.for$x))-1
fore = exp(as.double(ser.for$fitted))-1
plotgg(ori,fore)

ori = as.double(ser.for$x)
fore = as.double(ser.for$fitted)
plotgg(ori,fore)

forecast::accuracy(ori,fore)

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


################### 4 #################################
##################Per groups of product_id######################


#######Group 158737 and 13701
data_metro_eda <- data_metro %>%
  filter(product_id==158737 | product_id==13701) %>%
  mutate(retail_priceLag = dplyr::lag(retail_price, default = mean(retail_price)),
         mpdm1D = lag(mpdm1),
         mpdm1FixedLag = dplyr::if_else(mpdm1 == 0 , mpdm1D, mpdm1 ),
         stock_levelLag = dplyr::lag(stock_level, default = mean(stock_level))
  ) %>%
  select(-product_id) %>%
  arrange(as.Date(calendar_date))


data_metro_eda <- data_metro_eda %>%
  mutate(calendar_date = woy) %>%
  group_by(calendar_date) %>%
  summarise(volume_sold = sum(volume_sold),
            retail_price = sum(retail_price) / n(),
            retail_priceLag = sum(retail_priceLag) / n(),
            mpdm1FixedLag = sum(mpdm1FixedLag) / n(),
            priceLag = max(mpdm1FixedLag,retail_priceLag),
            stock_level = sum(stock_level),
            stock_levelLag = sum(stock_levelLag)
  ) %>%
  arrange(calendar_date) %>%
  filter(calendar_date<48)

write.csv(data_metro_eda, "joint.csv")

#data_metro_eda <- reshape2::melt(data_metro_eda, id = "calendar_date")
#rpivotTable::rpivotTable(data_metro_eda)

ser.model         <- data_metro_eda$volume_sold
ser.model         <- ifelse(ser.model<0,0,ser.model)
ser.retpriceLag   <- data_metro_eda$retail_priceLag
ser.stockLag      <- data_metro_eda$stock_levelLag
ser.maxpriceLag   <- data_metro_eda$mpdm1FixedLag
ser.price         <- data_metro_eda$priceLag
ser.outliers      <- 1*(seq(data_metro_eda$volume_sold)==17)



exo.vars <- data.frame(
  cbind(#ser.retpriceLag = log(ser.retpriceLag+1),
    ser.stockLag = log(ifelse(ser.stockLag<0,0,ser.stockLag)+1)
    ,ser.price = log(ser.price+1)
    #,ser.outliers
    #,ser.maxpriceLag = log(ser.maxpriceLag+1)
  )
)


TSAnalysis(log(ser.model+1),50)
TSA::eacf(log(ser.model+1))

model.exo.auto <- forecast::auto.arima(log(ser.model+1),
                                       xreg = exo.vars)

model.exo <- forecast::Arima(
  log(ser.model+1),
  order = c(6,0,1), #works good for weekly timeset 
  xreg = exo.vars,
  include.constant = TRUE,
  include.drift = FALSE)

summary(model.exo)
summary(model.exo.auto)

lmtest::coeftest(model.exo, level=0.95)
lmtest::coeftest(model.exo.auto, level=0.95)


TSAnalysis(model.exo$residuals, 50)
TSAnalysis(model.exo.auto$residuals, 50)

summary(model.exo$residuals)
plot(model.exo$residuals)

ser.for <- forecast::forecast(model.exo, xreg = exo.vars[1:8,])
plot(ser.for)

ori = exp(as.double(ser.for$x))-1
fore = exp(as.double(ser.for$fitted))-1
plotgg(ori,fore)

ori = as.double(ser.for$x)
fore = as.double(ser.for$fitted)
plotgg(ori,fore)

forecast::accuracy(ori,fore)




#######Group 26104	158105
data_metro_eda <- data_metro %>%
  filter(product_id==26104 | product_id==158105) %>%
  mutate(retail_priceLag = dplyr::lag(retail_price, default = mean(retail_price)),
         mpdm1D = lag(mpdm1),
         mpdm1FixedLag = dplyr::if_else(mpdm1 == 0 , mpdm1D, mpdm1 ),
         stock_levelLag = dplyr::lag(stock_level, default = mean(stock_level))
  ) %>%
  select(-product_id) %>%
  arrange(as.Date(calendar_date))


data_metro_eda <- data_metro_eda %>%
  mutate(calendar_date = woy) %>%
  group_by(calendar_date) %>%
  summarise(volume_sold = sum(volume_sold),
            retail_price = sum(retail_price) / n(),
            retail_priceLag = sum(retail_priceLag) / n(),
            mpdm1FixedLag = sum(mpdm1FixedLag) / n(),
            priceLag = max(mpdm1FixedLag,retail_priceLag),
            stock_level = sum(stock_level),
            stock_levelLag = sum(stock_levelLag)
  ) %>%
  arrange(calendar_date) %>%
  filter(calendar_date<48)

write.csv(data_metro_eda, "joint.csv")

#data_metro_eda <- reshape2::melt(data_metro_eda, id = "calendar_date")
#rpivotTable::rpivotTable(data_metro_eda)

ser.model         <- data_metro_eda$volume_sold
ser.model         <- ifelse(ser.model<0,0,ser.model)
ser.retpriceLag   <- data_metro_eda$retail_priceLag
ser.stockLag      <- data_metro_eda$stock_levelLag
ser.maxpriceLag   <- data_metro_eda$mpdm1FixedLag
ser.price         <- data_metro_eda$priceLag
ser.outliers      <- 1*(seq(data_metro_eda$volume_sold)==17)



exo.vars <- data.frame(
  cbind(#ser.retpriceLag = log(ser.retpriceLag+1),
    ser.stockLag = log(ifelse(ser.stockLag<0,0,ser.stockLag)+1)
    ,ser.price = log(ser.price+1)
    #,ser.outliers
    #,ser.maxpriceLag = log(ser.maxpriceLag+1)
  )
)


TSAnalysis(log(ser.model+1),50)
TSA::eacf(log(ser.model+1))

model.exo.auto <- forecast::auto.arima(log(ser.model+1),
                                       xreg = exo.vars)

model.exo <- forecast::Arima(
  log(ser.model+1),
  order = c(0,0,1), 
  xreg = exo.vars,
  include.constant = TRUE,
  include.drift = FALSE,
  method='ML')

summary(model.exo)
summary(model.exo.auto)

lmtest::coeftest(model.exo, level=0.95)
lmtest::coeftest(model.exo.auto, level=0.95)


TSAnalysis(model.exo$residuals, 50)
TSAnalysis(model.exo.auto$residuals, 50)

summary(model.exo$residuals)
plot(model.exo$residuals)

ser.for <- forecast::forecast(model.exo, xreg = exo.vars[1:8,])
plot(ser.for)

ori = exp(as.double(ser.for$x))-1
fore = exp(as.double(ser.for$fitted))-1
plotgg(ori,fore)

ori = as.double(ser.for$x)
fore = as.double(ser.for$fitted)
plotgg(ori,fore)

forecast::accuracy(ori,fore)
