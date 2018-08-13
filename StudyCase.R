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
library(keras)

setwd('C:/Users/a688291/Downloads/case_study/')
path = 'C:/Users/a688291/Downloads/case_study/'


data_metro <- read.csv(file= paste(path,"data.csv", sep = ""),
                         header = TRUE, 
                         sep=",")
data_metro$Date <- as.Date(data_metro$calendar_date,"%m/%d/%Y")

data_metro <- as.tbl(data_metro)

data_metro_pid <- data_metro %>%
  filter(product_id == 13701)%>%
  arrange(calendar_date)

x_ts <- data_metro_pid$volume_sold
x_1 <- data_metro_pid$cdm1

library(TSA)
mm <- arimax(x_ts, 
             order = c(1,1,0),
             #seasonal = list(order = c(0,0,0), period = NA),
             xtransf = data.frame(x_1),
             transfer = list(c(1,0)),
             method='CSS'
             )

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


data(oil.price)
oil.IMA11alt=arima(log(oil.price),order=c(0,1,1),
                   # create the design matrix of the covariate for prediction
                   xreg=data.frame (constant=seq(oil.price)))
n=length(oil.price)
n.ahead=24
newxreg=data.frame(constant=(n+1):(n+n.ahead))
# do the prediction and plot the results
plot(oil.IMA11alt,n.ahead=n.ahead,newxreg=newxreg,
     ylab='Log(Oil Price)',xlab='Year',n1=c(2000,1))
# do the same thing but on the orginal scale
plot(oil.IMA11alt,n.ahead=n.ahead,newxreg=newxreg,
     ylab='Oil Price',xlab='Year',n1=c(2000,1),transform=exp,pch=19, lty=1,type='o')
# Setting pch=19 plots the predicted values as solid circles.
res=plot(oil.IMA11alt,n.ahead=n.ahead,newxreg=newxreg,
         ylab='Oil Price',xlab='Year',n1=c(2000,1),transform=exp,pch=19,col=NULL)
# Setting col=NULL will make the prediction bands invisible. Try col='red'. 
res
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


xtransf=data.frame(I911=1*(seq(airmiles)==69),
                   I911=1*(seq(airmiles)==69))









data_metro <- data_metro %>%
  arrange(product_id, calendar_date) %>%
  mutate(vdm1_me = lag(volume_sold)) %>%
  select(product_id, calendar_date, volume_sold, vdm1, vdm1_me)





rpivotTable(data_metro)

data_metro_plot <- data_metro %>%
  filter(product_id == 13701)%>%
  select(-product_id, - vdm1_me)

data_metro_plot <- data.frame(data_metro_plot)

plotfun <- melt(data_metro_plot, id="calendar_date")

p <- ggplot(data = plotfun,
            aes(x = as.Date(calendar_date,"%m/%d/%Y"), 
                y = value, 
                colour = variable)) +
  geom_line()+
  xlab('Time')+
  ylab('Value')+
  theme_minimal()

ggplotly(p)


arimax


air.m1=arimax(log(airmiles),order=c(0,1,1),seasonal=list(order=c(0,1,1),
                                                         period=12),
              xtransf=data.frame(I911=1*(seq(airmiles)==69),  I911=1*(seq(airmiles)==69)),
              transfer=list(c(0,0),c(1,0)),xreg=data.frame(Dec96=1*(seq(airmiles)==12),
                                                           Jan97=1*(seq(airmiles)==13),Dec02=1*(seq(airmiles)==84)),method='ML')