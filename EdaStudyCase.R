corr = ccf(a,b)




data_metro_eda <- data_metro %>%
  filter(product_id==13701) %>%
  mutate(retail_priceLag = dplyr::lag(retail_price, default = mean(retail_price))) %>%
  mutate(mpdm1Fixed = dplyr::if_else(mpdm1 == 0 , retail_priceLag, mpdm1 ),
         mpdm1RollMean = rollmean(mpdm1,k=7,fill= c(mean(mpdm1),mean(mpdm1),mean(mpdm1))),
         mpdm1D = lag(mpdm1),
         mpdm1FixedLag = dplyr::if_else(mpdm1 == 0 , mpdm1D, mpdm1 )
         ) %>%
  select(-product_id) %>%
  arrange(as.Date(calendar_date))


ser.model      <- (data_metro_eda$volume_sold)
ser.stock      <- (data_metro_eda$stock_level)
ser.retprice   <- (data_metro_eda$retail_price)

forecast::Ccf(ser.model,ser.retprice)

data_metro_eda <- reshape2::melt(data_metro_eda, id = "calendar_date")
rpivotTable(data_metro_eda)

################Weekly

data_metro_eda <- data_metro_eda %>%
  #filter(product_id==13701) %>%
  mutate(calendar_date = woy) %>%
  group_by(calendar_date) %>%
  summarise_all(sum) %>%
  select(-woy) %>%
  arrange(calendar_date)



ser.model      <- (data_metro_eda$volume_sold)
ser.stock      <- (data_metro_eda$stock_level)
length(rollmean(ser.stock,4, fill= c(mean(ser.stock),mean(ser.stock),mean(ser.stock))))
length(ser.stock)



forecast::Ccf(ser.model,ser.stock)

data_metro_eda <- reshape2::melt(data_metro_eda, id = "calendar_date")
rpivotTable(data_metro_eda)

#######################################################
library("rstan")
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

schools_dat <- list(J = 8, 
                    y = c(28,  8, -3,  7, -1,  1, 18, 12),
                    sigma = c(15, 10, 16, 11,  9, 11, 10, 18))

fit <- stan(file = '8schools.stan', data = schools_dat, 
            iter = 1000, chains = 4)





##########################################################################################
##########################################################################################

data_metro_pid <-  data_metro_eda %>%
  ungroup() %>%
  filter(product_id==13701) %>%
  mutate(retail_priceLag = dplyr::lag(retail_price, default = mean(retail_price)),
         mpdm1D = lag(mpdm1),
         mpdm1FixedLag = dplyr::if_else(mpdm1 == 0 , mpdm1D, mpdm1 ),
         retail_priceLag = dplyr::lag(retail_price, default = mean(retail_price)),
         stock_levelLag = dplyr::lag(stock_level, default = mean(stock_level)),
         priceLag = dplyr::if_else(mpdm1FixedLag<retail_priceLag,retail_priceLag,mpdm1FixedLag))

rpivotTable(data_metro_pid)


data_metro_pid <- data_metro_pid %>%
  group_by(calendar_date) %>%
  summarise(volume_sold = sum(volume_sold),
            priceLag = sum(priceLag)/n(),
            stock_levelLag = sum(stock_levelLag)
  )


ser.model      <- data_metro_pid$volume_sold
ser.price      <- data_metro_pid$priceLag
ser.stock      <- data_metro_pid$stock_levelLag


exo.vars <- data.frame(
  cbind(price    = log(ifelse(ser.price<0, 0, ser.price)+1),
        stock    = log(ifelse(ser.stock<0, 0, ser.stock)+1)
  )
)


model.exo13701 <- forecast::auto.arima(
  log(ser.model+1),
  xreg = exo.vars)



model.exo13701 <- forecast::Arima(
  log(ser.model+1),
  order = c(2,1,0),
  xreg = exo.vars,
  include.drift = FALSE)
summary(model.exo13701)

ser.for <- forecast::forecast(model.exo13701, xreg = exo.vars[330:335,])
summary(model.exo13701)
plot(ser.for)

ori = exp(as.double(ser.for$x))-1
fore = exp(as.double(ser.for$fitted))-1
plotgg(ori,fore)
forecast::accuracy(ori,fore)

TSAnalysis(diff(ser.model),50)

dim(exo.vars)





####################################################



ser.model      <- data_metro_pid$volume_sold
ser.price      <- data_metro_pid$priceLag
ser.stock      <- data_metro_pid$stock_levelLag
ser.outliers   <- #1*(seq(data_metro_pid$volume_sold)==11)+
  1*(seq(data_metro_pid$volume_sold)==17)+
  1*(seq(data_metro_pid$volume_sold)==41)
ser.outliers1   <- 1*(seq(data_metro_pid$volume_sold)==11)


exo.vars <- data.frame(
  cbind(price    = log(ifelse(ser.price<0, 0, ser.price)+1),
        stock    = log(ifelse(ser.stock<0, 0, ser.stock)+1)
        ,outliers = ser.outliers
        ,outliers1= ser.outliers1
  ))

model.exo13701 <- forecast::Arima(
  log(ser.model+1),
  order = c(6,0,0),
  xreg = exo.vars,
  include.drift = FALSE)

ser.for <- forecast::forecast(model.exo13701, xreg = exo.vars[43:47,])


ori = exp(as.double(ser.for$x))-1
fore = exp(as.double(ser.for$fitted))-1


plotfun <- as.data.frame(
  cbind(ts = seq(1, length(ori), by =1 ),
        orig = ori,
        fore = fore))
plotfun <- reshape2::melt(plotfun, id="ts")
p <- ggplot2::ggplot(data = plotfun,
                     aes(x = ts, 
                         y = value, 
                         colour = variable )) +
  geom_line()+
  xlab('Time')+
  ylab('Volume sold')+
  theme_minimal()
plotly::ggplotly(p)

forecast::accuracy(ori,fore)






##Model 
-Weekly aggregation with lags for stock and price variables. Product id 158105.

```{r}
data_metro_pid <-  data_metro_eda %>%
  ungroup() %>%
  filter(product_id==158105) %>%
  mutate(retail_priceLag = dplyr::lag(retail_price, default = mean(retail_price)),
         mpdm1D = lag(mpdm1),
         mpdm1FixedLag = dplyr::if_else(mpdm1 == 0 , mpdm1D, mpdm1 ),
         retail_priceLag = dplyr::lag(retail_price, default = mean(retail_price)),
         stock_levelLag = dplyr::lag(stock_level, default = mean(stock_level)),
         priceLag = dplyr::if_else(mpdm1FixedLag<retail_priceLag,retail_priceLag,mpdm1FixedLag))

data_metro_pid <- data_metro_pid %>%
  group_by(woy) %>%
  summarise(volume_sold = sum(volume_sold),
            priceLag = sum(priceLag)/n(),
            stock_levelLag = sum(stock_levelLag)
  )%>%
  filter(woy<48)

p<- ggplot(data_metro_pid)+
  geom_line(aes(x=woy,y=volume_sold, colour="VolumeSold"), group=1)+
  geom_line(aes(x=woy,y=0.1*stock_levelLag, colour="StockLevelLag"), group=1)+
  geom_line(aes(x=woy,y=12*priceLag-1352, colour="priceLag"), group=1)+
  scale_colour_manual("", 
                      breaks = c("VolumeSold", "StockLevelLag", "priceLag"),
                      values = c("tomato1", "royalblue1", "orange2")) +
  xlab("week of year")+
  ylab("units")+
  theme_minimal()
ggplotly(p)

```