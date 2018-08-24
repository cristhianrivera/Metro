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


