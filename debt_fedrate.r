library(tidyverse)
library(ggplot2)
library(readxl)
library(ggpubr)
library(gridExtra)
df=read_excel("fed_rate_debt.xlsx",sheet="nominal")



ggscatter(data=df,x="frate",y="tp_all")


# min max normalization in order to better visualize the kind of relationship between different variables

normalize <- function(x, na.rm = TRUE) {
  return((x- min(x)) /(max(x)-min(x)))
}

#tibble with normalized variables

dfnorm=df%>% select_if(is.numeric) %>% transmute_all(normalize)


## Scatter plots between the federal rate and the various secotr's debt 

ptp=ggplot(data=dfnorm) +
  geom_point(aes(x=frate,y=tp_all))+
  ggtitle("total private")

ph=ggplot(data=dfnorm) +
  geom_point(aes(x=frate,y=h_all))+
  ggtitle("total household")

pnf=ggplot(data=dfnorm) +
  geom_point(aes(x=frate,y=nf_all))+
  ggtitle("total non-financial")

pg=ggplot(data=dfnorm) +
  geom_point(aes(x=frate,y=general_gov))+
  ggtitle("general goverment")


grid.arrange(ptp,ph,pnf,pg)


# from the above plots, the relationship between the federal rate and the level of debt of the various household
#sectors, seems to be negative and non-linear (convex)

# The level of debt is not a good indicator of sustainability, for this reason scatterplots between the various 
#sectors shares of debt to gdp and the federal rate are produced.


dfper=read_excel("fed_rate_debt.xlsx",sheet="percent")



ptp=ggplot(data=dfper) +
  geom_point(aes(x=frate,y=tp_all))+
  ggtitle("total private")

ph=ggplot(data=dfper) +
  geom_point(aes(x=frate,y=h_all))+
  ggtitle("total household")

pnf=ggplot(data=dfper) +
  geom_point(aes(x=frate,y=nf_all))+
  ggtitle("total non-financial")

pg=ggplot(data=dfper) +
  geom_point(aes(x=frate,y=general_gov))+
  ggtitle("general goverment")


grid.arrange(ptp,ph,pnf,pg)



# The above plots show a highly non linear relationship, still negative and non-linear. However there is an
#indication of a possible positive relationship (except for the general goverment's debt). Thus it is very likely
#that there is a confounding variable that distorts the association between the federal rate and the percentage of
#debt to gdp.


## We will produce a time series plot the various sector debt percentage to gdp ( normalized) and the federal rate
#in order to visualize their intertemporal relationship/



dfnorm=dfper%>% select_if(is.numeric) %>% transmute_all(normalize)
t=c(1954:2020)

ptp <- dfnorm %>% 
  ggplot()+
  geom_line(aes(x=t,y=tp_all),color="red",size=1.3)+
  geom_line(aes(x=t,y=frate),color="black",size=1.3)+
  ggtitle("Total Private")


ph <- dfnorm %>% 
  ggplot()+
  geom_line(aes(x=t,y=h_all),color="red",size=1.3)+
  geom_line(aes(x=t,y=frate),color="black",size=1.3)+
  ggtitle("Total Household")



pnf <-  dfnorm %>% 
  ggplot()+
  geom_line(aes(x=t,y=nf_all),color="red",size=1.3)+
  geom_line(aes(x=t,y=frate),color="black",size=1.3)+
  ggtitle("Total Non-Financial")


pgov <- dfnorm %>% 
  ggplot()+
  geom_line(aes(x=t,y=central_gov),color="red",size=1.3)+
  geom_line(aes(x=t,y=frate),color="black",size=1.3)+
  ggtitle("General Goverment")

grid.arrange(ptp,ph,pnf,pgov)

dfnorm %>% 
  ggplot()+
  geom_line(aes(x=t,y=nf_all),color="red",size=1.3)+
  geom_line(aes(x=t,y=frate),color="black",size=1.3)+
  geom_line(aes(x=t,y=h_all),color="blue",size=1.3)


#The federal rate reached its maximum value at 1981. Before that year, there was a slight increace in both household
#and non-financial corporations debt to gdp ratio, which accelerated in the period 1981-2020, a period where the 
#federal rate shows a decreasing trend

which(dfnorm$frate==1)

cbind(t,dfnorm)
trend=1:67

## Simple Regressions



model_total=lm(data=df,log(tp_all)~log(frate))

model_total %>% summary

model_house=lm(data=df,log(h_all)~log(frate))

model_house %>% summary


model_nf=lm(data=df,log(nf_all)~log(frate))

model_nf %>% summary

model_gov=lm(data=df,log(general_gov)~log(frate))

model_gov %>% summary


model_total$coefficients[2]
model_house$coefficients[2]
model_nf$coefficients[2]
model_gov$coefficients[2]



##correlation and cross correlation


df %>% select_if(is.numeric) %>% cor

### - means x causes y, lead/+ means y causes x

ccf(x=df$frate, df$tp_all, lag.max = 5,plot=F)

#in differences
ccf(y=diff(df$tp_all), x=diff(df$frate), lag.max = 5,plot=F)
####

ccf(x=df$frate, df$tp_all, lag.max = 5,plot=F)

pacf(df$frate)
pacf(df$tp_all)
acf(df$tp_all)




# FORECASTING -------------------------------------------------------------





df$Class="Train"



df$Class %>% length
df$Class[63:67]="Test"


df %>% tail(10)
dat=df
dat %>% tail
dat_train = subset(dat, Class == 'Train')
dat_test = subset(dat, Class == 'Test')

nrow(dat_train); nrow(dat_test)


### CONVERT TO TS for forecast package
dat_train %>% tail

dat_ts <- ts(dat_train[, 3], start = c(1954, 1), end = c(2015, 1), frequency = 1)

##Mean absolute error percentage 

#lines 2 to 4
mape <- function(actual,pred){
  mape <- mean(abs((actual - pred)/actual))*100
  return (mape)
}


#NAIVE FORECASTING

naive_mod <- naive(dat_ts, h = 5)
summary(naive_mod)


#add naive forecast to test

dat_test$naive=722.773  

mape(dat_test$tp_all, dat_test$naive) #98&


####Simple Exponential Smoothing
se_model <- ses(dat_ts, h = 5)
summary(se_model)

df_fc = as.data.frame(se_model)  ##save output in dataframe

dat_test$simplexp = df_fc$`Point Forecast`

mape(dat_test$tp_all, dat_test$simplexp)  #13.6%



### holts trend method

holt_model <- holt(dat_ts, h = 5)
summary(holt_model)

df_holt = as.data.frame(holt_model)   #save results on df_holt
dat_test$holt = df_holt$`Point Forecast`    #add holt forecast to test
mape(dat_test$tp_all, dat_test$holt)  #2.3%



###arima

arima_model <- auto.arima(dat_ts)
summary(arima_model)


fore_arima = forecast::forecast(arima_model, h=5)
df_arima = as.data.frame(fore_arima)
dat_test$arima = df_arima$`Point Forecast`
mape(dat_test$tp_all, dat_test$arima)  ## 5%


#Tbats

model_tbats <- tbats(dat_ts)
summary(model_tbats)


for_tbats <- forecast::forecast(model_tbats, h = 5)
df_tbats = as.data.frame(for_tbats)
dat_test$tbats = df_tbats$`Point Forecast`
mape(dat_test$tp_all, dat_test$tbats) #3.8%


#### percentage deviation of preds
pred_results=dat_test %>% select(-c(frate,tp_ld,h_ld,nf_all,h_all,nf_ld,general_gov,Central_gov,gdp,Class))

pred_results[,-1] %>% transmute_all(.funs=function(x) 1- x/pred_results$tp_all)





df1960=df %>% filter(year>1960)
df1960



rep(1:3,each=20)

df1960=df1960 %>% mutate(decade=rep(1:3,each=20))
df1960

df1960 %>% group_by(decade) %>% summarize(sd=sd(frate)/mean(frate))

df1960 %>% split(.$decade) %>%
  map_dfr(~sort(.x$frate,decreasing=TRUE))


boxplot(frate~decade,data=df1960)


