
# rm(list = ls(all.names = TRUE)) # will clear all objects
# gc() # free up memory and report the memory usage

library(tidyverse)
library(readxl)
library(openxlsx)
library(xts)
library(stringi)
library(lubridate)
library(dynlm)
library(vars)
library(plotly)
library(dplyr)
library(zoo)
library(ggplot2)
library(ggfortify)
library(ggpubr)


# start date of the data and end date for data and forecasting in the imported csv file (check the file first)
datestart.data <- "2020/1/22"   ### Start of past date
dateend.data <- "2021/10/06"      ### End of past date
datestart.fcst <- "2021/10/07"    ### Start of forecast date
dateend.fcst <- "2023/12/31"    ### End of forecast date
date.data <- seq(as.Date(datestart.data), as.Date(dateend.fcst),"days")
date.fcst <- seq(as.Date(datestart.fcst), as.Date(dateend.fcst),"days")
date.full <- seq(as.Date(datestart.data), as.Date(dateend.fcst),"days")

date.full.wd <- wday(date.full)

dd.Mon <- ifelse(date.full.wd==2, 1, 0)
dd.Tue <- ifelse(date.full.wd==3, 1, 0)
dd.Wed <- ifelse(date.full.wd==4, 1, 0)
dd.Thu <- ifelse(date.full.wd==5, 1, 0)
dd.Fri <- ifelse(date.full.wd==6, 1, 0)
dd.Sat <- ifelse(date.full.wd==7, 1, 0)
dd.Sun <- ifelse(date.full.wd==1, 1, 0)

setwd("/home/sysadm/Desktop/pogba_backup2/0_Gil_Prediction/02_code/VAR/R/211102")
# Import the csv data
df <- read.csv("../../../../01_data/VAR/var_211102.csv")   #### Update csv file
df <- cbind(df,dd.Mon,dd.Tue,dd.Wed,dd.Thu,dd.Fri,dd.Sat,dd.Sun)


df <- df %>% 
  mutate(Confirmed.MA7 = rollmean(Confirmed, k = 7, fill = 0, align = "right")) %>%
  mutate(Deaths.MA7 = rollmean(Deaths, k = 7, fill = 0, align = "right")) %>%
  mutate(Severity.MA7 = rollmean(Severity, k = 7, fill = 0, align = "right")) 


# select the endogenous variables in y
y.df <- select(df,Confirmed.MA7,Deaths.MA7,Severity.MA7)
y.xts <- as.xts(y.df, order.by = date.data)

# select the exogenous variables in x
x.df <- select(df,VM_alpha,VM_delta,d_Vaccine)
x.xts <- as.xts(x.df, order.by = date.data)

# select weekday dummies (excluding Sunday)
wd.df <- select(df,dd.Mon,dd.Tue,dd.Wed,dd.Thu,dd.Fri,dd.Sat)
wd.xts <- as.xts(wd.df, order.by = date.data)

# select social distance varaible
OX.df <- select(df,Ox)
OX.xts <- as.xts(OX.df, order.by = date.data)

# sample size and forecasting window
size.H <- length(date.fcst)         # forecasting window: in this case, the length of future values in x
size.T <- length(date.data)-size.H  # sample size for estimation

n <- ncol(y.df) # Number of endogenous variables (y) 
k <- ncol(x.df) # Number of exogenous variables (x) 

## VAR-X estimation ------------------------------------------------------------------------------

x.xts.est <- x.xts[7:size.T,] 
y.xts.est <- y.xts[7:size.T,]
wd.xts.est <- wd.xts[7:size.T,]
OX.xts.est <- OX.xts[7:size.T,]

VAR.exogen <- cbind(OX.xts.est)

# Lag selection (including x into the VAR)
VAR.xts.est <- cbind(y.xts.est,x.xts.est)
# VAR.xts.est <- na.omit(VAR.xts.est)
VAR.select <- VARselect(VAR.xts.est, lag.max = 10, type = "const", exogen =VAR.exogen) # Lag selection

opt.p <- VAR.select$selection[3] # the opt.p (= optimal p) is determined by the AIC in VAR.select
VAR.trial <- VAR(y=VAR.xts.est,p = opt.p, type = "const",exogen =VAR.exogen)

x.xts.exg <- lag.xts(x.xts,1:opt.p)
x.xts.exg.est <- x.xts.exg[7:size.T,]

VAR.exog <- cbind(x.xts.exg.est,OX.xts.est)

VAR.est <- VAR(y = y.xts.est, p = opt.p,type = "const",exogen = VAR.exog)


## Forecasting with VAR-X ------------------------------------------------------------------------

x.xts.fcst <- x.xts.exg[(size.T+1):(size.T+size.H),]
wd.xts.fcst <- wd.xts[(size.T+1):(size.T+size.H),]
OX.xts.fcst <- OX.xts[(size.T+1):(size.T+size.H),]
#x.xts.exg.trial <- x.xts[(size.T+1):(size.T+size.H),]

fcst.exog <- cbind(x.xts.fcst,OX.xts.fcst)
fcst.trial <- cbind(OX.xts.fcst)

y.fcst <- predict(VAR.est, n.ahead = size.H, ci = 0.95, dumvar = fcst.exog)

fcst.Confirmed <- as.xts(y.fcst$fcst$Confirmed.MA7,order.by = date.fcst)
fcst.Deaths <- as.xts(y.fcst$fcst$Deaths.MA7,order.by = date.fcst)
fcst.Severity <- as.xts(y.fcst$fcst$Severity.MA7,order.by = date.fcst)

plot.xts(fcst.Confirmed[,1:3],plot.type = "single")
plot.xts(fcst.Deaths[,1:3],plot.type = "single")
plot.xts(fcst.Severity[,1:3],plot.type = "single")


################ PLOT 1 ##################################

#### Re-reading data from raw data
Date <- strftime(df$Date, format = "%Y-%m-%d") 
Confirmed <- df$Confirmed.MA7
raw.df <-data.frame(Date, Confirmed)   ### Bring past dataset
raw.df$Date <- seq(ymd("2020-01-22"), ymd("2023-12-31"), by="day")

fcst.Confirmed <- as.data.frame(fcst.Confirmed)
fcst.Confirmed$Date <- seq(ymd("2021-10-07"), ymd("2023-12-31"), by="day") ### Bring forecasting dataset

forecast <- full_join(raw.df, fcst.Confirmed, by = 'Date')
names(forecast)
names(forecast) <- c("Date", "Confirmed", "PointForecast", "Lo95", "Hi95", "CI")
write.csv(forecast,file='../../../../03_result/VAR/211102/var_211102_i.csv')


################ PLOT 2 ##################################

#### Re-reading data from raw data
Date <- strftime(df$Date, format = "%Y-%m-%d") 
Severity <- df$Severity.MA7
raw.df <-data.frame(Date, Severity)   ### Bring past dataset
raw.df$Date <- seq(ymd("2020-01-22"), ymd("2023-12-31"), by="day")

fcst.Severity <- as.data.frame(fcst.Severity)
fcst.Severity$Date <- seq(ymd("2021-10-07"), ymd("2023-12-31"), by="day") ### Bring forecasting dataset

forecast <- full_join(raw.df, fcst.Severity, by = 'Date')
names(forecast)
names(forecast) <- c("Date", "Severity", "PointForecast", "Lo95", "Hi95", "CI")
write.csv(forecast,file='../../../../03_result/VAR/211102/var_211102_s.csv')




################ PLOT 3 ##################################

#### Re-reading data from raw data
Date <- strftime(df$Date, format = "%Y-%m-%d") 
Deaths <- df$Deaths.MA7
raw.df <-data.frame(Date, Deaths)   ### Bring past dataset
raw.df$Date <- seq(ymd("2020-01-22"), ymd("2023-12-31"), by="day")

fcst.Deaths <- as.data.frame(fcst.Deaths)
fcst.Deaths$Date <- seq(ymd("2021-10-07"), ymd("2023-12-31"), by="day") ### Bring forecasting dataset

forecast <- full_join(raw.df, fcst.Deaths, by = 'Date')
names(forecast)
names(forecast) <- c("Date", "Deaths", "PointForecast", "Lo95", "Hi95", "CI")
write.csv(forecast,file='../../../../03_result/VAR/211102/var_211102_d.csv')

