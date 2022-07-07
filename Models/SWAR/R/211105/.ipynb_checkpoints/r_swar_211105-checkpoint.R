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
dateend.data <- "2021/12/08"      ### End of past date
datestart.fcst <- "2021/12/09"    ### Start of forecast date
dateend.fcst <- "2023/12/31"    ### End of forecast date
predate.start <-  as.Date(datestart.fcst)
predate.end <- as.Date(dateend.fcst)
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

setwd("/home/sysadm/Desktop/pogba_backup2/0_Gil_Prediction/02_code/SWAR/R/211105")
path <- read.csv("../../../../00_path/SWAR/swar_path.csv")
input_path <- path$input
output_path_1 <- path$output_1
output_path_2 <- path$output_2
output_path_3 <- path$output_3

# Import the csv data
df <- read.csv(input_path)   #### Update csv file
df <- cbind(df,dd.Mon,dd.Tue,dd.Wed,dd.Thu,dd.Fri,dd.Sat,dd.Sun)
df <- df %>% 
  mutate(Confirmed.MA7 = rollmean(Confirmed, k = 7, fill = NA, align = "right")) %>%
  mutate(Deaths.MA7 = rollmean(Deaths, k = 7, fill = NA, align = "right")) %>%
  mutate(Severity.MA7 = rollmean(Severity, k = 7, fill = NA, align = "right")) 

# select the endogenous variables in y
y.df <- select(df,Confirmed.MA7)
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


################# Confirmed #####################################################################
## AR-X estimation ------------------------------------------------------------------------------

x.xts.est <- x.xts[7:size.T,] 
y.xts.est <- y.xts[7:size.T,]
wd.xts.est <- wd.xts[7:size.T,]
OX.xts.est <- OX.xts[7:size.T,]

VAR.exogen <- cbind(OX.xts.est)

# Lag selection (including x into the VAR)
VAR.xts.est <- cbind(y.xts.est,x.xts.est)
VAR.select <- VARselect(VAR.xts.est, lag.max = 10,type = "const",exogen =VAR.exogen) # Lag selection
opt.p1 <- VAR.select$selection[1] # the opt.p (= optimal p) is determind by the AIC in VAR.select

x.xts.exg <- x.xts
x.xts.exg.est <- x.xts.exg[7:size.T,]

AR.exog <- cbind(x.xts.exg.est,OX.xts.est)

AR.est <- arima(y.xts.est, order = c(opt.p1,0,0),xreg = AR.exog, include.mean = TRUE, method = "ML")

## Forecasting with AR-X ------------------------------------------------------------------------

x.xts.fcst <- x.xts.exg[(size.T+1):(size.T+size.H),]
wd.xts.fcst <- wd.xts[(size.T+1):(size.T+size.H),]
OX.xts.fcst <- OX.xts[(size.T+1):(size.T+size.H),]

fcst.exog <- cbind(x.xts.fcst,OX.xts.fcst)

y.fcst <- predict(AR.est, n.ahead = size.H, ci = 0.95, newxreg = fcst.exog)

fcst.y.pt <- y.fcst$pred
fcst.y.se <- y.fcst$se
fcst.y.95upp <- fcst.y.pt+1.96*fcst.y.se
fcst.y.95low <- fcst.y.pt-1.96*fcst.y.se

fcst.y.set <- as.data.frame(cbind(fcst.y.pt,fcst.y.95low,fcst.y.95upp))
fcst.Confirmed.set.xts <- as.xts(fcst.y.set,order.by=date.fcst)
forecast <- as.data.frame(fcst.Confirmed.set.xts)

names(forecast) <- c("PointForecast", "Lo95", "Hi95")
write.csv(forecast,file=output_path_1)


################# Deaths #####################################################################
##---------------------------------------------------------------------------------------------------

# select the endogenous variables in y
y.df <- select(df,Deaths.MA7)
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

## AR-X estimation ------------------------------------------------------------------------------

x.xts.est <- x.xts[7:size.T,] 
y.xts.est <- y.xts[7:size.T,]
wd.xts.est <- wd.xts[7:size.T,]
OX.xts.est <- OX.xts[7:size.T,]

VAR.exogen <- cbind(OX.xts.est)

# Lag selection (including x into the VAR)
VAR.xts.est <- cbind(y.xts.est,x.xts.est)
VAR.select <- VARselect(VAR.xts.est, lag.max = 10,type = "const",exogen =VAR.exogen) # Lag selection
opt.p2 <- VAR.select$selection[1] # the opt.p (= optimal p) is determind by the AIC in VAR.select

x.xts.exg <- x.xts
x.xts.exg.est <- x.xts.exg[7:size.T,]

AR.exog <- cbind(x.xts.exg.est,OX.xts.est)

AR.est <- arima(y.xts.est, order = c(opt.p2,0,0),xreg = AR.exog, include.mean = TRUE, method = "ML")

## Forecasting with AR-X ------------------------------------------------------------------------

x.xts.fcst <- x.xts.exg[(size.T+1):(size.T+size.H),]
wd.xts.fcst <- wd.xts[(size.T+1):(size.T+size.H),]
OX.xts.fcst <- OX.xts[(size.T+1):(size.T+size.H),]

fcst.exog <- cbind(x.xts.fcst,OX.xts.fcst)

y.fcst <- predict(AR.est, n.ahead = size.H, ci = 0.95, newxreg = fcst.exog)

fcst.y.pt <- y.fcst$pred
fcst.y.se <- y.fcst$se
fcst.y.95upp <- fcst.y.pt+1.96*fcst.y.se
fcst.y.95low <- fcst.y.pt-1.96*fcst.y.se

fcst.y.set <- as.data.frame(cbind(fcst.y.pt,fcst.y.95low,fcst.y.95upp))
fcst.Deaths.set.xts <- as.xts(fcst.y.set,order.by=date.fcst)
forecast <- as.data.frame(fcst.Deaths.set.xts)
# forecast$Date <- seq(predate.start, predate.end, by="day") ### Bring forecasting dataset

names(forecast) <- c("PointForecast", "Lo95", "Hi95")
write.csv(forecast,file=output_path_3)


################# Severity #####################################################################
##-------------------------------------------------------------------------------------------------
# select the endogenous variables in y
y.df <- select(df,Severity.MA7)
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

## AR-X estimation ------------------------------------------------------------------------------

x.xts.est <- x.xts[7:size.T,] 
y.xts.est <- y.xts[7:size.T,]
wd.xts.est <- wd.xts[7:size.T,]
OX.xts.est <- OX.xts[7:size.T,]

VAR.exogen <- cbind(OX.xts.est)

# Lag selection (including x into the VAR)
VAR.xts.est <- cbind(y.xts.est,x.xts.est)
VAR.select <- VARselect(VAR.xts.est, lag.max = 10,type = "const",exogen =VAR.exogen) # Lag selection
opt.p3 <- VAR.select$selection[1] # the opt.p (= optimal p) is determind by the AIC in VAR.select

x.xts.exg <- x.xts
x.xts.exg.est <- x.xts.exg[7:size.T,]

AR.exog <- cbind(x.xts.exg.est,OX.xts.est)

AR.est <- arima(y.xts.est, order = c(opt.p3,0,2),xreg = AR.exog, include.mean = TRUE, method = "ML")

## Forecasting with AR-X ------------------------------------------------------------------------

x.xts.fcst <- x.xts.exg[(size.T+1):(size.T+size.H),]
wd.xts.fcst <- wd.xts[(size.T+1):(size.T+size.H),]
OX.xts.fcst <- OX.xts[(size.T+1):(size.T+size.H),]

fcst.exog <- cbind(x.xts.fcst,OX.xts.fcst)

y.fcst <- predict(AR.est, n.ahead = size.H, ci = 0.95, newxreg = fcst.exog)

fcst.y.pt <- y.fcst$pred
fcst.y.se <- y.fcst$se
fcst.y.95upp <- fcst.y.pt+1.96*fcst.y.se
fcst.y.95low <- fcst.y.pt-1.96*fcst.y.se

fcst.y.set <- as.data.frame(cbind(fcst.y.pt,fcst.y.95low,fcst.y.95upp))
fcst.Severity.set.xts <- as.xts(fcst.y.set,order.by=date.fcst)
forecast <- as.data.frame(fcst.Severity.set.xts)
# forecast$Date <- seq(predate.start, predate.end, by="day") ### Bring forecasting dataset

names(forecast) <- c("PointForecast", "Lo95", "Hi95")
write.csv(forecast,file=output_path_2)


