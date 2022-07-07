library(EpiEstim)
library(ggplot2)
library(tidyverse)
#install.packages('tidyverse')
library('tidyverse')

#install.packages('lubridate')
library('lubridate')

library(EpiEstim)
library(ggplot2)
library(tidyverse)
require(data.table)
require(deSolve)
library(gridExtra)
library(gridGraphics)
require(ggplot2)
library(dplyr)
library(lubridate)
require("sfsmisc")





original_date <-'2021-06-01'
start_date <-'2021-07-01'
final_date <- '2021-08-19'
last_date<- '2021-12-30' #184 숫자를 맞추기 위해
date_ <- as.Date(last_date)-as.Date(start_date)
date_<- as.double(date_) +2  #'2022-01-16' - '2021-07-17' +2 ###hey### 184
###이유엔 데이터를 사용할때 ###
# setwd("/Users/hong-eun-yeong/Desktop/hey/AI_gil/GIL_prediction_2021-master/01_data/")
# df1<-read.csv("seir_data_0915_v2.csv")
# 
# colnames(df1)[1]<-"date"
# colnames(df1)[2]<-"state"
# colnames(df1)[3]<-"cases"
# colnames(df1)[4]<-"deaths"
# colnames(df1)[5]<-"new_tests"
# colnames(df1)[6]<-"people_vaccinated"
# colnames(df1)[7]<-"people_fully_vaccinated"
# df1$date =as.Date(df1$date, format="%Y-%m-%d")
# 
# 
# df1$date <- as.Date(df1$date)
# df_kr1<-subset(df1, date>=original_date & date<=final_date)


###owid.csv 파일을 사용할때 ###

df<-read.csv("/Users/hong-eun-yeong/Desktop/hey/AI_gil/GIL_prediction_2021-master/03_code/SEIR/version2/owid-covid-data_0819.csv")
df1<-df[c(4,3,6,9, 26, 36,37)]
colnames(df1)[2]<-"state"
colnames(df1)[3]<-"cases"
colnames(df1)[4]<-"deaths"
df1$date =as.Date(df1$date, format="%Y-%m-%d")

df_kr<-df1%>% filter(state=="South Korea")
df_kr1 <- df_kr[c(498:576),]

###owid.csv 파일을 사용할때 ###

cases<-df_kr1[c(1,3)]
colnames(cases)[2]<-"I"
colnames(cases)[1]<-"dates"

EL <- rep(0, times = 16)
ER <- rep(1, times = 16)
SL <- c(4,3,2,2,2,2,4,4,3,4,5,4,8,3,5,4)-1
SR <- c(8,4,4,5,9,4,6,5,4,5,6,5,9, 4,6,5)-1
type <- rep(0, times = 16)
a<-data.frame(EL, ER, SL, SR, type)

si<-c(0.05, 0.12, 0.18, 0.17, 0.16, 0.08, 0.05, 0.11, 0.04, 0.02, 0.01, 0.01)

Covid2021<-list("incidence" = cases, "si_distr" =si, "si_data" = a )
head(Covid2021$incidence)
Covid2021$si_distr
head(Covid2021$si_data)

library(incidence)
# plot(as.incidence(Covid2021$incidence$I, dates = Covid2021$incidence$dates))
# 
incidence<-as.data.frame(Covid2021$incidence)
incidence$dates =as.Date(incidence$dates, format="%Y-%m-%d")
incidence1<-subset(incidence, dates >= start_date & dates <= final_date)
incidence1$time <- 1:nrow(incidence1)

##Estimating R on sliding weekly windows, with a parametric serial interval
res_parametric_si <- estimate_R(Covid2021$incidence, 
                                method="parametric_si",
                                config = make_config(list(
                                  mean_si = 6, 
                                  std_si = 2))
)

head(res_parametric_si$R)
plot(res_parametric_si, legend = FALSE)

Rt<-as.data.frame(res_parametric_si$R)
colnames(Rt)[3]<-"average_Rt"
Rt1<-subset(Rt, Rt$t_start > 24)
Rt1$time <- 1:nrow(Rt1)
beta<-Rt1$average_Rt/14
Rt1$beta<-Rt1$average_Rt/14

# plot(Rt1$time, Rt1$average_Rt)
# plot(Rt1$time, Rt1$beta)
##Estimating R accounting for uncertainty on the serial interval distribution
# config <- make_config(list(mean_si = 7.5, std_mean_si = 3,
#                            min_mean_si = 4, max_mean_si = 10,
#                            std_si = 3, std_std_si = 1,
#                            min_std_si = 1, max_std_si = 5))
# res_uncertain_si <- estimate_R(Covid2021$incidence,
#                                method = "uncertain_si",
#                                config = config)
# 
# plot(res_uncertain_si, legend = FALSE) 

##Changing the time windows for estimation
# T <- nrow(Covid2021$incidence)
# t_start <- seq(2, T-6) # starting at 2 as conditional on the past observations
# t_end <- t_start + 6 # adding 6 to get 7-day windows as bounds included in window
# res_weekly <- estimate_R(Covid2021$incidence, 
#                          method="parametric_si",
#                          config = make_config(list(
#                            t_start = t_start,
#                            t_end = t_end,
#                            mean_si = 7.5, 
#                            std_si = 3))
# )
# plot(res_weekly, "R") 
# 
# t_start <- seq(2, T-13) # starting at 2 as conditional on the past observations
# t_end <- t_start + 13 
# res_biweekly <- estimate_R(Covid2021$incidence, 
#                            method="parametric_si",
#                            config = make_config(list(
#                              t_start = t_start,
#                              t_end = t_end,
#                              mean_si = 7.5, 
#                              std_si = 3))
# )
# plot(res_biweekly, "R") 
# 
# 
# t_start <- c(2, 18, 25) # starting at 2 as conditional on the past observations
# t_end <- c(17, 24, 50)
# res_before_during_after_closure <- estimate_R(Covid2021$incidence, 
#                                               method="parametric_si",
#                                               config = make_config(list(
#                                                 t_start = t_start,
#                                                 t_end = t_end,
#                                                 mean_si = 7.5, 
#                                                 std_si = 3))
# )
# plot(res_before_during_after_closure, "R") +
#   geom_hline(aes(yintercept = 1), color = "red", lty = 2)

