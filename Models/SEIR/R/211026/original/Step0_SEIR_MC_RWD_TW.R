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
library(scales)
library(gcookbook)
library(readxl)
library(readr)
library(summarytools)
library(incidence)
library(tidyr)

setwd("C://Users//Youngji Jo//Google Drive//2021_Korea SIR//Data")

lastdate<-548+360 ## by 2023

######################################Incidence data#################################################################
df<-read.csv("owid-covid-data_1005.csv")
df1<-df[c(4,3,6,9, 26, 36,37)]
colnames(df1)[2]<-"state"
colnames(df1)[3]<-"cases"
colnames(df1)[4]<-"deaths"
df1$date =as.Date(df1$date, format="%Y-%m-%d")

df_tw<-df1%>% filter(state=="Taiwan")
df_tw1<-df_tw[df_tw$date>="2021-06-01" & df_tw$date <"2021-10-04",]
df_tw1$people_vaccinated[is.na(df_tw1$people_vaccinated)]<-mean(df_tw1$people_vaccinated, na.rm = TRUE)
df_tw1$people_fully_vaccinated[is.na(df_tw1$people_fully_vaccinated)]<-mean(df_tw1$people_fully_vaccinated, na.rm = TRUE)
#df_tw1[is.na(df_tw1)] <- 0

cases<-df_tw1[c(1,3)]
colnames(cases)[2]<-"I"
colnames(cases)[1]<-"dates"

EL <- rep(0, times = 16)
ER <- rep(1, times = 16)
SL <- c(4,3,2,2,2,2,4,4,3,4,5,4,8,3,5,4)-1
SR <- c(8,4,4,5,9,4,6,5,4,5,6,5,9, 4,6,5)-1
type <- rep(0, times = 16)
df<-data.frame(EL, ER, SL, SR, type)

si<-c(0.05, 0.12, 0.18, 0.17, 0.16, 0.08, 0.05, 0.11, 0.04, 0.02, 0.01, 0.01)

Covid2021<-list("incidence" = cases, "si_distr" =si, "si_data" = df )
# head(Covid2021$incidence)
# Covid2021$si_distr
# head(Covid2021$si_data)


# plot(as.incidence(Covid2021$incidence$I, dates = Covid2021$incidence$dates))
# 
 incidence<-as.data.frame(Covid2021$incidence)
 incidence$dates =as.Date(incidence$dates, format="%Y-%m-%d")
 incidence1<-subset(incidence, dates >= "2021-07-01")
 incidence1$time <- 1:nrow(incidence1)

##Estimating R on sliding weekly windows, with a parametric serial interval
res_parametric_si <- estimate_R(Covid2021$incidence, 
                                method="parametric_si",
                                config = make_config(list(
                                  mean_si = 6, 
                                  std_si = 2))
)

#head(res_parametric_si$R)
plot(res_parametric_si, legend = FALSE)


Rt<-as.data.frame(res_parametric_si$R)
colnames(Rt)[3]<-"average_Rt"
Rt1<-subset(Rt, Rt$t_start > 24)
Rt1$time <- 1:nrow(Rt1)
aver_Rt<-mean(Rt1$average_Rt)
beta<-Rt1$average_Rt/14
#plot(aver_Rt)

