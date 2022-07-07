library(EpiEstim)
library(ggplot2)
library(tidyverse)
#install.packages('tidyverse')
library('tidyverse')

#install.packages('lubridate')
library('lubridate')

original_date <-'2021-06-01'
start_date <-'2021-07-01'
final_date <- '2021-08-19'
last_date<- '2021-12-30' #184 숫자를 맞추기 위해

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
setwd('/Users/hong-eun-yeong/Desktop/hey/K6_AI_gil/SEIR/02_code/SEIR/R/210920/')
df<-read.csv("../../../../01_data/SEIR/owid-covid-data_0819.csv")
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
incidence1<-subset(incidence, dates >= start_date & dates < "2021-08-20")
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

########################step1#############################

##Korea SEIR_MC_age specific model ver.3 as of July 18 2021 
require(data.table)
require(ggplot2)
require(deSolve)
library(tidyverse)

date_ <- as.Date(last_date)-as.Date(start_date)
date_<- as.double(date_) +2  #'2022-01-16' - '2021-07-17' +2 ###hey### 184


#setwd("C://Users//Youngji Jo//Google Drive//2021_Korea SIR//Data")

sir <- function(t,X,parms){
  
  for (i in 1:length(X)) {
    if (X[i]< 0) {
      X[i] <- 0
    }}  
  
  ncompartment=9
  nage=length(X)/ncompartment
  
  U =(X[1:nage])
  V =(X[(nage+1):(2*nage)])
  E =(X[(2*nage+1):(3*nage)])
  A =(X[(3*nage+1):(4*nage)])
  TP=(X[(4*nage+1):(5*nage)])
  FP=(X[(5*nage+1):(6*nage)])
  S =(X[(6*nage+1):(7*nage)])
  R =(X[(7*nage+1):(8*nage)])
  D =(X[(8*nage+1):(9*nage)])
  #I =(X[(10*nage+1):(11*nage)])
  
  b <- list()
  
  with(as.list(c(parms, X)), {
    screeningreturn <- parms$mu*FP
    vaccination <- matrix(vac1(t), nrow=4, ncol=lastdate, byrow=T)*U
    infection <- matrix(contactpattern(t), nrow=4, ncol=lastdate, byrow=T)* as.vector(((U*(C))%*%(A/(U+A+V+E))))
    incubation <- parms$theta*(E)
    testingrate <- matrix(tau2_1(t), nrow=4, ncol=lastdate, byrow=T)*Se*A
    screeningrate <- parms$tau1*(1-Sp)*(U)
    progression1 <-parms$sigma*(A)
    progression2 <-parms$sigma*(TP)
    recovery1 <-parms$rho*(1-parms$delta)*(S)
    recovery2 <-parms$rho*(TP)
    recovery3 <-parms$rho*(A)
    #recovery4 <-rho*as.matrix(V)
    dead <-parms$delta *(S)
    
    dUdt <- -infection - vaccination 
    dVdt <- vaccination 
    dEdt <- infection - incubation
    dAdt <- incubation - testingrate - progression1 - recovery3
    dTPdt<- testingrate #- recovery2 - progression2
    dFPdt<- screeningrate - screeningreturn
    dSdt <- progression2 + progression1 - recovery1 - dead
    dRdt <- recovery1+recovery2+recovery3
    dDdt <- dead
    
    #dIdt<- testingrate 
    #dTP2dt<- - recovery2
    #dTP3dt<- - progression2
    
    SEIR <- rbind(dUdt, dVdt, dEdt, dAdt, dTPdt,dFPdt, dSdt, dRdt, dDdt )
    
    for (i in 1:date_){
      b[[i]] <- SEIR[,i]
      
    }
    b
    
  })
}


sir1 <- function(t,X,parms){
  
  for (i in 1:length(X)) {
    if (X[i]< 0) {
      X[i] <- 0
    }}  
  
  ncompartment=9
  nage=length(X)/ncompartment
  
  U =(X[1:nage])
  V =(X[(nage+1):(2*nage)])
  E =(X[(2*nage+1):(3*nage)])
  A =(X[(3*nage+1):(4*nage)])
  TP=(X[(4*nage+1):(5*nage)])
  FP=(X[(5*nage+1):(6*nage)])
  S =(X[(6*nage+1):(7*nage)])
  R =(X[(7*nage+1):(8*nage)])
  D =(X[(8*nage+1):(9*nage)])
  #I =(X[(10*nage+1):(11*nage)])
  
  b <- list()
  
  with(as.list(c(parms, X)), {
    screeningreturn <- parms$mu*FP
    vaccination <- matrix(vac1(t), nrow=4, ncol=lastdate, byrow=T)*U
    infection <- matrix(contactpattern(t), nrow=4, ncol=lastdate, byrow=T)* as.vector(((U*(C))%*%(A/(U+A+V+E))))
    incubation <- parms$theta*(E)
    testingrate <- matrix(tau2_1(t), nrow=4, ncol=lastdate, byrow=T)*Se*A
    screeningrate <- parms$tau1*(1-Sp)*(U)
    progression1 <-parms$sigma*(A)
    progression2 <-parms$sigma*(TP)
    recovery1 <-parms$rho*(1-parms$delta)*(S)
    recovery2 <-parms$rho*(TP)
    recovery3 <-parms$rho*(A)
    #recovery4 <-rho*as.matrix(V)
    dead <-parms$delta *(S)
    
    dUdt <- -infection - vaccination 
    dVdt <- vaccination 
    dEdt <- infection - incubation
    dAdt <- incubation - testingrate - progression1 - recovery3
    dTPdt<- testingrate - recovery2 - progression2
    dFPdt<- screeningrate - screeningreturn
    dSdt <- progression2 + progression1 - recovery1 - dead
    dRdt <- recovery1+recovery2+recovery3
    dDdt <- dead
    
    #dIdt<- testingrate 
    #dTP2dt<- - recovery2
    #dTP3dt<- - progression2
    
    SEIR <- rbind(dUdt, dVdt, dEdt, dAdt, dTPdt,dFPdt, dSdt, dRdt, dDdt )
    
    for (i in 1:date_){
      b[[i]] <- SEIR[,i]
      
    }
    b
    
  })
}

#######################step2###########################

function_result <-function(result,result_list){
  result1<-result[,c(1:37)]
  
  result2<-as.data.frame(result1)
  
  result2$U<-result2$U1+result2$U2+result2$U3+result2$U4
  result2$V<-result2$V1+result2$V2+result2$V3+result2$V4
  result2$E<-result2$E1+result2$E2+result2$E3+result2$E4
  result2$A<-result2$A1+result2$A2+result2$A3+result2$A4
  result2$TP<-result2$TP1+result2$TP2+result2$TP3+result2$TP4
  result2$FP<-result2$FP1+result2$FP2+result2$FP3+result2$FP4
  result2$S<-result2$S1+result2$S2+result2$S3+result2$S4
  result2$R<-result2$R1+result2$R2+result2$R3+result2$R4
  result2$D<-result2$D1+result2$D2+result2$D3+result2$D4
  #result2$I<-result2$I1+result2$I2+result2$I3
  
  result2$TP1.incidence <- c(100,diff(result2$TP1))
  result2$TP2.incidence <- c(100,diff(result2$TP2))
  result2$TP3.incidence <- c(500,diff(result2$TP3))
  result2$TP4.incidence <- c(300,diff(result2$TP4))
  result2$TP.incidence <- c(1000,diff(result2$TP))
  
  result2$S<-result2$S*0.01
  result2$D<-cumsum(result2$D*0.01)
  
  result2$case <-  paste("RT_",r,"NPI_",n_,"VACCINE_",v,"TEST_",te)#column$case
  result_list <-rbind(result_list, result2)
  
  return (result_list)
  
}



###plot_graph_TP###
plot_graph_TP <- function (file_save,r,n_,v,te) {
  plot1<-ggplot(q1) +
    geom_point(aes(x = dates, y = I), size = 1.5, color = "darkblue", fill = "white") +
    geom_line(aes(x = dates, y = I), size = 1, color = "darkblue", fill = "white") +
    geom_line(aes(x = dates, y = TP.incidence), size = 2, color="red", group = 1) +
    geom_ribbon(data = q1, aes(x = dates, ymin = TP.incidence-300, ymax = TP.incidence+300), alpha = .1) +
    
    geom_line(data=q2, aes(x = dates, y = TP.incidence), size = 2, color = "#FF9999", fill = "white") +
    geom_ribbon(data = q2, aes(x = dates, ymin = TP.incidence-300, ymax = TP.incidence+300), alpha = .1) +
    
    geom_line(data=q3, aes(x = dates, y = TP.incidence), size = 2, color = "#CC0033", fill = "white") +
    geom_ribbon(data = q3, aes(x = dates, ymin = TP.incidence-300, ymax = TP.incidence+300), alpha = .1) +
    
    geom_vline(aes(xintercept = as.numeric(dates[c(50)])), linetype = 2, color = 'red') +
    scale_y_continuous(breaks = seq(0,5000,1000), minor_breaks = seq(0,5000,1000), limits = c(0, 5000)) +
    ylab('Incidence') +
    xlab('Date') +
    ggtitle('') +
    scale_x_date(limits = as.Date(c("2021-07-01","2021-12-31")),
                 breaks = as.Date(c("2021-07-01","2021-08-01","2021-09-01", "2021-10-01", "2021-11-01", "2021-12-01")),
                 labels=c ("07-01", "08-01","09-01", "10-01", "11-01", "12-01"))+
    theme(axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 0.1, color = "black"),
          axis.text.y = element_text(color = "black"),
          panel.grid.major.x = element_blank(),
          panel.grid.major.y = element_blank(),
          panel.grid.minor = element_blank(),
          axis.line.x = element_line(color = "black"),
          axis.line.y = element_line(color = "black"),
          panel.background = element_rect(fill = "white"),
          legend.key=element_blank(),
          legend.title = element_blank(),
          text = element_text(size=12),
          panel.spacing = unit(2, "lines"))
  
  plot1
  name <-paste(file_save,"RT_",r,"NPI_",n_,"VACCINE_",v,"TEST_",te,".png")
  ggsave(plot1
         , filename = name, width = 6, height = 3)
  
  
  return()
}

###plot_graph_S###
plot_graph_S <- function (file_save,r,n_,v,te) {
  
  
  plot1<-ggplot(q1) + 
    # geom_point(aes(x = dates, y = I), size = 1.5, color = "darkblue", fill = "white") +
    # geom_line(aes(x = dates, y = I), size = 1, color = "darkblue", fill = "white") +
    geom_line(aes(x = dates, y = S), size = 2, color="red", group = 1) + 
    geom_ribbon(data = q1, aes(x = dates, ymin = S-50, ymax = S+50), alpha = .1) +
    
    geom_line(data=q2, aes(x = dates, y = S), size = 2, color = "#FF9999", fill = "white") +
    geom_ribbon(data = q2, aes(x = dates, ymin = S-50, ymax = S+50), alpha = .1) +
    
    geom_line(data=q3, aes(x = dates, y = S), size = 2, color = "#CC0033", fill = "white") +
    geom_ribbon(data = q3, aes(x = dates, ymin = S-50, ymax = S+50), alpha = .1) +
    
    geom_vline(aes(xintercept = as.numeric(dates[c(50)])), linetype = 2, color = 'red') +
    scale_y_continuous(breaks = seq(0,1000,100), minor_breaks = seq(0,1000,100), limits = c(0, 1000)) +
    ylab('Prevalence') +
    xlab('Date') +
    ggtitle('') +
    scale_x_date(limits = as.Date(c("2021-07-01","2021-12-31")), 
                 breaks = as.Date(c("2021-07-01","2021-08-01","2021-09-01", "2021-10-01", "2021-11-01", "2021-12-01")),
                 labels=c ("07-01", "08-01","09-01", "10-01", "11-01", "12-01"))+
    theme(axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 0.1,color = "black"),
          axis.text.y = element_text(color = "black"),
          panel.grid.major.x = element_blank(),
          panel.grid.major.y = element_blank(),
          panel.grid.minor = element_blank(),
          axis.line.x = element_line(color = "black"),
          axis.line.y = element_line(color = "black"),
          panel.background = element_rect(fill = "white"),
          legend.key=element_blank(),
          legend.title = element_blank(),
          text = element_text(size=12),
          panel.spacing = unit(2, "lines"))
  
  plot1
  name <-paste(file_save,"RT_",r,"NPI_",n_,"VACCINE_",v,"TEST_",te,".png")
  ggsave(plot1
         , filename = name, width = 6, height = 3)
  
  
  return()
}

###plot_graph_D###
plot_graph_D <- function (file_save,r,n_,v,te) {
  
  plot1<-ggplot(q1) + 
    # geom_point(aes(x = dates, y = I), size = 1.5, color = "darkblue", fill = "white") +
    # geom_line(aes(x = dates, y = I), size = 1, color = "darkblue", fill = "white") +
    geom_line(aes(x = dates, y = D), size = 2, color="red", group = 1) + 
    geom_ribbon(data = q1, aes(x = dates, ymin = D-30, ymax = D+30), alpha = .1) +
    
    geom_line(data=q2, aes(x = dates, y = D), size = 2, color = "#FF9999", fill = "white") +
    geom_ribbon(data = q2, aes(x = dates, ymin = D-30, ymax = D+30), alpha = .1) +
    
    geom_line(data=q3, aes(x = dates, y = D), size = 2, color = "#CC0033", fill = "white") +
    geom_ribbon(data = q3, aes(x = dates, ymin = D-30, ymax = D+30), alpha = .1) +
    
    geom_vline(aes(xintercept = as.numeric(dates[c(50)])), linetype = 2, color = 'red') +
    scale_y_continuous(breaks = seq(0,1200,200), minor_breaks = seq(0,1200,200), limits = c(0, 1200)) +
    ylab('Cumulative deaths') +
    xlab('Date') +
    ggtitle('') +
    scale_x_date(limits = as.Date(c("2021-07-01","2021-12-31")), 
                 breaks = as.Date(c("2021-07-01","2021-08-01","2021-09-01", "2021-10-01", "2021-11-01", "2021-12-01")),
                 labels=c ("07-01", "08-01","09-01", "10-01", "11-01", "12-01"))+
    theme(axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 0.1, color = "black"),
          axis.text.y = element_text(color = "black"),
          panel.grid.major.x = element_blank(),
          panel.grid.major.y = element_blank(),
          panel.grid.minor = element_blank(),
          axis.line.x = element_line(color = "black"),
          axis.line.y = element_line(color = "black"),
          panel.background = element_rect(fill = "white"),
          legend.key=element_blank(),
          legend.title = element_blank(),
          text = element_text(size=12),
          panel.spacing = unit(2, "lines"))
  
  plot1
  name <-paste(file_save,"RT_",r,"NPI_",n_,"VACCINE_",v,"TEST_",te,".png")
  ggsave(plot1
         , filename = name, width = 6, height = 3)
  
  
  return()
}

######################step3##############################
getwd()
rt_ <- c(1.05,1,1.1)
npi_ <- c(49,64,78) # (t >= 49 & t < n_) * mean(beta)*0.343*r*0.95에 49를 넣으면 0으로 되어 결국 1<= t <184가 되어 0week이 49라는 변수를 가지게 됩니다.
vac_ <- c(1,0.5,1.5)
test_ <- c(1,0.8,1.2)


result_TP_list =  data.frame()
result_S_D_list =  data.frame()

c_list <- c('TP','S_D')

result_list = data.frame()#make data_frame, 여기에 모든 값들이 한번에 저장됩니다.


lastdate<-date_
test<-df_kr1[c(1,5)]
test[is.na(test)] <- 20000

#test$new_tests.p <- test$new_tests/100000
test<-subset(test, test$date >= start_date)
tau2<-test$new_tests/100000



cum_vac<-df_kr1[c(1,6)]
cum_vac$new_vac <- c(382029,diff(cum_vac$people_vaccinated))
cum_vac<-subset(cum_vac, cum_vac$date >= start_date)

vac<-cum_vac$new_vac/50000000
cum_vac$vac_rate<-cum_vac$new_vac/50000000


for (value_ in c_list){
  
  for (r in rt_){
    for (n_ in npi_){
      
      t <- seq(1, date_, by=1)
      contactpattern <- function(t) (t >= 1 & t < 49) * mean(beta)*0.3+
        (t >= 49 & t < n_) * mean(beta)*0.3*r*0.95+
        (t >=n_ & t < date_) * mean(beta)*0.3*r
      
      
      for (v in vac_){
        vac<-cum_vac$new_vac/50000000
        t <- seq(1, date_, by=1)
        vac1 <- function(t) (t >= 1 & t < 49) * mean(vac)*0.7+
          (t >= 49 & t < date_) * mean(vac)*0.7*v
        
        for (te in test_){
          tau2<-test$new_tests/100000
          t <- seq(1, date_, by=1)
          tau2_1 <- function(t) (t >= 1 & t < 49) * mean(tau2)*0.36 +
            (t >= 49 & t < date_) * mean(tau2)*0.36*te
          
          npop <- 51822000
          f <- c(0.17, 0.27, 0.33, 0.23) # three age classes, with 16% age 0-19; 59% age 20-59; 24% age >60 in Korea 2021
          nage <-  length(f)
          N <-  npop*f       # number in each age class
          
          Se<- 0.95   #sensitivity
          Sp<-0.998
          
          mu = c(0, 0, 0,0) # screening return per day
          tau1 = c(0, 0, 0,0)
          #tau2= c(0.01, 0.01 , 0.01)*1 # testing per day (0.3)/(1-(0.3))*sigma :: assuming 30% testing among asymptomatic pop
          #beta = c(0.1, 0.12, 0.13)*1 # infectious contacts per capita per day
          theta = c(0.35, 0.35, 0.35, 0.35)  # incubation per day
          sigma = c(0.035, 0.035, 0.035, 0.035) # progression per day
          rho = c(1/14, 1/14, 1/14, 1/14)    # recovery per day
          delta = c(0.0001, 0.0002, 0.0004,0.0005)   # case fatality among symptomatic patients per day
          #vac = c(0, 0.03, 0.031)*1 ## July 0.03
          
          values <- list (mu = matrix(mu,nrow=4, ncol=lastdate ),
                          tau1= matrix(tau1,nrow=4, ncol=lastdate ),
                          theta= matrix(theta,nrow=4, ncol=lastdate ),
                          sigma= matrix(sigma,nrow=4, ncol=lastdate),
                          rho= matrix(rho,nrow=4, ncol=lastdate ),
                          delta= matrix(delta,nrow=4, ncol=lastdate ))
          
          C = matrix(0,nrow=nage,ncol=nage)
          C[1,1] = 4  # number contacts per day kids make with kids
          C[1,2] = 3
          C[1,3] = 2  # number contacts per day kids make with adults (all kids have an adult in the home)
          C[1,4] = 2
          
          C[2,1] = 3  # number contacts per day adults make with kids (not all adults have kids)
          C[2,2] = 3
          C[2,3] = 2
          C[2,4] = 2
          
          C[3,1] = 2  # number contacts per day adults make with kids (not all adults have kids)
          C[3,2] = 2
          C[3,3] = 3  # number contacts per day adults make with adults
          C[3,4] = 2
          
          C[4,1] = 2  # number contacts per day adults make with kids (not all adults have kids)
          C[4,2] = 2
          C[4,3] = 2  # number contacts per day adults make with adults
          C[4,4] = 3
          
          V=c(0, 0, 2000000, 3000000) ##July fit c(0, 2000000, 3000000)
          E=c(1000, 1000, 5000, 2000)
          A=c(1000, 1000, 5000, 2000)
          TP=c(500, 500, 2000, 1000) ##July fit c(100, 400, 300)
          S=c(10, 10, 200, 200)
          R=c(10000,2000, 80000, 150000)
          D=c(0, 0, 0, 0)
          FP=c(0, 0, 0, 0)
          U = N-A-V
          #I = c(200, 500, 300)
          
          pop.SI <- c(U = N-A-V, V=V, E=E, A=A ,TP=TP, FP=FP,  S=S, R=R, D=D)
          
          
          ##ODE simulation
          if (value_ == 'TP'){
            result_TP=lsoda(
              y = pop.SI,               # Initial conditions for population
              times = t,         # Timepoints for evaluation
              func = sir,               # Function to evaluate
              parms = values            # Vector of parameters
            )
            result_TP_list<- function_result(result_TP,result_TP_list)
          }
          
          else {
            result_S_D=lsoda(
              y = pop.SI,               # Initial conditions for population
              times = t,         # Timepoints for evaluation
              func = sir1,               # Function to evaluate
              parms = values            # Vector of parameters
            )
            result_S_D_list<- function_result(result_S_D,result_S_D_list)
          }
          
          
        }
      }
    }
  }
  
}
setwd("../../../../03_result/SEIR/210920/")

write.csv(result_TP_list,file ="result_TP_final.csv")
write.csv(result_S_D_list,file ="result_S_D_final.csv")

##########################step4################################ 이부분부터 주석해도 됨
# library(gridExtra)
# library(gridGraphics)
# require(ggplot2)
# library(dplyr)
# library(lubridate)
# 
# setwd("../../../../03_result/SEIR/210920/")
# rt_ <- c(1.05,1,1.1)
# npi_ <- c(49,64,78) # (t >= 49 & t < n_) * mean(beta)*0.343*r*0.95에 49를 넣으면 0으로 되어 결국 1<= t <184가 되어 0week이 49라는 변수를 가지게 됩니다.
# vac_ <- c(1,0.5,1.5)
# test_ <- c(1,0.8,1.2)
# 
# parameter_lsit1 <- c(test_, npi_, vac_, rt_)
# parameter_lsit2 <- c(te, n_, v, r)
# 
# #c_list <- c('TP','S_D')
# 
# ###RT###
# print('RT graph start')
# for (te in test_){
#   for (n_ in npi_){
#     for (v in vac_){
#       for(i in 1:3) {
#         assign(paste0("q",i),i)  #q1,q2,q3 변수 생성 
#       }
#       num  =1 
#       # rt(1,1.05,1.1)을 한번에 plot 하기 위해 이렇게 작성하였습니다. 따로 plot 을 원하시면 아래 for문 코드를 나누시면 될 것 같습니다.
#       
#       for (r in rt_){ 
#         result_final <- read.csv(file = "result_TP_final.csv")
#         result3 <- result_final[result_final$case==paste("RT_",r,"NPI_",n_,"VACCINE_",v,"TEST_",te),]
#         assign(paste0("q",num), merge(incidence1, result3, by="time", all.y = TRUE, no.dups = TRUE))
#         calender <- seq(ymd(final_date), ymd(last_date), by="day")
#         for (i in 50:date_){
#           a <- (eval(parse(text = paste0("q",num))))
#           a$dates[i]<-calender[i-50+1]
#           assign(paste0("q",num),a)
#         }
#         num = num+1
#       }
#       #plot_graph_TP( file_save="image/RT/TP/",r='1,1.05,1.1',n_,v,te)
#       
#       for(i in 1:3) {
#         assign(paste0("q",i),i)  #q1,q2,q3 변수 생성 
#       }
#       num  =1
#       
#       for (r in rt_){ 
#         result_final <- read.csv(file = "result_S_D_final.csv")
#         result3 <- result_final[result_final$case==paste("RT_",r,"NPI_",n_,"VACCINE_",v,"TEST_",te),]
#         assign(paste0("q",num), merge(incidence1, result3, by="time", all.y = TRUE, no.dups = TRUE))
#         calender <- seq(ymd(final_date), ymd(last_date), by="day")
#         for (i in 50:date_){
#           a <- (eval(parse(text = paste0("q",num))))
#           a$dates[i]<-calender[i-50+1]
#           assign(paste0("q",num),a)
#         }
#         num = num+1
#       }
#       #plot_graph_TP(file_save="image/RT/TP2/",r,n_,v,te='parameter')
#       #plot_graph_S(file_save="image/RT/S/",r='1,1.05,1.1',n_,v,te)
#       #plot_graph_D(file_save="image/RT/D/",r='1,1.05,1.1',n_,v,te)
#       
#     }
#   }
# }
# 
# ###TEST###
# print('TEST graph start')
# for (r in rt_){
#   for (n_ in npi_){
#     for (v in vac_){
#       for(i in 1:3) {
#         assign(paste0("q",i),i)  #q1,q2,q3
#       }
#       num  =1 
#       for (te in test_){
#         result_final <- read.csv(file = "result_TP_final.csv")
#         result3 <- result_final[result_final$case==paste("RT_",r,"NPI_",n_,"VACCINE_",v,"TEST_",te),]
#         assign(paste0("q",num), merge(incidence1, result3, by="time", all.y = TRUE, no.dups = TRUE))
#         calender <- seq(ymd(final_date), ymd(last_date), by="day")
#         for (i in 50:date_){
#           a <- (eval(parse(text = paste0("q",num))))
#           a$dates[i]<-calender[i-50+1]
#           assign(paste0("q",num),a)
#         }
#         num = num+1
#       }
#       #plot_graph_TP(file_save="image/TEST/TP/",r,n_,v,te='0.8,1,1.2')
#       
#       for(i in 1:3) {
#         assign(paste0("q",i),i)  #q1,q2,q3 변수 생성 
#       }
#       num  =1
#       
#       for (te in test_){
#         result_final <- read.csv(file = "result_S_D_final.csv")
#         result3 <- result_final[result_final$case==paste("RT_",r,"NPI_",n_,"VACCINE_",v,"TEST_",te),]
#         assign(paste0("q",num), merge(incidence1, result3, by="time", all.y = TRUE, no.dups = TRUE))
#         calender <- seq(ymd(final_date), ymd(last_date), by="day")
#         for (i in 50:date_){
#           a <- (eval(parse(text = paste0("q",num))))
#           a$dates[i]<-calender[i-50+1]
#           assign(paste0("q",num),a)
#         }
#         num = num+1
#       }
#       #plot_graph_TP(file_save="image/TEST/TP2/",r,n_,v,te='parameter')
#       #plot_graph_S(file_save="image/TEST/S/",r,n_,v,te='0.8,1,1.2')
#       #plot_graph_D(file_save="image/TEST/D/",r,n_,v,te='0.8,1,1.2')
#       
#     }
#   }
# }
# 
# ###NPI###
# print('NPI graph start')
# for (r in rt_){
#   for (te in test_){
#     for (v in vac_){
#       for(i in 1:3) {
#         assign(paste0("q",i),i)  #q1,q2,q3
#       }
#       num  =1 
#       for (n_ in npi_){
#         result_final <- read.csv(file = "result_TP_final.csv")
#         result3 <- result_final[result_final$case==paste("RT_",r,"NPI_",n_,"VACCINE_",v,"TEST_",te),]
#         assign(paste0("q",num), merge(incidence1, result3, by="time", all.y = TRUE, no.dups = TRUE))
#         calender <- seq(ymd(final_date), ymd(last_date), by="day")
#         for (i in 50:date_){
#           a <- (eval(parse(text = paste0("q",num))))
#           a$dates[i]<-calender[i-50+1]
#           assign(paste0("q",num),a)
#         }
#         num = num+1
#       }
#       #plot_graph_TP(file_save="image/NPI/TP/",r,n_='49,64,78',v,te)
#       
#       for(i in 1:3) {
#         assign(paste0("q",i),i)  #q1,q2,q3 변수 생성 
#       }
#       num  =1
#       
#       for (n_ in npi_){
#         result_final <- read.csv(file = "result_S_D_final.csv")
#         result3 <- result_final[result_final$case==paste("RT_",r,"NPI_",n_,"VACCINE_",v,"TEST_",te),]
#         assign(paste0("q",num), merge(incidence1, result3, by="time", all.y = TRUE, no.dups = TRUE))
#         calender <- seq(ymd(final_date), ymd(last_date), by="day")
#         for (i in 50:date_){
#           a <- (eval(parse(text = paste0("q",num))))
#           a$dates[i]<-calender[i-50+1]
#           assign(paste0("q",num),a)
#         }
#         num = num+1
#       }
#       
#       
#       #plot_graph_TP(file_save="image/NPI/TP2/",r,n_,v,te='parameter')
#       #plot_graph_S( file_save="image/NPI/S/",r,n_='49,64,78',v,te)
#       #plot_graph_D(file_save="image/NPI/D/",r,n_='49,64,78',v,te)
#       
#     }
#   }
# }
# 
# 
# ###VAC###
# print('VAC graph start')
# for (r in rt_){
#   for (n_ in npi_){
#     for (te in test_){
#       for(i in 1:3) {
#         assign(paste0("q",i),i)  #q1,q2,q3
#       }
#       num  =1 
#       for (v in vac_){
#         result_final <- read.csv(file = "result_TP_final.csv")
#         result3 <- result_final[result_final$case==paste("RT_",r,"NPI_",n_,"VACCINE_",v,"TEST_",te),]
#         assign(paste0("q",num), merge(incidence1, result3, by="time", all.y = TRUE, no.dups = TRUE))
#         calender <- seq(ymd(final_date), ymd(last_date), by="day")
#         for (i in 50:date_){
#           a <- (eval(parse(text = paste0("q",num))))
#           a$dates[i]<-calender[i-50+1]
#           assign(paste0("q",num),a)
#         }
#         num = num+1
#       }
#       
#       #plot_graph_TP( file_save="image/VAC/TP/",r,n_,v='0.5,1,1.5',te)
#       
#       for(i in 1:3) {
#         assign(paste0("q",i),i)  #q1,q2,q3 변수 생성 
#       }
#       num  =1
#       
#       for (v in vac_){
#         result_final <- read.csv(file = "result_S_D_final.csv")
#         result3 <- result_final[result_final$case==paste("RT_",r,"NPI_",n_,"VACCINE_",v,"TEST_",te),]
#         assign(paste0("q",num), merge(incidence1, result3, by="time", all.y = TRUE, no.dups = TRUE))
#         calender <- seq(ymd(final_date), ymd(last_date), by="day")
#         for (i in 50:date_){
#           a <- (eval(parse(text = paste0("q",num))))
#           a$dates[i]<-calender[i-50+1]
#           assign(paste0("q",num),a)
#         }
#         num = num+1
#       }
#       
#       #plot_graph_TP(file_save="image/VAC/TP2/",r,n_,v,te='parameter')
#       #plot_graph_S(file_save="image/VAC/S/",r,n_,v='0.5,1,1.5',te)
#       #plot_graph_D( file_save="image/VAC/D/",r,n_,v='0.5,1,1.5',te)
#     }
#   }
# }


#####################step5############################


sir <- function(t,X,parms){
  
  for (i in 1:length(X)) {
    if (X[i]< 0) {
      X[i] <- 0
    }}  
  
  ncompartment=9
  nage=length(X)/ncompartment
  
  U =(X[1:nage])
  V =(X[(nage+1):(2*nage)])
  E =(X[(2*nage+1):(3*nage)])
  A =(X[(3*nage+1):(4*nage)])
  TP=(X[(4*nage+1):(5*nage)])
  FP=(X[(5*nage+1):(6*nage)])
  S =(X[(6*nage+1):(7*nage)])
  R =(X[(7*nage+1):(8*nage)])
  D =(X[(8*nage+1):(9*nage)])
  #I =(X[(10*nage+1):(11*nage)])
  
  with(as.list(c(parms, X)), {
    # if (t==1){
    #  print(testing)
    #  print(vaccinating)
    # }
    screeningreturn <- mu*FP
    vaccination <- matrix(vac1(t, vac, vaccinating), nrow=4, ncol=184, byrow=T)*U
    infection <- matrix(contactpattern(t, beta, Rt, npi1, npi2), nrow=4, ncol=184, byrow=T)* as.vector(((U*(C))%*%(A/(U+A+V+E))))
    incubation <- theta*(E)
    testingrate <- matrix(tau2_1(t, tau2, testing), nrow=4, ncol=184, byrow=T)*Se*A
    screeningrate <- tau1*(1-Sp)*(U)
    progression1 <-sigma*(A)
    progression2 <-sigma*(TP)
    recovery1 <-rho*(1-delta)*(S)
    recovery2 <-rho*(TP)
    recovery3 <-rho*(A)
    #recovery4 <-rho*as.matrix(V)
    dead <-delta *(S)
    
    dUdt <- -infection - vaccination 
    dVdt <- vaccination 
    dEdt <- infection - incubation
    dAdt <- incubation - testingrate - progression1 - recovery3
    dTPdt<- testingrate #- recovery2 - progression2
    dFPdt<- screeningrate - screeningreturn
    dSdt <- progression2 + progression1 - recovery1 - dead
    dRdt <- recovery1+recovery2+recovery3
    dDdt <- dead
    
    #dIdt<- testingrate 
    #dTP2dt<- - recovery2
    #dTP3dt<- - progression2
    
    SEIR <- rbind(dUdt, dVdt, dEdt, dAdt, dTPdt,dFPdt, dSdt, dRdt, dDdt )
    #as.list(SEIR)
    
    list(SEIR[,1], SEIR[,2], SEIR[,3], SEIR[,4], SEIR[,5], SEIR[,6], SEIR[,7], SEIR[,8], SEIR[,9],SEIR[,10],
         SEIR[,11], SEIR[,12], SEIR[,13], SEIR[,14], SEIR[,15], SEIR[,16], SEIR[,17], SEIR[,18],SEIR[,19],SEIR[,20],
         SEIR[,21], SEIR[,22], SEIR[,23], SEIR[,24], SEIR[,25], SEIR[,26], SEIR[,27], SEIR[,28],SEIR[,29],SEIR[,30],
         SEIR[,31],SEIR[,32], SEIR[,33], SEIR[,34], SEIR[,35], SEIR[,36], SEIR[,37], SEIR[,38],SEIR[,39],SEIR[,40],
         SEIR[,41],SEIR[,42], SEIR[,43], SEIR[,44], SEIR[,45], SEIR[,46], SEIR[,47], SEIR[,48],SEIR[,49],SEIR[,50],
         SEIR[,51],SEIR[,52], SEIR[,53], SEIR[,54], SEIR[,55], SEIR[,56], SEIR[,57], SEIR[,58],SEIR[,59],SEIR[,60],
         SEIR[,61],SEIR[,62], SEIR[,63], SEIR[,64], SEIR[,65], SEIR[,66], SEIR[,67], SEIR[,68],SEIR[,69],SEIR[,70],
         SEIR[,71],SEIR[,72], SEIR[,73], SEIR[,74], SEIR[,75], SEIR[,76], SEIR[,77], SEIR[,78],SEIR[,79],SEIR[,80],
         SEIR[,81],SEIR[,82], SEIR[,83], SEIR[,84], SEIR[,85], SEIR[,86], SEIR[,87], SEIR[,88],SEIR[,89],SEIR[,90],
         SEIR[,91],SEIR[,92], SEIR[,93], SEIR[,94], SEIR[,95], SEIR[,96], SEIR[,97], SEIR[,98],SEIR[,99],SEIR[,100],
         SEIR[,101], SEIR[,102], SEIR[,103], SEIR[,104], SEIR[,105], SEIR[,106], SEIR[,107], SEIR[,108], SEIR[,109],SEIR[,110],
         SEIR[,111], SEIR[,112], SEIR[,113], SEIR[,114], SEIR[,115], SEIR[,116], SEIR[,117], SEIR[,118],SEIR[,119],SEIR[,120],
         SEIR[,121], SEIR[,122], SEIR[,123], SEIR[,124], SEIR[,125], SEIR[,126], SEIR[,127], SEIR[,128],SEIR[,129],SEIR[,130],
         SEIR[,131],SEIR[,132], SEIR[,133], SEIR[,134], SEIR[,135], SEIR[,136], SEIR[,137], SEIR[,138],SEIR[,139],SEIR[,140],
         SEIR[,141],SEIR[,142], SEIR[,143], SEIR[,144], SEIR[,145], SEIR[,146], SEIR[,147], SEIR[,148],SEIR[,149],SEIR[,150],
         SEIR[,151],SEIR[,152], SEIR[,153], SEIR[,154], SEIR[,155], SEIR[,156], SEIR[,157], SEIR[,158],SEIR[,159],SEIR[,160],
         SEIR[,161],SEIR[,162], SEIR[,163], SEIR[,164], SEIR[,165], SEIR[,166], SEIR[,167], SEIR[,168],SEIR[,169],SEIR[,170],
         SEIR[,171],SEIR[,172], SEIR[,173], SEIR[,174], SEIR[,175], SEIR[,176], SEIR[,177], SEIR[,178],SEIR[,179],SEIR[,180],
         SEIR[,181],SEIR[,182], SEIR[,183], SEIR[,184]
         #SEIR[,185], SEIR[,186], SEIR[,187], SEIR[,188],SEIR[,189],SEIR[,190],
         #SEIR[,191],SEIR[,192], SEIR[,193], SEIR[,194], SEIR[,195], SEIR[,196], SEIR[,197], SEIR[,198],SEIR[,199],SEIR[,200]
    )
    
    # for (i in 1:184){
    #   b[[i]] <- SEIR[,i]
    #   
    # }
    # b
    
  })
}

t <- seq(1, 184, by=1) 

test<-df_kr1[c(1,5)]
test[is.na(test)] <- 20000
#test$new_tests.p <- test$new_tests/100000
test<-subset(test, test$date >= "2021-07-01")
tau2<-test$new_tests/100000

vac<-cum_vac$new_vac/50000000
cum_vac<-df_kr1[c(1,6)]
cum_vac$new_vac <- c(382029,diff(cum_vac$people_vaccinated))
cum_vac<-subset(cum_vac, cum_vac$date >= "2021-07-01")


vac1 <- function(t, vac, vaccinating) (t >= 1 & t < 49) * mean(vac)*0.7+
  (t >= 49 & t < 184) * mean(vac)*0.7*vaccinating

tau2_1 <- function(t, tau2, testing) (t >= 1 & t < 49) * mean(tau2)*0.36 +
  (t >= 49 & t < 184) * mean(tau2)*0.36*testing


contactpattern <- function(t, beta, Rt, npi1, npi2) (t >= 1 & t < 49) * mean(beta)*0.343 +
  (t >= 49 & t < npi1) * mean(beta)*0.343*Rt*npi2 + 
  (t >=npi1 & t < 184) * mean(beta)*0.343*Rt


#time <- 0
npop <- 51822000
f <- c(0.16*11/18, 0.16*7/18, 0.59, 0.24) # three age classes, with 16% age 0-19; 59% age 20-59; 24% age >60 in Korea 2021
nage <-  length(f)
N <-  npop*f      # number in each age class

Se<- 0.95   #sensitivity
Sp<-0.998

mu = c(0, 0, 0,0) # screening return per day
tau1 = c(0, 0, 0,0)
#tau2= c(0.01, 0.01 , 0.01)*1 # testing per day (0.3)/(1-(0.3))*sigma :: assuming 30% testing among asymptomatic pop
#beta = c(0.1, 0.12, 0.13)*1 # infectious contacts per capita per day
theta = c(0.35, 0.35, 0.35, 0.35)  # incubation per day
sigma = c(0.035, 0.035, 0.035, 0.035) # progression per day
rho = c(1/14, 1/14, 1/14, 1/14)    # recovery per day
delta = c(0.0001, 0.0002, 0.0004,0.0005)   # case fatality among symptomatic patients per day
#vac = c(0, 0.03, 0.031)*1 ## July 0.03
mu = matrix(mu,nrow=4, ncol=184 )
tau1= matrix(tau1,nrow=4, ncol=184 )
theta= matrix(theta,nrow=4, ncol=184 )
sigma= matrix(sigma,nrow=4, ncol=184)
rho= matrix(rho,nrow=4, ncol=184 )
delta= matrix(delta,nrow=4, ncol=184 )

C = matrix(0,nrow=nage,ncol=nage)
C[1,1] = 4  # number contacts per day kids make with kids
C[1,2] = 3  
C[1,3] = 2  # number contacts per day kids make with adults (all kids have an adult in the home)
C[1,4] = 2 

C[2,1] = 3  # number contacts per day adults make with kids (not all adults have kids)
C[2,2] = 3
C[2,3] = 3
C[2,4] = 3

C[3,1] = 2  # number contacts per day adults make with kids (not all adults have kids)
C[3,2] = 2 
C[3,3] = 2  # number contacts per day adults make with adults
C[3,4] = 2 

C[4,1] = 2  # number contacts per day adults make with kids (not all adults have kids)
C[4,2] = 2 
C[4,3] = 2  # number contacts per day adults make with adults
C[4,4] = 2 

V=c(0, 0, 2000000, 3000000) ##July fit c(0, 2000000, 3000000)
E=c(1000, 1000, 5000, 2000)
A=c(1000, 1000, 5000, 2000)
TP=c(500, 500, 2000, 1000) ##July fit c(100, 400, 300)
S=c(10, 10, 200, 200)
R=c(10000,2000, 80000, 150000)
D=c(0, 0, 0, 0)
FP=c(0, 0, 0, 0)
U = N-A-V
#I = c(200, 500, 300)


ts.sir.calc1 <- function(Rt, npi1, npi2, vaccinating, testing){
  
  ## time series Rt (Beta), testing rate, vaccination rate
  test<-df_kr1[c(1,5)]
  test[is.na(test)] <- 20000
  #test$new_tests.p <- test$new_tests/100000
  test<-subset(test, test$date >= "2021-07-01")
  
  cum_vac<-df_kr1[c(1,6)]
  cum_vac$new_vac <- c(382029,diff(cum_vac$people_vaccinated))
  cum_vac<-subset(cum_vac, cum_vac$date >= "2021-07-01")
  
  #time <- 0
  npop <- 51822000
  f <- c(0.16*11/18, 0.16*7/18, 0.59, 0.24) # three age classes, with 16% age 0-19; 59% age 20-59; 24% age >60 in Korea 2021
  nage <-  length(f)
  N <-  npop*f      # number in each age class
  
  Se<- 0.95   #sensitivity
  Sp<-0.998
  
  mu = c(0, 0, 0,0) # screening return per day
  tau1 = c(0, 0, 0,0)
  #tau2= c(0.01, 0.01 , 0.01)*1 # testing per day (0.3)/(1-(0.3))*sigma :: assuming 30% testing among asymptomatic pop
  #beta = c(0.1, 0.12, 0.13)*1 # infectious contacts per capita per day
  theta = c(0.35, 0.35, 0.35, 0.35)  # incubation per day
  sigma = c(0.035, 0.035, 0.035, 0.035) # progression per day
  rho = c(1/14, 1/14, 1/14, 1/14)    # recovery per day
  delta = c(0.0001, 0.0002, 0.0004,0.0005)   # case fatality among symptomatic patients per day
  #vac = c(0, 0.03, 0.031)*1 ## July 0.03
  
  values <- list (mu = matrix(mu,nrow=4, ncol=184 ),
                  tau1= matrix(tau1,nrow=4, ncol=184 ),
                  theta= matrix(theta,nrow=4, ncol=184 ),
                  sigma= matrix(sigma,nrow=4, ncol=184),
                  rho= matrix(rho,nrow=4, ncol=184 ),
                  delta= matrix(delta,nrow=4, ncol=184 ),
                  vac=vac,
                  vaccinating=vaccinating,
                  tau2=tau2,
                  testing=testing,
                  beta=beta,
                  Rt=Rt,
                  npi1=npi1,
                  npi2=npi2)
  
  C = matrix(0,nrow=nage,ncol=nage)
  C[1,1] = 4  # number contacts per day kids make with kids
  C[1,2] = 3  
  C[1,3] = 2  # number contacts per day kids make with adults (all kids have an adult in the home)
  C[1,4] = 2 
  
  C[2,1] = 3  # number contacts per day adults make with kids (not all adults have kids)
  C[2,2] = 3
  C[2,3] = 2
  C[2,4] = 2
  
  C[3,1] = 2  # number contacts per day adults make with kids (not all adults have kids)
  C[3,2] = 2 
  C[3,3] = 3  # number contacts per day adults make with adults
  C[3,4] = 2 
  
  C[4,1] = 2  # number contacts per day adults make with kids (not all adults have kids)
  C[4,2] = 2 
  C[4,3] = 2  # number contacts per day adults make with adults
  C[4,4] = 3 
  
  V=c(0, 0, 2000000, 3000000) ##July fit c(0, 2000000, 3000000)
  E=c(1000, 1000, 5000, 2000)
  A=c(1000, 1000, 5000, 2000)
  TP=c(500, 500, 2000, 1000) ##July fit c(100, 400, 300)
  S=c(10, 10, 200, 200)
  R=c(10000,2000, 80000, 150000)
  D=c(0, 0, 0, 0)
  FP=c(0, 0, 0, 0)
  U = N-A-V
  #I = c(200, 500, 300)
  
  pop.SI <- c(U = N-A-V, V=V, E=E, A=A ,TP=TP, FP=FP,  S=S, R=R, D=D)
  
  ##ODE simulation
  
  result=lsoda(
    y = pop.SI,               # Initial conditions for population
    times = t,                # Timepoints for evaluation
    func = sir,               # Function to evaluate
    parms = values            # Vector of parameters
  )
  
  result1<-result[,c(1:37)]
  
  result2<-as.data.frame(result1)
  
  result2$U<-result2$U1+result2$U2+result2$U3+result2$U4
  result2$V<-result2$V1+result2$V2+result2$V3+result2$V4
  result2$E<-result2$E1+result2$E2+result2$E3+result2$E4
  result2$A<-result2$A1+result2$A2+result2$A3+result2$A4
  result2$TP<-result2$TP1+result2$TP2+result2$TP3+result2$TP4
  result2$FP<-result2$FP1+result2$FP2+result2$FP3+result2$FP4
  result2$S<-result2$S1+result2$S2+result2$S3+result2$S4
  result2$R<-result2$R1+result2$R2+result2$R3+result2$R4
  result2$D<-result2$D1+result2$D2+result2$D3+result2$D4
  #result2$I<-result2$I1+result2$I2+result2$I3
  
  result2$TP1.incidence <- c(100,diff(result2$TP1))
  result2$TP2.incidence <- c(100,diff(result2$TP2))
  result2$TP3.incidence <- c(500,diff(result2$TP3))
  result2$TP4.incidence <- c(300,diff(result2$TP4))
  result2$TP.incidence <- c(1000,diff(result2$TP))
  
  result2$S<-result2$S*0.01
  result2$D<-cumsum(result2$D*0.01)
  
  # result<-round(result)
  # result$TP.incidence <- c(1000,diff(result$TP))
  a<-colSums(result2)
  return(a)
  
  # if (type =="heatmap") {
  #   return(a)
  # }else{
  #   return(result)
  # }
  
}


ts.sir.calc2 <- function(Rt, npi1, npi2, vaccinating, testing){
  
  ## time series Rt (Beta), testing rate, vaccination rate
  test<-df_kr1[c(1,5)]
  test[is.na(test)] <- 20000
  #test$new_tests.p <- test$new_tests/100000
  test<-subset(test, test$date >= "2021-07-01")
  
  cum_vac<-df_kr1[c(1,6)]
  cum_vac$new_vac <- c(382029,diff(cum_vac$people_vaccinated))
  cum_vac<-subset(cum_vac, cum_vac$date >= "2021-07-01")
  
  #time <- 0
  npop <- 51822000
  f <- c(0.16*11/18, 0.16*7/18, 0.59, 0.24) # three age classes, with 16% age 0-19; 59% age 20-59; 24% age >60 in Korea 2021
  nage <-  length(f)
  N <-  npop*f      # number in each age class
  
  Se<- 0.95   #sensitivity
  Sp<-0.998
  
  mu = c(0, 0, 0,0) # screening return per day
  tau1 = c(0, 0, 0,0)
  #tau2= c(0.01, 0.01 , 0.01)*1 # testing per day (0.3)/(1-(0.3))*sigma :: assuming 30% testing among asymptomatic pop
  #beta = c(0.1, 0.12, 0.13)*1 # infectious contacts per capita per day
  theta = c(0.35, 0.35, 0.35, 0.35)  # incubation per day
  sigma = c(0.035, 0.035, 0.035, 0.035) # progression per day
  rho = c(1/14, 1/14, 1/14, 1/14)    # recovery per day
  delta = c(0.0001, 0.0002, 0.0004,0.0005)   # case fatality among symptomatic patients per day
  #vac = c(0, 0.03, 0.031)*1 ## July 0.03
  
  values <- list (mu = matrix(mu,nrow=4, ncol=184 ),
                  tau1= matrix(tau1,nrow=4, ncol=184 ),
                  theta= matrix(theta,nrow=4, ncol=184 ),
                  sigma= matrix(sigma,nrow=4, ncol=184),
                  rho= matrix(rho,nrow=4, ncol=184 ),
                  delta= matrix(delta,nrow=4, ncol=184 ),
                  vac=vac,
                  vaccinating=vaccinating,
                  tau2=tau2,
                  testing=testing,
                  beta=beta,
                  Rt=Rt,
                  npi1=npi1,
                  npi2=npi2)
  
  C = matrix(0,nrow=nage,ncol=nage)
  C[1,1] = 4  # number contacts per day kids make with kids
  C[1,2] = 3  
  C[1,3] = 2  # number contacts per day kids make with adults (all kids have an adult in the home)
  C[1,4] = 2 
  
  C[2,1] = 3  # number contacts per day adults make with kids (not all adults have kids)
  C[2,2] = 3
  C[2,3] = 3
  C[2,4] = 3
  
  C[3,1] = 2  # number contacts per day adults make with kids (not all adults have kids)
  C[3,2] = 2 
  C[3,3] = 2  # number contacts per day adults make with adults
  C[3,4] = 2 
  
  C[4,1] = 2  # number contacts per day adults make with kids (not all adults have kids)
  C[4,2] = 2 
  C[4,3] = 2  # number contacts per day adults make with adults
  C[4,4] = 2 
  
  V=c(0, 0, 2000000, 3000000) ##July fit c(0, 2000000, 3000000)
  E=c(1000, 1000, 5000, 2000)
  A=c(1000, 1000, 5000, 2000)
  TP=c(500, 500, 2000, 1000) ##July fit c(100, 400, 300)
  S=c(10, 10, 200, 200)
  R=c(10000,2000, 80000, 150000)
  D=c(0, 0, 0, 0)
  FP=c(0, 0, 0, 0)
  U = N-A-V
  #I = c(200, 500, 300)
  
  pop.SI <- c(U = N-A-V, V=V, E=E, A=A ,TP=TP, FP=FP,  S=S, R=R, D=D)
  
  ##ODE simulation
  
  result=lsoda(
    y = pop.SI,               # Initial conditions for population
    times = t,                # Timepoints for evaluation
    func = sir,               # Function to evaluate
    parms = values            # Vector of parameters
  )
  
  result1<-result[,c(1:37)]
  
  result2<-as.data.frame(result1)
  
  result2$U<-result2$U1+result2$U2+result2$U3+result2$U4
  result2$V<-result2$V1+result2$V2+result2$V3+result2$V4
  result2$E<-result2$E1+result2$E2+result2$E3+result2$E4
  result2$A<-result2$A1+result2$A2+result2$A3+result2$A4
  result2$TP<-result2$TP1+result2$TP2+result2$TP3+result2$TP4
  result2$FP<-result2$FP1+result2$FP2+result2$FP3+result2$FP4
  result2$S<-result2$S1+result2$S2+result2$S3+result2$S4
  result2$R<-result2$R1+result2$R2+result2$R3+result2$R4
  result2$D<-result2$D1+result2$D2+result2$D3+result2$D4
  #result2$I<-result2$I1+result2$I2+result2$I3
  
  result2$TP1.incidence <- c(100,diff(result2$TP1))
  result2$TP2.incidence <- c(100,diff(result2$TP2))
  result2$TP3.incidence <- c(500,diff(result2$TP3))
  result2$TP4.incidence <- c(300,diff(result2$TP4))
  result2$TP.incidence <- c(1000,diff(result2$TP))
  
  result2$S<-result2$S*0.01
  result2$D<-cumsum(result2$D*0.01)
  
  result2<-round(result2)
  result2$TP.incidence <- c(1000,diff(result2$TP))
  #a<-colSums(result2)
  #return(a)
  return(result2)
  
}

#plot(result$time, result$TP.incidence)

##Graphs: total 81 graphs : 9 graphs per vac.test.base x 9 vac.test.base = 81
# vac.test.base1 <-mapply(ts.sir.calc2, Rt=1, npi1=64, npi2 =1, vaccinating=rep(c(0.5, 1, 1.5), each=3), testing=rep(c(0.8, 1, 1.2), times=3), SIMPLIFY=FALSE)
# vac.test.base2 <-mapply(ts.sir.calc2, Rt=1, npi1=64, npi2 =0.95, vaccinating=rep(c(0.5, 1, 1.5), each=3), testing=rep(c(0.8, 1, 1.2), times=3), SIMPLIFY=FALSE)
# vac.test.base3 <-mapply(ts.sir.calc2, Rt=1, npi1=75, npi2 =0.95, vaccinating=rep(c(0.5, 1, 1.5), each=3), testing=rep(c(0.8, 1, 1.2), times=3), SIMPLIFY=FALSE)
# vac.test.base4 <-mapply(ts.sir.calc2, Rt=1.05, npi1=64, npi2 =1, vaccinating=rep(c(0.5, 1, 1.5), each=3), testing=rep(c(0.8, 1, 1.2), times=3), SIMPLIFY=FALSE)
# vac.test.base5 <-mapply(ts.sir.calc2, Rt=1.05, npi1=64, npi2 =0.95, vaccinating=rep(c(0.5, 1, 1.5), each=3), testing=rep(c(0.8, 1, 1.2), times=3), SIMPLIFY=FALSE)
# vac.test.base6 <-mapply(ts.sir.calc2, Rt=1.05, npi1=75, npi2 =0.95, vaccinating=rep(c(0.5, 1, 1.5), each=3), testing=rep(c(0.8, 1, 1.2), times=3), SIMPLIFY=FALSE)
# vac.test.base7 <-mapply(ts.sir.calc2, Rt=1.1, npi1=64, npi2 =1, vaccinating=rep(c(0.5, 1, 1.5), each=3), testing=rep(c(0.8, 1, 1.2), times=3), SIMPLIFY=FALSE)
# vac.test.base8 <-mapply(ts.sir.calc2, Rt=1.1, npi1=64, npi2 =0.95, vaccinating=rep(c(0.5, 1, 1.5), each=3), testing=rep(c(0.8, 1, 1.2), times=3), SIMPLIFY=FALSE)
# vac.test.base9 <-mapply(ts.sir.calc2, Rt=1.1, npi1=75, npi2 =0.95, vaccinating=rep(c(0.5, 1, 1.5), each=3), testing=rep(c(0.8, 1, 1.2), times=3), SIMPLIFY=FALSE)
# # 
# lapply(vac.test.base1)
# 
# 
# source("../Codes/Step3_SEIR_MC_simplot1_TPinc.R")
# out_plot <- SEIR_BMC_smplot(result3 = vac.test.base1[[1]],
#                             result4 = vac.test.base1[[2]],
#                             result5 = vac.test.base1[[3]])
# out_plot

##Heatmap
vac.test.base1 <-mapply(ts.sir.calc1 , Rt=1, npi1=64, npi2 =1, vaccinating=rep(c(0.6,0.8, 1, 1.2, 1.4), each=5), testing=rep(c(0.6,0.8, 1, 1.2, 1.4), times=5), SIMPLIFY=FALSE)
vac.test.base2 <-mapply(ts.sir.calc1, Rt=1, npi1=78, npi2 =0.9, vaccinating=rep(c(0.6,0.8, 1, 1.2, 1.4), each=5), testing=rep(c(0.6,0.8, 1, 1.2, 1.4), times=5), SIMPLIFY=FALSE)
vac.test.base3 <-mapply(ts.sir.calc1, Rt=1, npi1=103, npi2 =0.9, vaccinating=rep(c(0.6,0.8, 1, 1.2, 1.4), each=5), testing=rep(c(0.6,0.8, 1, 1.2, 1.4), times=5), SIMPLIFY=FALSE)
vac.test.base4 <-mapply(ts.sir.calc1, Rt=2, npi1=64, npi2 =1,  vaccinating=rep(c(0.6,0.8, 1, 1.2, 1.4), each=5), testing=rep(c(0.6,0.8, 1, 1.2, 1.4), times=5), SIMPLIFY=FALSE)
vac.test.base5 <-mapply(ts.sir.calc1, Rt=2, npi1=78, npi2 =0.9,  vaccinating=rep(c(0.6,0.8, 1, 1.2, 1.4), each=5), testing=rep(c(0.6,0.8, 1, 1.2, 1.4), times=5), SIMPLIFY=FALSE)
vac.test.base6 <-mapply(ts.sir.calc1, Rt=2, npi1=103, npi2 =0.9, vaccinating=rep(c(0.6,0.8, 1, 1.2, 1.4), each=5), testing=rep(c(0.6,0.8, 1, 1.2, 1.4), times=5), SIMPLIFY=FALSE)
vac.test.base7 <-mapply(ts.sir.calc1, Rt=3, npi1=64, npi2 =1,  vaccinating=rep(c(0.6,0.8, 1, 1.2, 1.4), each=5), testing=rep(c(0.6,0.8, 1, 1.2, 1.4), times=5), SIMPLIFY=FALSE)
vac.test.base8 <-mapply(ts.sir.calc1, Rt=3, npi1=78, npi2 =0.9,  vaccinating=rep(c(0.6,0.8, 1, 1.2, 1.4), each=5), testing=rep(c(0.6,0.8, 1, 1.2, 1.4), times=5), SIMPLIFY=FALSE)
vac.test.base9 <-mapply(ts.sir.calc1, Rt=3, npi1=103, npi2 =0.9, vaccinating=rep(c(0.6,0.8, 1, 1.2, 1.4), each=5), testing=rep(c(0.6,0.8, 1, 1.2, 1.4), times=5), SIMPLIFY=FALSE)

vac.test.df1 <- as.data.frame(do.call("rbind", vac.test.base1))
vac.test.df2 <- as.data.frame(do.call("rbind", vac.test.base2))
vac.test.df3 <- as.data.frame(do.call("rbind", vac.test.base3))
vac.test.df4 <- as.data.frame(do.call("rbind", vac.test.base4))
vac.test.df5 <- as.data.frame(do.call("rbind", vac.test.base5))
vac.test.df6 <- as.data.frame(do.call("rbind", vac.test.base6))
vac.test.df7 <- as.data.frame(do.call("rbind", vac.test.base7))
vac.test.df8 <- as.data.frame(do.call("rbind", vac.test.base8))
vac.test.df9 <- as.data.frame(do.call("rbind", vac.test.base9))

vac.test.com1 <- matrix(vac.test.df1$TP.incidence, nrow=5)
vac.test.com2 <- matrix(vac.test.df2$TP.incidence, nrow=5)
vac.test.com3 <- matrix(vac.test.df3$TP.incidence, nrow=5)
vac.test.com4 <- matrix(vac.test.df4$TP.incidence, nrow=5)
vac.test.com5 <- matrix(vac.test.df5$TP.incidence, nrow=5)
vac.test.com6 <- matrix(vac.test.df6$TP.incidence, nrow=5)
vac.test.com7 <- matrix(vac.test.df7$TP.incidence, nrow=5)
vac.test.com8 <- matrix(vac.test.df8$TP.incidence, nrow=5)
vac.test.com9 <- matrix(vac.test.df9$TP.incidence, nrow=5)

vac.test.all1<-cbind(vac.test.com1, vac.test.com2,vac.test.com3)/1000
vac.test.all2<-cbind(vac.test.com4, vac.test.com5,vac.test.com6)/1000
vac.test.all3<-cbind(vac.test.com7, vac.test.com8,vac.test.com9)/1000

## integrate data
vac.test.all4<-rbind(vac.test.all1, vac.test.all2, vac.test.all3)


rownames(vac.test.all1) <-c("0.2", "0.18", "0.16" , "0.14", "0.12")
colnames(vac.test.all1) <-c("0.003", "0.005", "0.007" , "0.009", "0.011","0.003", "0.005", "0.007" , "0.009", "0.011","0.003", "0.005", "0.007" , "0.009", "0.011")
rownames(vac.test.all2) <-c("0.2", "0.18", "0.16" , "0.14", "0.12")
colnames(vac.test.all2) <-c("0.003", "0.005", "0.007" , "0.009", "0.011","0.003", "0.005", "0.007" , "0.009", "0.011","0.003", "0.005", "0.007" , "0.009", "0.011")
rownames(vac.test.all3) <-c("0.2", "0.18", "0.16" , "0.14", "0.12")
colnames(vac.test.all3) <-c("0.003", "0.005", "0.007" , "0.009", "0.011","0.003", "0.005", "0.007" , "0.009", "0.011","0.003", "0.005", "0.007" , "0.009", "0.011")

## raw name to 15
rownames(vac.test.all4) <-c("0.2", "0.18", "0.16" , "0.14", "0.12","0.2", "0.18", "0.16" , "0.14", "0.12","0.2", "0.18", "0.16" , "0.14", "0.12")
colnames(vac.test.all4) <-c("0.003", "0.005", "0.007" , "0.009", "0.011","0.003", "0.005", "0.007" , "0.009", "0.011","0.003", "0.005", "0.007" , "0.009", "0.011")


library(gplots)
library(raster)
library(RColorBrewer)

par( mar = c(3,3,2,1) )

heatmap.vals = heatmap.2(vac.test.all4, 
                         dendrogram = 'none', 
                         #breaks = 12,
                         #breaks = c(200000,2300000,2600000,300000,3300000,3600000,400000,4300000,4600000,500000),
                         Rowv=FALSE, 
                         Colv=FALSE, 
                         trace='none', 
                         scale='none',
                         xlab=c("Vaccination rate"),
                         ylab=c("Testing rate"),
                         cexRow=1.0,
                         cexCol=1.0,
                         symkey = FALSE,
                         #symbreaks = FALSE,
                         key = FALSE,
                         
                         # separate 3X3 blocks
                         rowsep=c(5, 10),
                         colsep=c(5, 10),
                         
                         # if you want to change the block space width or color, using these options
                         #sepwidth = c(0.1,0.1),
                         #sepcolor = 'black',
                         
                         margins = c(4.5,4.5),
                         col=(brewer.pal(9,"Reds"))
)

# icer_trunc = format(ICER.limit,big.mark=",", trim=TRUE)
# trunc_string = sprintf ("ICER values above %s have been set to %s", icer_trunc, icer_trunc)
# text (x=1, y=1, labels=trunc_string, cex=.8)

heatmap.legend.labels = array(data=NA,dim=c(9, 1))

for (i in 1:nrow(heatmap.legend.labels)) {
  heatmap.legend.labels[i] = 
    sprintf("%.0f - %.0f", 
            round(heatmap.vals$colorTable[i,1],0), 
            round(heatmap.vals$colorTable[i,2],0)
    )
}

legend("bottomleft",      
       legend = heatmap.legend.labels,
       col=heatmap.vals$col,
       title="Cumulative incidence\n(in thousands)",
       bty="n",
       lty= 1,             
       lwd = 5,
       cex=.9
)


mtext("No NPI                        4wk NPI                     8wk NPI", line = 18, adj=0.65)
mtext("Total COVID-19 cases (Sep-Dec 2021) \n based on testing and vaccination rates by Rt scenarios", line = 21, adj=0.6)
