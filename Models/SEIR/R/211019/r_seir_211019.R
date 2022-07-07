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
library(readr) 
library(tidyverse)
library(readxl)
library(parallel)

##################step0########################
setwd("/home/sysadm/Desktop/pogba_backup2/0_Gil_Prediction/02_code/SEIR/R/211019")
df<-read.csv("../../../../01_data/SEIR/owid-covid-data_1115.csv")
read_csv("../../../../01_data/SEIR/vaxx_0926_child_70.csv")->vac_prop
read_excel("../../../../01_data/SEIR/variants_1005_1.xlsx") -> variants

lastdate<-914

original_date <-'2021-07-01'
start_date <-'2021-08-19'
final_date <- '2021-10-07' #
last_date<- '2023-12-31' #184 숫자를 맞추기 위해




######################################Incidence data#################################################################
#######################EUN###################################
# df1<-read.csv("seir_data_1004.csv")
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
# df_kr1<-subset(df1, date>=original_date & date<final_date)
#######################EUN###################################


#######################owid###################################

df1<-df[c(4,3,6,9, 26, 36,37)]
colnames(df1)[2]<-"state"
colnames(df1)[3]<-"cases"
colnames(df1)[4]<-"deaths"
df1$date =as.Date(df1$date, format="%Y-%m-%d")

df_kr<-df1%>% filter(state=="South Korea")
df_kr1<-df_kr[df_kr$date>=original_date & df_kr$date <final_date,]

#######################owid###################################


cases<-df_kr1[c(1,3)]
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


incidence<-as.data.frame(Covid2021$incidence)
incidence$dates =as.Date(incidence$dates, format="%Y-%m-%d")
incidence1<-subset(incidence, dates >= original_date)
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


######################################vaccination data#################################################################


colnames(vac_prop) <-  c("time",    "age",    "dose",    "date",    "pfizer",    "JJ",    "moderna",    "AZ",    "AZPF")
vac_prop$age1 = ifelse(grepl("12-17", vac_prop$age), "1",
                       ifelse( grepl("18-29", vac_prop$age),"2",
                               ifelse(grepl("30-39", vac_prop$age), "2",
                                      ifelse(grepl("40-49", vac_prop$age), "3",
                                             ifelse( grepl("50-59", vac_prop$age),"3",
                                                     ifelse(  grepl("60-64", vac_prop$age), "4",
                                                              ifelse(grepl("65-69", vac_prop$age), "4",
                                                                     ifelse( grepl("70-74", vac_prop$age), "4",
                                                                             ifelse(grepl("75-79", vac_prop$age), "4",
                                                                                    ifelse(grepl("80", vac_prop$age), "4",
                                                                                           NA)))))))))) ##case_when



vac_prop$date =as.Date(vac_prop$date , format="%Y-%m-%d")
popsize<-c(8907071, 13796133, 16695543, 11870436) ##0-19; 20-39; 40-59; 60+


vac_prop <- vac_prop %>%
  select("time",    "age1",    "dose",    "date",    "pfizer",    "JJ",    "moderna",    "AZ",    "AZPF")%>%
  mutate(mRNA = pfizer + moderna) %>%
  mutate(week = strftime(date, "%V")) 

vac_prop <- vac_prop %>%
  dplyr::select(week, date, age1, dose, mRNA, AZ, JJ, AZPF) %>%
  mutate(dose = case_when (dose == 1 ~ c("dose1"),dose == 2 ~ c("dose2")))



variants1 <- variants[c(4, 8, 16)]
colnames(variants1) <- c("age",  "date", "virus")
#freq(variants1$virus)

vac_prop$date<-as.Date(vac_prop$date)
variants1$date<-as.Date(variants1$date)

variants2<-variants1 %>%   filter(date > "2021-03-05" & date < "2021-09-26") 


a<-data.frame(unclass(table(variants2$date,variants2$virus)))
a$Wild_type.true<-round(a$Wild_type*0.3) #70% are missing testing data (to be confirmed)
a$total<-rowSums(a[, c(1:3,5)])
#a$total<-colSums(a)
a$Delta.p<-round(a$Delta/a$total,2)

aa <- cbind(rownames(a), data.frame(a, row.names=NULL))
names(aa)[1]<-"date"
aa$date<-as.Date(aa$date)
aaa<-aa %>% dplyr ::select(date, Delta.p)  
aaa$week= strftime(aaa$date, "%V")

aaa1<-aaa%>% 
  group_by(week)  %>%
  summarize(Delta.wp=max(Delta.p))

##increasing delta variants upto 100% by 2021
add_week<-seq(from =39, to=51)
add_Delta.wp<-seq(from=0.7, to=0.95, by=0.02)
proj<-cbind(add_week, add_Delta.wp)
colnames(proj)<-c("week", "Delta.wp")
aaa2<-rbind(aaa1, proj )

vac_prop1<-merge(vac_prop, aaa2, by=c("week"), all.x=TRUE)

ve_AZ1=0.5; ve_AZ2=0.77; ve_AZ1_delta=0.3; ve_AZ2_delta =0.66
ve_JJ = 0.5; ve_JJ_delta = 0.3; 

ve_mRNA1 = 0.5; ve_mRNA2=0.95
ve_mRNA1_delta=0.35; ve_mRNA2_delta=0.88

ve_mRNA_v <- c(ve_mRNA1, ve_mRNA2)
ve_mRNA_delta_v<- c(ve_mRNA1_delta, ve_mRNA2_delta)

ve_AZ_v <- c(ve_AZ1, ve_AZ2)
ve_AZ_delta_v<- c(ve_AZ1_delta, ve_AZ2_delta)

ve_JJ_v <- c(ve_JJ)
ve_JJ_delta_v<- c(ve_JJ_delta)

vac_prop3<-vac_prop1 %>%  
  ungroup()%>%
  mutate(age1=as.numeric(age1))%>%
  mutate(week=as.numeric(week))%>%
  mutate(totalpop=popsize[age1])%>%
  group_by(week, dose, age1) %>% 
  summarise (mRNA_p = sum (mRNA), AZ_p =sum(AZ), JJ_p=sum(JJ), AZPF_p=sum(AZPF), Delta.wp = first(Delta.wp), .groups="drop") %>%
  mutate(dose=factor(dose)) %>%
  mutate(ve_mRNA_w=(ve_mRNA_v[dose]*(1-Delta.wp)+ve_mRNA_delta_v[dose]*Delta.wp),
         mRNA_result = mRNA_p * ve_mRNA_w)%>%
  mutate(ve_AZ_w=(ve_AZ_v[dose]*(1-Delta.wp)+ve_AZ_delta_v[dose]*Delta.wp),
         AZ_result = AZ_p * ve_AZ_w)%>%
  mutate(ve_JJ_w=(ve_JJ_v*(1-Delta.wp)+ve_JJ_delta_v*Delta.wp),
         JJ_result = JJ_p * ve_JJ_w)%>%
  mutate(ve_AZPF_w=(ve_AZ_v[dose]*(1-Delta.wp)+ve_mRNA_delta_v[dose]*Delta.wp),
         AZPF_result = AZPF_p * ve_AZPF_w)

vac_prop4 <- vac_prop3 %>% 
  group_by(week, age1) %>%
  summarise (mRNA_p=sum(mRNA_p),
             AZ_p=sum(AZ_p),
             JJ_p=sum(JJ_p),
             AZPF_p=sum(AZPF_p),
             ve_mRNA_w = sum(ve_mRNA_w),
             ve_AZ_w = sum(ve_AZ_w),
             ve_JJ_w = sum(ve_JJ_w),
             ve_AZPF_w = sum(ve_AZPF_w),
             mRNA_result=sum(mRNA_result),
             AZ_result=sum(AZ_result),
             JJ_result=sum(JJ_result),
             AZPF_result=sum(AZPF_result),
             .groups="drop") %>%
  mutate(tot_result = rowSums(.[11:14])) 

vac_prop5 <- vac_prop4 %>% dplyr ::select(week, age1, tot_result)

library(tidyr)
vac_prop6<- round(spread(vac_prop5, key=week, value=tot_result))
vac_prop6 <- vac_prop6[,-1 ] 

vac_prop7<-subset(vac_prop6[, c(18:43)])

vac_prop8<-vac_prop7/7  ## divide by 7 in order to convert to daily vaccination number

vac_prop8<-as.data.frame(vac_prop8,use.names=FALSE)
vac_prop8<-as.matrix(vac_prop8)
colnames(vac_prop8)<- NULL

aa1<-as.vector(vac_prop8[1,1:26])
aa2<-as.vector(vac_prop8[2,1:26])
aa3<-as.vector(vac_prop8[3,1:26])
aa4<-as.vector(vac_prop8[4,1:26])

age.vac<-matrix(NA, nrow=4, ncol=280) #30*7

for(i in 0:29){
  age.vac[1,(i*7+1):(i*7+7)]<-rep(aa1[i+1], each =7)
  age.vac[2,(i*7+1):(i*7+7)]<-rep(aa2[i+1], each =7)
  age.vac[3,(i*7+1):(i*7+7)]<-rep(aa3[i+1], each =7)
  age.vac[4,(i*7+1):(i*7+7)]<-rep(aa4[i+1], each =7)
}

age.vac[is.na(age.vac)] <- 0

age.vac1<-matrix(NA, nrow=4, ncol=lastdate)
age.vac1[1,]<-round(mean(age.vac[1,1:180]))#6개월 구간 
age.vac1[2,]<-round(mean(age.vac[2,1:180]))
age.vac1[3,]<-round(mean(age.vac[3,1:180]))
age.vac1[4,]<-round(mean(age.vac[4,1:180]))

#################step1##################


sir <- function(t,X,parms){
  
  ncompartment=10
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
  TP.I =(X[(10*nage+1):(11*nage)])
  
  with(as.list(c(parms, X)), {
    
    for (i in 1:length(X)) {
      if (X[i]< 0) {
        X[i]<- 0
      }}
    
    screeningreturn <- parms$mu*FP
    vaccination <- matrix(vac1(t, age.vac1,vr), nrow=4, ncol=lastdate, byrow=F)
    #vaccination <- matrix(vac1(t), nrow=4, ncol=lastdate, byrow=T)*U
    infection <- matrix(contactpattern(t,rt, aver_Rt,npi_d,npi), nrow=4, ncol=lastdate, byrow=T)* as.vector(((U*(C))%*%(A/(U+A+V+E))))
    incubation <- parms$theta*(E)
    testingrate <- matrix(tau2_1(t, tau2,tr), nrow=4, ncol=lastdate, byrow=T)*Se*A
    screeningrate <- parms$tau1*(1-Sp)*(U)
    progression1 <-parms$sigma*(A)
    progression2 <-parms$sigma*(TP)
    recovery1 <-parms$rho*(1-parms$delta)*(S)
    recovery2 <-parms$rho*(TP)
    recovery3 <-parms$rho*(A)
    #recovery4 <-rho*as.matrix(V)
    dead <-parms$delta *(S)*0.2
    
    dUdt <- -infection - vaccination 
    dVdt <- vaccination 
    dEdt <- infection - incubation
    dAdt <- incubation - testingrate - progression1 - recovery3
    dTPdt<- testingrate #-recovery2 - progression2
    dFPdt<- screeningrate - screeningreturn
    dSdt <- progression2 + progression1 - recovery1 - dead
    dRdt <- recovery1+recovery2+recovery3+vaccination
    dDdt <- dead
    
    dTP.Idt<- testingrate 
    
    
    SEIR <- rbind(dUdt, dVdt, dEdt, dAdt, dTPdt,dFPdt, dSdt, dRdt, dDdt, dTP.Idt )
    SEIR=as.data.frame(SEIR)
    
  })
}


sir1 <- function(t,X,parms){
  
  ncompartment=10
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
  TP.I =(X[(10*nage+1):(11*nage)])
  
  with(as.list(c(parms, X)), {
    
    
    for (i in 1:length(X)) {
      if (X[i]< 0) {
        X[i]<- 0
      }}
    
    screeningreturn <- parms$mu*FP
    vaccination <- matrix(vac1(t, age.vac1,vr), nrow=4, ncol=lastdate, byrow=F)
    #vaccination <- matrix(vac1(t), nrow=4, ncol=lastdate, byrow=T)*U
    infection <- matrix(contactpattern(t,rt, aver_Rt,npi_d,npi), nrow=4, ncol=lastdate, byrow=T)* as.vector(((U*(C))%*%(A/(U+A+V+E))))
    incubation <- parms$theta*(E)
    testingrate <- matrix(tau2_1(t, tau2,tr), nrow=4, ncol=lastdate, byrow=T)*Se*A
    screeningrate <- parms$tau1*(1-Sp)*(U)
    progression1 <-parms$sigma*(A)
    progression2 <-parms$sigma*(TP)
    recovery1 <-parms$rho*(1-parms$delta)*(S)
    recovery2 <-parms$rho*(TP)
    recovery3 <-parms$rho*(A)
    #recovery4 <-rho*as.matrix(V)
    dead <-parms$delta *(S)*0.2
    
    dUdt <- -infection - vaccination 
    dVdt <- vaccination 
    dEdt <- infection - incubation
    dAdt <- incubation - testingrate - progression1 - recovery3
    dTPdt<- testingrate -recovery2 - progression2
    dFPdt<- screeningrate - screeningreturn
    dSdt <- progression2 + progression1 - recovery1 - dead
    dRdt <- recovery1+recovery2+recovery3+vaccination
    dDdt <- dead
    
    dTP.Idt<- testingrate 
    
    
    SEIR <- rbind(dUdt, dVdt, dEdt, dAdt, dTPdt,dFPdt, dSdt, dRdt, dDdt, dTP.Idt )
    SEIR=as.data.frame(SEIR)
    
  })
}

result_list = data.frame()


ts.sir.timeseries <- function( rt,npi_d, npi, vr, tr){
  
  values <- list (mu = matrix(mu,nrow=4, ncol=lastdate ),
                  tau1= matrix(tau1,nrow=4, ncol=lastdate ),
                  theta= matrix(theta,nrow=4, ncol=lastdate ),
                  sigma= matrix(sigma,nrow=4, ncol=lastdate),
                  rho= matrix(rho,nrow=4, ncol=lastdate ),
                  delta= matrix(delta,nrow=4, ncol=lastdate),
                  age.vac1=age.vac1,
                  aver_Rt=aver_Rt,
                  tau2=tau2,
                  vr=vr,
                  rt= rt,
                  tr=tr,
                  
                  npi_d = npi_d,
                  npi=npi)
  
  
  
  ############################################ ODE simulation ###################################################################
  result=lsoda(
    y = pop.SI,               # Initial conditions for population
    times = t,                # Timepoints for evaluation
    func = sir,               # Function to evaluate
    parms = values            # Vector of parameters
    
  )
  
  result1<-result[,c(1:41)]
  
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
  result2$TP.I<-result2$TP.I1+result2$TP.I2+result2$TP.I3++result2$TP.I4
  
  result2$TP1.incidence <- c(100,diff(result2$TP.I1))
  result2$TP2.incidence <- c(100,diff(result2$TP.I2))
  result2$TP3.incidence <- c(500,diff(result2$TP.I3))
  result2$TP4.incidence <- c(300,diff(result2$TP.I4))
  result2$TP.incidence <- c(1000,diff(result2$TP.I))
  
  
  result2$S<-result2$S*0.015
  
  
  result2$D1.incidence <- c(0,diff(result2$D1))
  result2$D2.incidence <- c(0,diff(result2$D2))
  result2$D3.incidence <- c(500,diff(result2$D3))
  result2$D4.incidence <- c(1500,diff(result2$D4))
  result2$D.incidence <- c(2000,diff(result2$D))
  
  result2$D_cum<-cumsum(result2$D.incidence)
  
  result2$V1.incidence <- c(0,diff(result2$V1))
  result2$V2.incidence <- c(0,diff(result2$V2))
  result2$V3.incidence <- c(1000,diff(result2$V3))
  result2$V4.incidence <- c(4000,diff(result2$V4))
  result2$V.incidence <- c(5000,diff(result2$V))
  
  result2$V_cum<-cumsum(result2$V.incidence)
  
  ##csv save##
  result2$case <-  paste("RT", rt,"NPID",npi_d,"NPI1",npi,"VACCINE_",vr,"TEST_",tr)#column$case
  result2$RT_ <- paste(rt)
  result2$NPID <- paste(npi_d)
  result2$NPI <- paste(npi)
  result2$VACCINE_ <- paste(vr)
  result2$TEST_ <- paste(tr)
  # 
  result_list <-rbind(result_list, result2)
  
  return(result_list)
  
}

ts.sir1.timeseries <- function( rt,npi_d, npi, vr, tr){
  
  values <- list (mu = matrix(mu,nrow=4, ncol=lastdate ),
                  tau1= matrix(tau1,nrow=4, ncol=lastdate ),
                  theta= matrix(theta,nrow=4, ncol=lastdate ),
                  sigma= matrix(sigma,nrow=4, ncol=lastdate),
                  rho= matrix(rho,nrow=4, ncol=lastdate ),
                  delta= matrix(delta,nrow=4, ncol=lastdate),
                  age.vac1=age.vac1,
                  aver_Rt=aver_Rt,
                  tau2=tau2,
                  vr=vr,
                  rt= rt,
                  tr=tr,
                  
                  npi_d = npi_d,
                  npi=npi)
  
  
  
  ############################################ ODE simulation ###################################################################
  result=lsoda(
    y = pop.SI,               # Initial conditions for population
    times = t,                # Timepoints for evaluation
    func = sir1,               # Function to evaluate
    parms = values            # Vector of parameters
    
  )
  
  result1<-result[,c(1:41)]
  
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
  result2$TP.I<-result2$TP.I1+result2$TP.I2+result2$TP.I3++result2$TP.I4
  
  result2$TP1.incidence <- c(100,diff(result2$TP.I1))
  result2$TP2.incidence <- c(100,diff(result2$TP.I2))
  result2$TP3.incidence <- c(500,diff(result2$TP.I3))
  result2$TP4.incidence <- c(300,diff(result2$TP.I4))
  result2$TP.incidence <- c(1000,diff(result2$TP.I))
  
  
  result2$S<-result2$S*0.015
  
  
  result2$D1.incidence <- c(0,diff(result2$D1))
  result2$D2.incidence <- c(0,diff(result2$D2))
  result2$D3.incidence <- c(500,diff(result2$D3))
  result2$D4.incidence <- c(1500,diff(result2$D4))
  result2$D.incidence <- c(2000,diff(result2$D))
  
  result2$D_cum<-cumsum(result2$D.incidence)
  
  result2$V1.incidence <- c(0,diff(result2$V1))
  result2$V2.incidence <- c(0,diff(result2$V2))
  result2$V3.incidence <- c(1000,diff(result2$V3))
  result2$V4.incidence <- c(4000,diff(result2$V4))
  result2$V.incidence <- c(5000,diff(result2$V))
  
  result2$V_cum<-cumsum(result2$V.incidence)
  
  ##csv save##
  result2$case <-  paste("RT", rt,"NPID",npi_d,"NPI1",npi,"VACCINE_",vr,"TEST_",tr)#column$case
  result2$RT_ <- paste(rt)
  result2$NPID <- paste(npi_d)
  result2$NPI <- paste(npi)
  result2$VACCINE_ <- paste(vr)
  result2$TEST_ <- paste(tr)
  # 
  result_list <-rbind(result_list, result2)
  
  return(result_list)
  
  
}


#######step2########
## input param


############################### Rt, Testing, vaccination time functions ##########################################################
t <- seq(1, lastdate, by=1) 

vac1 <- function(t, age.vac1,vr) (t >= 1 & t < 96) * age.vac1 +
  (t >= 96 & t < 180) * age.vac1 *c(1.5,0.5,0.5,0.1) *vr +  ## by Dec 2021
  (t >= 180 & t < 360) * age.vac1*c(1.2,0.17,0.1,0.03) *vr + #0.25  ## by Jun 2022
  (t >= 360 & t < lastdate) * age.vac1*c(0,0,0,0) *vr ##0.1  ## by Dec 2022 #백신 다 맞았을거다 라는 가정


test<-df_kr1[c(1,5)]
test[is.na(test)] <- 20000 #가장 낮은값을 넣음(한국기준)
test<-subset(test, test$date >= "2021-07-01")
tau2<-test$new_tests/250000 ## assumed total asymptomatic population#백신안맞고 감염되지않은, (한국기준)

#tr=1 #by test rate: 1,0.8,1.2
tau2_1 <- function(t, tau2,tr) (t >= 1 & t < 96) * mean(tau2) + #10월 5일 기준 96 
  (t >= 96 & t < lastdate) * mean(tau2)*tr

##transimissibility
#rt=3.5# by delta : 1, 1.05, 1.1
contact=2.3
transimissibility=aver_Rt*1/14*(1/contact)


contactpattern <- function(t,rt, aver_Rt,npi_d,npi) (t >= 1 & t < 96) * aver_Rt*1/14*(1/contact) + ##Rt1.1
  (t >= 96 & t  < npi_d) * aver_Rt*1/14*(1/contact)*1.7*npi+ ##Rt1.3
  (t >= npi_d & t  < lastdate) * aver_Rt*1/14*(1/contact)*1.7*rt

########################################Initial conditions ######################################################################
npop <- 51822000
f <- c(0.17, 0.27, 0.33, 0.23) # four age classes, with 0-19; 20-39; 40-59; 60+ in Korea 2021
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

V=c(724, 1257656, 1360168, 7290718) #by end of June cumulative vaccine uptake ( 724 1257656 1360168 7290718)
E=c(1000, 2000, 2000, 2000)
A=c(1000, 2000, 2000, 2000)
TP=c(100, 1000, 1000, 1000) ##July fit c(100, 400, 300)
S=c(300, 800, 800, 800)
R=c(10000,2000, 80000, 150000)
D=c(0, 500, 1000, 1000)
FP=c(0, 0, 0, 0)
TP.I=c(100, 1000, 1000, 1000)
U = N-E-A-V-TP-S-R-D

pop.SI <- c(U = N-E-A-V-TP-S-R-D, V=V, E=E, A=A ,TP=TP, FP=FP,  S=S, R=R, D=D, TP.I=TP.I)



#################step3#####################



result3 <-mapply(ts.sir.timeseries, rt =rep(c(3.5, 4, 4.5), each=9,times=1), npi_d=300, npi =rep(c( 0.85, 0.85,1), each=3,times=3), vr=rep(c(0.8, 1, 1.2), each=1,times=9), tr= 1, SIMPLIFY=FALSE)
ll = rbindlist(result3, use.names=TRUE)
write.csv(ll ,file ="../../../../03_result/SEIR/211019/result_TP_final_1115.csv")

result3 <-mapply(ts.sir1.timeseries, rt =rep(c(3.5, 4, 4.5), each=9,times=1), npi_d=300, npi =rep(c( 0.85, 0.85,1), each=3,times=3), vr=rep(c(0.8, 1, 1.2), each=1,times=9), tr= 1, SIMPLIFY=FALSE)
ll = rbindlist(result3, use.names=TRUE)
write.csv(ll ,file ="../../../../03_result/SEIR/211019/result_SD_final_1115.csv")



result_final <-read.csv('../../../../03_result/SEIR/211019/result_TP_final_1115.csv')
rt<- 3.5
npi_d<-300
npi<- 1
vr <- 1
tr <- 1
result3 <- result_final[result_final$case ==paste("RT", rt,"NPID",npi_d,"NPI1",npi,"VACCINE_",vr,"TEST_",tr),]
# 
rt<- 4
npi_d<-300
npi<- 1
vr <- 1
tr <- 1
result4 <- result_final[result_final$case ==paste("RT", rt,"NPID",npi_d,"NPI1",npi,"VACCINE_",vr,"TEST_",tr),]
npi_d<-300
npi<- 1
vr <- 1
tr <- 1
result5 <- result_final[result_final$case ==paste("RT", rt,"NPID",npi_d,"NPI1",npi,"VACCINE_",vr,"TEST_",tr),]

# 
# 
calender <- seq(ymd("2021-08-19"), ymd("2023-12-30"), by="day")

q1 <- merge(incidence1, result3,by = "time", all.y = TRUE, no.dups = TRUE)
#q1 <- cbind(result.lhs.ci, q1)
for (i in 50:lastdate) {
  q1$dates[i] = calender[i - 50 + 1]
}

#result4<-result2
q2 <- merge(incidence1, result4,by = "time", all.y = TRUE, no.dups = TRUE )
#q2 <- cbind(result.lhs.ci, q2)
for (i in 50:lastdate) {
  q2$dates[i] = calender[i - 50 + 1]
}

#result5<-result2
q3 <-  merge(incidence1,result5,by = "time",all.y = TRUE, no.dups = TRUE)
#q3 <- cbind(result.lhs.ci, q3)
for (i in 50:lastdate) {
  q3$dates[i] = calender[i - 50 + 1]
}
# 
# 
# 
# ##Rt 0.39, 0.41, 0.43

plot1<-ggplot(q1) +
  #geom_rect(data=q1, aes(xmin=dates[c(96)], xmax=dates[c(126)], ymin=0, ymax=6000), fill="light grey", alpha=0.02)+
  geom_point(aes(x = dates, y = I), size = 1.5, color = "darkblue") +
  geom_line(aes(x = dates, y = I), size = 1, color = "darkblue") +
  geom_line(aes(x = dates, y = TP.incidence), size = 2, color="red", group = 1) +
  #  geom_ribbon(data = q1, aes(x = dates, ymin = low, ymax = high), fill="red", alpha = .1) +
  geom_line(data=q2, aes(x = dates, y = TP.incidence), size = 2, color = "#FF9999") +
  #  geom_ribbon(data = q2, aes(x = dates, ymin = low, ymax = high), fill="red", alpha = .05) +

  geom_line(data=q3, aes(x = dates, y = TP.incidence), size = 2, color = "#CC0033") +
  #  geom_ribbon(data = q3, aes(x = dates, ymin = low, ymax = high), fill="red", alpha = .15) +

  geom_vline(aes(xintercept = as.numeric(dates[c(96)])), linetype = 2, color = 'red') +
  scale_y_continuous(breaks = seq(0,20000,5000), minor_breaks = seq(0,20000,50000), limits = c(0, 20000), expand = c(0, 0)) + ## scale_y_continuous(label=comma)
  ylab('Incidence') +
  xlab('Date') +
  ggtitle('') +
  scale_x_date( limits = as.Date(c("2021-07-01", "2023-12-30")), breaks = as.Date( c( "2021-07-01",  "2021-09-01",  "2021-11-01",
                                                                                      "2022-01-01", "2022-03-01",   "2022-05-01",
                                                                                      "2022-07-01",  "2022-09-01",  "2022-11-01",
                                                                                      "2023-01-01",  "2023-03-01",  "2023-05-01",
                                                                                      "2023-07-01",  "2023-09-01",  "2023-11-01"  ) ),
                labels = c ("07",  "09",  "11", "01",  "03", "05", "07", "09","11", "01",  "03", "05", "07", "09","11"),expand = c(0, 0)) +
  #theme_classic()+
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 0.1, color = "black"),
        axis.text.y = element_text(color = "black"),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line.x = element_line(color = "black"),
        axis.line.y = element_line(color = "black"),
        panel.background = element_rect(fill = "white"),
        # legend.key=element_blank(),
        # legend.title = element_blank(),
        text = element_text(size=12),
        panel.spacing = unit(2, "lines"))+
  scale_color_manual(values = c("#FF9999","red", "#CC0033"))

plot1


