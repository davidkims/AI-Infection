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

setwd("D://My Drive//2021_Korea SIR//Data")

lastdate<-545 ## by 2023

######################################Incidence data#################################################################
df_new<-read.csv("owid-covid-data_0509.csv")

df<-df_new
df1<-df[c("date","location","new_cases","new_deaths", "new_tests", "people_vaccinated","people_fully_vaccinated")]
colnames(df1)[2]<-"state"
colnames(df1)[3]<-"cases"
colnames(df1)[4]<-"deaths"
df1$date =as.Date(df1$date, format="%Y-%m-%d")

df_kr<-df1%>% filter(state=="South Korea")
df_kr1<-df_kr[df_kr$date>="2022-01-01" & df_kr$date <"2022-04-30",]

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
# head(Covid2021$incidence)
# Covid2021$si_distr
# head(Covid2021$si_data)


# plot(as.incidence(Covid2021$incidence$I, dates = Covid2021$incidence$dates))
# 
 incidence<-as.data.frame(Covid2021$incidence)
 incidence$dates =as.Date(incidence$dates, format="%Y-%m-%d")
 incidence1<-subset(incidence, dates >= "2022-01-01")
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

# min(Rt)
# max(Rt)
# summary(Rt$average_Rt)
# 
# plot(df_kr$date, df_kr$new_tests)

######################################vaccination & variants data#################################################################
read_csv("vaxx_1123_1.csv")->vac_prop

colnames(vac_prop) <-  c("time",    "age",    "dose",    "date",    "pfizer",    "JJ",    "moderna",    "AZ",    "AZPF")
vac_prop$age1 = ifelse(grepl("17", vac_prop$age), "1",
                       ifelse( grepl("18-19", vac_prop$age),"1",
                               ifelse(grepl("20-29", vac_prop$age),"2",
                               ifelse(grepl("30-39", vac_prop$age), "2",
                                      ifelse(grepl("40-49", vac_prop$age), "3",
                                             ifelse( grepl("50-59", vac_prop$age),"3",
                                                     ifelse(  grepl("60-64", vac_prop$age), "4",
                                                              ifelse(grepl("65-69", vac_prop$age), "4",
                                                                     ifelse( grepl("70-74", vac_prop$age), "4",
                                                                             ifelse(grepl("75-79", vac_prop$age), "4",
                                                                                    ifelse(grepl("80", vac_prop$age), "4",
                                                                                           NA))))))))))) ##case_when



vac_prop$date =as.Date(vac_prop$date , format="%m/%d/%Y")
popsize<-c(8907071, 13796133, 16695543, 11870436) ##0-19; 20-39; 40-59; 60+


vac_prop <- vac_prop %>% dplyr::select("time",    "age1",    "dose",    "date",    "pfizer",    "JJ",    "moderna",    "AZ",    "AZPF") %>%
        mutate(mRNA = pfizer + moderna) %>%
        mutate(week = strftime(date, "%V")) 

vac_prop <- vac_prop %>%
        dplyr::select(week, date, age1, dose, mRNA, AZ, JJ, AZPF) %>%
        mutate(dose = case_when (dose == 1 ~ c("dose1"),dose == 2 ~ c("dose2"), dose == 3 ~ c("dose3")))

variants<-read_excel("variants_1123_3.xlsx") ##old data

variants1 <- variants[c(4, 8, 16)]
colnames(variants1) <- c("age",  "date", "virus")
# freq(variants1$virus)
# summary(variants1$date)

vac_prop$date<-as.Date(vac_prop$date)
variants1$date<-as.Date(variants1$date)

variants2<-variants1 %>% filter(date > "2021-03-05" & date < "2021-11-20") %>%
        #filter(!is.na(virus)) %>% filter(virus!=c("분석불가")) %>% 
        mutate(virus=case_when(virus==c("-") ~ c("Wild_type"),virus == NA ~ c("Wild_type"),
                               virus==c("alpha") ~ c("alpha"),
                               virus==c("delta") ~ c("delta"),
                               TRUE ~ c("etc"))) 

a<-data.frame(unclass(table(variants2$date,variants2$virus)))
a$Wild_type.true<-round(a$Wild_type*0.3) #70% are missing testing data (to be confirmed)
a$total<-rowSums(a[, c(1:4)])
#a$total<-colSums(a)
a$Delta.p<-round(a$delta/a$total,2)

aa <- cbind(rownames(a), data.frame(a, row.names=NULL))
names(aa)[1]<-"date"
aa$date<-as.Date(aa$date)
aaa<-aa %>% dplyr ::select(date, Delta.p)  
aaa$week= strftime(aaa$date, "%V")

aaa1<-aaa%>% 
        group_by(week)  %>%
        summarize(Delta.wp=max(Delta.p))

vac_prop1<-merge(vac_prop, aaa1, by=c("week"), all.x=TRUE)

ve_AZ1=0.5; ve_AZ2=0.77; ve_AZ3=0.8; ve_AZ1_delta=0.3; ve_AZ2_delta =0.66; ve_AZ3_delta=0.7
ve_JJ = 0.5; ve_JJ_delta = 0.3; 

ve_mRNA1 = 0.5; ve_mRNA2=0.95; ve_mRNA3=0.98
ve_mRNA1_delta=0.35; ve_mRNA2_delta=0.88; ve_mRNA3_delta=0.9

ve_mRNA_v <- c(ve_mRNA1, ve_mRNA2, ve_mRNA3)
ve_mRNA_delta_v<- c(ve_mRNA1_delta, ve_mRNA2_delta, ve_mRNA3_delta)

ve_AZ_v <- c(ve_AZ1, ve_AZ2, ve_AZ3)
ve_AZ_delta_v<- c(ve_AZ1_delta, ve_AZ2_delta, ve_AZ3_delta)

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
vac_prop6<- round(spread(vac_prop5, key=week, value=tot_result))
vac_prop6 <- vac_prop6[,-1 ] 


##vac uptake from July-Oct first week
vac_prop7<-subset(vac_prop6[, c(18:38)])

vac_prop8<-vac_prop7/7  ## divide by 7 in order to convert to daily vaccination number

vac_prop8<-as.data.frame(vac_prop8,use.names=FALSE)
vac_prop8<-as.matrix(vac_prop8)
colnames(vac_prop8)<- NULL

aa1<-as.vector(vac_prop8[1,1:21])
aa2<-as.vector(vac_prop8[2,1:21])
aa3<-as.vector(vac_prop8[3,1:21])
aa4<-as.vector(vac_prop8[4,1:21])

age.vac<-matrix(NA, nrow=4, ncol=280) #30*7

for(i in 0:29){
        age.vac[1,(i*7+1):(i*7+7)]<-rep(aa1[i+1], each =7)
        age.vac[2,(i*7+1):(i*7+7)]<-rep(aa2[i+1], each =7)
        age.vac[3,(i*7+1):(i*7+7)]<-rep(aa3[i+1], each =7)
        age.vac[4,(i*7+1):(i*7+7)]<-rep(aa4[i+1], each =7)
}

age.vac[is.na(age.vac)] <- 0

age.vac1<-matrix(NA, nrow=4, ncol=lastdate)
age.vac1[1,]<-round(mean(age.vac[1,1:200]))
age.vac1[2,]<-round(mean(age.vac[2,1:200]))
age.vac1[3,]<-round(mean(age.vac[3,1:200]))
age.vac1[4,]<-round(mean(age.vac[4,1:200]))


