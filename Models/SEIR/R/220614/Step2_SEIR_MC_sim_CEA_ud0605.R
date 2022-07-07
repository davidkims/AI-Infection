## input param


############################### Rt, Testing, vaccination time functions ##########################################################
t <- seq(1, lastdate, by=1) 

vr=0.5 #by vaccination rate: 0.8,1
vac1 <- function(t, U, age.vac1) {
  res <- (t >= 1 & t < 142) * age.vac1  +
    (t >= 142 & t < 180) * age.vac1*c(3,0.5,0.5,0.1) *vr+  ## by Dec 2021
    (t >= 180 & t < 360) * age.vac1*c(2,0.3,0.2,0.01) *vr + #0.25  ## by Jun 2022
    (t >= 360 & t < 540) *age.vac1*c(0,0,0,0) *vr ##0.1  ## by Dec 2022
  return(pmin(res, U))
}


test<-df_kr[c("date","new_tests")]
test1<-subset(test, test$date >= "2022-01-01")
aver_test<-mean(test1$new_tests, na.rm=TRUE)
tau2<-aver_test/5000000 ## assumed total asymptomatic population

tr=1 #by test rate: 1,0.8,1.2
tau2_1 <- function(t, tau2) (t >= 1 & t < 142) * mean(tau2) +
  (t >= 142 & t < lastdate) * mean(tau2)*tr

##transimissibility
contact=1
transimissibility=aver_Rt*1/14*(1/contact)
npi=0.5 #0.5 0.6 0.7
npi_d=150+90 ##NPI by Feb, Apr, and Jun 2022 210 240 270


contactpattern <- function(t, aver_Rt) (t >= 1 & t < 200) * aver_Rt*1/14*(1/contact)*2 + ##Rt1.1
  (t >= 200 & t < 300) * aver_Rt*1/14*(1/contact)*7 +
  (t >= 300 & t  < lastdate) * aver_Rt*1/14*(1/contact)*1 ##Transmiss ##Transmissibility increase 2/2.2/2.4


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
sigma = c(0.14, 0.2, 0.15, 0.12)
#sigma = c(0.00025, 0.0058, 0.029, 0.19) # progression per day
rho = c(1/14, 1/14, 1/14, 1/14) 
rho_h = c(1/14, 1/14, 1/14, 1/14) # recovery per day
delta = c(0.00004, 0.0001, 0.00098, 0.047)  # case fatality among symptomatic patients per day
#vac = c(0, 0.03, 0.031)*1 ## July 0.03
eff<-1
adm_ud=c(0.00002,0.00152,0.00739,0.07959)*c(1, eff, eff, eff)
adm_noud=c(0.00002,0.00152,0.00739,0.07959)*c(1, 1, 1, 1) ## 0.7 as mol and 0.16 as pax
wan=c(0,0.0001,0.0001,0.0001)

# adm_ud=c(0.00025,0.0058,0.029,0.19)*0.2*c(1, eff, eff, eff)
# adm_noud=c(0.00025,0.0058,0.029,0.19)*0.2*c(1, eff, eff, eff) ## 0.7 as mol and 0.16 as pax
ud_p=c(0.3,0.3,0.3,0.3)

values <- list (mu = matrix(mu,nrow=4, ncol=lastdate ),
                tau1= matrix(tau1,nrow=4, ncol=lastdate ),
                theta= matrix(theta,nrow=4, ncol=lastdate ),
                sigma= matrix(sigma,nrow=4, ncol=lastdate),
                rho= matrix(rho,nrow=4, ncol=lastdate ),
                rho_h= matrix(rho_h,nrow=4, ncol=lastdate ),
                delta= matrix(delta,nrow=4, ncol=lastdate ),
                adm_ud=matrix(adm_ud,nrow=4, ncol=lastdate ),
                adm_noud=matrix(adm_noud,nrow=4, ncol=lastdate ),
                ud_p=matrix(ud_p,nrow=4, ncol=lastdate ),
                wan=matrix(wan,nrow=4, ncol=lastdate ),
                age.vac1=age.vac1,
                aver_Rt=aver_Rt,
                tau2=tau2  
                )

C = matrix(0,nrow=nage,ncol=nage)
C[1,1] = 2.3  # number contacts per day kids make with kids
C[1,2] = 0.6  
C[1,3] = 0.5  # number contacts per day kids make with adults (all kids have an adult in the home)
C[1,4] = 0.1 

C[2,1] = 0.7  # number contacts per day adults make with kids (not all adults have kids)
C[2,2] = 1.9
C[2,3] = 1.0
C[2,4] = 0.1

C[3,1] = 0.9  # number contacts per day adults make with kids (not all adults have kids)
C[3,2] = 1.2 
C[3,3] = 1.5  # number contacts per day adults make with adults
C[3,4] = 0.2 

C[4,1] = 0.3  # number contacts per day adults make with kids (not all adults have kids)
C[4,2] = 0.4 
C[4,3] = 0.5  # number contacts per day adults make with adults
C[4,4] = 0.4 


#install.packages("rootSolve")
#library(rootSolve)


##https://cran.r-project.org/web/packages/LaplacesDemon/index.html

V=c(724, 1257656, 1360168, 7290718) #by end of June cumulative vaccine uptake ( 724 1257656 1360168 7290718)
E=c(1000, 2000, 2000, 2000)
A=c(1000, 2000, 2000, 2000)
TP_ud=c(100, 200, 400, 300)*ud_p
TP_noud=c(100, 200, 400, 300)*(1-ud_p)
S_ud=c(0, 200, 400, 300)*ud_p
S_noud=c(200, 200, 400, 300)*(1-ud_p)
H_ud=c(2, 4, 8, 6)*ud_p
H_noud=c(2, 4, 8, 6)*(1-ud_p)
R_ud=c(50000,50000, 50000, 100000)*ud_p 
R_noud=c(50000,50000, 50000, 100000)*(1-ud_p) ## total confirmed cases 450,000 
D_ud=c(0, 0, 500, 1000)*ud_p
D_noud=c(0, 0, 500, 1000)*(1-ud_p)

TP_ud.I=c(100, 1000, 1000, 1000)*ud_p 
TP_noud.I=c(100, 1000, 1000, 1000)*(1-ud_p)
S.I_ud=c(100, 1000, 1000, 1000)*ud_p
S.I_noud=c(100, 1000, 1000, 1000)*(1-ud_p)
H.I_ud=c(2, 20, 20, 20)*ud_p
H.I_noud=c(2, 20, 20, 20)*(1-ud_p)
U = N-E-A-V-R_ud-R_noud

pop.SI <- c(U = N-E-A-V-R_ud-R_noud, V=V, E=E, A=A ,TP_ud=TP_ud, TP_noud=TP_noud, S_ud=S_ud, S_noud=S_noud, H_ud=H_ud, H_noud=H_noud,  R_ud=R_ud, R_noud=R_noud, 
            D_ud=D_ud, D_noud=D_noud,  TP_ud.I=TP_ud.I, TP_noud.I=TP_noud.I, S.I_ud=S.I_ud, S.I_noud=S.I_noud, H.I_ud=H.I_ud, H.I_noud=H.I_noud)


############################################ ODE simulation ###################################################################
result=lsoda(
  y = pop.SI,               # Initial conditions for population
  times = t,                # Timepoints for evaluation
  func = sir_CEA_ud2,           # Function to evaluate
  parms = values            # Vector of parameters
  
)

result1<-result[,c(1:81)]
result2<-as.data.frame(result1)

result2$U<-result2$U1+result2$U2+result2$U3+result2$U4
result2$V<-result2$V1+result2$V2+result2$V3+result2$V4
result2$E<-result2$E1+result2$E2+result2$E3+result2$E4
result2$A<-result2$A1+result2$A2+result2$A3+result2$A4
result2$TP_ud<-result2$TP_ud1+result2$TP_ud2+result2$TP_ud3+result2$TP_ud4
result2$TP_noud<-result2$TP_noud1+result2$TP_noud2+result2$TP_noud3+result2$TP_noud4

result2$TP<-result2$TP_ud+result2$TP_noud

result2$S_ud<-result2$S_ud1+result2$S_ud2+result2$S_ud3+result2$S_ud4
result2$S_noud<-result2$S_noud1+result2$S_noud2+result2$S_noud3+result2$S_noud4

result2$S<-result2$S_ud+result2$S_noud

result2$H_ud<-result2$H_ud1+result2$H_ud2+result2$H_ud3+result2$H_ud4
result2$H_noud<-result2$H_noud1+result2$H_noud2+result2$H_noud3+result2$H_noud4

result2$H<-result2$H_ud+result2$H_noud

result2$R_ud<-result2$R_ud1+result2$R_ud2+result2$R_ud3+result2$R_ud4
result2$R_noud<-result2$R_noud1+result2$R_noud2+result2$R_noud3+result2$R_noud4

result2$D_ud<-result2$D_ud1+result2$D_ud2+result2$D_ud3+result2$D_ud4
result2$D_noud<-result2$D_noud1+result2$D_noud2+result2$D_noud3+result2$D_noud4

result2$TP_ud.I<-result2$TP_ud.I1+result2$TP_ud.I2+result2$TP_ud.I3+result2$TP_ud.I4
result2$TP_noud.I<-result2$TP_noud.I1+result2$TP_noud.I2+result2$TP_noud.I3+result2$TP_noud.I4

result2$TP.I<-result2$TP_ud.I+result2$TP_noud.I

result2$S.I_ud<-result2$S.I_ud1+result2$S.I_ud2+result2$S.I_ud3+result2$S.I_ud4

#result2$S.I_ud<-result2$S.I_ud2+result2$S.I_ud3+result2$S.I_ud4
result2$S.I_noud<-result2$S.I_noud1+result2$S.I_noud2+result2$S.I_noud3+result2$S.I_noud4

result2$S.I<-result2$S.I_ud+result2$S.I_noud

result2$H.I_ud<-result2$H.I_ud1+result2$H.I_ud2+result2$H.I_ud3+result2$H.I_ud4
result2$H.I_noud<-result2$H.I_noud1+result2$H.I_noud2+result2$H.I_noud3+result2$H.I_noud4
result2$H.I<-result2$H.I_ud+result2$H.I_noud

result2$TP_ud1.incidence <- c(100*0.3,diff(result2$TP_ud.I1))
result2$TP_ud2.incidence <- c(200*0.3,diff(result2$TP_ud.I2))
result2$TP_ud3.incidence <- c(400*0.3,diff(result2$TP_ud.I3))
result2$TP_ud4.incidence <- c(300*0.3,diff(result2$TP_ud.I4))
result2$TP_ud.incidence <- c(1000*0.3,diff(result2$TP_ud.I))

result2$TP_noud1.incidence <- c(100*0.7,diff(result2$TP_noud.I1))
result2$TP_noud2.incidence <- c(200*0.7,diff(result2$TP_noud.I2))
result2$TP_noud3.incidence <- c(400*0.7,diff(result2$TP_noud.I3))
result2$TP_noud4.incidence <- c(300*0.7,diff(result2$TP_noud.I4))
result2$TP_noud.incidence <- c(1000*0.7,diff(result2$TP_noud.I))

result2$TP.incidence<-result2$TP_ud.incidence+result2$TP_noud.incidence

result2$S_ud1.incidence <- c(100*0.3,diff(result2$S.I_ud1))
result2$S_ud2.incidence <- c(200*0.3,diff(result2$S.I_ud2))
result2$S_ud3.incidence <- c(400*0.3,diff(result2$S.I_ud3))
result2$S_ud4.incidence <- c(300*0.3,diff(result2$S.I_ud4))
result2$S_ud.incidence <- c(1000*0.3,diff(result2$S.I_ud))

result2$S_noud1.incidence <- c(100*0.7,diff(result2$S.I_noud1))
result2$S_noud2.incidence <- c(200*0.7,diff(result2$S.I_noud2))
result2$S_noud3.incidence <- c(400*0.7,diff(result2$S.I_noud3))
result2$S_noud4.incidence <- c(300*0.7,diff(result2$S.I_noud4))
result2$S_noud.incidence <- c(1000*0.7,diff(result2$S.I_noud))

result2$S.incidence<-result2$S_ud.incidence +result2$S_noud.incidence 
result2$S_adult.incidence<-result2$S_ud2.incidence+result2$S_ud3.incidence+result2$S_ud4.incidence+result2$S_noud2.incidence+result2$S_noud3.incidence+result2$S_noud4.incidence

result2$H_ud1.incidence <- c(2*0.3,diff(result2$H.I_ud1))
result2$H_ud2.incidence <- c(4*0.3,diff(result2$H.I_ud2))
result2$H_ud3.incidence <- c(8*0.3,diff(result2$H.I_ud3))
result2$H_ud4.incidence <- c(6*0.3,diff(result2$H.I_ud4))
result2$H_ud.incidence <- c(20*0.3,diff(result2$H.I_ud))

result2$H_noud1.incidence <- c(2*0.7,diff(result2$H.I_noud1))
result2$H_noud2.incidence <- c(4*0.7,diff(result2$H.I_noud2))
result2$H_noud3.incidence <- c(8*0.7,diff(result2$H.I_noud3))
result2$H_noud4.incidence <- c(6*0.7,diff(result2$H.I_noud4))
result2$H_noud.incidence <- c(20*0.7,diff(result2$H.I_noud))

result2$H.incidence<-result2$H_ud.incidence+result2$H_noud.incidence

result2$V1.incidence <- c(724,diff(result2$V1))
result2$V2.incidence <- c(1257656,diff(result2$V2))
result2$V3.incidence <- c(1360168,diff(result2$V3))
result2$V4.incidence <- c(7290718,diff(result2$V4))
result2$V.incidence <- c(9909266,diff(result2$V))

result2$V1_cum<-cumsum(result2$V1.incidence)
result2$V2_cum<-cumsum(result2$V2.incidence)
result2$V3_cum<-cumsum(result2$V3.incidence)
result2$V4_cum<-cumsum(result2$V4.incidence)

result2$V_cum<-cumsum(result2$V.incidence)

# plot(q1$date, q1$V_cum/51000000)
# plot(cum_vac$date, cum_vac$people_vaccinated/51000000)

result2<-round(result2)


