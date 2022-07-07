## input param

############################### Rt, Testing, vaccination time functions ##########################################################
t <- seq(1, lastdate, by=1) 

cum_vac<-df_au1[c(1,6,7)]
cum_vac<-subset(cum_vac, cum_vac$date >= "2021-07-01")
cum_vac$new_vac <- c(20000,diff(cum_vac$people_vaccinated))
cum_vac[cum_vac<0] <- 0
npop <- 25788217
vac<-cum_vac$new_vac/npop #country total pop

vr=1 #by vaccination rate: 0.8,1
vac1 <- function(t) (t >= 1 & t < 96) * mean(vac)*2+
  (t >= 96 & t < 180) * mean(vac)*1.2+
  (t >= 180 & t < 360) * mean(vac)*vr*0.1+
  (t >= 360 & t < 540) * mean(vac)*0.1
# 
# vac1_lhs <- function(t, vac_k) (t >= 1 & t < 96) * mean(vac)*vac_k+
#   (t >= 96 & t < 180) * mean(vac)*vac_k+
#   (t >= 180 & t < 360) * mean(vac)*0.5*vac_k+
#   (t >= 360 & t < 540) * mean(vac)*0.2*vac_k
# 
# vac1_SA <- function(t, vaccinating) (t >= 1 & t < 96) * mean(vac) +
#   (t >= 96 & t < 180) * mean(vac) *vaccinating +
#   (t >= 180 & t < 360) * mean(vac)*0.5 *vaccinating +
#   (t >= 360 & t < 540) * mean(vac)*0.2 *vaccinating

test<-df_au1[c(1,5)]
test[is.na(test)] <- 10000
test<-subset(test, test$date >= "2021-07-01")
tau2<-test$new_tests/250000 ## assumed total asymptomatic population

tr=1 #by test rate: 1,0.8,1.2
tau2_1 <- function(t, tau2) (t >= 1 & t < 96) * mean(tau2) +
                      (t >= 96 & t < lastdate) * mean(tau2)*tr

# tau2_lhs <- function(t,tau2, tau_k) (t >= 1 & t < 96) * mean(tau2)*tau_k +
#   (t >= 96 & t < lastdate) * mean(tau2)*tr*tau_k
# 
# tau2_SA <- function(t, tau2, testing) (t >= 1 & t < 96) * mean(tau2) +
#   (t >= 96 & t < lastdate) * mean(tau2)*testing
# 

##transimissibility
rt=1 # by delta : 1, 1.05, 1.1
contact=1
transimissibility=aver_Rt*1/14*(1/contact)
npi=1 #0.85
npi_d=300 ##NPI by April 2022 by Feb 2022 by Jun 2022

contactpattern <- function(t, aver_Rt) (t >= 1 & t <30) * aver_Rt*1/14*(1/contact)*0.5 + 
  (t >= 30 & t < 96) * aver_Rt*1/14*(1/contact)*1.1+   ##Rt1.1
  (t >= 96 & t  < npi_d) * aver_Rt*1/14*(1/contact)*rt*npi+ ##Rt1.3
  (t >= npi_d & t  < lastdate) * aver_Rt*1/14*(1/contact)*rt*2

# contactpattern_lhs <- function(t,aver_Rt, beta_k) (t >= 1 & t < 96) * aver_Rt*1/14*(1/contact)*beta_k + ##Rt1.1
#   (t >= 96 & t  < npi_d) * aver_Rt*1/14*(1/contact)*rt*beta_k*npi+ ##Rt1.3
#   (t >= npi_d & t  < lastdate) * aver_Rt*1/14*(1/contact)*rt*beta_k ##Rt2 with 1.2: Rt1.8 with 1.1: Rt1.65 with 1
# 
# 
# contactpattern_SA <- function(t, aver_Rt, Rt, npi1, npi2) (t >= 1 & t < 96) * aver_Rt*1/14*(1/contact) + ##Rt1.1
#   (t >= 96 & t  < npi1) * aver_Rt*1/14*(1/contact)*Rt*npi2+ ##Rt1.3
#   (t >= npi1 & t  < lastdate) * aver_Rt*1/14*(1/contact)*Rt ##Rt2 with 1.2: Rt1.8 with 1.1: Rt1.65 with 1

########################################Initial conditions ######################################################################
npop <- 25788217
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

values <- list (mu = matrix(mu,nrow=4, ncol=lastdate ),
                tau1= matrix(tau1,nrow=4, ncol=lastdate ),
                theta= matrix(theta,nrow=4, ncol=lastdate ),
                sigma= matrix(sigma,nrow=4, ncol=lastdate),
                rho= matrix(rho,nrow=4, ncol=lastdate ),
                delta= matrix(delta,nrow=4, ncol=lastdate ),
                #age.vac1=age.vac1,
                aver_Rt=aver_Rt,
                tau2=tau2               )

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
C[4,4] = 2 

V=c(724, 1257656, 1360168, 7290718)*0.5 #by end of June cumulative vaccine uptake ( 724 1257656 1360168 7290718)
E=c(1000, 2000, 2000, 1000)*0.1
A=c(1000, 2000, 2000, 1000)*0.1
TP=c(100, 1000, 1000, 1000)*0.1 ##July fit c(100, 400, 300)
S=c(300, 800, 800, 800)
R=c(10000,2000, 80000, 150000)
D=c(0, 500, 1000, 1000)
FP=c(0, 0, 0, 0)
TP.I=c(100, 1000, 1000, 1000)
U = N-E-A-V-TP-S-R-D

pop.SI <- c(U = N-E-A-V-TP-S-R-D, V=V, E=E, A=A ,TP=TP, FP=FP,  S=S, R=R, D=D, TP.I=TP.I)

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

result2$TP1.incidence <- c(100*0.1,diff(result2$TP.I1))
result2$TP2.incidence <- c(100*0.1,diff(result2$TP.I2))
result2$TP3.incidence <- c(500*0.1,diff(result2$TP.I3))
result2$TP4.incidence <- c(300*0.1,diff(result2$TP.I4))
result2$TP.incidence <- c(1000*0.1,diff(result2$TP.I))


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

result2<-round(result2)
# plot(q1$date, q1$V_cum/51000000)
# plot(cum_vac$date, cum_vac$people_vaccinated/51000000)


