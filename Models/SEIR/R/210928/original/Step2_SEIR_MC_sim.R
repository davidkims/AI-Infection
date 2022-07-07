## input param
lastdate<-184

test<-df_kr1[c(1,5)]
test[is.na(test)] <- 20000
#test$new_tests.p <- test$new_tests/100000
test<-subset(test, test$date >= "2021-07-01")
#plot(test$date, test$new_tests)
test$test_rate<-test$new_tests/100000
plot(test$date, test$test_rate)

cum_vac<-df_kr1[c(1,6)]
cum_vac$new_vac <- c(382029,diff(cum_vac$people_vaccinated))
cum_vac<-subset(cum_vac, cum_vac$date >= "2021-07-01")

vac<-cum_vac$new_vac/50000000
cum_vac$vac_rate<-cum_vac$new_vac/50000000

## time series Rt (Beta), testing rate, vaccination rate
tau2<-test$new_tests/100000
t <- seq(1, lastdate, by=1) 
tau2_1 <- function(t) (t >= 1 & t < 80) * mean(tau2)*0.35 +
  (t >= 80 & t < lastdate) * mean(tau2)*0.35*1
#plot(1:100,tau2_1(1:lastdate))
mean_vac<-mean(vac)
vac1 <- function(t) (t >= 1 & t < 87) * mean(vac)*0.7+
  (t >= 87 & t < lastdate) * mean(vac)*0.7*1
#plot(1:100,vac1(1:lastdate))

contactpattern <- function(t) (t >= 1 & t < 80) * mean(beta)*0.35 +
  (t >= 80 & t < 87) * mean(beta)*0.45*1+
  (t >= 87 & t < 147) * mean(beta)*0.42*1+ 
  (t >=147 & t < lastdate) * mean(beta)*0.42*1

# contactpattern_age <- function(t) (t >= 1 & t < 80) * mean(beta)*0.35 +
#   (t >= 80 & t < 87) * mean(beta)*0.45*1+
#   (t >= 87 & t < 147) * mean(beta)*0.42*0.85+ 
#   (t >=147 & t < lastdate) * mean(beta)*0.42*1


# age_contact<-matrix(contactpattern(t), nrow=4, ncol=lastdate, byrow=T)
# age_contact[3,1:184]<-contactpattern_age(t)
# age_contact<-function(t) (t >= 1 & t < lastdate) * mean(beta)*0.35

#plot(1:100,contactpattern(1:lastdate))
# vac_trend<- ggplot( cats, aes(  x = t, y = mean_vac) ) + 
#   geom_point() + 
#   geom_smooth(method="lm")



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

