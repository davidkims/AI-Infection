
set.seed(156829)
require("sfsmisc")
require("deSolve")
require(data.table)

source("../Codes/Step0_SEIR_MC_Rt.R")

niter = 5
##################################
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
    vaccination <- vac2*U
    infection <- matrix(contactpattern(t), nrow=4, ncol=lastdate, byrow=T)* as.vector(((U*(C))%*%(A/(U+A+V+E))))
    incubation <- theta*(E)
    testingrate <- tau2_2*Se*A
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

lastdate<-184

test<-df_kr1[c(1,5)]
test[is.na(test)] <- 20000
#test$new_tests.p <- test$new_tests/100000
test<-subset(test, test$date >= "2021-07-01")

tau2<-test$new_tests/100000
t <- seq(1, lastdate, by=1) 
# tau2_1 <- function(t) (t >= 1 & t < 49) * mean(tau2)*0.36 +
#   (t >= 49 & t < lastdate) * mean(tau2)*0.36*1
#plot(1:100,tau2_1(1:lastdate))
tau2_1<-mean(tau2)*0.3
  
cum_vac<-df_kr1[c(1,6)]
cum_vac$new_vac <- c(382029,diff(cum_vac$people_vaccinated))
cum_vac<-subset(cum_vac, cum_vac$date >= "2021-07-01")

vac<-cum_vac$new_vac/50000000
# t <- seq(1, lastdate, by=1) 
# vac1 <- function(t) (t >= 1 & t < 49) * mean(vac)*0.7+
#   (t >= 49 & t < lastdate) * mean(vac)*0.7*1
#plot(1:100,vac1(1:lastdate))
vac1<-mean(vac)*0.7

t <- seq(1, lastdate, by=1) 
contactpattern <- function(t) (t >= 1 & t < 49) * mean(beta)*0.28 +
  (t >= 49 & t < 184) * mean(beta)*0.28*1+
  (t >=184 & t < lastdate) * mean(beta)*0.28
#plot(1:100,contactpattern(1:lastdate))

#time <- 0
npop <- 51822000
f <- c(0.17, 0.27, 0.33, 0.23)# four age classes, with 0-19, 20-39, 40-59, 60+  in Korea 2021
nage <-  length(f)
N <-  npop*f      # number in each age class

Se<- 0.95   #sensitivity
Sp<-0.998

vac2=c(vac1, vac1, vac1, vac1)
tau2_2=c(tau2_1,tau2_1,tau2_1,tau2_1)
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
                vac2= matrix(vac2,nrow=4, ncol=lastdate ),
                tau2_2= matrix(tau2_2,nrow=4, ncol=lastdate )
                )

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
result2$TP2.incidence <- c(200,diff(result2$TP2))
result2$TP3.incidence <- c(400,diff(result2$TP3))
result2$TP4.incidence <- c(300,diff(result2$TP4))
result2$TP.incidence <- c(1000,diff(result2$TP))

result2<-round(result2)

zstate = list()
i = 1
for (iter in 1:niter){ 
  time = 0
  vstate = pop.SI/100
  
  K = length(vstate)  # number of compartments
  J = 48               # number of possible state changes
  lambda = matrix(0,nrow=J,ncol=length(vstate))
              #U1,U2,U3,U4,V1,V2,V3,V4,E1,E2,E3,E4,A1,A2,A3,A4,TP1,TP2,TP3,TP4,FP1,FP2,FP3,FP4,S1,S2,S3,S4,R1,R2,R3,R4,D1,D2,D3,D4
  lambda[1,] = c(1,0,0,0, #U
                 0,0,0,0,#V
                 0,0,0,0, #E
                 0,0,0,0, #A
                 0,0,0,0, #TP
                 -1,0,0,0, #FP
                 0,0,0,0, #S
                 0,0,0,0, #R
                 0,0,0,0) #D # screeningreturn <- mu*FP 
  
  lambda[2,] = c(0,1,0,0, #U
                 0,0,0,0,#V
                 0,0,0,0, #E
                 0,0,0,0, #A
                 0,0,0,0, #TP
                 0,-1,0,0, #FP
                 0,0,0,0, #S
                 0,0,0,0, #R
                 0,0,0,0) #D
  
  lambda[3,] = c(0,0,1,0, #U
                 0,0,0,0,#V
                 0,0,0,0, #E
                 0,0,0,0, #A
                 0,0,0,0, #TP
                 0,0,-1,0, #FP
                 0,0,0,0, #S
                 0,0,0,0, #R
                 0,0,0,0) #D
  #U3
  lambda[4,] = c(0,0,0,1, #U
                 0,0,0,0,#V
                 0,0,0,0, #E
                 0,0,0,0, #A
                 0,0,0,0, #TP
                 0,0,0,-1, #FP
                 0,0,0,0, #S
                 0,0,0,0, #R
                 0,0,0,0) #D #U4
  
  lambda[5,] = c(-1,0,0,0, #U
                 1,0,0,0,#V
                 0,0,0,0, #E
                 0,0,0,0, #A
                 0,0,0,0, #TP
                 0,0,0,0, #FP
                 0,0,0,0, #S
                 0,0,0,0, #R
                 0,0,0,0 ) ##  vaccination <- vac*U
  
  lambda[6,] = c(0,-1,0,0, #U
                 0,1,0,0,#V
                 0,0,0,0, #E
                 0,0,0,0, #A
                 0,0,0,0, #TP
                 0,0,0,0, #FP
                 0,0,0,0, #S
                 0,0,0,0, #R
                 0,0,0,0)
  
  lambda[7,] = c(0,0,-1,0, #U
                 0,0,1,0,#V
                 0,0,0,0, #E
                 0,0,0,0, #A
                 0,0,0,0, #TP
                 0,0,0,0, #FP
                 0,0,0,0, #S
                 0,0,0,0, #R
                 0,0,0,0)
  
  lambda[8,] = c(0,0,0,-1, #U
                 0,0,0,1,#V
                 0,0,0,0, #E
                 0,0,0,0, #A
                 0,0,0,0, #TP
                 0,0,0,0, #FP
                 0,0,0,0, #S
                 0,0,0,0, #R
                 0,0,0,0)
  
  lambda[9,] = c(-1,0,0,0, #U
                 0,0,0,0,#V
                 1,0,0,0, #E
                 0,0,0,0, #A
                 0,0,0,0, #TP
                 0,0,0,0, #FP
                 0,0,0,0, #S
                 0,0,0,0, #R
                 0,0,0,0 )  ## infection <- beta*U*A/(U+A+V+E)
  
  lambda[10,] = c(0,-1,0,0, #U
                  0,0,0,0,#V
                  0,1,0,0, #E
                  0,0,0,0, #A
                  0,0,0,0, #TP
                  0,0,0,0, #FP
                  0,0,0,0, #S
                  0,0,0,0, #R
                  0,0,0,0 )
  
  lambda[11,] = c(0,0,-1,0, #U
                  0,0,0,0,#V
                  0,0,1,0, #E
                  0,0,0,0, #A
                  0,0,0,0, #TP
                  0,0,0,0, #FP
                  0,0,0,0, #S
                  0,0,0,0, #R
                  0,0,0,0 )
  
  lambda[12,] = c(0,0,0,-1, #U
                  0,0,0,0,#V
                  0,0,0,1, #E
                  0,0,0,0, #A
                  0,0,0,0, #TP
                  0,0,0,0, #FP
                  0,0,0,0, #S
                  0,0,0,0, #R
                  0,0,0,0 )
  
  lambda[13,] = c(0,0,0,0, #U
                  0,0,0,0,#V
                  -1,0,0,0, #E
                  1,0,0,0, #A
                  0,0,0,0, #TP
                  0,0,0,0, #FP
                  0,0,0,0, #S
                  0,0,0,0, #R
                  0,0,0,0)
  
  lambda[14,] = c(0,0,0,0, #U
                  0,0,0,0,#V
                  0,-1,0,0, #E
                  0,1,0,0, #A
                  0,0,0,0, #TP
                  0,0,0,0, #FP
                  0,0,0,0, #S
                  0,0,0,0, #R
                  0,0,0,0)
  
  lambda[15,] = c(0,0,0,0, #U
                  0,0,0,0,#V
                  0,0,-1,0, #E
                  0,0,1,0, #A
                  0,0,0,0, #TP
                  0,0,0,0, #FP
                  0,0,0,0, #S
                  0,0,0,0, #R
                  0,0,0,0)
  
  lambda[16,] = c(0,0,0,0, #U
                  0,0,0,0,#V
                  0,0,0,-1, #E
                  0,0,0,1, #A
                  0,0,0,0, #TP
                  0,0,0,0, #FP
                  0,0,0,0, #S
                  0,0,0,0, #R
                  0,0,0,0) ## incubation <- theta*E
  
  lambda[17,] = c(0,0,0,0, #U
                  0,0,0,0,#V
                  0,0,0,0, #E
                  -1,0,0,0, #A
                  1,0,0,0, #TP
                  0,0,0,0, #FP
                  0,0,0,0, #S
                  0,0,0,0, #R
                  0,0,0,0)  ## testingrate <- tau*Se*A
  
  lambda[18,] = c(0,0,0,0, #U
                  0,0,0,0,#V
                  0,0,0,0, #E
                  0,-1,0,0, #A
                  0,1,0,0, #TP
                  0,0,0,0, #FP
                  0,0,0,0, #S
                  0,0,0,0, #R
                  0,0,0,0)
  
  lambda[19,] = c(0,0,0,0, #U
                  0,0,0,0,#V
                  0,0,0,0, #E
                  0,0,-1,0, #A
                  0,0,1,0, #TP
                  0,0,0,0, #FP
                  0,0,0,0, #S
                  0,0,0,0, #R
                  0,0,0,0)
  
  lambda[20,] = c(0,0,0,0, #U
                  0,0,0,0,#V
                  0,0,0,0, #E
                  0,0,0,-1, #A
                  0,0,0,1, #TP
                  0,0,0,0, #FP
                  0,0,0,0, #S
                  0,0,0,0, #R
                  0,0,0,0)
  
  lambda[21,] = c(-1,0,0,0, #U
                  0,0,0,0,#V
                  0,0,0,0, #E
                  0,0,0,0, #A
                  0,0,0,0, #TP
                  1,0,0,0, #FP
                  0,0,0,0, #S
                  0,0,0,0, #R
                  0,0,0,0)  ## screeningrate <- tau*(1-Sp)*U
  
  lambda[22,] = c(0,-1,0,0, #U
                  0,0,0,0,#V
                  0,0,0,0, #E
                  0,0,0,0, #A
                  0,0,0,0, #TP
                  0,1,0,0, #FP
                  0,0,0,0, #S
                  0,0,0,0, #R
                  0,0,0,0)
  
  lambda[23,] = c(0,0,-1,0, #U
                  0,0,0,0,#V
                  0,0,0,0, #E
                  0,0,0,0, #A
                  0,0,0,0, #TP
                  0,0,1,0, #FP
                  0,0,0,0, #S
                  0,0,0,0, #R
                  0,0,0,0)
  
  lambda[24,] = c(0,0,0,-1, #U
                  0,0,0,0,#V
                  0,0,0,0, #E
                  0,0,0,0, #A
                  0,0,0,0, #TP
                  0,0,0,1, #FP
                  0,0,0,0, #S
                  0,0,0,0, #R
                  0,0,0,0)
  
  lambda[25,] = c(0,0,0,0, #U
                  0,0,0,0,#V
                  0,0,0,0, #E
                  -1,0,0,0, #A
                  0,0,0,0, #TP
                  0,0,0,0, #FP
                  1,0,0,0, #S
                  0,0,0,0, #R
                  0,0,0,0)  ## progression1 <-sigma*A
  
  lambda[26,] = c(0,0,0,0, #U
                  0,0,0,0,#V
                  0,0,0,0, #E
                  0,-1,0,0, #A
                  0,0,0,0, #TP
                  0,0,0,0, #FP
                  0,1,0,0, #S
                  0,0,0,0, #R
                  0,0,0,0)
  
  lambda[27,] = c(0,0,0,0, #U
                  0,0,0,0,#V
                  0,0,0,0, #E
                  0,0,-1,0, #A
                  0,0,0,0, #TP
                  0,0,0,0, #FP
                  0,0,1,0, #S
                  0,0,0,0, #R
                  0,0,0,0)
  
  lambda[28,] = c(0,0,0,0, #U
                  0,0,0,0,#V
                  0,0,0,0, #E
                  0,0,0,-1, #A
                  0,0,0,0, #TP
                  0,0,0,0, #FP
                  0,0,0,1, #S
                  0,0,0,0, #R
                  0,0,0,0)
  
  lambda[29,] = c(0,0,0,0, #U
                  0,0,0,0,#V
                  0,0,0,0, #E
                  0,0,0,0, #A
                  -1,0,0,0, #TP
                  0,0,0,0, #FP
                  1,0,0,0, #S
                  0,0,0,0, #R
                  0,0,0,0)  ## progression2 <-sigma*TP
  
  lambda[30,] = c(0,0,0,0, #U
                  0,0,0,0,#V
                  0,0,0,0, #E
                  0,0,0,0, #A
                  0,-1,0,0, #TP
                  0,0,0,0, #FP
                  0,1,0,0, #S
                  0,0,0,0, #R
                  0,0,0,0)
  
  lambda[31,] = c(0,0,0,0, #U
                  0,0,0,0,#V
                  0,0,0,0, #E
                  0,0,0,0, #A
                  0,0,-1,0, #TP
                  0,0,0,0, #FP
                  0,0,1,0, #S
                  0,0,0,0, #R
                  0,0,0,0)
  
  lambda[32,] = c(0,0,0,0, #U
                  0,0,0,0,#V
                  0,0,0,0, #E
                  0,0,0,0, #A
                  0,0,0,-1, #TP
                  0,0,0,0, #FP
                  0,0,0,1, #S
                  0,0,0,0, #R
                  0,0,0,0)
  
  lambda[33,] = c(0,0,0,0, #U
                  0,0,0,0,#V
                  0,0,0,0, #E
                  0,0,0,0, #A
                  0,0,0,0, #TP
                  0,0,0,0, #FP
                  -1,0,0,0, #S
                  1,0,0,0, #R
                  0,0,0,0)  ## recovery1 <-rho*S
  
  lambda[34,] = c(0,0,0,0, #U
                  0,0,0,0,#V
                  0,0,0,0, #E
                  0,0,0,0, #A
                  0,0,0,0, #TP
                  0,0,0,0, #FP
                  0,-1,0,0, #S
                  0,1,0,0, #R
                  0,0,0,0)
  
  lambda[35,] = c(0,0,0,0, #U
                  0,0,0,0,#V
                  0,0,0,0, #E
                  0,0,0,0, #A
                  0,0,0,0, #TP
                  0,0,0,0, #FP
                  0,0,-1,0, #S
                  0,0,1,0, #R
                  0,0,0,0)
  
  lambda[36,] = c(0,0,0,0, #U
                  0,0,0,0,#V
                  0,0,0,0, #E
                  0,0,0,0, #A
                  0,0,0,0, #TP
                  0,0,0,0, #FP
                  0,0,0,-1, #S
                  0,0,0,1, #R
                  0,0,0,0)
  
  lambda[37,]= c(0,0,0,0, #U
                 0,0,0,0,#V
                 0,0,0,0, #E
                 0,0,0,0, #A
                 -1,0,0,0, #TP
                 0,0,0,0, #FP
                 0,0,0,0, #S
                 1,0,0,0, #R
                 0,0,0,0)  ## recovery2 <-rho*TP
  
  lambda[38,]= c(0,0,0,0, #U
                 0,0,0,0,#V
                 0,0,0,0, #E
                 0,0,0,0, #A
                 0,-1,0,0, #TP
                 0,0,0,0, #FP
                 0,0,0,0, #S
                 0,1,0,0, #R
                 0,0,0,0)
  
  lambda[39,]= c(0,0,0,0, #U
                 0,0,0,0,#V
                 0,0,0,0, #E
                 0,0,0,0, #A
                 0,0,-1,0, #TP
                 0,0,0,0, #FP
                 0,0,0,0, #S
                 0,0,1,0, #R
                 0,0,0,0)
  
  lambda[40,]= c(0,0,0,0, #U
                 0,0,0,0,#V
                 0,0,0,0, #E
                 0,0,0,0, #A
                 0,0,0,-1, #TP
                 0,0,0,0, #FP
                 0,0,0,0, #S
                 0,0,0,1, #R
                 0,0,0,0)
  
  lambda[41,]= c(0,0,0,0, #U
                 0,0,0,0,#V
                 0,0,0,0, #E
                 -1,0,0,0, #A
                 0,0,0,0, #TP
                 0,0,0,0, #FP
                 0,0,0,0, #S
                 1,0,0,0, #R
                 0,0,0,0)  ## recovery3 <-rho*A
  
  lambda[42,]= c(0,0,0,0, #U
                 0,0,0,0,#V
                 0,0,0,0, #E
                 0,-1,0,0, #A
                 0,0,0,0, #TP
                 0,0,0,0, #FP
                 0,0,0,0, #S
                 0,1,0,0, #R
                 0,0,0,0)
  
  lambda[43,]= c(0,0,0,0, #U
                 0,0,0,0,#V
                 0,0,0,0, #E
                 0,0,-1,0, #A
                 0,0,0,0, #TP
                 0,0,0,0, #FP
                 0,0,0,0, #S
                 0,0,1,0, #R
                 0,0,0,0)
  
  lambda[44,]= c(0,0,0,0, #U
                 0,0,0,0,#V
                 0,0,0,0, #E
                 0,0,0,-1, #A
                 0,0,0,0, #TP
                 0,0,0,0, #FP
                 0,0,0,0, #S
                 0,0,0,1, #R
                 0,0,0,0)
  
  lambda[45,]= c(0,0,0,0, #U
                 0,0,0,0,#V
                 0,0,0,0, #E
                 0,0,0,0, #A
                 0,0,0,0, #TP
                 0,0,0,0, #FP
                 -1,0,0,0, #S
                 0,0,0,0, #R
                 1,0,0,0) ## delta *S
  
  lambda[46,]= c(0,0,0,0, #U
                 0,0,0,0,#V
                 0,0,0,0, #E
                 0,0,0,0, #A
                 0,0,0,0, #TP
                 0,0,0,0, #FP
                 0,-1,0,0, #S
                 0,0,0,0, #R
                 0,1,0,0)
  
  lambda[47,]= c(0,0,0,0, #U
                 0,0,0,0,#V
                 0,0,0,0, #E
                 0,0,0,0, #A
                 0,0,0,0, #TP
                 0,0,0,0, #FP
                 0,0,-1,0, #S
                 0,0,0,0, #R
                 0,0,1,0)
  
  lambda[48,]= c(0,0,0,0, #U
                 0,0,0,0,#V
                 0,0,0,0, #E
                 0,0,0,0, #A
                 0,0,0,0, #TP
                 0,0,0,0, #FP
                 0,0,0,-1, #S
                 0,0,0,0, #R
                 0,0,0,1)
  
  
  
  while(vstate[17]>0&vstate[18]>0&vstate[19]>0&vstate[20]>0){
    zstate[[i]] = c(vstate,time,iter)
    U1 = vstate[1]
    U2 = vstate[2]
    U3 = vstate[3]
    U4 = vstate[4]
    V1 = vstate[5]
    V2 = vstate[6]
    V3 = vstate[7]
    V4 = vstate[8]
    E1 = vstate[9]
    E2 = vstate[10]
    E3 = vstate[11]
    E4 = vstate[12]
    A1 = vstate[13]
    A2 = vstate[14]
    A3 = vstate[15]
    A4 = vstate[16]
    TP1 = vstate[17]
    TP2 = vstate[18]
    TP3 = vstate[19]
    TP4 = vstate[20]
    FP1 = vstate[21]
    FP2 = vstate[22]
    FP3 = vstate[23]
    FP4 = vstate[24]
    S1 = vstate[25]
    S2 = vstate[26]
    S3 = vstate[27]
    S4 = vstate[28]
    R1 = vstate[29]
    R2 = vstate[30]
    R3 = vstate[31]
    R4 = vstate[32]
    D1 = vstate[33]
    D2 = vstate[34]
    D3 = vstate[35]
    D4 = vstate[36]
    

    #vec_p = c(beta*U*A/N, theta*E, tau*A)
    vec_p = c(mu[1]*FP[1], mu[2]*FP[2], mu[3]*FP[3], mu[4]*FP[4],
              vac2[1]*U[1], vac2[2]*U[2], vac2[3]*U[3], vac2[4]*U[4],
              contactpattern(1)* as.vector(((U*(C))%*%(A/(U+A+V+E))))[1],
              contactpattern(2)* as.vector(((U*(C))%*%(A/(U+A+V+E))))[2],
              contactpattern(3)* as.vector(((U*(C))%*%(A/(U+A+V+E))))[3],
              contactpattern(4)* as.vector(((U*(C))%*%(A/(U+A+V+E))))[4],
              theta[1]*E[1], theta[2]*E[2],theta[3]*E[3],theta[4]*E[4],
              tau2_2[1]*Se*A[1], tau2_2[2]*Se*A[2], tau2_2[3]*Se*A[3], tau2_2[4]*Se*A[4],
              tau1[1]*(1-Sp)*U[1], tau1[2]*(1-Sp)*U[2], tau1[3]*(1-Sp)*U[3], tau1[4]*(1-Sp)*U[4],
              sigma[1]*A[1], sigma[2]*A[2], sigma[3]*A[3], sigma[4]*A[4],
              sigma[1]*TP[1], sigma[2]*TP[2], sigma[3]*TP[3], sigma[4]*TP[4],
              rho[1]*(1-delta[1])*S[1], rho[2]*(1-delta[2])*S[2], rho[3]*(1-delta[3])*S[3], rho[4]*(1-delta[4])*S[4],
              rho[1]*TP[1], rho[2]*TP[2], rho[3]*TP[3], rho[4]*TP[4],
              rho[1]*A[1], rho[2]*A[2], rho[3]*A[3], rho[4]*A[4],
              delta[1]*S[1], delta[2]*S[2], delta[3]*S[3], delta[4]*S[4]    )
    
    #delta_t = 1/sum(vec_p)
    delta_t = 0.01
    
    vec_l = rpois(length(vec_p),vec_p*delta_t)
    vstate = vstate + vec_l%*%lambda
    
    vstate[vstate<0] = 0
    i = i+1
    time = time + delta_t
  }
  cat("Doing realisation:",iter,niter," ",time,vstate,"\n")
}

##################################################################################
##################################################################################
# the sapply functions here extract the elements of the zstate list into vectors
##################################################################################
par(mfrow=c(2,2))
vU1 = sapply(zstate, "[[", 1)
vU2 = sapply(zstate, "[[", 2)
vU3 = sapply(zstate, "[[", 3)
vU4 = sapply(zstate, "[[", 4)

vV1 = sapply(zstate, "[[", 5)
vV2 = sapply(zstate, "[[", 6)
vV3 = sapply(zstate, "[[", 7)
vV4 = sapply(zstate, "[[", 8)

vE1 = sapply(zstate, "[[", 9)
vE2 = sapply(zstate, "[[", 10)
vE3 = sapply(zstate, "[[", 11)
vE4 = sapply(zstate, "[[", 12)

vA1 = sapply(zstate, "[[", 13)
vA2 = sapply(zstate, "[[", 14)
vA3 = sapply(zstate, "[[", 15)
vA4 = sapply(zstate, "[[", 16)

vTP1 = sapply(zstate, "[[", 17)
vTP2 = sapply(zstate, "[[", 18)
vTP3 = sapply(zstate, "[[", 19)
vTP4 = sapply(zstate, "[[", 20)

vFP1 = sapply(zstate, "[[", 21)
vFP2 = sapply(zstate, "[[", 22)
vFP3 = sapply(zstate, "[[", 23)
vFP4 = sapply(zstate, "[[", 24)

vS1 = sapply(zstate, "[[", 25)
vS2 = sapply(zstate, "[[", 26)
vS3 = sapply(zstate, "[[", 27)
vS4 = sapply(zstate, "[[", 28)

vR1 = sapply(zstate, "[[", 29)
vR2 = sapply(zstate, "[[", 30)
vR3 = sapply(zstate, "[[", 31)
vR4 = sapply(zstate, "[[", 32)

vD1 = sapply(zstate, "[[", 33)
vD2 = sapply(zstate, "[[", 34)
vD3 = sapply(zstate, "[[", 35)
vD4 = sapply(zstate, "[[", 36)

vtime = sapply(zstate, "[[", 37)
viter = sapply(zstate, "[[", 38)

for (i in 1: length(zstate)){
zstate[[i]][39]<-zstate[[i]][17]+zstate[[i]][18]+zstate[[i]][19]+zstate[[i]][20]
}

vTP = sapply(zstate, "[[", 39)

vTP.incidence <- c(40,diff(vTP))
##################################################################################
##################################################################################
#mult.fig(2)
# x100 in order to match the real population number 
for (iter in 1:niter){
  l = which(viter==iter)
  if (iter==1){
    plot(vtime[l],vTP[l]*100, col="grey", xlab="Time, in Days",ylab="TP",main=paste("N=",N*100," and TP0=",vTP[1]*100,sep=""),ylim=c(0,150*max(vTP)),xlim=c(0,max(vtime)),type="l")
  }else{
    lines(vtime[l],vTP[l]*100, type="l",  col="grey")
  }
}
lines(ts.sir$time,ts.sir$TP*100,col=2,lwd=2)

##################################################################################
##################################################################################


# result3 <- data.frame(time = 1: length(t) )
# q1 <- merge(incidence1, result3, by="time", all.y = TRUE, no.dups = TRUE)
# calender <- seq(ymd("2021-08-19"), ymd("2022-06-30"), by="day")
# for (i in 50:lastdate){
#   q1$dates[i] = calender[i-50+1]
# }
# 
# plot1<-ggplot(q1) + 
#   geom_point(aes(x = dates, y = I), size = 1.5, color = "darkblue", fill = "white") +
#   geom_line(aes(x = dates, y = I), size = 1, color = "darkblue", fill = "white") +
#   #geom_line(aes(x = dates, y = TP.incidence), size = 2, color="red", group = 1) + 
#   #geom_ribbon(data = q1, aes(x = dates, ymin = TP.incidence-300, ymax = TP.incidence+300), alpha = .1) +
#   
#   # geom_line(data=q2, aes(x = dates, y = TP.incidence), size = 2, color = "#FF9999", fill = "white") +
#   # geom_ribbon(data = q2, aes(x = dates, ymin = TP.incidence-300, ymax = TP.incidence+300), alpha = .1) +
#   # 
#   # geom_line(data=q3, aes(x = dates, y = TP.incidence), size = 2, color = "#CC0033", fill = "white") +
#   # geom_ribbon(data = q3, aes(x = dates, ymin = TP.incidence-300, ymax = TP.incidence+300), alpha = .1) +
#   
#   geom_vline(aes(xintercept = as.numeric(dates[c(50)])), linetype = 2, color = 'red') +
#   scale_y_continuous(breaks = seq(0,5000,1000), minor_breaks = seq(0,5000,1000), limits = c(0, 5000)) +
#   ylab('Incidence') +
#   xlab('Date') +
#   ggtitle('') +
#   scale_x_date(limits = as.Date(c("2021-07-01","2021-12-30")), 
#                breaks = as.Date(c("2021-07-01","2021-08-01","2021-09-01", "2021-10-01", "2021-11-01", "2021-12-01")),
#                labels=c ("07-01", "08-01","09-01", "10-01", "11-01", "12-01"))+
#   theme(axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 0.1, color = "black"),
#         axis.text.y = element_text(color = "black"),
#         panel.grid.major.x = element_blank(),
#         panel.grid.major.y = element_blank(),
#         panel.grid.minor = element_blank(),
#         axis.line.x = element_line(color = "black"),
#         axis.line.y = element_line(color = "black"),
#         panel.background = element_rect(fill = "white"),
#         legend.key=element_blank(),
#         legend.title = element_blank(),
#         text = element_text(size=12),
#         panel.spacing = unit(2, "lines"))
# 
# plot1
# mcmc.plot <- plot1
# for (i in 1: length(vTP.incidence)) {
#   TP.mcmc <- data.frame(dates = q1$dates, TP.incidence=vTP.incidence[[i]]*100)
#   mcmc.plot <- mcmc.plot + 
#     geom_line(data=TP.mcmc, aes(x = dates, y = vTP.incidence), size = 0.5, color = "grey", fill = "white")
# }
# mcmc.plot



