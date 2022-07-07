
source("../Codes/Step0_SEIR_MC_Rt.R")

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
    vaccination <- matrix(vac1(t, vac, vaccinating), nrow=4, ncol=lastdate, byrow=T)*U
    infection <- matrix(contactpattern(t, beta, Rt, npi1, npi2), nrow=4, ncol=lastdate, byrow=T)* as.vector(((U*(C))%*%(A/(U+A+V+E))))
    incubation <- theta*(E)
    testingrate <- matrix(tau2_1(t, tau2, testing), nrow=4, ncol=lastdate, byrow=T)*Se*A
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
         SEIR[,141],SEIR[,142], SEIR[,143], SEIR[,144], SEIR[,145],
         SEIR[,146], SEIR[,147], SEIR[,148],SEIR[,149],SEIR[,150],
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
t <- seq(1, lastdate, by=1) 

test<-df_kr1[c(1,5)]
test[is.na(test)] <- 20000
#test$new_tests.p <- test$new_tests/100000
test<-subset(test, test$date >= "2021-07-01")
tau2<-test$new_tests/100000

vac<-cum_vac$new_vac/50000000
cum_vac<-df_kr1[c(1,6)]
cum_vac$new_vac <- c(382029,diff(cum_vac$people_vaccinated))
cum_vac<-subset(cum_vac, cum_vac$date >= "2021-07-01")


vac1 <- function(t, vac, vaccinating) (t >= 1 & t < 87) * mean(vac)*0.7+
  (t >= 87 & t < lastdate) * mean(vac)*0.7*vaccinating

tau2_1 <- function(t, tau2, testing) (t >= 1 & t < 87) * mean(tau2)*0.4 +
  (t >= 87 & t < lastdate) * mean(tau2)*0.42*testing


contactpattern <- function(t, beta, Rt, npi1, npi2) (t >= 1 & t < 82) * mean(beta)*0.36 +
  (t >= 82 & t < npi1) * mean(beta)*0.5*Rt*npi2 + 
  (t >=npi1 & t < lastdate) * mean(beta)*0.45*Rt


#time <- 0
npop <- 51822000
f <- c(0.17, 0.27, 0.33, 0.23)# four age classes, with 0-19, 20-39, 40-59, 60+  in Korea 2021
#f <- c(0.16*11/18, 0.16*7/18, 0.59, 0.24)# three age classes, with 16% age 0-19; 59% age 20-59; 24% age >60 in Korea 2021  
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
mu = matrix(mu,nrow=4, ncol=lastdate)
tau1= matrix(tau1,nrow=4, ncol=lastdate )
theta= matrix(theta,nrow=4, ncol=lastdate )
sigma= matrix(sigma,nrow=4, ncol=lastdate)
rho= matrix(rho,nrow=4, ncol=lastdate )
delta= matrix(delta,nrow=4, ncol=lastdate )

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
f <- c(0.17, 0.27, 0.33, 0.23) # three age classes, with 16% age 0-19; 59% age 20-59; 24% age >60 in Korea 2021
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
result2$TP2.incidence <- c(200,diff(result2$TP2))
result2$TP3.incidence <- c(400,diff(result2$TP3))
result2$TP4.incidence <- c(300,diff(result2$TP4))
result2$TP.incidence <- c(1000,diff(result2$TP))

result2$S<-result2$S*0.01
result2$D<-cumsum(result2$D*0.01)

 result2<-round(result2)
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
  f <- c(0.17, 0.27, 0.33, 0.23) # three age classes, with 16% age 0-19; 59% age 20-59; 24% age >60 in Korea 2021
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
                  delta= matrix(delta,nrow=4, ncol=lastdate),
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
  result2$TP2.incidence <- c(200,diff(result2$TP2))
  result2$TP3.incidence <- c(400,diff(result2$TP3))
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

