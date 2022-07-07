
sir <- function(t,X,parms){
# 
#   for (i in 1:length(X)) {
#     if (X[i]< 0) {
#       X[i]<- 0
#     }}
  
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
    
    # if (t==1){
    #   print(testing)
    #   print(vaccinating)
    # }
    
    for (i in 1:length(X)) {
      if (X[i]< 0) {
        X[i]<- 0
      }}

      screeningreturn <- parms$mu*FP
      #vaccination <- matrix(vac1(t), nrow=4, ncol=lastdate, byrow=T)
      vaccination <- matrix(vac1(t), nrow=4, ncol=lastdate, byrow=T)*U
      infection <- matrix(contactpattern(t, aver_Rt), nrow=4, ncol=lastdate, byrow=T)* as.vector(((U*(C))%*%(A/(U+V+A+E))))
      incubation <- parms$theta*(E)
      testingrate <- matrix(tau2_1(t, tau2), nrow=4, ncol=lastdate, byrow=T)*Se*A
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
        dTPdt<- testingrate - recovery2 - progression2
        dFPdt<- screeningrate - screeningreturn
        dSdt <- progression2 + progression1 - recovery1 - dead
        dRdt <- recovery1+recovery2+recovery3
        dDdt <- dead
        
        dTP.Idt<- testingrate 

        SEIR <- rbind(dUdt, dVdt, dEdt, dAdt, dTPdt,dFPdt, dSdt, dRdt, dDdt, dTP.Idt )
        SEIR=as.data.frame(SEIR)
        
       })
}

sir.lhs <- function(t,X,parms){
  
  for (i in 1:length(X)) {
    if (X[i]< 0) {
      X[i] <- 0
    }}  
  
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
    screeningreturn <- parms$mu*FP
    vaccination <- matrix(vac1_lhs(t, vac_k), nrow=4, ncol=lastdate, byrow=T)*U
    infection <- matrix(contactpattern_lhs(t, aver_Rt, beta_k), nrow=4, ncol=lastdate, byrow=T)* as.vector(((U*(C))%*%(A/(U+V+A+E))))
    incubation <- parms$theta*(E)
    testingrate <- matrix(tau2_lhs(t, tau2, tau_k), nrow=4, ncol=lastdate, byrow=T)*Se*A
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
    dTPdt<- testingrate #- recovery2 - progression2
    dFPdt<- screeningrate - screeningreturn
    dSdt <- progression2 + progression1 - recovery1 - dead
    dRdt <- recovery1+recovery2+recovery3+vaccination
    dDdt <- dead
    
    dTP.Idt<- testingrate 
  
    SEIR <- rbind(dUdt, dVdt, dEdt, dAdt, dTPdt,dFPdt, dSdt, dRdt, dDdt, dTP.Idt )
    SEIR=as.data.frame(SEIR)
    
  })
}



sir.hm <- function(t,X,parms){
  
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
  TP.I =(X[(10*nage+1):(11*nage)])

  
  with(as.list(c(parms, X)), {
    
    # if (t==1){
    #  print(testing)
    #  print(vaccinating)
    # }
    
    screeningreturn <- mu*FP
    vaccination <- matrix(vac1_SA(t, vaccinating), nrow=4, ncol=lastdate, byrow=T)*U
    infection <- matrix(contactpattern_SA(t, aver_Rt,  Rt, npi1, npi2), nrow=4, ncol=lastdate, byrow=T)* as.vector(((U*(C))%*%(A/(U+A+V+E))))
    incubation <- theta*(E)
    testingrate <- matrix(tau2_SA(t, tau2, testing), nrow=4, ncol=lastdate, byrow=T)*Se*A
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
    dRdt <- recovery1+recovery2+recovery3+vaccination
    dDdt <- dead
    dTP.Idt<- testingrate 
    
    SEIR <- rbind(dUdt, dVdt, dEdt, dAdt, dTPdt,dFPdt, dSdt, dRdt, dDdt, dTP.Idt )
    SEIR=as.data.frame(SEIR)
    
    # for (i in 1:184){
    #   b[[i]] <- SEIR[,i]
    #   
    # }
    # b
    
  })
}

ts.sir.lhs <- function( vac_k, tau_k, beta_k){
  
  values <- list (mu = matrix(mu,nrow=4, ncol=lastdate ),
                  tau1= matrix(tau1,nrow=4, ncol=lastdate ),
                  theta= matrix(theta,nrow=4, ncol=lastdate ),
                  sigma= matrix(sigma,nrow=4, ncol=lastdate),
                  rho= matrix(rho,nrow=4, ncol=lastdate ),
                  delta= matrix(delta,nrow=4, ncol=lastdate ),
                  #age.vac1=age.vac1,
                  aver_Rt=aver_Rt,
                  tau2=tau2,
                  tau_k=tau_k,
                  vac_k=vac_k,
                  beta_k=beta_k)
  
  ##ODE simulation
  result=lsoda(
    y = pop.SI,               # Initial conditions for population
    times = t,                # Timepoints for evaluation
    func = sir.lhs,               # Function to evaluate
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
  
  result2<-round(result2)
  return(result2$TP.incidence)
}


ts.sir.hm <- function(Rt, npi1, npi2, vaccinating, testing){
  
  values <- list (mu = matrix(mu,nrow=4, ncol=lastdate ),
                  tau1= matrix(tau1,nrow=4, ncol=lastdate ),
                  theta= matrix(theta,nrow=4, ncol=lastdate ),
                  sigma= matrix(sigma,nrow=4, ncol=lastdate),
                  rho= matrix(rho,nrow=4, ncol=lastdate ),
                  delta= matrix(delta,nrow=4, ncol=lastdate ),
                  #age.vac1=age.vac1,
                  aver_Rt=aver_Rt,
                  tau2=tau2,
                  vaccinating=vaccinating,
                  testing=testing,
                  Rt=Rt,
                  npi1=npi1,
                  npi2=npi2)
  
  ##ODE simulation
  
  result=lsoda(
    y = pop.SI,               # Initial conditions for population
    times = t,                # Timepoints for evaluation
    func = sir.hm ,               # Function to evaluate
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

ts.sir.timeseries <- function(Rt, npi1, npi2, vaccinating, testing){
  
  values <- list (mu = matrix(mu,nrow=4, ncol=lastdate ),
                  tau1= matrix(tau1,nrow=4, ncol=lastdate ),
                  theta= matrix(theta,nrow=4, ncol=lastdate ),
                  sigma= matrix(sigma,nrow=4, ncol=lastdate),
                  rho= matrix(rho,nrow=4, ncol=lastdate ),
                  delta= matrix(delta,nrow=4, ncol=lastdate),
                  #age.vac1=age.vac1,
                  aver_Rt=aver_Rt,
                  tau2=tau2,
                  vaccinating=vaccinating,
                  testing=testing,
                  Rt=Rt,
                  npi1=npi1,
                  npi2=npi2)
  

  ##ODE simulation

  result=lsoda(
    y = pop.SI,               # Initial conditions for population
    times = t,                # Timepoints for evaluation
    func = sir.hm,               # Function to evaluate
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
  
  result2<-round(result2)
  result2$TP.incidence <- c(1000,diff(result2$TP))
  #a<-colSums(result2)
  #return(a)
  return(result2)
  
}

SEIR_smplot <- function(result3, result4, result5, result6, result7, result8, result9, result10, result11) {
  #result3<-result2
  calender <- seq(ymd("2021-08-19"), ymd("2022-12-30"), by = "day")
  
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
  
  q4<-merge(incidence1, result6,by = "time",all.y = TRUE,   no.dups = TRUE  )
  #q4 <- cbind(result.lhs.ci, q4)
  for (i in 50:lastdate) {
    q4$dates[i] = calender[i - 50 + 1]
  }
  
  #result4<-result2
  q5 <- merge(incidence1,  result7,  by = "time", all.y = TRUE, no.dups = TRUE)
  #q5 <- cbind(result.lhs.ci, q5)
  for (i in 50:lastdate) {
    q5$dates[i] = calender[i - 50 + 1]
  }
  
  #result5<-result2
  q6 <- merge(incidence1, result8,by = "time",all.y = TRUE, no.dups = TRUE  )
  #q6 <- cbind(result.lhs.ci, q6)
  for (i in 50:lastdate) {
    q6$dates[i] = calender[i - 50 + 1]
  }
  
  q7<-merge(incidence1, result9,by = "time",all.y = TRUE,  no.dups = TRUE  )
  #q7 <- cbind(result.lhs.ci, q7)
  for (i in 50:lastdate) {
    q7$dates[i] = calender[i - 50 + 1]
  }
  
  #result4<-result2
  q8 <- merge(incidence1,  result10,  by = "time",all.y = TRUE, no.dups = TRUE)
  #q8 <- cbind(result.lhs.ci, q8)
  for (i in 50:lastdate) {
    q8$dates[i] = calender[i - 50 + 1]
  }
  
  #result5<-result2
  q9 <- merge(incidence1,  result11,by = "time",all.y = TRUE, no.dups = TRUE)
  #q9 <- cbind(result.lhs.ci, q9)
  for (i in 50:lastdate) {
    q9$dates[i] = calender[i - 50 + 1]
  }  
  
  #q2<-round(q1[,4:43])
  plot1 <- ggplot(q1) +
    geom_point(aes(x = dates, y = I), size = 0.8, color = "darkblue") + 
    geom_line(aes(x = dates, y = I), size = 0.5,color = "darkblue") +
    geom_line(aes(x = dates, y = TP.incidence), size = 2, color = "#FF9999", group = 1) +
#    geom_ribbon(data = q1, aes(x = dates,  ymin = low, ymax = high ), alpha = .1) +
    geom_line(  data = q2,aes(x = dates, y = TP.incidence), size = 2, color = "red",fill = "white"  ) +
#    geom_ribbon(data = q2, aes( x = dates,  ymin = low, ymax = high), alpha = .1 ) +
    geom_line(data = q3,aes(x = dates, y = TP.incidence), size = 2, color = "#CC0033", fill = "white" ) +
#    geom_ribbon(data = q3,aes(  x = dates,  ymin = low, ymax = high ),alpha = .1  ) +
    geom_vline(aes(xintercept = as.numeric(dates[c(96)])), linetype = 2,   color = 'red') +
    scale_y_continuous( breaks = seq(0, 5000, 1000),minor_breaks = seq(0, 5000, 1000),limits = c(0, 5000) , expand = c(0, 0) ) +
    ylab('Incidence') + xlab('Date') +  ggtitle('') +
    scale_x_date( limits = as.Date(c("2021-07-01", "2022-12-30")), breaks = as.Date( c( "2021-07-01",  "2021-09-01",  "2021-11-01",
                                                                                        "2022-01-01",  "2022-03-01",  "2022-05-01",
                                                                                        "2022-07-01",  "2022-09-01", "2022-11-01") ),
                  labels = c ("07", "09",  "11",  "01", "03", "05", "07",  "09", "11"),expand = c(0, 0)) +
    theme(axis.text.x = element_text(angle = 0, hjust = 0.5,vjust = 0.1,color = "black"   ),axis.text.y = element_text(color = "black"),
          panel.grid.major.x = element_blank(),
          panel.grid.major.y = element_blank(),
          panel.grid.minor = element_blank(),
          axis.line.x = element_line(color = "black"),
          axis.line.y = element_line(color = "black"),
          panel.background = element_rect(fill = "white"),
          legend.key = element_blank(),
          legend.title = element_blank(),
          text = element_text(size = 8),
          panel.spacing = unit(2, "lines")
    )
  
  plot1
  
  plot2 <- ggplot(q4) +
    geom_point(aes(x = dates, y = I), size = 0.8, color = "darkblue") + 
    geom_line(aes(x = dates, y = I), size = 0.5,color = "darkblue") +
    geom_line(aes(x = dates, y = TP.incidence), size = 2, color = "#FF9999", group = 1) +
#    geom_ribbon(data = q4, aes(x = dates, ymin = low, ymax = high ), alpha = .1) +
    geom_line(  data = q5,aes(x = dates, y = TP.incidence), size = 2, color = "red",fill = "white"  ) +
#    geom_ribbon(data = q5, aes( x = dates,  ymin = low, ymax = high), alpha = .1 ) +
    geom_line(data = q6,aes(x = dates, y = TP.incidence), size = 2, color = "#CC0033", fill = "white" ) +
#    geom_ribbon(data = q6,aes(  x = dates, ymin = low, ymax = high ),alpha = .1  ) +
    geom_vline(aes(xintercept = as.numeric(dates[c(96)])), linetype = 2,   color = 'red') +
    scale_y_continuous( breaks = seq(0, 5000, 1000),minor_breaks = seq(0, 5000, 1000),limits = c(0,5000) , expand = c(0, 0) ) +
    ylab('Incidence') + xlab('Date') +  ggtitle('') +
    scale_x_date( limits = as.Date(c("2021-07-01", "2022-12-30")), breaks = as.Date( c( "2021-07-01",  "2021-09-01",  "2021-11-01",
                                                                                        "2022-01-01",  "2022-03-01",  "2022-05-01",
                                                                                        "2022-07-01",  "2022-09-01", "2022-11-01") ),
                  labels = c ("07", "09",  "11",  "01", "03", "05", "07",  "09", "11"),expand = c(0, 0)) +
    theme(axis.text.x = element_text(angle = 0, hjust = 0.5,vjust = 0.1,color = "black"   ),axis.text.y = element_text(color = "black"),
          panel.grid.major.x = element_blank(),
          panel.grid.major.y = element_blank(),
          panel.grid.minor = element_blank(),
          axis.line.x = element_line(color = "black"),
          axis.line.y = element_line(color = "black"),
          panel.background = element_rect(fill = "white"),
          legend.key = element_blank(),
          legend.title = element_blank(),
          text = element_text(size = 8),
          panel.spacing = unit(2, "lines")
    )
  
  plot2
  
  
  plot3 <- ggplot(q7) +
    geom_point(aes(x = dates, y = I), size = 0.8, color = "darkblue") + 
    geom_line(aes(x = dates, y = I), size = 0.5,color = "darkblue") +
    geom_line(aes(x = dates, y = TP.incidence), size = 2, color = "#FF9999", group = 1) +
#    geom_ribbon(data = q7, aes(x = dates, ymin = low, ymax = high ), alpha = .1) +
    geom_line(  data = q8,aes(x = dates, y = TP.incidence), size = 2, color = "red",fill = "white"  ) +
#    geom_ribbon(data = q8, aes( x = dates, ymin = low, ymax = high ), alpha = .1 ) +
    geom_line(data = q9,aes(x = dates, y = TP.incidence), size = 2, color = "#CC0033", fill = "white" ) +
#    geom_ribbon(data = q9,aes(  x = dates, ymin = low, ymax = high ),alpha = .1  ) +
    geom_vline(aes(xintercept = as.numeric(dates[c(96)])), linetype = 2,   color = 'red') +
    scale_y_continuous( breaks = seq(0, 5000, 1000),minor_breaks = seq(0, 5000, 1000),limits = c(0, 5000)  , expand = c(0, 0)) +
    ylab('Incidence') + xlab('Date') +  ggtitle('') +
    scale_x_date( limits = as.Date(c("2021-07-01", "2022-12-30")), breaks = as.Date( c( "2021-07-01",  "2021-09-01",  "2021-11-01",
                                                                                        "2022-01-01",  "2022-03-01",  "2022-05-01",
                                                                                        "2022-07-01",  "2022-09-01", "2022-11-01") ),
                  labels = c ("07", "09",  "11",  "01", "03", "05", "07",  "09", "11"),expand = c(0, 0)) +
    theme(axis.text.x = element_text(angle = 0, hjust = 0.5,vjust = 0.1,color = "black"   ),axis.text.y = element_text(color = "black"),
          panel.grid.major.x = element_blank(),
          panel.grid.major.y = element_blank(),
          panel.grid.minor = element_blank(),
          axis.line.x = element_line(color = "black"),
          axis.line.y = element_line(color = "black"),
          panel.background = element_rect(fill = "white"),
          legend.key = element_blank(),
          legend.title = element_blank(),
          text = element_text(size = 8),
          panel.spacing = unit(2, "lines")
    )
  
  plot3
  
  
  library(ggpubr)
  plot.all <- ggarrange(
    plot1,plot2,plot3, 
    ncol = 3, nrow = 1, common.legend = TRUE, legend = "bottom",
    #hjust=-2, #vjust=0.2,
    #labels = c(  "Vac 0.8",   "Vac 1",   "Vac 1.2"  ),
    font.label = list(size = 12, color = "dark blue")) +  
    theme(legend.title = element_text(size = 10, face = "bold"))
  
  output <- annotate_figure(
    plot.all, 
    #top = text_grob("Epidemic/Intervention scenarios",color = "black",  face = "bold",  size = 12  ),
    bottom = text_grob("",  color = "black",  hjust = 1,  x = 1,  face = "italic",  size = 8  ),
    left = text_grob( "",   color = "black",  rot = 90,  size = 8   ),
    right = "",
    #fig.lab = "Figure",
    fig.lab.face = "bold"
  )
  return(output)
}

