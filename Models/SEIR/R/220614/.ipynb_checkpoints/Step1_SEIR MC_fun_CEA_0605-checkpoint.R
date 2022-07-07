
sir_CEA_ud2 <- function(t,X,parms){
  # 
  # for (i in 1:length(X)) {
  #   if (X[i]< 0) {
  #     X[i]<- 0
  #   }}
  
  ncompartment=20
  nage=length(X)/ncompartment
  
  U =(X[1:nage])
  V =(X[(nage+1):(2*nage)])
  E =(X[(2*nage+1):(3*nage)])
  A =(X[(3*nage+1):(4*nage)])
  TP_ud=(X[(4*nage+1):(5*nage)])
  TP_noud=(X[(5*nage+1):(6*nage)])
  S_ud =(X[(6*nage+1):(7*nage)])
  S_noud = (X[(7*nage+1):(8*nage)])
  H_ud =(X[(8*nage+1):(9*nage)])
  H_noud = (X[(9*nage+1):(10*nage)])
  R_ud =(X[(10*nage+1):(11*nage)])
  R_noud = (X[(11*nage+1):(12*nage)])
  D_ud =(X[(12*nage+1):(13*nage)])
  D_noud = (X[(13*nage+1):(14*nage)])
  TP_ud.I =(X[(14*nage+1):(15*nage)])
  TP_noud.I =(X[(15*nage+1):(16*nage)])
  S.I_ud =(X[(16*nage+1):(17*nage)])
  S.I_noud= (X[(17*nage+1):(18*nage)])
  H.I_ud=(X[(18*nage+1):(19*nage)])
  H.I_noud= (X[(19*nage+1):(20*nage)])
  
  with(as.list(c(parms, X)), {
    
    # if (t==1){
    #   print(testing)
    #   print(vaccinating)
    # }
    # 
    # for (i in 1:length(X)) {
    #   if (X[i]< 0) {
    #     X[i]<- 0
    #   }}
    
    vaccination <- matrix(vac1(t, U, age.vac1), nrow=4, ncol=lastdate, byrow=F)
    #waining1 <- parms$wan*(V)
    waining2 <- parms$wan*(R_ud)
    waining3 <- parms$wan*(R_noud)
    #vaccination <- matrix(vac1(t), nrow=4, ncol=lastdate, byrow=T)*U
    infection1 <- matrix(contactpattern(t, aver_Rt), nrow=4, ncol=lastdate, byrow=T)* as.vector(((U*(C))%*%(A/(U+A+V+E))))
    infection2 <- matrix(contactpattern(t, aver_Rt), nrow=4, ncol=lastdate, byrow=T)* as.vector((V)*(A/(U+A+V+E)))
    #infection2 <- matrix(contactpattern(t, aver_Rt), nrow=4, ncol=lastdate, byrow=T)* as.vector((V*(C))%*%(A/(U+A+V+E)))
    incubation <- parms$theta*(E)
    testingrate <- matrix(tau2_1(t, tau2), nrow=4, ncol=lastdate, byrow=T)*Se*A
    screeningrate <- parms$tau1*(1-Sp)*(U)
    
    progression1 <-parms$sigma*(A)
    
    progression2 <-parms$sigma*(TP_ud)
    progression3 <-parms$sigma*(TP_noud)
    
    recovery1_ud <-parms$rho*(S_ud)
    recovery1_noud <-parms$rho*(S_noud)
    
    recovery2 <-parms$rho*(A)
    recovery3_ud <-parms$rho*(TP_ud)
    recovery3_noud <-parms$rho*(TP_noud)
    
    recovery4_ud <-parms$rho_h*(1-parms$delta )*H_ud
    recovery4_noud <-parms$rho_h*(1-parms$delta )*H_noud
    
    dead_ud <-parms$delta *(H_ud)
    dead_noud <-parms$delta *(H_noud)
    
    hospitalization_ud<-parms$adm_ud*S_ud
    hospitalization_noud<-parms$adm_noud*S_noud
    
    dUdt <- -infection1 - vaccination  +waining2 +waining3 #+waining1
    dVdt <- vaccination  - infection2 #-waining1
    dEdt <- infection1 - incubation + infection2
    dAdt <- incubation - testingrate - progression1 - recovery2
    dTP_uddt<- testingrate - recovery3_ud - progression2
    dTP_nouddt<- testingrate - recovery3_noud - progression3
    
    dS_uddt <- progression2 + progression1 - recovery1_ud - hospitalization_ud 
    dS_nouddt <- progression3 + progression1 - recovery1_noud - hospitalization_noud 
    
    dH_uddt <- hospitalization_ud - recovery4_ud - dead_ud
    dH_nouddt <- hospitalization_noud - recovery4_noud - dead_noud
    
    dR_uddt <- recovery1_ud+recovery2+recovery3_ud+recovery4_ud-waining2
    dR_nouddt <- recovery1_noud+recovery2+recovery3_noud+recovery4_noud-waining3
    
    dD_uddt <- dead_ud
    dD_nouddt <- dead_noud
    
    dTP_ud.Idt<- testingrate 
    dTP_noud.Idt<- testingrate 
    
    dS.I_uddt<- progression2 + progression1
    dS.I_nouddt<- progression3 + progression1
    
    dH.I_uddt<-hospitalization_ud
    dH.I_nouddt<-hospitalization_noud
    
    SEIR <- rbind(dUdt, dVdt, dEdt, dAdt, dTP_uddt,dTP_nouddt,dS_uddt,dS_nouddt, dH_uddt, dH_nouddt, dR_uddt,dR_nouddt, dD_uddt,dD_nouddt, dTP_ud.Idt,  dTP_noud.Idt, 
                  dS.I_uddt, dS.I_nouddt, dH.I_uddt, dH.I_nouddt)
    SEIR=as.data.frame(SEIR)
    
  })
}

