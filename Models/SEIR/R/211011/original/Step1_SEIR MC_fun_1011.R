
sir <- function(t,X,parms){
  
  for (i in 1:length(X)) {
    if (X[i]< 0) {
      X[i]<- 0
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
    #   print(testing)
    #   print(vaccinating)
    # }
    
      screeningreturn <- parms$mu*FP
      vaccination <- matrix(vac1(t, age.vac1), nrow=4, ncol=lastdate, byrow=T)
      #vaccination <- matrix(vac1(t), nrow=4, ncol=lastdate, byrow=T)*U
      infection <- matrix(contactpattern(t, aver_Rt), nrow=4, ncol=lastdate, byrow=T)* as.vector(((U*(C))%*%(A/(U+A+V+E))))
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
        
        #dIdt<- testingrate 
        #dTP2dt<- - recovery2
        #dTP3dt<- - progression2

        SEIR <- rbind(dUdt, dVdt, dEdt, dAdt, dTPdt,dFPdt, dSdt, dRdt, dDdt )
        # as.list(SEIR)
        
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
              SEIR[,181],SEIR[,182], SEIR[,183], SEIR[,184], SEIR[,185], SEIR[,186], SEIR[,187], SEIR[,188],SEIR[,189],SEIR[,190],
              SEIR[,191],SEIR[,192], SEIR[,193], SEIR[,194], SEIR[,195], SEIR[,196], SEIR[,197], SEIR[,198],SEIR[,199],SEIR[,200],

              SEIR[,201], SEIR[,202], SEIR[,203], SEIR[,204], SEIR[,205], SEIR[,206], SEIR[,207], SEIR[,208], SEIR[,209],SEIR[,210],
              SEIR[,211], SEIR[,212], SEIR[,213], SEIR[,214], SEIR[,215], SEIR[,216], SEIR[,217], SEIR[,218],SEIR[,219],SEIR[,220],
              SEIR[,221], SEIR[,222], SEIR[,223], SEIR[,224], SEIR[,225], SEIR[,226], SEIR[,227], SEIR[,228],SEIR[,229],SEIR[,230],
              SEIR[,231],SEIR[,232], SEIR[,233], SEIR[,234], SEIR[,235], SEIR[,236], SEIR[,237], SEIR[,238],SEIR[,239],SEIR[,240],
              SEIR[,241],SEIR[,242], SEIR[,243], SEIR[,244], SEIR[,245], SEIR[,246], SEIR[,247], SEIR[,248],SEIR[,249],SEIR[,250],
              SEIR[,251],SEIR[,252], SEIR[,253], SEIR[,254], SEIR[,255], SEIR[,256], SEIR[,257], SEIR[,258],SEIR[,259],SEIR[,260],
              SEIR[,261],SEIR[,262], SEIR[,263], SEIR[,264], SEIR[,265], SEIR[,266], SEIR[,267], SEIR[,268],SEIR[,269],SEIR[,270],
              SEIR[,271],SEIR[,272], SEIR[,273], SEIR[,274], SEIR[,275], SEIR[,276], SEIR[,277], SEIR[,278],SEIR[,279],SEIR[,280],
              SEIR[,281],SEIR[,282], SEIR[,283], SEIR[,284], SEIR[,285], SEIR[,286], SEIR[,287], SEIR[,288],SEIR[,289],SEIR[,290],
              SEIR[,291],SEIR[,292], SEIR[,293], SEIR[,294], SEIR[,295], SEIR[,296], SEIR[,297], SEIR[,298],SEIR[,299],SEIR[,300],
              SEIR[,301], SEIR[,302], SEIR[,303], SEIR[,304], SEIR[,305], SEIR[,306], SEIR[,307], SEIR[,308], SEIR[,309],SEIR[,310],
              SEIR[,311], SEIR[,312], SEIR[,313], SEIR[,314], SEIR[,315], SEIR[,316], SEIR[,317], SEIR[,318],SEIR[,319],SEIR[,320],
              SEIR[,321], SEIR[,322], SEIR[,323], SEIR[,324], SEIR[,325], SEIR[,326], SEIR[,327], SEIR[,328],SEIR[,329],SEIR[,330],
              SEIR[,331],SEIR[,332], SEIR[,333], SEIR[,334], SEIR[,335], SEIR[,336], SEIR[,337], SEIR[,338],SEIR[,339],SEIR[,340],
              SEIR[,341],SEIR[,342], SEIR[,343], SEIR[,344], SEIR[,345],
              SEIR[,346], SEIR[,347], SEIR[,348],SEIR[,349],SEIR[,350]
              # SEIR[,151],SEIR[,152], SEIR[,153], SEIR[,154], SEIR[,155], SEIR[,156], SEIR[,157], SEIR[,158],SEIR[,159],SEIR[,160],
              # SEIR[,161],SEIR[,162], SEIR[,163], SEIR[,164], SEIR[,165], SEIR[,166], SEIR[,167], SEIR[,168],SEIR[,169],SEIR[,170],
              # SEIR[,171],SEIR[,172], SEIR[,173], SEIR[,174], SEIR[,175], SEIR[,176], SEIR[,177], SEIR[,178],SEIR[,179],SEIR[,180],
              # SEIR[,181],SEIR[,182], SEIR[,183], SEIR[,184], SEIR[,185], SEIR[,186], SEIR[,187], SEIR[,188],SEIR[,189],SEIR[,190],
              # SEIR[,191],SEIR[,192], SEIR[,193], SEIR[,194], SEIR[,195], SEIR[,196], SEIR[,197], SEIR[,198],SEIR[,199],SEIR[,200],
              # 
        )  
    })
}

sir.lhs <- function(t,X,parms){
  
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
  
  with(as.list(c(parms, X)), {
    screeningreturn <- parms$mu*FP
    vaccination <- matrix(vac1_lhs(t,age.vac1, vac_k), nrow=4, ncol=lastdate, byrow=T)
    infection <- matrix(contactpattern_lhs(t, aver_Rt, beta_k), nrow=4, ncol=lastdate, byrow=T)* as.vector(((U*(C))%*%(A/(U+A+V+E))))
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
    dTPdt<- testingrate - recovery2 - progression2
    dFPdt<- screeningrate - screeningreturn
    dSdt <- progression2 + progression1 - recovery1 - dead
    dRdt <- recovery1+recovery2+recovery3
    dDdt <- dead
    
    SEIR <- rbind(dUdt, dVdt, dEdt, dAdt, dTPdt,dFPdt, dSdt, dRdt, dDdt )
    
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
    )
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
  #I =(X[(10*nage+1):(11*nage)])
  
  with(as.list(c(parms, X)), {
    
    # if (t==1){
    #  print(testing)
    #  print(vaccinating)
    # }
    
    screeningreturn <- mu*FP
    vaccination <- matrix(vac1_SA(t, age.vac1, vaccinating), nrow=4, ncol=lastdate, byrow=T)
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
         SEIR[,91],SEIR[,92], SEIR[,93], SEIR[,94], SEIR[,95], SEIR[,96], SEIR[,97], SEIR[,98],SEIR[,99],SEIR[,100]

    )
    
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
                  age.vac1=age.vac1,
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
  #result2$I<-result2$I1+result2$I2+result2$I3
  
  result2$TP1.incidence <- c(100,diff(result2$TP1))
  result2$TP2.incidence <- c(200,diff(result2$TP2))
  result2$TP3.incidence <- c(400,diff(result2$TP3))
  result2$TP4.incidence <- c(300,diff(result2$TP4))
  result2$TP.incidence <- c(1000,diff(result2$TP))
  
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
                  age.vac1=age.vac1,
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
  #result2$I<-result2$I1+result2$I2+result2$I3
  
  result2$TP1.incidence <- c(100,diff(result2$TP1))
  result2$TP2.incidence <- c(200,diff(result2$TP2))
  result2$TP3.incidence <- c(400,diff(result2$TP3))
  result2$TP4.incidence <- c(300,diff(result2$TP4))
  result2$TP.incidence <- c(1000,diff(result2$TP))
  
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
                  age.vac1=age.vac1,
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
  #result2$I<-result2$I1+result2$I2+result2$I3
  
  result2$TP1.incidence <- c(100,diff(result2$TP1))
  result2$TP2.incidence <- c(200,diff(result2$TP2))
  result2$TP3.incidence <- c(400,diff(result2$TP3))
  result2$TP4.incidence <- c(300,diff(result2$TP4))
  result2$TP.incidence <- c(1000,diff(result2$TP))
  
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

