nntsuniformitytestlikelihoodratio<-function(data,M=1, iter=1000, initialpoint = FALSE, cinitial,gradientstop=1e-10){
  
  caux0<-nntsmanifoldnewtonestimationgradientstop(data,M,iter,initialpoint,cinitial,gradientstop)
  caux<-caux0$loglik
  c0unifstat<-2*(caux + length(data)*log(2*pi))
  
  samplesize<-length(data)
  
  if (M >= 8)
    return("Error: Currently the test is only implemented for values of M between 1 and 7")
  
  if (M==1){
    if (samplesize < 15)
      return("Error: Sample size for M=1 must be at least equal to 15")
    
    if ((samplesize >= 15) & (samplesize < 85)){
      criticalvalue10<-round(4.512840+10.80621*(1/samplesize),1) 
      criticalvalue05<-round(5.926881+12.74605*(1/samplesize),1) 
      criticalvalue01<-round(9.062970+24.53770*(1/samplesize),1)
    }
    if (samplesize >= 85){
      criticalvalue10<-4.6 
      criticalvalue05<-6.1 
      criticalvalue01<-9.3
    }
  }
  
  if (M==2){
    if (samplesize < 25)
      return("Error: Sample size for M=2 must be at least equal to 25")
    
    if ((samplesize >= 25) & (samplesize < 98)){
      criticalvalue10<-round(7.68072+24.16980*(1/samplesize),1) 
      criticalvalue05<-round(9.31176+34.17495*(1/samplesize),1) 
      criticalvalue01<-round(13.1063+43.70942*(1/samplesize),1)
    }
    if (samplesize >= 98){
      criticalvalue10<-7.9 
      criticalvalue05<-9.7 
      criticalvalue01<-13.5
    }
  }
  
  if ((M>=3) & (M<=7)){
    criticalvalue10aux<-round(3.2703+2.53172*M-108.32353*(1/samplesize)+32.83310*M*(1/samplesize)+1618.55353*((1/samplesize)^2),1) 
    criticalvalue05aux<-round(4.6077+2.72912*M - 91.8270*(1/samplesize)+31.88203*M*(1/samplesize)+1368.61867*((1/samplesize)^2),1) 
    criticalvalue01aux<-round(7.2135+3.15550*M + 26.9335*(1/samplesize)+21.03190*M*(1/samplesize)-1549.48940*((1/samplesize)^2),1) 
    
    if (M==3){	
      if (samplesize < 40)
        return("Error: Sample size for M=3 must be at least equal to 40")
      
      if ((samplesize >= 40) & (samplesize < 173)){
        criticalvalue10<-criticalvalue10aux 
        criticalvalue05<-criticalvalue05aux
        criticalvalue01<-criticalvalue01aux
      }
      if (samplesize >= 173){
        # conservative values in M=3 (.1 larger than the obtained by simulation)
        criticalvalue10<-10.9
        criticalvalue05<-12.9 
        criticalvalue01<-17.1
      }
    }
    
    if (M==4){	
      if (samplesize < 50)
        return("Error: Sample size for M=4 must be at least equal to 50")
      
      if ((samplesize >= 50) & (samplesize < 203)){
        criticalvalue10<-criticalvalue10aux 
        criticalvalue05<-criticalvalue05aux
        criticalvalue01<-criticalvalue01aux
      }
      if (samplesize >= 203){
        criticalvalue10<-13.5
        criticalvalue05<-15.7 
        criticalvalue01<-20.3
      }
    }
    
    if (M==5){	
      if (samplesize < 60)
        return("Error: Sample size for M=5 must be at least equal to 60")
      
      if ((samplesize >= 60) & (samplesize < 278)){
        criticalvalue10<-criticalvalue10aux 
        criticalvalue05<-criticalvalue05aux
        criticalvalue01<-criticalvalue01aux
      }
      if (samplesize >= 278){
        # conservative value for criticalvalue10 equal to 16.2 instead of 16.1 obtained by simulation
        criticalvalue10<-16.2
        criticalvalue05<-18.5 
        criticalvalue01<-23.4
      }
    }
    
    if (M==6){	
      if (samplesize < 70)
        return("Error: Sample size for M=6 must be at least equal to ")
      
      if ((samplesize >= 70) & (samplesize < 386)){
        criticalvalue10<-criticalvalue10aux 
        criticalvalue05<-criticalvalue05aux
        criticalvalue01<-criticalvalue01aux
      }
      if (samplesize >= 386){
        criticalvalue10<-18.7
        criticalvalue05<-21.2 
        criticalvalue01<-26.5
      }
    }
    
    if (M==7){	
      if (samplesize < 80)
        return("Error: Sample size for M=7 must be at least equal to ")
      
      if ((samplesize >= 80) & (samplesize < 562)){
        criticalvalue10<-criticalvalue10aux 
        criticalvalue05<-criticalvalue05aux
        criticalvalue01<-criticalvalue01aux
      }
      if (samplesize >= 562){
        criticalvalue10<-21.2
        criticalvalue05<-23.9 
        criticalvalue01<-29.6
      }
    }
    
  }
  
  res<-list()
  res["gradient"]<-caux0$gradnormerror	
  res["likratiounifstat"]<-c0unifstat
  res["criticalvalue10percent"]<-criticalvalue10
  res["criticalvalue05percent"]<-criticalvalue05
  res["criticalvalue01percent"]<-criticalvalue01
  
  res
}
