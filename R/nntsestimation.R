nntsestimation <-
function(M=0,data,maxit=500){
if (M==0)
return("Uniform distribution: no parameters to estimate")
A<-rbind(diag(1,2*M,2*M),cbind(matrix(0,nrow=M,ncol=M),diag(-1,M,M)),c(rep(-1,M),rep(0,M)))
cvector<-rbind(matrix(0,nrow=2*M,ncol=1),matrix(-2*pi+.00000000001,nrow=M,ncol=1),-1/(2*pi))
IP<-nntsrandominitial(M)
r<-constrOptim(theta=IP,f=nntsloglik,grad=NULL,ui=A,ci=cvector,method="Nelder-Mead",data=data,M=M,control=list(maxit=maxit))
AIC<-(2*r$value)+2*(2*M)
BIC<-(2*r$value)+(2*M)*log(length(data))
res<-list(coef=r$par,loglik=-r$value,AIC=AIC,BIC=BIC,convergence=r$convergence)
return(res)
}

