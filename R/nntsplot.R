nntsplot <-
function(cpars=c(0,0),M=0,...){
if (M==0)
{
x<-rep(1/(2*pi),2)
return(plot(c(0,2*pi),x,type="l",xlab="theta"))
}
if (1/(2*pi) - sum(cpars[1:M])<0) 
return("sum of componentes greater than condition")
size<-length(cpars)
if ((size %% 2)==1)
return("Length of cpar must be an even number")
if (size<2*M)
{
temp<-size/2+1
cparscorr<-c(cpars[1:(size/2)],array(0,(M-temp+1)),cpars[temp:size],array(0,(M-temp+1)))
cpars<-cparscorr
cat("Warning: Missing parameters set to 0
")
}

nntsplotint<-function(theta){
res <- nntsABDensity(cpars,M,theta) 
}
return(curve(nntsplotint,0,2*pi,xlab="theta",...))
}

