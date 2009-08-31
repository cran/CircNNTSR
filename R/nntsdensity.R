nntsdensity <-
function(cpars=c(0,0),M=0,theta){

# cpars is a vector of real numbers of dimension 2*M. The first M numbers are the SQUARED norms. The last M numbers are the angles
# theta is an angle in radians

if (M==0) 
return(sqrt(1/(2*pi)))
else 
{
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


cparsnew <- complex(M)

for (k in 1:M)
{
cparsnew[k] <- sqrt(cpars[k])*exp(1i*cpars[(k+M)]) 
}
if (1/(2*pi) - sum(cpars[1:M])<0) 
return("sum of componentes greater than condition") 
else 
{
cpars0 <- sqrt(1/(2*pi) - sum(cpars[1:M]))
cparsnew <- c(cpars0,cparsnew)
aux <- complex(M+1)
for (k in 0:M)
{
aux[k+1] <- exp(1i*k*theta)
}
aux <- cparsnew*aux

res <- Re(sum(aux)*Conj(sum(aux)))
return(res)
}
}
}

