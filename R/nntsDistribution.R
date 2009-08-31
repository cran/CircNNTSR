nntsDistribution <-
function(cpars=c(0,0),M=0,theta){

if (M==0)
return(theta/(2*pi))

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


ab <- nntsABcoefficients(cpars,M)
y <- theta/(2*pi)
for (k in 1:M)
{
y  <- y + (ab[k]/k)*sin(k*theta) + (ab[(k+M)]/k)*(1 - cos(k*theta))
} 
return(y)
}

