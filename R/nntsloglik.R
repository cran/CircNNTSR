nntsloglik <-
function(cpars=c(0,0),M=0,data){

size<-length(cpars)
if ((size %% 2)==1)
return("Length of cpar must be an even number")
if (size<2*M)
{
temp<-size/2+1
cparscorr<-c(cpars[1:(size/2)],array(0,(M-temp+1)), cpars[temp:size], array(0,(M-temp+1)))
cpars<-cparscorr
cat("Warning: Missing parameters set to 0
")
}

y <- 0
for (k in 1:length(data))
{
y <- y - log(nntsdensity(cpars,M,data[k]))
}
res <- Re(y)
return(res)
}

