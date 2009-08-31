nntsloglikInterval <-
function(cpars=c(0,0),M=0,data,limits){

# The dimension of limits has to be 1 plus the dimension of data
# data number of cases in each interval 

data <- data/sum(data)
y <- 0
for (k in 1:length(data))
{
y <- y - data[k]*log(nntsDistribution(cpars,M,limits[(k+1)]) - nntsDistribution(cpars,M,limits[k]))
}
res <- Re(y)
return(res)
}

