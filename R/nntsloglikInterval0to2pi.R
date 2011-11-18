nntsloglikInterval0to2pi <-
function (data, cutpoints, cpars = 1/sqrt(2*pi), M = 0) 
{
    #data <- data/sum(data)
    y <- 0
    for (k in 1:length(data)) {
if (cutpoints[k+1] < cutpoints[k])
        y <- y + data[k] * log(1 - nntsDistributioninterval0to2pi(cutpoints[(k + 
            1)],cpars, M)+ nntsDistributioninterval0to2pi(cutpoints[k], cpars, M))
else
        y <- y + data[k] * log(nntsDistributioninterval0to2pi(cutpoints[(k + 
            1)], cpars, M)- nntsDistributioninterval0to2pi(cutpoints[k], cpars, M))
    }
    res <- Re(y)
    return(res)
}

