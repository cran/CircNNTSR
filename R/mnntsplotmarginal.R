mnntsplotmarginal<-function(cestimatesarray,M,component, ...){

    nntsplotint <- function(theta) {
        res <- mnntsmarginal(cestimatesarray,M,component,theta)
	return(res)
    }
    return(curve(nntsplotint, 0, 2 * pi, xlab = paste("Component:",component), ...))
}