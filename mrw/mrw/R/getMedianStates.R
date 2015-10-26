##' @rdname getMeanStates

getMedianStates <- function(jagsResults, data = NULL)
{
	if(class(jagsResults) != "rjags") stop("jagsResults parameter must be an rjags object")
	if(!("idx" %in% jagsResults$parameters.to.save)) stop("Must record idx parameter to get states")
	
	medians <- apply(.getIdxArray(jagsResults), 3, median)
  
	if ( !is.null(data) )
	{
	  if (nrow(data) != length(medians)) stop("Provided data must be same length as data used to fit jagsResults object")
	  medians <- data.frame(State = medians, Time = data$T.POSIX)
	}
	
	return(medians)
}
