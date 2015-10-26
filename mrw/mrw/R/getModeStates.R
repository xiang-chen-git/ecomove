##' @rdname getMeanStates

getModeStates <- function(jagsResults, data = NULL)
{
	# best for 3 or more states
	probabilities <- getStateProbabilities(jagsResults)
	modes <- apply(probabilities, 1, which.max)
  
	if ( !is.null(data) )
	{
	  if (nrow(data) != length(modes)) stop("Provided data must be same length as data used to fit jagsResults object")
	  modes <- data.frame(State = modes, Time = data$T.POSIX)
	}
	
  return(modes)
}
