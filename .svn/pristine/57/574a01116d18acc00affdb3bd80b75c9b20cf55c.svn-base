#' Multi-phase CVM model 
#' 
#' Multi-phase CVM
#' 
#' @details Uses the correlated velocity movement continuous time model (CVM) parameterized in terms of the mean speed `nu' and characteristic time scale `tau' to generate a multi-phase model, useful for testing robustness of various change point methods 
#' 
#' @param {taus,nus,Ts} vector values of parameter values for the multiple phases.  Ts refers to the duration of each phase.  Note that the sampling is assumed to occur at integer time intervals
#' 
#' @examples 
#' 
#' # Time scale changes
#' 
#' taus <- c(2,20,40,2)
#' nus <- rep(1,4)
#' Ts <- c(100,50,50,100)
#' Tau.sim <- multiCVM(taus, nus, Ts)
#' plot(Tau.sim)
#' 
#' # Mean speed changes
#' 
#' nus <- c(1,5,10,1)
#' taus <- rep(2,4)
#' Ts <- c(100,50,50,100)
#' Nu.sim <- multiCVM(taus, nus, Ts)
#' plot(Nu.sim)


multiCVM <- function(taus, nus, Ts)
{
  TauSims.list <- list()
  for(i in 1:length(Ts))
  {
    T <- 1:Ts[i]
    if(i == 1) Z0 <- 0 else (Z0 <- tail(TauSims.list[[i-1]],1))
    TauSims.list[[i]] <- CVM(T-T[1], nu = nus[i], tau = taus[i])$Z + Z0
  }
  Z.tau <- eval(parse(text = paste("c(", paste("TauSims.list[[",1:length(taus),"]]", sep="", collapse=","), ")")))
  Phases <- rep(1:length(Ts), Ts)
  multipath <- list(Z=Z.tau, Phase=Phases, Ts = Ts)
  class(multipath) <- "multipath"
  return(multipath)
}