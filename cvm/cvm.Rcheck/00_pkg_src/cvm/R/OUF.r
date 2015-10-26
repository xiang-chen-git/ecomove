#' Simulating OUF process
#' 
#' Simulates 2D OUF movement model
#' 
#' @details This function numerically integrates an OUF process  using a discretization in which dt << tau_u.   
#' 
#' @param Tmax max time
#' @param mu.z central location of movement
#' @param tau.z characteristic time scale of regression to spatial mean
#' @param tau.u characteristic time scale of ballistic component
#' @param beta magnitude of stochastic acceleration
#' @param dt time interval of integration
#' @return a data frame with the following elements \describe{\item{T}{the time vector} \item{Z}{the (complex) vector of locations}}
#' @examples
#' nu <- 2; tau <- 5; dt <- .1; cvm <- CVM(nu, tau, Tmax = 1000, dt = dt)
#' plot(OUF(tau.z = 10), asp=1, type="l", main=expression(tau[z]==10))

OUF <- function(Tmax=100, mu.z = 0, tau.z = 10, tau.u = 1, beta = 1, dt=.1, z0=0, u0=0)
{
  T <- seq(0,Tmax,dt)
  n <- length(T)
  
  U <- T*0
  Z <- T*0
  
  dW <- (rnorm(n) + 1i*rnorm(n))*sqrt(dt)
  
  U[1] <- u0
  Z[1] <- z0
  
  for(i in 2:n)
  {
    U[i] <- U[i-1] - (1/tau.u * U[i-1])*dt + beta*dW[i]
    Z[i] <- Z[i-1] - (1/tau.z * Z[i-1])*dt + U[i]*dt
  }
  return(Z)
}