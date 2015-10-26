#' Correlated velocity movement: OU Simulation
#' 
#' Simulates 2D correlated velocity movement model
#' 
#' @details This function simulates a CVM explicitly using the governing OU equation, using a discretization in which dt << tau.   It is used primarily to simulate "true" trajectories against which other simulation/estimation routines can be tested. 
#' @param nu mean speed of movement
#' @param tau characteristic time scale of movement
#' @param v0 initial velocity vector.  Default is randomly oriented vector with magnitude \code{nu}
#' @param Tmax max time
#' @param dt time interval of simulation
#' @return a list with the following elements \describe{\item{T}{the time vector} \item{V}{the (complex) vector of velocities} \item{Z}{the (complex) vector of locations} \item{X}{a 4xn matrix containing columns for, respectively, Vi, Zi, Vj and Zj where i and j refer to the x and y coordinates of the movement} \item{dt, tau, nu,vo}{the parameters of the model.}}
#' @examples
#' nu <- 2; tau <- 5; dt <- .1; cvm <- CVM(nu, tau, Tmax = 1000, dt = dt)
#' plot(cvm$Z, asp=1, type="l", main = "CVM(2,5)")
#' title(sub = "0-1000 time units") 


CVM <-
function(nu = 1, tau=1, v0=nu*exp(1i*runif(1,0,2*pi)), Tmax=10, dt=.1)
{
  T <- seq(0,Tmax,dt)
  n <- length(T)
  V <- T*0
  dW <- (rnorm(n) + 1i*rnorm(n))*sqrt(dt)
  V[1] <- v0
  for(i in 2:n)
    V[i] <-  V[i-1] - V[i-1] * dt/tau + 2*nu/sqrt(pi*tau) * dW[i]              
  Z <- cumsum(V)*dt
  X <- cbind(Re(V), Re(Z), Im(V), Im(Z))
  return(list(T = T, V = V, Z = Z, X = X, dt = dt, tau=tau, nu=nu, v0=v0))
}
