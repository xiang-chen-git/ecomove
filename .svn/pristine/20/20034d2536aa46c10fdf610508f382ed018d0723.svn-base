reload()
require(cvm)

  nu <- 3
  tau <- 5 
  T <- seq(0,100,1)
	
	Z <- CVM2(T, nu, tau)$Z
	
	estimateCVM(Z,T,method="vaf", diagnose=TRUE, CI=TRUE)	
	estimateCVM(Z,T,method="vaf", spline=TRUE, diagnose=TRUE, CI=TRUE)
	

getV.spline <- function(Z, T, dt.spline=.001)
{
  fX.spline <- splinefun(T, Re(Z))
  fY.spline <- splinefun(T, Im(Z))
  
  T.spline <- seq(0,max(T),dt.spline)
  
  X.spline <- fX.spline(T.spline )
  Y.spline <- fY.spline(T.spline )
  
  Z.spline <- X.spline + 1i*Y.spline
  
  which.spline <- match(round(T,-log(dt.spline,10)), round(T.spline,-log(dt.spline,10)))
  which.spline[length(which.spline)] <- which.spline[length(which.spline)]-1
  
  diff(Z.spline)[which.spline]/dt.spline
}
