##' Get splined velocity estimates
##'
##' A cubic spline of a 2D track over time.
##'
#' @param Z location data in complex form (X + iY)
#' @param T time of observations (NOTE: must be regularly spaced for methods "vaf" and "crw")
#' @return a list with the estimated velocity (V), as well as the position (Z) and time (T).  

getV.spline <- function(Z, T)
{
  dt.spline <- min(diff(T))/30
  fX.spline <- splinefun(T, Re(Z))
  fY.spline <- splinefun(T, Im(Z))
  T.spline <- seq(min(T),max(T),dt.spline)
  
  X.spline <- fX.spline(T.spline )
  Y.spline <- fY.spline(T.spline )
  Z.spline <- X.spline + 1i*Y.spline
  V.spline <- diff(Z.spline)/diff(T.spline)
  
  # sample the INTER-times
  T.mid <- (T[-1] + T[-length(T)])/2
  
  which.spline <- match(round(T.mid,-log(dt.spline,10)), round(T.spline,-log(dt.spline,10))) 
  
  Z.mid <- Z.spline[which.spline]
  V.mid <- V.spline[which.spline]
  
  return(list(V = V.mid, Z = Z.mid, T = T.mid))
}