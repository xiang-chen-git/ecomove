require(cvm)

#----------------------------------------------------------
# Example 1: VAF method (high resolution, regular sampling)
#----------------------------------------------------------

  nu <- 10
  tau <- 2
  v0 <- 10
	T <- seq(0,100,.1)
  mycvm <- CVM2(T, nu, tau, v0)	
  plot(mycvm$Z, asp=1, cex=0.5, pch=19, col=rgb(0,0,0,.1))
  estimateCVM(mycvm$Z,mycvm$T,CI=TRUE,method="vaf",diagnose=TRUE)
	
  # coarser sampling 
	tau <- 2
	T <- seq(0,1000,1)
  mycvm <- CVM2(T, nu, tau, v0)  
  plot(mycvm$Z, asp=1, type="o", cex=0.5, pch=19)
  estimateCVM(mycvm$Z,mycvm$T,CI=TRUE,method="vaf",diagnose=TRUE)
	
	# improved speed estimate because of spline
	estimateCVM(mycvm$Z,mycvm$T,CI=TRUE,method="vaf",spline=TRUE)
	
#----------------------------------------------------------
# Example 2: CRW method (low resolution, regular sampling)
#----------------------------------------------------------

	tau <- 1
  nu <- 8
  mycvm <- CVM2(T=1:1000, nu, tau)
  plot(mycvm$Z, asp=1, type="o", cex=0.5, pch=19)
  estimateCVM(mycvm$Z,mycvm$T,CI=TRUE,method="crw", diagnose=TRUE)
	
# Example 2b: CRW method, poor diagnostics (auto-correlated step-lengths)
  tau <- 100
  mycvm <- CVM2(T=1:1000, nu, tau)
  plot(mycvm$Z, asp=1, type="o", cex=0.5, pch=19)
  estimateCVM(mycvm$Z,mycvm$T,method="crw", CI=TRUE, diagnose=TRUE)

#--------------------------------------------------------------------------------
# Example 3: One-step likelihood method (higher resolution, irregular sampling)
#--------------------------------------------------------------------------------

  nu <- 10
  tau <- 3
  v0 <- 10

# irregular timing
  
  T <- cumsum(rexp(1000,1/.1))
  mycvm <- CVM2(T, nu, tau, v0)	
  plot(mycvm$Z, asp=1, type="o", cex=0.5, pch=19)
  estimateCVM(mycvm$Z,mycvm$T,CI=TRUE,method="onestep", parameters=c("tau", "nu"))

# low resolution example
  
  T <- cumsum(rexp(1000,1))
  mycvm <- CVM2(T, nu, tau, v0)	
  plot(mycvm$Z, asp=1, type="o", cex=0.5, pch=19)
  estimateCVM(mycvm$Z,mycvm$T,CI=TRUE,method="onestep", parameters=c("tau", "nu"))

# improved with splining
  estimateCVM(mycvm$Z,mycvm$T,CI=TRUE,method="onestep", parameters=c("tau", "nu"), spline=TRUE)

#--------------------------------------
# Example 4: Full-likelihood method 
#--------------------------------------

  nu <- 10; tau <- 3; v0 <- 10
  T <- cumsum(rexp(100,1))
  mycvm <- CVM2(T, nu, tau, v0) 
  plot(mycvm$Z, asp=1, type="o", cex=0.5, pch=19)
  estimateCVM(mycvm$Z,mycvm$T,CI=TRUE,method="fullL")

#-------------------------------------------------------
# Example 5: `crawl' method (from Johnson et al. 2008) 
#-------------------------------------------------------

  # 5a. same track as above
  estimateCVM(mycvm$Z,mycvm$T,method="crawl")

  
# 5b. much longer track
  T <- cumsum(rexp(1000,1))
  mycvm <- CVM2(T, nu, tau, v0) 
  plot(mycvm$Z, asp=1, type="o", cex=0.5, pch=19, col=rgb(0,0,0,.2))
  estimateCVM(mycvm$Z,mycvm$T,method="crawl")

