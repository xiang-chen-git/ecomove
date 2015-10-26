pkgname <- "cvm"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
library('cvm')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
cleanEx()
nameEx("Bowhead")
### * Bowhead

flush(stderr()); flush(stdout())

### Name: Bowhead
### Title: Bowhead GPS data
### Aliases: Bowhead
### Keywords: datasets

### ** Examples

data(Bowhead)

# googlemap (requires internet access)
require(ggmap)
DiskoBay <- get_map(location = c(-53.4, 69), maptype = "terrain", zoom=8)
ggmap(DiskoBay) + geom_path(data = Bowhead, mapping = aes(x = Long, y = Lat))



cleanEx()
nameEx("CVM")
### * CVM

flush(stderr()); flush(stdout())

### Name: CVM
### Title: Correlated velocity movement: OU Simulation
### Aliases: CVM

### ** Examples

nu <- 2; tau <- 5; dt <- .1; cvm <- CVM(nu, tau, Tmax = 1000, dt = dt)
plot(cvm$Z, asp=1, type="l", main = "CVM(2,5)")
title(sub = "0-1000 time units")



cleanEx()
nameEx("CVM2")
### * CVM2

flush(stderr()); flush(stdout())

### Name: CVM2
### Title: Correlated velocity movement: Exact Updating
### Aliases: CVM2 getSigma getX,

### ** Examples

# sampling 100 random times up to (about) 1000:
T <- cumsum(rexp(100)*10)
# Simulate
cvm2 <- CVM2(T, nu=2, tau=5)
# Illustrate
layout(rbind(c(1,3,3,2), c(1,4,4,2)))
par(bty="l", oma=c(0,0,4,0))
plot(cvm2$V, type="l", asp=1, main="Velocity", xlab="", ylab="", col="darkgrey")
points(cvm2$V, pch=19, cex=0.5)
plot(cvm2$Z, type="l", main="Position", asp=1, col="darkgrey")
points(cvm2$Z, pch=19, cex=0.5)
plot(cvm2$T, Re(cvm2$V), col=2, type="l", main="Velocity (decomposed)", ylim=range(c(Re(cvm2$V), Im(cvm2$V))))
lines(cvm2$T, Im(cvm2$V), col=3, type="l")
plot(cvm2$T, Re(cvm2$Z), col=2, type="l", main="Position (decomposed)", ylim=range(c(Re(cvm2$Z), Im(cvm2$Z))))
lines(cvm2$T, Im(cvm2$Z), col=3, type="l")
title("CVM(2, 5): 0-1000, 100 random samples", outer=TRUE, cex=1.5)



graphics::par(get("par.postscript", pos = 'CheckExEnv'))
cleanEx()
nameEx("CVMstan2D")
### * CVMstan2D

flush(stderr()); flush(stdout())

### Name: CVMstan2D
### Title: STAN model for fitting CVM
### Aliases: CVMstan2D
### Keywords: datasets

### ** Examples

data(CVMstan2D)
## maybe str(CVMstan2D) ; plot(CVMstan2D) ...



cleanEx()
nameEx("OUF")
### * OUF

flush(stderr()); flush(stdout())

### Name: OUF
### Title: Simulating OUF process
### Aliases: OUF

### ** Examples

nu <- 2; tau <- 5; dt <- .1; cvm <- CVM(nu, tau, Tmax = 1000, dt = dt)
plot(OUF(tau.z = 10), asp=1, type="l", main=expression(tau[z]==10))



cleanEx()
nameEx("Z.like1D")
### * Z.like1D

flush(stderr()); flush(stdout())

### Name: Z.like1D
### Title: Likelihood functions for cvm estimation
### Aliases: Z.like1D Z.like2D, Z.likeBlock

### ** Examples

# Simulate some 1-D data

nu <- 10
tau <- 3
v0 <- 20
mycvm <- CVM2(nu, tau, T = cumsum(rexp(50)), v0)
X <- Re(myvm$Z)
T <- mycvm$T

# Obtain and plot marginal likelihoods with correct values

LikelihoodScan(X,T,nu,tau,v0,Z.like1)
LikelihoodScan(X,T,nu,tau,v0,Z.like2, k=5)

# Obtain MLE

nu <- 3; tau <- 0.5; v0 <- 0
T <- cumsum(rexp(50))
Z <- Re(CVM2(T, nu, tau, v0)$Z)
plot(T,Z, type="o")

optim(c(1,1), Z.likelihood, Z=Z, T=T, control=list(fnscale=-1))



cleanEx()
nameEx("Z.likeBLOCK")
### * Z.likeBLOCK

flush(stderr()); flush(stdout())

### Name: Z.likeBLOCK
### Title: Likelihood functions for cvm estimation
### Aliases: Z.like2D, Z.likeBLOCK Z.likeBlock

### ** Examples

# Simulate some 1-D data

nu <- 10
tau <- 3
v0 <- 20
mycvm <- CVM2(nu, tau, T = cumsum(rexp(50)), v0)
X <- Re(myvm$Z)
T <- mycvm$T

# Obtain and plot marginal likelihoods with correct values

LikelihoodScan(X,T,nu,tau,v0,Z.like1)
LikelihoodScan(X,T,nu,tau,v0,Z.like2, k=5)

# Obtain MLE

nu <- 3; tau <- 0.5; v0 <- 0
T <- cumsum(rexp(50))
Z <- Re(CVM2(T, nu, tau, v0)$Z)
plot(T,Z, type="o")

optim(c(1,1), Z.likelihood, Z=Z, T=T, control=list(fnscale=-1))



cleanEx()
nameEx("Z.likelihood")
### * Z.likelihood

flush(stderr()); flush(stdout())

### Name: Z.likelihood
### Title: Likelihood of 1-D Z estimation
### Aliases: Estimate, MinMe, RightSide Z.likelihood

### ** Examples

# Obtain marginal likelihoods of the parameters
nu <- 10
tau <- 3
v0 <- 20
T <- cumsum(rexp(50))
Z <- Re(CVM2(nu, tau, T, v0)$Z)

nus <- seq(nu/3,3*nu, length=30)
ll.nu <- c()
for(mynu in nus)
  ll.nu <- c(ll.nu, Z.likelihood(c(mynu,tau,v0),Z,T))

taus <- seq(tau/5,5*tau,length=30)
ll.tau <- c()
for(mytau in taus)
  ll.tau <- c(ll.tau, Z.likelihood(c(nu,mytau,v0),Z,T))

v0s <- seq(-v0,5*v0,length=30)
ll.v0 <- c()
for(myv0 in v0s)
  ll.v0 <- c(ll.v0, Z.likelihood(c(nu,tau,myv0),Z,T))

# Plot the marginal likelihoods

par(bty="n", pch=19, mfrow=c(1,3))
plot(nus, ll.nu, type="l"; abline(v=nu, col=2, lwd=3, lty=3); abline(v=nus[ll.nu == max(ll.nu)], col=3, lwd=2, lty=2)

plot(taus, ll.tau, type="l", ylab=""); abline(v=tau, col=2, lwd=3, lty=3); abline(v=taus[ll.tau == max(ll.tau)], col=3, lwd=2, lty=2)

plot(v0s, ll.v0, type="l", ylab=""); abline(v=v0, col=2, lwd=3, lty=3); abline(v=v0s[ll.v0 == max(ll.v0)], col=3, lwd=2, lty=2)

# Obtain MLE

nu <- 3; tau <- 0.5; v0 <- 0
T <- cumsum(rexp(50))
Z <- Re(CVM2(T, nu, tau, v0)$Z)
plot(T,Z, type="o")

optim(c(1,1), Z.likelihood, Z=Z, T=T, control=list(fnscale=-1))



graphics::par(get("par.postscript", pos = 'CheckExEnv'))
cleanEx()
nameEx("cvm-package")
### * cvm-package

flush(stderr()); flush(stdout())

### Name: cvm-package
### Title: What the package does (short line) ~~ package title ~~
### Aliases: cvm-package cvm
### Keywords: package

### ** Examples

~~ simple examples of the most important functions ~~



cleanEx()
nameEx("estimateCVM")
### * estimateCVM

flush(stderr()); flush(stdout())

### Name: estimateCVM
### Title: Estimating parameters of CVM
### Aliases: estimateCVM

### ** Examples

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




cleanEx()
nameEx("getCov.vz")
### * getCov.vz

flush(stderr()); flush(stdout())

### Name: getCov.vz
### Title: Var-Cov block of 1-D V and Z
### Aliases: getCov.vz getCov.zz

### ** Examples

getCov.vz(t1=5,t2=5, nu=2, tau=10)
getCov.vz(t1=5,t2=10, nu=2, tau=10)
getCov.vz(t1=95,t2=100, 2,10)



cleanEx()
nameEx("getSigma.VZ")
### * getSigma.VZ

flush(stderr()); flush(stdout())

### Name: getSigma.VZ
### Title: Obtain 1D complete VV-VZ-ZZ or ZZ covariance matrix
### Aliases: getSigma.VZ getSigma.ZZ

### ** Examples

require(gridExtra)
T <- cumsum(rexp(10))
# separated into Sigma_vv, Sigma_vz (assymetric), and Sigma_zz
Sigma <- Matrix(getSigma.VZ(T, nu=2, tau=5, v0=2))
i1 <- image(Sigma[1:10,1:10], main="V-V", colorkey=TRUE, useRaster=TRUE)
i2 <- image(Sigma[11:20,1:10], main="V-Z", colorkey=TRUE, useRaster=TRUE)
i3 <- image(Sigma[11:20,11:20], main="Z-Z", colorkey=TRUE, useRaster=TRUE)
grid.arrange(i1,i2,i3,ncol=3)
# And the complete matrix (log transformed):
image(log(Sigma), colorkey=TRUE, main="log(Sigma)")



cleanEx()
nameEx("getcov.zz")
### * getcov.zz

flush(stderr()); flush(stdout())

### Name: getcov.zz
### Title: Obtain 1D complete S.zz matrix
### Aliases: getcov.zz

### ** Examples

require(gridExtra)
T <- cumsum(rexp(10))
# separated into Sigma_vv, Sigma_vz (assymetric), and Sigma_zz
Sigma <- Matrix(getSigma.VZ(T, nu=2, tau=5, v0=2))
i1 <- image(Sigma[1:10,1:10], main="V-V", colorkey=TRUE, useRaster=TRUE)
i2 <- image(Sigma[11:20,1:10], main="V-Z", colorkey=TRUE, useRaster=TRUE)
i3 <- image(Sigma[11:20,11:20], main="Z-Z", colorkey=TRUE, useRaster=TRUE)
grid.arrange(i1,i2,i3,ncol=3)
# And the complete matrix (log transformed):
image(log(Sigma), colorkey=TRUE, main="log(Sigma)")



### * <FOOTER>
###
options(digits = 7L)
base::cat("Time elapsed: ", proc.time() - base::get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')
