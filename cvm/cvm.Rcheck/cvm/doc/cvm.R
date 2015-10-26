### R code from vignette source 'cvm.Rnw'

###################################################
### code chunk number 1: loading_cvm
###################################################
require(cvm)


###################################################
### code chunk number 2: FirstPlot
###################################################
nu <- 2
tau <- 5
dt <- .1 
cvm <- CVM(nu, tau, Tmax = 1000, dt = dt)
plot(cvm$Z, asp=1, type="l", main = "CVM(2,5)")
title(sub = "0-1000 time units")


