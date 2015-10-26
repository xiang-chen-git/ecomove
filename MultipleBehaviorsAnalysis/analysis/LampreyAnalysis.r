rm(list=ls())

setwd("c:/eli/programming/ecomove/MultipleBehaviorsAnalysis/analysis")
	
require(waddle)

	data(Lamprey)
	Data <- Lamprey

## smooth 
  Smooth <- function(X) rowMeans(cbind(X[-(1:2)], X[-c(1,length(X))], X[-c(length(X)-1, length(X))]))	
	L2 <- data.frame(Time = as.POSIXlt(Smooth(Data$Time), origin = "1970-01-01 00:00.00 UTC", tz="America/Detroit"),	X = Smooth(Data$X), 	Y = Smooth(Data$Y),	Z  = Smooth(Data$Z))

## @knitr 
	DataRaw.traj <- as.ltraj(data.frame(Data$X, Data$Y),as.POSIXct(Data$Time), id = "Data")
	DataSmooth.traj <- as.ltraj(data.frame(L2$X, L2$Y),as.POSIXct(L2$Time), id = "Data")

## @knitr FPTexample,  fig.width=9, fig.height=6, out.width="\\textwidth", size="small", echo=-1
  par(mfrow=c(2,3))
  r <- c(5,10,20)
  DataRaw.fpt <- fpt(DataRaw.traj, r)
  DataSmooth.fpt <- fpt(DataSmooth.traj, r)
	Data.fpt <- DataSmooth.fpt
	
## @knitr FPTpath, fig.width=5, fig.height=4, out.width="0.6\\textwidth", size="small", echo=-1
par(mar = c(0,0,0,0))
PathPlot.fpt(DataSmooth.traj, 20, scale=0.1, k=2)

## @knitr BPMMexample,  fig.width=8, fig.height=4, out.width="0.7\\textwidth", size="small", echo=-1
  par(bty='l', mar=c(4,4,1,1))
  plot(DataSmooth.traj[[1]]$date, DataSmooth.traj[[1]]$dist, xlab="", ylab="Distance (m)", type="l", col="darkgrey")
  points(DataSmooth.traj[[1]]$date, DataSmooth.traj[[1]]$dist, pch=19, cex=0.5)

## @knitr BPMM.likelihoods, fig.height=4, out.width="0.7\\textwidth", size="small", echo=-1
  par(mar = c(4,4,1,1), bty="l")
  Data.reg <- InterpolatePoints(Data, 60)$Data
  Data.traj <- as.ltraj(data.frame(Data.reg$X, Data.reg$Y),Data.reg$Time, id = "Data")
  Data.segments <- PrepSegments(Data.traj, units="min", dt = 1, sd=5, nmodels=20)
	
## @knitr 
  Data.VT <- GetVT(Data, units = "min")

## @knitr DataBCPA, cache=TRUE
	Data.ws <- WindowSweep(Data.VT, "V*cos(Theta)", windowsize=30, plotme = FALSE, K = 0.5)

## @knitr BCPA

	par(mfrow=c(2,1), mar=c(0,4,0,1), oma=c(4,0,1,0), bty="l")
	plot(Data.ws, type="flat", clusterwidth = 1, legend=FALSE, xaxt="n", pt.cex=1, ylab=expression(V * cos(theta)))
	MakeLetter("Flat BCPA", "top")
	plot(Data.ws, type="smooth", threshold = 5, legend=FALSE, pt.cex=1, ylab=expression(V * cos(theta)), col.cp = rgb(0.5,0,0.5,.3)); MakeLetter("Smooth BCPA", "top")

## Load MRW resutls

	Data.mrw <- CompileResults.mrw("Lamprey")
	save(Data, Data.fpt, Data.segments, Data.ws, Data.traj, Data.mrw, file="./results/LampreyAnalysis.robj")
