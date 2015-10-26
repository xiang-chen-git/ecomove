require(waddle)

# subsample reindeer to daily time step

data(WFR)
WFR2 <- data.frame(X = tapply(WFR$X, substr(WFR$Time, 1, 10), mean),
										Y = tapply(WFR$Y, substr(WFR$Time, 1, 10), mean))
WFR2$Time <- as.POSIXct(row.names(WFR2))

Data <- WFR2
Data.VT <- GetVT(Data, units = "day")

# Perform first passage time analysis

	Data.traj <- as.ltraj(data.frame(Data$X, Data$Y),as.POSIXct(Data$Time, origin = "1970-01-01"), id = "WFR")
	
	# Using scaling radius of 50 km
	r <- 50
	Data.fpt <- fpt(Data.traj, r)[[1]]
	plot(fpt(Data.traj, r), r)
	
# Gueguen segmentation 

	plotltr(Data.traj, "dist")
	Data.segments <- PrepSegments(Data.traj, units="day", dt = 1, sd=3, Km=50, nmodels=10, log=TRUE)
	Data.segments$pm <- PlotSegments(Data.segments, 37)
	plot(Data.segments$pm)
	
# Perform BCPA

	windowsize <- 20
	windowstep <- 1
	K <- 1
	system.time(Data.ws <- WindowSweep(Data.VT, "V*cos(Theta)", windowsize, windowstep, plotme = FALSE, K = K, tau=TRUE, progress=TRUE))
	
	par(mfrow=c(2,1))
	plot(Data.ws, type="smooth", threshold = 3, truetime=TRUE, legend=FALSE)
	plot(Data.ws, type="flat", clusterwidth = 4, truetime=TRUE, legend=FALSE)

	DiagPlot(Data.ws, "smooth")
	DiagPlot(Data.ws, "flat")

	PartitionParameters(Data.ws, phaseplot=TRUE)
	plot(PartitionParameters(Data.ws, phaseplot=FALSE))


setwd("c:/eli/research/MovementProjects/Gurarie/waddle")
save(Data, Data.traj, Data.fpt, Data.segments, Data.ws, file="./results/WFRAnalysis.robj")
