setwd("c:/eli/research/MovementProjects/Gurarie/waddle")

unloadNamespace("waddle")
unloadNamespace("bcpa")
require(waddle)

# subsample wolf to daily time step

data(Wolf)
Wolf2 <- data.frame(X = tapply(Wolf$X, substr(Wolf$Time, 1, 10), mean),
										Y = tapply(Wolf$Y, substr(Wolf$Time, 1, 10), mean))
Wolf2$Time <- as.POSIXct(row.names(Wolf2))

Data <- Wolf2
Data.VT <- GetVT(Data, units = "day")


# Perform first passage time analysis

	Data.traj <- as.ltraj(data.frame(Data$X, Data$Y),Data$Time, id = "Wolf")
	
	# Using scaling radius of 100 km
	r <- 50
	Data.fpt <- fpt(Data.traj, r)[[1]]
	plot(fpt(Data.traj, r), r)
	
# Gueguen segmentation 

	Data.segments <- PrepSegments(Data.traj, units="day", dt = 1, sd=5, Km=30, log=TRUE)
	Data.segments$pm <- PlotSegments(Data.segments)
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

save(Data, Data.fpt, Data.segments, Data.ws, Data.traj, file="./results/WolfAnalysis.robj")
