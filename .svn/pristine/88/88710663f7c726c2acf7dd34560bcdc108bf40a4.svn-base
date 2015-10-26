	load(file="./results/WFRAnalysis.robj")
	par(mfrow=c(5,1), mar=c(0,5,0,0), oma=c(5,1,2,2), cex.lab=1.25)

# FPT
	r <- 20
	Data.fpt <- fpt(Data.traj, r)[[1]]
	att <- attr(Data.fpt, "radii")
	plot(attr(Data.fpt, "date"), Data.fpt[, 1],  
						ylab = "20 km FPT", xaxt="n")
	lines(attr(Data.fpt, "date"), Data.fpt[, 1])
	MakeLetter("a)")

# BPMM
	PlotSegments(Data.segments, ylab="Step distance (km)", xaxt="n")
	MakeLetter("b)")
	
# BCPA
	plot(Data.ws, type="smooth", threshold = 3, legend=FALSE, xaxt="n", pt.cex=1, ylab=expression(V * cos(theta)), col.cp = rgb(0.5,0,0.5,.3))
	MakeLetter("c)")

	plot(Data.ws, type="flat", clusterwidth = 3, legend=FALSE, xaxt="n", xlab="", pt.cex=1, ylab=expression(V * cos(theta)), col.cp = rgb(0.5,0,0.5,.3))
	MakeLetter("d)")

#MRW
	Data.mrw <- CompileResults.mrw("WFR")
	cols <- c("antiquewhite", "dodgerblue", "blue")
	plot.mrw(Data.mrw, cols = cols, v.col="darkred", v.lab=FALSE, ylab="Model", xlab="Time", bg="red", cex=0.5, xlim=range(WFR$Time)) 
	MakeLetter("e)")
