load(file="./results/WFRAnalysis.robj")

	par(mfrow=c(4,1), mar=c(0,5,0,0), oma=c(5,1,2,2), cex.lab=1.25)
		
	plot(Data.ws, type="smooth", threshold = 3, legend=FALSE, xaxt="n", pt.cex=1, ylab=expression(V * cos(theta)), col.cp = rgb(0.5,0,0.5,.3))
	MakeLetter("a)")
	
	plot(Data.ws, type="flat", clusterwidth = 3, legend=FALSE, xaxt="n", xlab="", pt.cex=1, ylab=expression(V * cos(theta)), col.cp = rgb(0.5,0,0.5,.3))
	MakeLetter("b)")
	
	PlotSegments(Data.segments, ylab="Step distance (km)", xaxt="n")
	MakeLetter("c)")
	
	r <- 50
	Data.fpt <- fpt(Data.traj, r)[[1]]
	att <- attr(Data.fpt, "radii")
	plot(attr(Data.fpt, "date"), Data.fpt[, 1],  
						ylab = "50 km FPT", xlab="Time")
	lines(attr(Data.fpt, "date"), Data.fpt[, 1])
	MakeLetter("d)") 
