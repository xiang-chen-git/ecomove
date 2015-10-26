# PLOT PATHS
	load(file="./results/WolfAnalysis.robj")
	par(mfrow=c(2,2), mar=c(0,0,0,0), oma=c(4,4,2,2), tck=0, mgp = c(2,.5,0))
	
# plot bcpa results
		PathPlot(Data, Data.ws, type="smooth", plotlegend=TRUE,  tauwhere="bottomleft", ncol.legend = 2, axes=FALSE)
		MakeLetter("a)")
		axis(2)
		PathPlot(Data, Data.ws, type="flat", clusterwidth = 3, ylab="", yaxt="n", axes=FALSE, plotlegend=FALSE, ncol.legend = 2)
		MakeLetter("b)")

# plot segmentation results
		PathPlot.segments(Data.segments, lwd=1, xaxt="n", xlab="", ylab="",  where="bottomleft", cex=1, n.dot=6, ncol=2, bty="n")
		MakeLetter("c)")
		axis(1)
# plot fpt output
		PathPlot.fpt(Data.traj, 50, where="bottomleft", n.dots = 4, scale=1/3, k=2, ncol=2)
		MakeLetter("d)")
		axis(1)
		