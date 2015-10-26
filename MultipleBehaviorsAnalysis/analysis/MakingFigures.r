# Plot all results

setwd("c:/eli/programming/ecomove/MultipleBehaviorsAnalysis/analysis")

pdf("./plots/LampreyResults.pdf", width=10)
source("PlotLampreyResults.r")
dev.off()

pdf("./plots/WolfResults.pdf", width=10)
source("PlotWolfResults.r")
dev.off()

pdf("./plots/WFRResults.pdf", width=10)
source("PlotWFRResults.r")
dev.off()

# Plot three tracks
pdf("./plots/ThreeTracks.pdf", width=10, height=4)
par(mfrow=c(1,3), bty="l", cex.lab=1.25, mar=c(5,5,2,2))
data(Lamprey); plot(Lamprey, xlab="X (m)", ylab="Y (m)"); MakeLetter("a)")
data(Wolf); plot(Wolf, xlab="X (km)", ylab="Y (km)"); MakeLetter("b)")
data(WFR); plot(WFR, xlab="X (km)", ylab="Y (km)"); MakeLetter("c)")
dev.off()
