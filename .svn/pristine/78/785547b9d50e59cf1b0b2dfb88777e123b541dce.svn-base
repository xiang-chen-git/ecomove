setwd("c:/eli/programming/mrw")


rm(list=ls())
load("./rawmaterial/LampreyAnalysisRW.robj")
objects <- ls()
newobjects <- paste("Lamprey",objects,sep=".")
eval(parse(text=paste(newobjects, "<-", objects, collapse=";")))
save(list=newobjects, file = "./rawmaterial/LampreyResults.robj")

rm(list=ls())
load("./rawmaterial/WolfAnalysisRW.robj")
objects <- ls()
newobjects <- paste("Wolf",objects,sep=".")
eval(parse(text=paste(newobjects, "<-", objects, collapse=";")))
save(list=newobjects, file = "./rawmaterial/WolfResults.robj")

rm(list=ls())
load("./rawmaterial/WFRAnalysisRW.robj")
objects <- ls()
newobjects <- paste("WFR",objects,sep=".")
eval(parse(text=paste(newobjects, "<-", objects, collapse=";")))
save(list=newobjects, file = "./rawmaterial/WFRResults.robj")


rm(list=ls())
source("./rawmaterial/MRWfunctions.r")
load("./rawmaterial/LampreyResults.robj")
load("./rawmaterial/WolfResults.robj")
load("./rawmaterial/WFRResults.robj")
package.skeleton("mrw")
	