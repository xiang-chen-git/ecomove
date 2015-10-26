rm(list=ls())


Wolf <- read.csv("./datacsv/Wolf.csv")
head(Wolf)
Wolf$Time <- as.POSIXlt(Wolf$Time, format = "%Y-%m-%d %H:%M")
diff(Wolf$Time)
Wolf <- Wolf[which(diff(Wolf$Time) !=0) +1,]
Wolf <- Wolf[which(!is.na(diff(Wolf$Time))) +1,]
dim(Wolf)

class(Wolf) <- c('data.frame', 'track')
save(Wolf, file="./waddle/data/Wolf.rda")

data(Wolf)
data(Lamprey)
data(Lamprey1)
data(Lamprey2)
data(Lamprey3)
data(WFR)




UpdateTrack <- function(foo)
paste("data(",foo,"); class(",foo,") <- c('data.frame','track'); save(",foo, ", file='./waddle/data/WFR.rda')", sep="")



for(foo in c("Wolf", "WFR", "Lamprey", "Lamprey1", "Lamprey2", "Lamprey3"))
try(eval(parse(text = UpdateTrack(foo))))

class(Wolf)
class(Lamprey3)


## Need to remove stupid daylight savings hour in wolf:
wd <- ("c:/eli/programming/ecomove/waddle")
setwd(wd)
data(Wolf)
is(Wolf$Time)
attr(Wolf$Time, "names")
t <- Wolf$Time
t <- as.POSIXlt(as.character(Wolf$Time))
Wolf$Time <- t
range(t)
save(Wolf, file = "./waddle/data/Wolf.rda")

## Is there this problem in WFR?
data(WFR)
range(WFR$Time)