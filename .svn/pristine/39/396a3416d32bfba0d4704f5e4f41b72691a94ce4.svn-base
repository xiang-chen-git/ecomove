require(plyr)

Lraw <- read.csv("./datacsv/raw/Lamprey11-12RAW.csv")
t <- Lraw$DATETIME
t <- as.POSIXlt(t, tz="America/Detroit") - 4*60*60
Lamprey <- data.frame(Time = t, X = Lraw$Easting, Y = Lraw$Northing, Depth = Lraw$DEPTH)
Lamprey1 = Lamprey
save(Lamprey1, file = "./waddle/data/Lamprey1.rda")

Lraw <- read.csv("./datacsv/raw/Lamprey10-28RAW.csv")
t <- Lraw$DATETIME
t <- as.POSIXlt(t, tz="America/Detroit") - 4*60*60
Lamprey <- data.frame(Time = t, X = Lraw$X, Y = Lraw$Y, Depth = Lraw$DEPTH)
Lamprey2 = Lamprey
save(Lamprey2, file = "./waddle/data/Lamprey2.rda")

Lraw <- read.csv("./datacsv/raw/Lamprey10-27RAW.csv")
t <- Lraw$DATETIME
t <- as.POSIXlt(t, tz="America/Detroit") - 4*60*60
Lamprey <- data.frame(Time = t, X = Lraw$X, Y = Lraw$Y, Depth = Lraw$DEPTH)
Lamprey3 = Lamprey
save(Lamprey3, file = "./waddle/data/Lamprey3.rda")


Lamprey <- read.csv("./datacsv/Lamprey.csv")
Lamprey <- Lamprey[Lamprey$Z!=5.0,]
Lamprey <- Lamprey[-nrow(Lamprey),]
Lamprey <- ddply(Lamprey, .(Time), function(x) x[1,])
Lamprey$Time <- as.POSIXlt(Lamprey$Time, tz="America/Detroit") - 4 * 60 * 60
Lamprey$Depth <- Lamprey$Z
Lamprey$Z <- Lamprey$X + 1i * Lamprey$Y
save(Lamprey, file = "./waddle/data/Lamprey.rda")

Wolf <- read.csv("./datacsv/Wolf.csv")
Wolf$Time <- as.POSIXlt(Wolf$Time)
Wolf <- Wolf[-which(diff(Wolf$Time) == 0),]
save(Wolf, file = "./waddle/data/Wolf.rda")



WFRraw <- read.csv("./datacsv/WFR1raw.csv", comment="#")
t <- WFRraw$GMT_date
t <- as.POSIXlt(t, tz="GMT")

WFR <- data.frame(Time = t, X = WFRraw$Locale_E/1000, Y = WFRraw$Locale_N/1000)
head(WFR)
save(WFR, file = "./waddle/data/WFR.rda")

WFR <- data.frame(

#WFR$Time <- as.POSIXlt(WFR$Time)
save(WFR, file = "./waddle/data/WRF.rda")



#######################
# PROCESS OWL DATA

Owl.raw <- read.csv("./datacsv/raw/OwlRAW.csv")
Owl.raw$ID <- paste("Owl",Owl.raw$owl,sep="")
head(Owl.raw)

topowls <- names(sort(table(Owl.raw$ID), decreasing=TRUE))[1:6]
O <- Owl.raw[which(Owl.raw$ID %in% topowls),]

Owl <- data.frame(ID = O$ID,
                  Sex = c("M","F")[O$sex],
                  Phase = relevel(factor(c("Wandering", "Settling")[O$phase]), ref="Wandering"),
                  Date = as.POSIXlt(O$date),
                  X = O$XP0_D,
                  Y = O$YP0_D,
                  X.nest = O$X_nest,
                  Y.nest = O$Y_nest)
save(Owl, file = "./waddle/data/Owl.rda")

require(plyr)

Owl$N <- rep(table(Owl$ID), table(Owl$ID))
Owl$LABEL <-  factor(paste(Owl$ID, " (n = ", Owl$N, ")", sep=""))
require(ggplot2)
qplot(X/1000, Y/1000, geom="path", facets = ~ LABEL, data=Owl, asp=1, col=Phase ,xlab="X (km)", ylab="Y (km)")




a <- read.table("clipboard", header=TRUE)
qplot(a[,1], a[,2], geom="line")

GetVT <-
  function(Data, units = "hour")
  {
    Z.start <- Data$Z[-nrow(Data)]
    Z.end <- Data$Z[-1]
    S <- Mod(diff(Data$Z))
    Phi <- Arg(diff(Data$Z))
    Theta <- c(NA, diff(Phi))  
    
    if(inherits(Data$Time, "POSIXt"))
    {  
      Data$Time <- as.numeric(Data$Time-Data$Time[1])
      Data$Time <- Data$Time/ifelse(units == "sec", 1, 
                                    ifelse(units == "min", 60, 
                                           ifelse(units == "hour", 60*60, 
                                                  ifelse(units == "day", 60*60*24, 
                                                         stop("Invalid time unit.")))))
    }
    
    T.start <- Data$Time[-nrow(Data)]
    T.end <- Data$Time[-1]
    dT <- T.end-T.start
    V <- S/as.vector(dT)
    
    T.mid <- (T.start + T.end)/2
    VT.table <- data.frame(Z.start, Z.end, S, Phi, Theta, T.start, T.end, T.mid, dT, V)
    VT.table[-1,]
  }

package.skeleton("waddle")

# save(Lamprey, file="./metamovement/data/Lamprey.rda")
# save(Wolf, file="./metamovement/data/Wolf.rda")
# save(WFR, file="./metamovement/data/WFR.rda")


