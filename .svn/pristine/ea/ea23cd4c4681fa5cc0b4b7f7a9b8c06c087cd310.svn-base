##' Compile Multi-state random walk results
##' 
##' Loads the results of the analysis from the \code{mrw} package
##' 
##' @param animal a character string with the animal name (must be one of the available results of the mrw analyses).
##' @param flip which of the chains to reassign - can be a vector including up to 1:3.  The Double and DoubleSwitch chain are flipped easily, whereas ...
##' @param tripleflip is a vector with 2 or 3 elements that determines which of the three-state chains should be flipped.  If the vector has three elements , then the first two are flipped, and then the second and the third, so: "1,2,3" change an "ABC" first to a "BAC" and then to a "CAB".  
##' 
##' @return a data frame with the following columns: \item{Time}{POSIX Time} \item{Z1}{initial location} \item{Z2}{next location} \item{V}{Speed of step} \item{Double}{Mode of states (1 or 2) in the converged chain for the double state model.} \item{DoubleSwitch}{Mode of states (1 or 2) in the converged chain for the double state switching model.} \item{TripleSwitch}{Mode of states (1, 2 or 3) in the converged chain for the triple state switching model.}
##' @seealso \code{\link{plot.mrw}}


CompileResults.mrw <- function(animal, flip=NULL, tripleflip = NULL)
{
  
  if(!(animal %in% data(package = "waddle")$results[,3]))
     stop(call="So sorry, there are no results available for this animal.")
  
  # Note that "data(Lamprey.Data)" is from mrw, and is NOT the same thing as "data(Lamprey)"
  
  eval(parse(text=paste0("data(", animal, ".Data)")))
  Data <- eval(parse(text=paste0(animal,".Data")))
  eval(parse(text=paste0("data(",animal,".doubleState)")))
  eval(parse(text=paste0("data(",animal,".doubleStateSwitch)")))
  eval(parse(text=paste0("data(",animal,".tripleStateSwitch)")))

  doubleState <- eval(parse(text=paste0(animal,".doubleState")))
  doubleStateSwitch <- eval(parse(text=paste0(animal,".doubleStateSwitch")))
  tripleStateSwitch <- eval(parse(text=paste0(animal,".tripleStateSwitch")))
  
  Double <- getModeStates(doubleState)
  DoubleSwitch <- getModeStates(doubleStateSwitch)
  TripleSwitch <- getModeStates(tripleStateSwitch)

  if(1 %in% flip)
    Double <- 3-Double
  if(2 %in% flip)
    DoubleSwitch <- 3-DoubleSwitch
  if(3 %in% flip)
  {
    T2 <- TripleSwitch
    T2[TripleSwitch == tripleflip[1]] <- tripleflip[2]
    T2[TripleSwitch == tripleflip[2]] <- tripleflip[1]
    TripleSwitch <- T2
    if(length(tripleflip) == 3)
    {
      T2[TripleSwitch == tripleflip[2]] <- tripleflip[3]
      T2[TripleSwitch == tripleflip[3]] <- tripleflip[2]
    }
    TripleSwitch <- T2
  }
  
  Data.VT <- GetVT(Data)
  
  Results <- data.frame(
    Time = Data.VT$T.POSIX,
    Z1 = Data.VT$Z.start,
    Z2 = Data.VT$Z.end,
    V  = Data.VT$V,
    Double, 
    DoubleSwitch, 
    TripleSwitch)
    
  class(Results) <- c("mrw", "data.frame")
  return(Results)
}

