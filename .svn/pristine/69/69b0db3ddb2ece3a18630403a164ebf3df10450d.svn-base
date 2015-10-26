#ComplexDot <- function(a,b)
#  return(Re(a*Conj(b)) + Im(a*Conj(b)))

ComplexDot <- function(a,b)
  Re(b)*(Re(a) + Im(a)) - Im(b)*(Re(a)-Im(a))

getVaf <- function(lag, V)
  mean.default(ComplexDot(V[-(1:lag)],V[-((length(V)-lag+1):length(V))]))/mean.default(Mod(V)^2)


