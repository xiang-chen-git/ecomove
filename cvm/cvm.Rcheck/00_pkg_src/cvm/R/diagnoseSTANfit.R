#' Diagnose STAN fit
#' 
#' Used internally in \code\link{estimateCVM.fullL}
#' 
#' @details provides diagnosis plots of chain convergence and posterior density histograms for nu and tau parameters as fitted by STAN

diagnoseSTANfit <- function(STANfit, iter)
{
  layout(rbind(c(1,1,2),c(3,3,4)))
  par(bty="l")
  tau.sim <- STANfit@sim$samples[[1]]$tau
  nu.sim <- STANfit@sim$samples[[1]]$nu
  
  p <- c(0.025, 0.25, 0.5, 0.75, 0.975)
  
  plot(tau.sim, type="l", main="tau chain")
  hist(tau.sim[50:iter], col="grey", bor="darkgrey", main="95 and 50% C.I.")
  abline(v=quantile(tau.sim[50:iter], p), lty=c(3,2,1,2,3), col=2, lwd=2)
  plot(nu.sim, type="l", main="nu chain")
  hist(nu.sim[50:iter], col="grey", bor="darkgrey", main="95 and 50% C.I.")
  abline(v=quantile(nu.sim[50:iter], p), lty=c(3,2,1,2,3), col=2, lwd=2)
}
