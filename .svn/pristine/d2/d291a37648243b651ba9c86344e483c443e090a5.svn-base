dmvnorm2 <- function (x, mean, sigma, log = FALSE) 
{
  distval <- Mahalanobis(x, center = mean, cov = sigma)
  logdet <- sum(log(eigen(sigma, symmetric = TRUE, only.values = TRUE)$values))
  logretval <- -(ncol(x) * log(2 * pi) + logdet + distval)/2
  if (log) 
    return(logretval)
  exp(logretval)
}