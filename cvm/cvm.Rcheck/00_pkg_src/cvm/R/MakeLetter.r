#' Make Letter 
#' 
#' Quick and dirty figure annotator.
#' 
#' @details Annotate a figure with any expression - basically just the title of an empty, boxless legend. 
#' @param text text to add (e.g. "a.", "b.", etc.)
#' @param where one of "topleft", "top", "topright", etc.
#' @param cex Size expansion of text.
#' @param ... additional parameters to pass to \code{\link{legend}}
#' @return NULL

MakeLetter <- function(text, where="topleft", cex=2, ...)
{
  legend("topleft", title=text, cex=cex, pch=NA, legend=NA, bty="n", ...)
  return(NULL)
}
