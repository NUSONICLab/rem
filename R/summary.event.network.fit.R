#' Summarizing Event Network Fits
#'
#'
#' @aliases print.summary.event.network.fit
#' @param object an object of class \code{"event.network.fit"}
#' @param ... ....
#' @return The function \code{\link{summary.event.network.fit}} computes and
#'   returns a list of summary statistics of the fitted REM  given in \code{object}.
#' @method summary event.network.fit
#' @export
summary.event.network.fit <- function(object, ...)
{
  class(object) <- c("summary.event.network.fit", class(object))
  object
}
