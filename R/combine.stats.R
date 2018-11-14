#' Format Event Network Statistics
#'
#' Takes network statistics as arguments and combines them into a
#' 4 dimensional array
#'
#' @param ... Named arguments with statistics
#'
#' @return A 4 dimensional array of statistics
#' @import abind
#' @export
combine.stats <- function(...){
  abind(...,along=2.5)
}
