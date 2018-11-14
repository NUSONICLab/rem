#' Generates fixed effects
#'
#' Fixed sender/receiver/unordered pair effects
#'
#' @param data ...
#'
#' @return Array of E x S x R
#' @name fixedstats
NULL

#' @rdname fixedstats
#' @export
FESnd <- function(data){
  CovSnd( fixed.effect, data=data, time.varying=FALSE)
}
#' @rdname fixedstats
#' @export
FERec <- function(data)
  CovRec( fixed.effect, data=data, time.varying=FALSE)
#' @rdname fixedstats
#' @export
FEInt <- function(data)
  CovInt( fixed.effect, data=data, time.varying=FALSE)
