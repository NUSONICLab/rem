#' Fraction of event statistics
#'
#' Compute effects of fraction of sending or receiving effects on sending
#'
#' @param data Array (E x 3) of events
#'
#' @return Array of E x S x R
#' @name fractionstats
NULL
#' @rdname fractionstats
#' @export
FrSndSnd <- function(data){
  CovEvent(function(X,s,r,t){event.count(X,s,r,t)/event.count(X,s,r=NA,t)},data=data)
}
#' @rdname fractionstats
#' @export
FrRecSnd <- function(data)
  CovEvent(function(X,s,r,t){event.count(X,s,r,t)/event.count(X,s=NA,r,t)},data=data)
