#' Participation shift event statistics
#'
#' Compute pshift effects
#'
#' @param data Array (E x 3) of events
#'
#' @return Array of E x S x R
#' @name pshiftstats
NULL
#' @rdname pshiftstats
#' @export
PSAB.BA <- function(data){
  CovEvent(function(X,s,r,t){pshift(X,s,r,t,s2=r,r2=s,ordinal=TRUE)},data=data)
}
#' @rdname pshiftstats
#' @export
PSAB.BY <- function(data){
  CovEvent(function(X,s,r,t){pshift(X,s,r,t,s2=r,r2=NA,ordinal=TRUE)},data=data)
}
#' @rdname pshiftstats
#' @export
PSAB.XA <- function(data) {
  CovEvent(function(X,s,r,t){pshift(X,s,r,t,s2=NA,r2=s,ordinal=TRUE)},data=data)
}
#' @rdname pshiftstats
PSAB.XB <- function(data) {
  CovEvent(function(X,s,r,t){pshift(X,s,r,t,s2=NA,r2=r,ordinal=TRUE)},data=data)
}
#' @rdname pshiftstats
#' @export
PSAB.XY <- function(data) {
  CovEvent(function(X,s,r,t){pshift(X,s,r,t,s2=NA,r2=NA,ordinal=TRUE)},data=data)
}
#' @rdname pshiftstats
#' @export
PSAB.AY <- function(data){
  CovEvent(function(X,s,r,t){pshift(X,s,r,t,s2=s,r2=NA,ordinal=TRUE)},data=data)
}
