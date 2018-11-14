#' Format event statistics of different types
#'
#' These functions sweep out statistics to construct constant effects,
#' sender effects, receiver effects, dyadic effects, and unordered pair effects.
#'
#' @param stat.func A function used to compute statistics
#' @param stats An array containing pre-computed statistics
#' @param data ......
#' @param ... ......
#' @param time.varying Boolean value indicating whether statistic should be
#'  computed at each time point
#'
#' @return An array of statistics of dimension (S x R x E).
#' @name basestats
NULL

#' @rdname basestats
#' @export
Constant <- function(data)
  rem.stat(function(X,s,r,t){1},data=data,time.varying=FALSE,dyadic=FALSE)

#' @rdname basestats
#' @export
CovSnd <- function(stat.func=NULL,stats=NULL, data=NULL, time.varying=TRUE,...){
  rem.stat(stat.func,stats,data, time.varying = time.varying, dyadic=FALSE,sender=TRUE, receiver=FALSE,...)
}
#' @rdname basestats
#' @export
CovRec <- function(stat.func=NULL,stats=NULL,data=NULL,time.varying=TRUE,...)
  rem.stat(stat.func,stats,data, time.varying = time.varying, dyadic=FALSE,sender=FALSE, receiver=TRUE,...)

#' @rdname basestats
#' @export
CovEvent <- function(stat.func=NULL,stats=NULL,data=NULL,time.varying=TRUE,...)
  rem.stat(stat.func,stats,data, time.varying = time.varying, dyadic=TRUE,...)

#' @rdname basestats
#' @export
CovInt <- function(stat.func=NULL,stats=NULL,data=NULL,time.varying=TRUE,...)
  rem.stat(stat.func,stats,data, time.varying = time.varying, dyadic=TRUE, sender=TRUE, receiver=TRUE, either=TRUE,...)
