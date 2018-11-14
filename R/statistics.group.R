#' Group statistics
#' @name groupstats
#' @param data data file
#' @param covar covariates

#' @rdname groupstats
#' @export
SameConstGroup <- function(data,covar) {
  CovInt(function(X,s,r,t){same.const.cov(X,s,r,t,covar)},data=data,time.varying = FALSE)
}
#' @rdname groupstats
#' @export
DiffConstGroup <- function(data,covar) {
  CovInt(function(X,s,r,t){diff.const.cov(X,s,r,t,covar)},data=data,time.varying = FALSE)
}
