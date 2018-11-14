#' Recency statistics
#'
#' Compute effects of recency of receipt or sending on sending or recieving
#' in terms of time or number of events. Name format:
#' \code{R}[\code{E}vent][recency of \code{rec}/\code{snd}][effect on \code{snd}/\code{rec}]
#'
#' @param data Array (E x 3) of events
#'
#' @return Array of E x S x R
#' @name recencystats
NULL
#' @rdname recencystats
#' @export
RRecSnd <- function(data){
  CovEvent(function(X,s,r,t){-time_since(X,r,s,t)},data=data)
}
#' @rdname recencystats
#' @export
RSndSnd <- function(data)
  CovEvent(function(X,s,r,t){-time_since(X,s,r,t)},data=data)
#' @rdname recencystats
#' @export
RERecSnd <- function(data)
  CovEvent(function(X,s,r,t){-events_since(X,r,s,t)},data=data)
#' @rdname recencystats
#' @export
RESndSnd <- function(data)
  CovEvent(function(X,s,r,t){-events_since(X,s,r,t)},data=data)
