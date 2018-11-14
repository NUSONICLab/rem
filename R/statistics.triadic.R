#' Triadic event statistics
#'
#' Compute effects of outbound/inbound transitive path/shared path
#' effects on sending
#'
#' @param data Array (E x 3) of events
#'
#' @return Array of E x S x R
#' @name triadstats
NULL
#' @rdname triadstats
#' @export
OTPSnd <- function(data){
  CovEvent(outbound.path.count,data=data)
}
#' @rdname triadstats
#' @export
ITPSnd <- function(data)
  CovEvent(inbound.path.count,data=data)
#' @rdname triadstats
#' @export
OSPSnd <- function(data)
  CovEvent(outbound.shared.partner.count,data=data)
#' @rdname triadstats
#' @export
ISPSnd <- function(data)
  CovEvent(inbound.shared.partner.count,data=data)
