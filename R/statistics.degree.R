#' Degree statistics
#'
#' Compute in/out/total degree sender and receiver effects, normalized or unnormalized
#'
#' @param data Array (E x 3) of events
#'
#' @return Array of E x S x R
#' @name degreestats
NULL

#' @rdname degreestats
#' @export
IDSnd<-function(data){
  CovRec(event.count,data=data)
}
#' @rdname degreestats
#' @export
IDRec<-function(data)
  CovRec(event.count,data=data,effects='recvingrate')
#' @rdname degreestats
#' @export
ODSnd<-function(data)
  CovSnd(event.count,data=data)
#' @rdname degreestats
#' @export
ODRec<-function(data)
  CovSnd(event.count,data=data,effects='recvingrate')
#' @rdname degreestats
#' @export
TDSnd<-function(data)
  CovInt(event.count,data=data)
#' @rdname degreestats
#' @export
TDRec<-function(data)
  CovInt(event.count,data=data,effects='recvingrate')
#' @rdname degreestats
#' @export
NIDSnd<-function(data)
  CovRec(event.count,data=data,normalize=TRUE)
#' @rdname degreestats
#' @export
NIDRec<-function(data)
  CovRec(event.count,data=data,effects='recvingrate',normalize=TRUE)
#' @rdname degreestats
#' @export
NODSnd<-function(data)
  CovSnd(event.count,data=data,normalize=TRUE)
#' @rdname degreestats
#' @export
NODRec<-function(data)
  CovSnd(event.count,data=data,effects='recvingrate',normalize=TRUE)
#' @rdname degreestats
#' @export
NTDSnd<-function(data)
  CovInt(event.count,data=data,normalize=TRUE)
#' @rdname degreestats
#' @export
NTDRec<-function(data)
  CovInt(event.count,data=data,effects='recvingrate',normalize=TRUE)
