#' Primitive functions for computing statistics.
#'
#' Each function takes four or more arguments, a dataset, a sender index, a receiver
#' index, and a time. These are meant to be used in conjunction with sweeping functions
#' to generate appropriate statistics arrays. These functions are general purpose.
#' They are intended to work for sender, receiver, dyadic, and unordered pair effects.
#' \code{time_since} returns the time since event last occurred.
#' \code{events_since} returns the number of events since event last occurred.
#' \code{event.count} returns the number of times this event has already occurred.
#' \code{fixed.effect} returns the indicator for whether event at time t has sender s and receiver r.
#' \code{alter.count} returns the number of alters of a certain type.
#' \code{outbound.path.count} returns a count of the number of outbound transitive paths.
#' \code{inbound.path.count} returns a count of the number of inbound transitive paths.
#' \code{outbound.shared.partner.count} returns a count of the number of outbound shared partners.
#' \code{inbound.shared.partner.count} returns a count of the number of inbound shared partners.
#' \code{pshift} returns the indicator for whether a pshift has occurred.
#'
#' @param X An Ex3 matrix of values where the first column is the event
#'   sender, the second column is the event receiver, and the last column is the
#'   time
#' @param s The sender index. Use NA to ignore
#' @param r The reciever index. Use NA to ignore
#' @param t time
#' @param sndtype Type of alter of first node ('rec'/'snd')
#' @param rectype Type of alter of second node ('snd'/'rec')
#' @param s2 Sender index of second dyad. Use NA to ignore.
#' @param r2 Receiver index of second dyad. Use NA to ignore.
#' @param ordinal Logical. Whether recency is calculated in number of events or time. Not implemented
#'
#'
#' @return A scalar statistic for the given sender/receiver/dyad at time t
#' @name remstats
NULL

#' @rdname remstats
time_since <- function(X,s=NA,r=NA,t){
  k <- X[,3]<t
  if(!is.na(s))
    k <- k & X[,1]==s
  if(!is.na(r))
    k <- k & X[,2]==r
  if(sum(k)==0){
    time <- 0
  }else{
    time <- max(X[k,3])
  }
  t-time
}
#' Time since event last occured
#' @rdname remstats
events_since <- function(X,s=NA,r=NA,t){
  now <- which(X[,3]==t)
  k <- X[,3]<t
  if(!is.na(s))
    k <- k & X[,1]==s
  if(!is.na(r))
    k <- k & X[,2]==r
  if(sum(k)==0){
    time <- 0
  }else{
    time <- max(which(k))
  }
  now-time
}
#' @rdname remstats
event.count <- function(X,s=NA,r=NA,t){
  k <- X[,3]<t
  if(!is.na(s))
    k <- k & X[,1]==s
  if(!is.na(r))
    k <- k & X[,2]==r
  sum(k)
}
#' @rdname remstats
fixed.effect <- function(X,s=NA,r=NA,t){
  k <- X[,3]==t
  if(!is.na(s))
    k <- k & X[,1]==s
  if(!is.na(r))
    k <- k & X[,2]==r
  (sum(k)>0)*1
}
#' @rdname remstats
alter.count <- function(X,s,r,t,sndtype='rec',rectype='snd'){
  if(sndtype=='rec'){
    snd.col <- 1
    snd.alter.col <- 2
  }
  if(sndtype=='snd'){
    snd.col <- 2
    snd.alter.col <- 1
  }
  if(rectype=='rec'){
    rec.col <- 1
    rec.alter.col <- 2
  }
  if(rectype=='snd'){
    rec.col <- 2
    rec.alter.col <- 1
  }
  k <- X[,3]<t
  ks <- k & X[,snd.col]==s
  kr <- k & X[,rec.col]==r
  alters <- intersect(X[ks,snd.alter.col],X[kr,rec.alter.col])
  length(alters)
}
#' @rdname remstats
outbound.path.count <- function(X,s,r,t)
  alter.count(X,s,r,t,sndtype='rec',rectype='snd')
#' @rdname remstats
inbound.path.count <- function(X,s,r,t)
  alter.count(X,s,r,t,sndtype='snd',rectype='rec')
#' @rdname remstats
outbound.shared.partner.count <- function(X,s,r,t)
  alter.count(X,s,r,t,sndtype='rec',rectype='rec')
#' @rdname remstats
inbound.shared.partner.count <- function(X,s,r,t)
  alter.count(X,s,r,t,sndtype='snd',rectype='snd')
#' @rdname remstats
pshift <- function(X,s,r,t,s2=NA,r2=NA,ordinal=TRUE){
  k <- which(X[,3]==t)
  if(k==1)
    return(0)
  prevk <- k-1
  out <- 1
  if(!(X[prevk,1]==s & X[prevk,2]==r)){
    return(0)
  }
  if(!is.na(s2))
    out <- out*(X[prevk+1,1]==s2)
  if(!is.na(r2))
    out <- out * (X[prevk+1,2]==r2)
  if(ordinal){
    out
  }else{
    X[prevk,3]-t
  }
}
same.const.cov <- function(X,s,r,t,covar)
  (covar[s]==covar[r])*1
diff.const.cov <- function(X,s,r,t,covar)
  (covar[s]!=covar[r])*1
