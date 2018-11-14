#' Sweep out network statistics
#'
#' Basic function to calculate network statistics and format proper statistic arrays.
#' Compatible with both functions and raw data.
#'
#' @param stat.func Statistic function \code{\link{remstats}}
#' @param stats Raw data array of appropriate dimension: E x S, E x R, S x R, or E x S x R
#' @param data Event data (E x 3)
#' @param time.varying Logical
#' @param dyadic Logical
#' @param sender Logical
#' @param receiver Logical
#' @param either Logical value indicating whether order matters
#' @param effects Character indicates what the statistic effects ('sendingrate' or 'recvingrate').
#'                Transposes array as necessary
#' @param normalize Logical - indicating whether should be zero centered with sd of 1
#'
#' @return Array of size E x S x R
rem.stat <- function(stat.func=NULL, stats=NULL,
                     data,
                     time.varying=TRUE,
                     dyadic=TRUE,
                     sender=TRUE,
                     receiver=FALSE,
                     either=FALSE,
                     effects='sendingrate',
                     normalize=FALSE){
  if(dyadic){
    sender <- TRUE
    receiver <- TRUE
  }
  if(!dyadic & sender)
    receiver<-FALSE

  # Create a statistics function out of supplied stats
  if (is.null(stat.func)){
    if(time.varying){
      if(dyadic){
        stat.func <- function(X,s,r,t){
          stats[t,s,r]
        }
      }else{
        if(sender){
          stat.func <- function(X,s,r,t){
            stats[t,s]
          }
        }else{
          stat.func <- function(X,s,r,t){
            stats[t,r]
          }
        }
      }
    }else{
      if(dyadic){
        stat.func <- function(X,s,r,t){
          stats[s,r]
        }
      }else{
        if(sender){
          stat.func <- function(X,s,r,t){
            stats[s]
          }
        }else{
          stat.func <- function(X,s,r,t){
            stats[r]
          }
        }
      }
    }
  }

  nodes <- sort(unique(c(data[,1],data[,2])))
  num_nodes <- length(nodes)

  stat <- lapply(data[,3],function(t){
    stat0 <- t(sapply(1:num_nodes,function(s){
      if(!sender)
        s <- NA
      sapply(1:num_nodes,function(r){
        if(!receiver)
          r <- NA
        if(either){
          (stat.func(data,s,r,t) + stat.func(data,r,s,t))/2
        }else{
          stat.func(data,s,r,t)
        }
      })
    }))
    if (normalize) stat0 <- (stat0-mean(stat0,na.rm=TRUE))/sd(stat0,na.rm=TRUE)
    stat0
  })
  if(effects=='recvingrate')
    stat <- lapply(stat,t)
  stat <- Reduce(function(a,b) abind(a,b,along=3),stat)
  stat
}
