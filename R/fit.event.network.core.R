#' Core Relational Event Model fitting function
#'
#' @param sequence An (E x 3) matrix of values where the first column is the event
#'   sender, the second column is the event receiver, and the last column is the
#'   time
#' @param stats An array of dimension (S x R x K x E). An entry (i,j,k,t) is
#'   interpreted as the value of statistic k
#'   from sender i to receiver j at event time t
#' @param x0 a vector of length numStats - User-supplied initial point. We can
#'   possibly make it so there is a default here
#' @param ordinal Binary variable, where TRUE = ordinal likelihood (order
#'   only) and FALSE = temporal (full time info).
#' @param blocked (not implemented) a list of indices for the events that cannot happen. As of
#'   now, this is implemented as just numbers corresponding to matrix locations.
#' @param epsilon Convergence criterion.
#' @param iter_max Iteration limit
#' @param networkStatTerms A numerical list of the terms to be included in the
#'   model. These numbers should correspond to the statistic from the list in the
#'   provided array.
#'
#' @param ... more functions
#' @return An object of class 'rem.fit'
#' @import stats
#' @rawNamespace import(Matrix, except = c(cov2cor, toeplitz, update))
#' @export
FitEventNetworkCore <- function(sequence,stats,
                                x0=rep(0,dim(stats)[3]),ordinal=FALSE,
                                blocked=c(),epsilon = 1e-6,iter_max=200,
                                networkStatTerms=1:dim(stats)[3],...){

  #-------------------------------------------------------------------------#
  # Likelihood function, gradient, and hessian
  #-------------------------------------------------------------------------#
  LGH <- function(theta,ret.grad=TRUE, ret.hess=TRUE){

    # separate data matrix for ease of computation
    statistics <- data[,2:ncol(data)] # a numDyads*numEvents by numStats matrix
    index <- data[,1]

    R <- as.vector(statistics %*% theta)            # Combine statistics and parameters
    rate <- exp(R)                       # Take the exponential
    #rate[blocked] <- 0                   # Zero out the rates of events that cannot happen

    if (ordinal == FALSE){
      L <- sum(R[index==1]) - sum(D*rate)   # not ordinal, so use temporal likelihood
      gradient <- colSums(statistics[index==1,]) - colSums(statistics*rate*D)#/numEvents
      hessian <- -t(statistics) %*% (statistics*rate*D)
    }else{
      L <- sum(R[index==1]) - sum(index==1)*log(sum(rate)) # ordinal, so use ordinal likelihood
      gradient <- colSums(statistics[index==1,]) - sum(index==1)*colSums(statistics*rate)/sum(rate)
      #hessian <- -t(statistics) %*% (statistics*rate) / sum(rate) + (t(statistics)%*%t(t(rate)))%*%(t(rate)%*%statistics) / sum(rate)^2
      hessian <- -sum(index==1)*t(statistics) %*% (statistics*rate)/sum(rate) +
        sum(index==1)*(t(statistics)%*%t(t(rate)))%*%(t(rate)%*%statistics) / sum(rate)^2
    }

    out <- -L
    if(ret.grad)
      attr(out,'gradient') <- -gradient
    if(ret.hess)
      attr(out,'hessian') <- -matrix(hessian,nrow=numStats)
    out
  }

  #-------------------------------------------------------------------------#
  # Initialization
  #-------------------------------------------------------------------------#

  numStats <- length(networkStatTerms)   # Number of sufficient statistics
  senders <- sequence[,1]        # source of relational events
  receivers <- sequence[,2]      # target of events
  time <- sequence[,3]              # times in seconds (or order)
  numEvents <- nrow(sequence)    # number of events
  numSenders <- dim(stats)[1]     # number of potential senders
  numReceivers <- dim(stats)[2]   # number of potential receivers
  numDyads <- numSenders*numReceivers

  # Ahead of time, create a sparse matrix containing all of the statistics.
  # Note that this can be memory expensive, but give serious speed advantages
  # data is a (numEvents*numDyads) by (numStats+1) matrix of stats
  data <- Reduce(rbind, lapply(1:numEvents, function(e){    # loop through each event
    s <- senders[e]    # select the sender
    r <- receivers[e]  # select the receiver

    # m is a matrix of dyad indices, stat indices, and stat values
    M <- c( ((r-1)*numSenders+s), 1, 1)      # Demarcate the index for the event (when considering them in vector form)
    M <- rbind(M,
               Reduce(rbind, lapply(1:numStats,function(p){
                 values <- as.vector(stats[,,networkStatTerms[p],e]) # Loop through statistics and collect the matrix of values
                 idx <- which(abs(values)>0)                         # Get locations of all non-zero values

                 # Create the index list - location of non-zero element (row), the number of the statistic (column), and the value (value)
                 cbind(idx, rep(p+1,length(idx)), values[idx])
               }))
    )

    # Turn the list of row/column/value into a sparse matrix and concatenate it with the previous matrix
    sparseMatrix(i=M[,1],j=M[,2],x=M[,3],dims=c(numDyads,numStats+1))
  }))

  if (ordinal == FALSE){
    delta_t <- c(0, diff(time)) # Not ordinal, so we compute the waiting time between events
  }else{
    delta_t <- time # Ordinal, so this is irrelevant
  }
  # Build a vector of times
  # D is a vector of length numEvents*numDyads
  D <- rep(delta_t,each=numDyads)


  #-------------------------------------------------------------------------#
  # Optimization Routine
  #-------------------------------------------------------------------------#

  theta <- x0  # User-supplied initial point

  # Run prebuilt solver for unconstrained non-linear problems

  opt <- nlm(LGH, p=theta, hessian=TRUE,
             #gradtol = epsilon,
             steptol=epsilon,
             iterlim = iter_max,
             check.analyticals = FALSE,
             typsize=rep(.1,dim(stats)[3]),...)
  x <- opt$estimate
  fval <- opt$minimum
  exitflag <- opt$code
  grad <- opt$gradient
  hess <- opt$hessian

  #-------------------------------------------------------------------------#
  # Various statistics
  #-------------------------------------------------------------------------#

  # SD and Var
  V <- solve(hess)
  std <- diag(V)
  std[std<0] <- 0
  std <- sqrt(std)

  # Compute Correlations
  corr <- V/std %*% t(std)

  #-------------------------------------------------------------------------#
  # Output
  #-------------------------------------------------------------------------#

  names(x) <- dimnames(stats)[[3]]

  output <- list(parameters = x,
                 grad = grad,
                 hess = hess,
                 cov = V,
                 likelihood = -fval,
                 residual.deviance = 2*fval,
                 status = exitflag,
                 ordinal = ordinal,
                 iterations = opt$iterations,
                 df.null = numEvents,
                 df.model = length(x),
                 stds = std,
                 var = V,
                 pval = 2*(1-pnorm(abs(x/std))),
                 correlation = corr,
                 AIC = 2*numEvents - 2*(-fval),
                 BIC = -2*-fval + numEvents*log(numEvents)
  )
  class(output) <- 'event.network.fit'
  output
}
