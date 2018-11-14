#' @rdname summary.event.network.fit
#' @param x object of class `summary.event.network.fit` returned by
#'   [summary.event.network.fit()].
#' @method print summary.event.network.fit
#' @export
print.summary.event.network.fit <- function (x, ...)
{
  cat("Relational Event Model ")
  if (x$ordinal) { cat("(Ordinal Likelihood)\n\n")
  }else{ cat("(Interval Likelihood)\n\n")}

  tab <- cbind(x$parameters, x$stds)
  tab <- cbind(tab, tab[, 1]/tab[, 2])
  tab <- cbind(tab, x$pval)
  colnames(tab) <- c("MLE", "Std.Err", "Z value", "Pr(>|z|)")
  rownames(tab) <- names(x$parameters)
  printCoefmat(tab, P.values = TRUE)
  cat("Residual deviance:", x$residual.deviance, "on",
      x$df.null - x$df.model, "degrees of freedom\n")
  cat("AIC:", x$AIC, "BIC:", x$BIC, "\n")

}
