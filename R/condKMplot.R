#' Generate conditional survival plots
#'
#' \code{condKMplot} produces a Kaplan-Meier plot for a variety of times on
#' which to condition
#'
#' @param .basekm \code{survfit} object
#' @param .at vector of times on which to condition
#' @param .main plot title
#' @param .xlab x-axis label
#' @param .ylab y-axis label, defaults to "Survival probability"
#' @param .lwd plot line width, defaults to 1
#' @param .mark controls whether censoring times are marked on curves,
#' defaults to F
#'
#' @return A plot with a line for the overall Kaplan-Meier plot and one
#' additional line for each value in \code{.at}
#'
#' @export
#'

condKMplot <- function(.basekm, .at, .main = NULL, .xlab = NULL,
                       .ylab = "Survival probability", .lwd = 1, .mark = F) {
  library(survival)

  if (class(.basekm) != "survfit") {
    stop("Argument to .basekm must be of class survfit")
  }
  if (max(.at) > max(.basekm$time)) {
    stop(paste(
      "Argument to .at specifies value(s) outside the range of observed times;",
      "the maximum observed time is", round(max(.basekm$time), 2)
    ))
  }
  plot(.basekm,
    conf.int = F, xlab = .xlab, ylab = .ylab, main = .main,
    lwd = .lwd, mark.time = .mark
  )
  nt <- length(.at)
  fitkm <- list()
  for (i in 1:nt) {
    fitkm[[i]] <- survfit(
      formula = as.formula(.basekm$call$formula),
      data = eval(.basekm$call$data),
      start.time = .at[i]
    )
    lines(fitkm[[i]], conf.int = F, col = i + 1, lwd = .lwd, mark.time = .mark)
    abline(v = i, lty = 3)
  }
}
