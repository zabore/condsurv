#' Estimate conditional survival with a 95\% confidence interval
#'
#' \code{condKMest} estimates the Kaplan-Meier conditional survival at
#' fixed time points and produces a 95\% confidence interval
#'
#' @param .basekm \code{survfit} object
#' @param .t1 the time on which to condition
#' @param .t2 the survival time to estimate
#'
#' @details For example, if \code{.t1} = 2 and \code{.t2} = 5, the function
#' will return the probability of surviving to year 5 conditioned on having
#' already survived to year 2
#'
#' @return A list where \code{cs} is the conditional survival estimate,
#' \code{cilow} is the lower bound of the 95% confidence interval and
#' \code{cihigh} is the upper bound of the 95% confidence interval
#'
#' @export
#'

condKMest <- function(.basekm, .t1, .t2) {

  if(class(.basekm) != "survfit") {stop("Argument to .basekm must be of class survfit")}
  if(max(.t1) > max(.basekm$time)) {stop(paste(
    "Argument to .t1 specifies a value outside the range of observed times;",
    "the maximum observed time is", round(max(.basekm$time),2)))}
  if(max(.t2) > max(.basekm$time)) {stop(paste(
    "Argument to .t2 specifies a value outside the range of observed times;",
    "the maximum observed time is", round(max(.basekm$time),2)))}
  cs <- summary(.basekm, times = c(.t1, .t2))$surv[2] /
    summary(.basekm, times = c(.t1, .t2))$surv[1]
  cs.sq <- cs^2
  d <- .basekm$n.event[.basekm$time >= .t1 & .basekm$time <= .t2 &
                         .basekm$n.event > 0]
  r <- .basekm$n.risk[.basekm$time >= .t1 & .basekm$time <= .t2 &
                        .basekm$n.event > 0]
  dr <- d / (r * (r - d))
  var.cs <- cs.sq * sum(dr)
  ci <- cs + c(-1, 1) * (qnorm(0.975) * sqrt(var.cs))
  if(ci[1] < 0) {warning("Lower bound of CI has been truncated to 0")}
  if(ci[2] > 1) {warning("Upper bound of CI has been truncated to 1")}
  ci.cs <- round(ci, 2)
  if(ci.cs[1] < 0) {ci.cs[1] <- 0}
  if(ci.cs[2] > 1) {ci.cs[2] <- 1}
  return(list(cs = round(cs, 2), cilow = ci.cs[1], cihigh = ci.cs[2]))
}
