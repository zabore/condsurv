#' Generate conditional survival plots using ggplot2
#'
#' \code{condKMggplot} produces a Kaplan-Meier plot for a variety of times on
#' which to condition using ggplot2
#'
#' @param .basekm \code{survfit} object
#' @param .at vector of times on which to condition
#' @param .main plot title
#' @param .xlab x-axis label
#' @param .ylab y-axis label, defaults to "Survival probability"
#' @param .lwd plot line width, defaults to 1
#'
#' @return A ggplot with a line for the overall Kaplan-Meier plot and one
#' additional line for each value in \code{.at}
#'
#' @export
#'


condKMggplot <- function(.basekm, .at, .main = NULL, .xlab = "Years",
                         .ylab = "Survival probability", .lwd = 1) {

  library(survival)
  library(dplyr)
  library(ggplot2)
  library(purrr)

  if(class(.basekm) != "survfit"){
    stop("Argument to .basekm must be of class survfit")}
  if(max(.at) > max(.basekm$time)){stop(paste(
    "Argument to .at specifies value(s) outside the range of observed times;",
    "the maximum observed time is", round(max(.basekm$time),2)))}

  .at <- c(0, .at)
  nt <- length(.at)
  fitkm <- list()
  fitkmdat <- list()
  for(i in 1:nt){
    fitkm[[i]] <- survfit(formula = as.formula(.basekm$call$formula),
                          data = eval(.basekm$call$data),
                          start.time = .at[i])
    fitkmdat[[i]] <- data_frame(time = fitkm[[i]]$time,
                                prob = fitkm[[i]]$surv)
  }

  condsurvdat <- fitkmdat %>%
    map_df(`[`, .id = "condtime") %>%
    mutate(condtime = factor(condtime, labels = .at))

  ggplot(condsurvdat, aes(x = time, y = prob, color = condtime)) +
    geom_step(lwd = .lwd) +
    ylim(0, 1) +
    xlab(.xlab) +
    ylab(.ylab) +
    labs(color = "x") +
    ggtitle(.main) +
    theme_bw()
}
