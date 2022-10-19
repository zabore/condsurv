#' Generate conditional survival plots using ggplot2
#'
#' \code{gg_conditional_surv} produces a Kaplan-Meier plot for a variety of times on
#' which to condition using ggplot2
#'
#' @param basekm \code{survfit} object
#' @param at vector of times on which to condition
#' @param main plot title
#' @param xlab x-axis label
#' @param ylab y-axis label, defaults to "Survival probability"
#' @param lwd plot line width, defaults to 1
#'
#' @return A ggplot with a line for the overall Kaplan-Meier plot and one
#' additional line for each value in \code{at}
#'
#' #' @details See the vignette
#' at \href{http://www.emilyzabor.me/condsurv/articles/plot_cs.html}{http://www.emilyzabor.me/condsurv/articles/plot_cs.html} for
#' details and examples.
#'
#' @importFrom magrittr "%>%"
#' @importFrom rlang .data
#'
#' @export
#'

gg_conditional_surv <- function(basekm,
                                at,
                                main = NULL,
                                xlab = "Years",
                                ylab = "Survival probability",
                                lwd = 1) {
  if (class(basekm) != "survfit") {
    stop(
      "Argument to basekm must be of class survfit"
    )
  }

  if (max(at) > max(basekm$time)) {
    stop(
      paste(
        "Argument to at specifies value(s) outside the range of observed times;",
        "the maximum observed time is", round(max(basekm$time), 2)
      )
    )
  }

  nt <- length(at)

  fitkm <- list()
  fitkmdat <- list()

  for (i in 1:nt) {
    fitkm[[i]] <- survival::survfit(
      formula = stats::as.formula(basekm$call$formula),
      data = eval(basekm$call$data),
      start.time = at[i]
    )

    fitkmdat[[i]] <- tibble::tibble(
      timept = fitkm[[i]]$time,
      prob = fitkm[[i]]$surv
    )
  }

  condsurvdat <- fitkmdat %>%
    purrr::map_df(`[`, .id = "which_at") %>%
    dplyr::mutate(condtime = factor(which_at, levels = seq(1, nt), labels = at))

  ggplot2::ggplot(
    condsurvdat,
    ggplot2::aes(x = timept, y = prob, color = condtime)
  ) +
    ggplot2::geom_step(lwd = lwd) +
    ggplot2::ylim(0, 1) +
    ggplot2::labs(
      x = xlab,
      y = ylab,
      title = main
    ) +
    ggplot2::labs(color = "x") +
    ggplot2::theme_bw()
}
