#' Interactive conditional survival Shiny app
#'
#' \code{condKMapp} produces an interactive Shiny app containing output from
#' both the \code{condKMest} function and the \code{condKMplot} function.
#'
#' @param .basekm \code{survfit} object
#'
#' @return The output includes 1) a plot with one overall Kaplan-Meier survival
#' curve and an additional Kaplan-Meier survival curve for each time on which
#' you are conditioning and 2) a table with conditional survival estimates at
#' the specified survival time along with their associated confidence intervals,
#' with one row for each time on which you are conditioning.
#'
#' @export
#'

condKMapp <- function(.basekm) {
  library(shiny)
  library(survival)

  shinyApp(ui = fluidPage(
    pageWithSidebar(
      headerPanel("Conditional survival estimates"),
      sidebarPanel(helpText("This interactive interface allows you to adjust the
                            survival time of interest (using the numeric entry box below)
                            and to select a single survival time, or range of survival times, on
                            which to condition (using the slider bar below)."),
                   numericInput(inputId = "survtime", label = "Enter the survival time of interest (in years)",
                                value = 5, min = 0, max = 10),
                   sliderInput(inputId = "condtime", label = "Select the range of times on which to condition (in years)",
                               value = c(1, 4), min = 1, max = 10, step = 1),
                   helpText("The output includes 1) a plot with one overall Kaplan-Meier
                            survival curve and an additional Kaplan-Meier survival curve for
                            each time on which you are conditioning and 2) a table
                            with conditional survival estimates at the specified
                            survival time along with their associated confidence intervals,
                            with one row for each time on which you are conditioning.")),
      mainPanel(plotOutput(outputId = "condplot"),
                tableOutput(outputId = "condtab"))
      )),

  server = function(input, output) {
    output$condplot <- renderPlot({
      if(class(.basekm) != "survfit"){stop("Argument to .basekm must be of class survfit")}
      if(max(input$condtime) > max(.basekm$time)){stop(paste(
        "The range of times on which to condition specifies value(s) outside the range of observed times;",
        "the maximum observed time is", round(max(.basekm$time), 2)))}

      plot(.basekm, conf.int = F, xlab = "Years", ylab = "Survival probability",
           lwd = 1, mark.time = F, main = "Conditional survival curves")
      at <- seq(from = min(input$condtime), to = max(input$condtime), by = 1)
      nt <- length(at)
      fitkm <- list()
      for(i in 1:nt){

        fitkm[[i]] <- survfit(formula = as.formula(.basekm$call$formula),
                              data = eval(.basekm$call$data),
                              start.time = at[i])
        lines(fitkm[[i]], conf.int = F, col = i + 1, lwd = 1, mark.time = F)
        abline(v = i, lty = 3)
      }
    }, height = 400, width = 500)

    output$condtab <- renderTable({
      if(class(.basekm) != "survfit") {stop("Argument to .basekm must be of class survfit")}
      if(max(input$condtime) > max(.basekm$time)) {stop(paste(
        "The range of times on which to condition specifies value(s) outside the range of observed times;",
        "the maximum observed time is", round(max(.basekm$time), 2)))}
      if(max(input$survtime) > max(.basekm$time)) {stop(paste(
        "The survival time of interest specifies a value outside the range of observed times;",
        "the maximum observed time is", round(max(.basekm$time), 2)))}

      at <- seq(from = min(input$condtime), to = max(input$condtime), by = 1)
      nt <- length(at)
      cstab <- NULL
      for(i in 1:nt) {
        if(at[i] < input$survtime) {
          cs <- summary(.basekm, times = c(at[i], input$survtime))$surv[2] /
            summary(.basekm, times = c(at[i], input$survtime))$surv[1]
          cs.sq <- cs^2
          d <- .basekm$n.event[.basekm$time >= at[i] & .basekm$time <= input$survtime &
                                .basekm$n.event > 0]
          r <- .basekm$n.risk[.basekm$time >= at[i] & .basekm$time <= input$survtime &
                               .basekm$n.event > 0]
          dr <- d / (r * (r - d))
          var.cs <- cs.sq * sum(dr)
          ci <- cs + c(-1, 1) * (qnorm(0.975) * sqrt(var.cs))
          if(ci[1] < 0) {warning("Lower bound of CI has been truncated to 0")}
          if(ci[2] > 1) {warning("Upper bound of CI has been truncated to 1")}
          ci.cs <- round(ci, 2)
          if(ci.cs[1] < 0) {ci.cs[1] <- 0}
          if(ci.cs[2] > 1) {ci.cs[2] <- 1}
          cstab <- data.frame(rbind(cstab, c(round(at[i]), round(cs, 2), ci.cs[1], ci.cs[2])),
                              stringsAsFactors = FALSE)
        }
      }
      colnames(cstab) <- c("Conditional time", "Conditional survival", "Lower 95% CI", "Upper 95% CI")
      cstab
    }, digits = c(0, 0, 2, 2, 2))
  })
}
