#' Run the NormalityAssessment Shiny application
#'
#' @description Runs the NormalityAssessment Shiny application.
#'
#' @return There is no return value.
#'
#' @author
#' Christopher Casement \cr
#' Department of Mathematics \cr
#' Fairfield University \cr
#' \email{casementc@@gmail.com}
#'
#' Laura McSweeney \cr
#' Department of Mathematics \cr
#' Fairfield University
#'
#' @references
#' Buja, A., Cook, D., Hofmann, H., Lawrence, M., Lee, E. K., Swayne, D. F.,
#' & Wickham, H. (2009). Statistical inference for exploratory data analysis and
#' model diagnostics. Philosophical Transactions of the Royal Society of London
#' A: Mathematical, Physical and Engineering Sciences, 367(1906), 4361-4383.
#'
#' Majumder, M., Hofmann, H., & Cook, D. (2013). Validation of visual
#' statistical inference, applied to linear models. Journal of the American
#' Statistical Association, 108(503), 942-956.
#'
#' Wickham, H., Cook, D., Hofmann, H., & Buja, A. (2010). Graphical inference
#' for infovis. IEEE Transactions on Visualization and Computer Graphics, 16(6),
#' 973-979.
#'
#' @examples
#' ## only run the app in an interactive R session
#' if (interactive()) {runNormalityAssessmentApp()}
#'
#' @export
runNormalityAssessmentApp <- function() {

  # find and launch app
  run_app_R_script <- '
    appDir <- system.file("NormalityAssessment", package = "NormalityAssessment")

    if (appDir == "") {
      stop("Could not find the NormalityAssessment directory. Try re-installing
        the `NormalityAssessment` package.", call. = FALSE
      )
    }

    shiny::runApp(appDir, launch.browser = TRUE, display.mode = "normal")
  '

  eval(parse(text = run_app_R_script))
}
