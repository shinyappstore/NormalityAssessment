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
#' @examples
#' \dontrun{runNormalityAssessmentApp()}
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
