#' NormalityAssessment: A Graphical User Interface for Testing Normality
#'   Visually
#'
#' @description The NormalityAssessment package creates plots for assessing
#'   normality. The methods implemented are based on recent development made in
#'   graphical inference. In the app, the features in the 'Explore Simulated
#'   Data' tab enable the user to run the Rorschach procedure, and those in the
#'   'Include Your Data' tab allow the user to run the line-up procedure.
#'
#' @aliases NormalityAssessment-package NormalityAssessment
#'
#' @import shiny
#' @importFrom DT renderDT
#' @importFrom dplyr select_if
#' @importFrom ggplot2 aes element_text facet_wrap geom_histogram ggplot labs
#'   margin stat_qq stat_qq_line theme theme_grey theme_set
#' @importFrom rio import
#' @importFrom rmatio read.mat
#' @importFrom shinyalert shinyalert useShinyalert
#' @importFrom shinyBS bsModal
#' @importFrom stats na.omit pbinom rnorm sd
#' @importFrom stringi stri_dup
#' @importFrom stringr str_trim
#' @importFrom utils head read.csv
#'
#' @section Function: \itemize{
#' \item \code{\link{runNormalityAssessmentApp}}
#' }
#'
#' @details Package: NormalityAssessment \cr
#' Type: Package \cr
#' Version: 0.1.0 \cr
#' Date: 2022-11-04 \cr
#' Depends: R (>= 3.5.0) \cr
#' Imports: dplyr, DT, ggplot2, rio, rmatio, shiny, shinyalert, shinyBS,
#' stringi, stringr
#' License: MIT \cr
#' BugReports: https://github.com/ccasement/NormalityAssessment/issues \cr
#' Encoding: UTF-8 \cr
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
#' @docType package
"_PACKAGE"
