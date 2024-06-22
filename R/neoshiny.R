#' Starts shiny application for the neodistr package
#'@return Starts shiny application for the neodistr package.
#'@examples
#' if 	(interactive()) {
#' 	suppressMessages(library(neodistr))	
#' 	neoshiny()
#' 	}
#'@import shiny
#'@importFrom shinythemes shinytheme
#'@import plotly
#'@importFrom ggplot2 ggplot aes
#'@author Anisa' Faoziah and Achmad Syahrul Choir


#'@author Anisa' Faoziah and Achmad Syahrul Choir
#'@export
neoshiny <- function() {
  if (!requireNamespace("plotly", quietly = TRUE)) {
    stop("Package 'plotly' needed for this function to work. Please install it.", call. = FALSE)
  }
  if (!requireNamespace("shiny", quietly = TRUE)) {
    stop("Package 'shiny' needed for this function to work. Please install it.", call. = FALSE)
  }
  if (!requireNamespace("shinythemes", quietly = TRUE)) {
    stop("Package 'shinythemes' needed for this function to work. Please install it.", call. = FALSE)
  }
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package 'ggplot2' needed for this function to work. Please install it.", call. = FALSE)
  }
  
appDir <- system.file("shiny",  package = "neodistr")
#  source(system.file("shiny/global.R", package = "neodistr"), local = environment())
  shiny::runApp(appDir = appDir, launch.browser = TRUE)
#  appDir <- system.file("neodistr",  package = "neodistr")
# shiny::runApp("inst/shiny/app.R", launch.browser = TRUE)
}