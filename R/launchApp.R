#' Launches ShinyApp
#'
#' @param None no input
#' @return initializes Shiny App
#'
#' @export
launchApp <- function() {
  library(NBAcompJON)
  options(shiny.autoload.r=FALSE)
  shinyApp(ui = ui, server = server)
}
