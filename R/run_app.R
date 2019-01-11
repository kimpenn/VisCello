

#' Launch App
#' @examples
#' cello()
#' @import shiny
#' @export
viscello <- function(RStudio = F) {
    Sys.setenv("R_MAX_NUM_DLLS"=180)
    cat("Launching VisCello...")
    if(RStudio) {
        options(shiny.launch.browser = .rs.invokeShinyWindowViewer)
    }
    shiny::runApp(system.file("app", package='VisCello.eht'))
}
