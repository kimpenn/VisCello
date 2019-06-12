

#' Launch App
#' @examples
#' cello()
#' @import shiny
#' @export
cello <- function(RStudio = F, config_file = NULL) {
    Sys.setenv("R_MAX_NUM_DLLS"=180)
    cat("Launching VisCello...")
    if(RStudio) {
        options(shiny.launch.browser = .rs.invokeShinyWindowViewer)
    }
    app_path <- system.file("app", package='VisCello.base')
    if(!is.null(config_file)) {
        .GlobalEnv$config_file <- config_file
    } else {
        .GlobalEnv$config_file <- "data/config.yml"
    }
    shiny::runApp(app_path)
}


#' Launch App old function
#' @examples
#' viscello()
#' @import shiny
#' @export
viscello <- function(...) {
    message("Function deprecated. Please use cello().")
    cello(...)
}
