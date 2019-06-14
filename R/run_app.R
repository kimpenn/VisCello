

#' Launch App
#' @examples
#' cello()
#' @import shiny
#' @export
cello <- function( config_file = NULL, RStudio = F,run_app = T) {
    Sys.setenv("R_MAX_NUM_DLLS"=180)
    cat("Launching VisCello...")
    if(RStudio) {
        options(shiny.launch.browser = .rs.invokeShinyWindowViewer)
    }
    app_path <- system.file("app", package='VisCello')
    if(is.null(config_file)) {
        config_file <- paste0(app_path, "/data/config.yml")
    } 
    
    .GlobalEnv$global_config <- config::get(file = config_file, use_parent = F)

    if(run_app) {
        shiny::runApp(app_path)
    }
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
