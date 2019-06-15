

#' Launch App
#' @examples
#' cello()
#' @import shiny
#' @export
cello <- function(config_file = NULL, RStudio = F) {
    Sys.setenv("R_MAX_NUM_DLLS"=180)
    cat("Launching VisCello...")
    if(RStudio) {
        options(shiny.launch.browser = .rs.invokeShinyWindowViewer)
    }
    app_path <- system.file("app", package='VisCello')
    if(exists("eset", env = .GlobalEnv)) rm(eset, envir = .GlobalEnv)
    if(exists("clist", env = .GlobalEnv)) rm(clist, envir = .GlobalEnv)
    if(exists("r_data", env = .GlobalEnv)) rm(r_data, envir = .GlobalEnv)
    
    
    if(is.null(config_file)) {
        config_file <- paste0(app_path,"/data/config.yml")
    }  
    .GlobalEnv$global_config <- config::get(file = config_file, use_parent = F)
    .GlobalEnv$mainTitle = paste0("VisCello - ",global_config$study_name)
    .GlobalEnv$organism = global_config$organism
    .GlobalEnv$study_info <- global_config$study_description
    .GlobalEnv$name_col = global_config$feature_name_column
    .GlobalEnv$id_col = global_config$feature_id_column
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
