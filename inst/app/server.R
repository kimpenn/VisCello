
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#


function(input, output, session) {
    #lapply(list.files("src/instR/", pattern = "\\.(r|R)$", recursive = TRUE, full.names = TRUE), function(x){source(file = x, local = TRUE)})
    
    #######################################
    # Save state
    #######################################
    saveState <- function(filename) {
        isolate({
            r_data <- list(
                cmeta = reactiveValuesToList(cmeta),
                usr = reactiveValuesToList(usr)
            )
            save(r_data, file = filename)
        })
    }

    output$state_save_sc <- downloadHandler(
        filename = function() { paste0("State-",Sys.Date(),".rda") },
        content = function(file) {
            saveState(file)
        }
    )

    #######################################
    # Load previous state
    #######################################
    observe({
        inFile <- input$uploadState
        if(!is.null(inFile)) {
            isolate({
                tmpEnv <- new.env()
                load(inFile$datapath, envir=tmpEnv)
                if (exists("r_data", envir=tmpEnv, inherits=FALSE)){
                    assign("r_data", tmpEnv$r_data, envir=.GlobalEnv)
                }
                rm(tmpEnv)
            })
        }
    })

    output$refreshOnUpload <- renderUI({
        inFile <- input$uploadState
        if(!is.null(inFile)) {
            # Joe Cheng: https://groups.google.com/forum/#!topic/shiny-discuss/Olr8m0JwMTo
            tags$script("window.location.reload();")
        }
    })


    ###### Save state on refresh #####

    saveStateOnRefresh <- function(session = session) {
        session$onSessionEnded(function() {
            isolate({
                if(is.null(input$uploadState)) {
                    r_data <- list(
                        cmeta = reactiveValuesToList(cmeta),
                        usr = reactiveValuesToList(usr)
                    )
                    assign("r_data", r_data, envir = .GlobalEnv)
                }
            })
        })
    }

    saveStateOnRefresh(session)


    if (
        exists("r_data")) {
        cmeta = do.call(reactiveValues,r_data$cmeta)
        usr <- do.call(reactiveValues,r_data$usr)
        rm(r_data, envir = .GlobalEnv)
    } else {
        cmeta <- reactiveValues(df=pData(cello))
        usr <- reactiveValues(clist = clist)
    }

    # Load data
    observeEvent(input$exit_app, {
        stopApp("C.elegans explorer closed.")
    })


    ################################ Explorer module ##############################

    rval_ct <- callModule(explorer_server, id="main",
                        sclist = usr,
                        useid = "clist",
                        cmeta = cmeta
    )

    observe({
        req(rval_ct$mclass)
        rval_ct$cells
        rval_ct$group_name
        isolate({
            if(!is.null(rval_ct$cells)) {
                if(!rval_ct$mclass %in% colnames(cmeta$df)) {
                    cmeta$df[, rval_ct$mclass] <- "unannotated"
                }
                cmeta$df[[rval_ct$mclass]][match(rval_ct$cells, rownames(cmeta$df))] <- rep(rval_ct$group_name, length(rval_ct$cells))
            } else {
                cmeta$df[[rval_ct$mclass]] <- NULL
            }
        })
    })

    observe({
        req(rval_ct$ustats, length(rval_ct$list))
        isolate({
            usr$clist <- rval_ct$list
        })
    })
    
   
    # DE
    
    callModule(de_server, id="eht",
               sclist = usr,
               cmeta = cmeta,
               organism = "mmu"
    )
    
}
