library(Biobase)
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#


function(input, output, session) {
    #lapply(list.files("src/instR/", pattern = "\\.(r|R)$", recursive = TRUE, full.names = TRUE), function(x){source(file = x, local = TRUE)})

    #######################################    
    # Things that were previously in global.R but depend on the values of eset/clist
    #######################################
    session_vals = c()
    meta_order <- colnames(pData(session_vals$eset))
    names(meta_order) <- colnames(pData(session_vals$eset))
    # Don't show factors that's not useful to the user
    meta_order <- meta_order[!meta_order %in% c("Amp_batch_ID", "well_coordinates", "Number_of_cells", "Plate_ID", "Batch_desc", "Pool_barcode", "Cell_barcode", "RMT_sequence", "no_expression")]
    meta_order["Gene Expression"] = "gene.expr"
    session_vals$pmeta_attr <- data.frame(meta_id = meta_order, meta_name = names(meta_order), stringsAsFactors=FALSE)
    pData(session_vals$eset) <- pData(session_vals$eset)[,which(colnames(pData(session_vals$eset)) %in% session_vals$pmeta_attr$meta_id)]
    session_vals$pmeta_attr$is_numeric <- sapply(as.character(session_vals$pmeta_attr$meta_id), function(x) {
	if(x %in% colnames(pData(session_vals$eset))) {
	  is.numeric(pData(session_vals$eset)[[x]])
	    } else if(x %in% colnames(session_vals$clist[[1]]@pmeta)) {
	  is.numeric(session_vals$clist[[1]]@pmeta[[x]])
	    } else if(x == "gene.expr") {
	  T
	    } else {
	  NA
	    }
      })
      # Edit if necessary, Which meta to show 
      session_vals$pmeta_attr$dpal <- ifelse(session_vals$pmeta_attr$is_numeric, "RdBu", "Set1")
      session_vals$pmeta_attr$dscale <- ifelse(session_vals$pmeta_attr$is_numeric, "log10", NA)
      session_vals$pmeta_attr$dscale[which(session_vals$pmeta_attr$meta_id %in% c("Size_Factor"))] <- "identity"

      ### Which meta data to show, and in what order ###	
      session_vals$showcols_meta <- session_vals$pmeta_attr$meta_id
      names(session_vals$showcols_meta) <- session_vals$pmeta_attr$meta_name
      session_vals$bp_colorBy_choices <- session_vals$showcols_meta[!session_vals$pmeta_attr$is_numeric]
      session_vals$de_meta_options <- session_vals$showcols_meta[!session_vals$pmeta_attr$is_numeric]
      session_vals$numeric_palettes <- numeric_color_opt()
      names(session_vals$numeric_palettes) <- session_vals$numeric_palettes
      session_vals$heatmap_palettes <- session_vals$numeric_palettes[session_vals$numeric_palettes %in% c("RdYlBu", "RdBu", "viridis", "magma", "plasma", "inferno")]
      session_vals$gene_symbol_choices <- rownames(fData(session_vals$eset))
      names(session_vals$gene_symbol_choices) <- fData(session_vals$eset)$symbol

      session_vals$gene_id_symbol <- names(session_vals$gene_symbol_choices)
      names(session_vals$gene_id_symbol) <- session_vals$gene_symbol_choices
      
    
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
        cmeta <- reactiveValues(df=pData(session_vals$eset))
        usr <- reactiveValues(clist = session_vals$clist)
    }

    # Load data
    observeEvent(input$exit_app, {
        stopApp("C.elegans explorer closed.")
    })


    ################################ Explorer module ##############################

    rval_ct <- callModule(explorer_server, id="main",
                        sclist = usr,
                        useid = "clist",
                        cmeta = cmeta,
                        session_vals = session_vals
    
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
               organism = organism,
               session_vals = session_vals
    )
    
}
