
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
        cmeta <- reactiveValues(df=pData(all_cds))
        usr <- reactiveValues(clist = clist, elist = elist)
    }

    # Load data
    observeEvent(input$exit_app, {
        stopApp("C.elegans explorer closed.")
    })


    ################################ Explorer module ##############################

    rval1 <- callModule(explorer_server, id="main",
                        sclist = usr,
                        useid = "clist",
                        #source = "main_dragselect", event = reactive(plotly::event_data("plotly_selected", source = "main_dragselect")),
                        cmeta = cmeta,
                        showcols_basic = ctype_cols_basic,
                        showcols_advanced = ctype_cols_advanced,
                        onlyEUI = F
    )

    rval2 <- callModule(explorer_server, id="early",
                        sclist = usr,
                        useid = "elist",
                        #source = "early_dragselect", event = reactive(plotly::event_data("plotly_selected", source = "early_dragselect")),
                        cmeta = cmeta,
                        showcols_basic = elin_cols_basic,
                        showcols_advanced = elin_cols_advanced,
                        onlyEUI = T
    )

    # Update gene search box
    updateSelectizeInput(session, "dy_ctype_gene", "Search Gene:", choices = gene_symbol_choices, selected = NULL, server=T)

    observe({
        req(rval1$mclass)
        rval1$cells
        rval1$group_name
        isolate({
            if(!is.null(rval1$cells)) {
                if(!rval1$mclass %in% colnames(cmeta$df)) {
                    cmeta$df[, rval1$mclass] <- "unannotated"
                }
                cmeta$df[[rval1$mclass]][match(rval1$cells, rownames(cmeta$df))] <- rep(rval1$group_name, length(rval1$cells))
            } else {
                cmeta$df[[rval1$mclass]] <- NULL
            }
        })
    })

    observe({
        req(rval1$ustats, length(rval1$list))
        isolate({
            usr$clist <- rval1$list
        })
    })

    observe({
        req(rval2$mclass)
        rval2$cells
        rval2$group_name
        isolate({
            if(!is.null(rval2$cells)) {
                if(!rval2$mclass %in% colnames(cmeta$df)) {
                    cmeta$df[, rval2$mclass] <- "unannotated"
                }
                cmeta$df[[rval2$mclass]][match(rval2$cells, rownames(cmeta$df))] <- rep(rval2$group_name, length(rval2$cells))
            } else {
                cmeta$df[[rval2$mclass]] <- NULL
            }
        })
    })

    observe({
        req(rval2$ustats, length(rval2$list))
        isolate({
            usr$elist <- rval2$list
        })
    })
    
   
    # DE
    
    callModule(de_server, id="cel",
               sclist = usr,
               cmeta = cmeta
    )
    

    # Image gene expression graph plot

    output$image_graph_plot_ui <- renderUI({
        req(input$image_ploth)
        plotOutput("image_graph_plot", height = paste0(500/5.5 *input$image_ploth,"px")) %>% withSpinner()
    })

    output$image_graph_plot <- renderPlot({
        req(input$image_colorBy, input$image_pal)
        t_cut <- 108
        plotg <- input$image_colorBy
        g<-g_all %>% activate("nodes") %>% 
            mutate(text.size = ifelse(time > t_cut, 0, 10/log10(time+1))) %>%
            mutate(name = ifelse(time > t_cut, "", name)) %>%
            filter(!(time > 200 & is.na(!!as.name(plotg))))
        range(as.data.frame(g)$text.size)
        plotGraph(g, color.by=plotg, pal=input$image_pal, label="name", type = "numeric",border.size=.3, legend.title = names(image_colorBy_choices)[which(image_colorBy_choices == input$image_colorBy)]) + 
            theme(
                  axis.ticks.x=element_blank(),
                  axis.text.x=element_blank(),
                  axis.ticks.y=element_blank(),
                  axis.text.y=element_blank(),
                  legend.margin=margin(15,0,0,0),
                  legend.box.margin=margin(-10,-10,-10,-10),
                  plot.margin = unit(c(.3,.5,.3,.3), "cm"))
    })

    output$g_meta_table <- DT::renderDataTable({
        req(input$image_colorBy)
        curg<- names(image_colorBy_choices)[which(image_colorBy_choices == input$image_colorBy)]
        req(curg %in% names(g_meta_list))
        DT::datatable(g_meta_list[[curg]], selection = 'none',
                      rownames=F, 
                      options = list(
                          searching=F, 
                          scrollX = TRUE,
                          paging = F
                      )
        ) 
    })
    
}
