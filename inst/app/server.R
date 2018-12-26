
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#


function(input, output, session) {

    #######################################
    # Save state
    #######################################
    saveState <- function(filename) {
        isolate({
            #LiveInputs <- reactiveValuesToList(input)
            #r_state[names(LiveInputs)] <- LiveInputs
            r_state <- lapply(reactiveValuesToList(input), unclass)
            r_data <- list(
                de_idx = reactiveValuesToList(de_idx),
                de_res = reactiveValuesToList(de_res),
                reset_idx = reactiveValuesToList(reset_idx),
                ct = reactiveValuesToList(ct),
                cmeta = reactiveValuesToList(cmeta),
                usr = reactiveValuesToList(usr)
            )
            save(r_state, r_data, file = filename)
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
                if (exists("r_state", envir=tmpEnv, inherits=FALSE)) {
                    assign("r_state", tmpEnv$r_state, envir=.GlobalEnv)
                    lapply(names(r_state),
                           function(x) session$sendInputMessage(x, list(value = r_state[[x]]))
                    )
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
                    assign("r_state", lapply(reactiveValuesToList(input), unclass), envir = .GlobalEnv)
                    r_data <- list(
                        de_idx = reactiveValuesToList(de_idx),
                        de_res = reactiveValuesToList(de_res),
                        reset_idx = reactiveValuesToList(reset_idx),
                        ct = reactiveValuesToList(ct),
                        cmeta = reactiveValuesToList(cmeta),
                        usr = reactiveValuesToList(usr)
                    )
                    assign("r_data", r_data, envir = .GlobalEnv)
                }
            })
        })
    }

    saveStateOnRefresh(session)


    if (exists("r_state") && exists("r_data")) {
        de_idx = do.call(reactiveValues, r_data$de_idx)
        de_res = do.call(reactiveValues, r_data$de_res)
        reset_idx = do.call(reactiveValues, r_data$reset_idx)
        ct = do.call(reactiveValues, r_data$ct)
        cmeta = do.call(reactiveValues,r_data$cmeta)
        usr <- do.call(reactiveValues,r_data$usr)
        lapply(names(r_state),
               function(x) session$sendInputMessage(x, list(value = r_state[[x]]))
        )
        rm(r_data, r_state, envir = .GlobalEnv)
    } else {
        r_state <- list()
        de_idx <- reactiveValues()
        de_idx$idx_list <- list(NA, NA)
        de_idx$group_name <- c("", "")
        de_res <- reactiveValues()
        reset_idx <- reactiveValues()
        cmeta <- reactiveValues(df=pmeta)
        usr <- reactiveValues(clist = clist, elist = elist)
        reset_idx$g1 <- 0
        reset_idx$g2 <- 0
        ct <- reactiveValues(tbl = data.frame(lapply(cell_type_markers, as.character), stringsAsFactors=FALSE))
    }

    # Load data
    observeEvent(input$exit_app, {
        stopApp("C.elegans explorer closed.")
    })


    ################################ Explorer module ##############################

    rval1 <- callModule(explorer_server, id="main",
                        sclist = usr,
                        useid = "clist",
                        source = "main_dragselect", event = reactive(plotly::event_data("plotly_selected", source = "main_dragselect")),
                        cmeta = cmeta,
                        showcols_basic = ctype_cols_basic,
                        showcols_advanced = ctype_cols_advanced
    )

    rval2 <- callModule(explorer_server, id="early",
                        sclist = usr,
                        useid = "elist",
                        source = "early_dragselect", event = reactive(plotly::event_data("plotly_selected", source = "early_dragselect")),
                        cmeta = cmeta,
                        showcols_basic = elin_cols_basic,
                        showcols_advanced = elin_cols_advanced
    )

    # Update gene search box
    updateSelectizeInput(session, "dy_ctype_gene", "Search Gene:", choices = gene_symbol_choices, selected = NULL, server=T)
    updateSelectizeInput(session, "bp_gene", "Search Gene:", choices = gene_symbol_choices, selected = NULL, server=T)

    observe({
        req(rval1$mclass)
        rval1$cells
        rval1$group_name
        isolate({
            if(!is.null(rval1$cells)) {
                if(!rval1$mclass %in% colnames(cmeta$df)) {
                    cmeta$df[, rval1$mclass] <- NA
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
                    cmeta$df[, rval2$mclass] <- NA
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

    observe({
        factor_cols <- sapply(colnames(cmeta$df), function(x) {
            ifelse(!is.numeric(cmeta$df[[x]]), x, NA)
        })
        factor_cols <- factor_cols[!is.na(factor_cols) & factor_cols != "NA"]
        cmeta$factor_cols <-c(factor_cols, "Cluster")
    })

    ##### Feature Plot Module #####
    output$bp_group_ui <- renderUI({
        bp_choices <- ctype_cols_basic[!ctype_cols_basic %in% "Gene Expression"]
        selectInput("bp_group", "Choose Grouping:", choices = bp_choices)
    })

    output$bp_include_ui <- renderUI({
        req(input$bp_group)
        cur_group <- cmeta$df[[input$bp_group]]
        # Downsample cells from each cell type
        unique_group <- unique(cur_group[!is.na(cur_group) & cur_group != "NA"])
        selectInput("bp_include", "Include:", choices = unique_group, selected=unique_group, multiple = T, width = '100%')
    })

    output$bp_gene_plot <- renderPlot({
        req(input$bp_group,input$bp_include)
        cur_group <- cmeta$df[[input$bp_group]]
        # Downsample cells from each cell type
        cur_idx <- unlist(lapply(input$bp_include, function(g) {
            cidx <- which(cur_group==g)
            sample(cidx, min(length(cidx),input$bp_downsample))
        }))
        cur_meta <- cmeta$df[cur_idx, input$bp_group, drop=F]
        df <- as.data.frame(as.matrix(sexpr_nmlog[input$bp_gene, cur_idx]))
        feature_plot(df, input$bp_gene, plot_by = input$bp_group, meta = cur_meta, palette = "Set1", style = input$bp_plot_type, log_scale = F, legend_pos = "top", textSize = 15, pointSize = 3)
    })




    ################################# DE Module ###################################

    des <- reactiveValues()

    ############ Interactive UI for adding new groups for comparison ###################

    output$de_metaclass_ui <- renderUI({
        options <- cmeta$factor_cols[!cmeta$factor_cols %in% c("time.point", "batch", "to.filter")]
        selectInput("de_metaclass", "Meta Class", choices = options)
    })

    output$de_g1_ui <- renderUI({
      req(input$de_metaclass, des$meta)
      reset_idx$g1
      groups <- as.character(levels(factor(des$meta[[input$de_metaclass]])))
      groups <- groups[!is.na(groups) & groups!="NA"]
      tagList(
          column(3, selectInput("de_g1_group", "Group 1", choices = groups, multiple = T))
      )
    })

    output$de_g2_ui <- renderUI({
        req(input$de_metaclass, des$meta)
        reset_idx$g2
        groups <- as.character(levels(factor(des$meta[[input$de_metaclass]])))
        groups <- groups[!is.na(groups) & groups!="NA"]
        tagList(
            column(3, selectInput("de_g2_group", "Group 2", choices = groups, multiple = T))
        )
    })



    output$add_clus_ui <- renderUI({
        req(input$de_metaclass, des$meta)
      input$de_new_add
      groups <- as.character(levels(factor(des$meta[[input$de_metaclass]])))
      groups <- groups[!is.na(groups) & groups!="NA"]
      dropdownButton2(inputId="add_clus",
                      selectInput("de_new_group", "Group", choices = groups, multiple = T),
                      actionButton("de_new_add", "Add Group", icon = icon("plus-circle"), class = "btn-danger btn_rightAlign"),
                      circle = T, label ="Add Another Group", tooltip=T, right = T,
                      icon = icon("plus"), size = "sm", status="primary", class = "btn_rightAlign")
    })



    observeEvent(input$de_new_add, {
        req(input$de_metaclass, input$de_new_group)
        groups <- des$meta[[input$de_metaclass]]
        gname <- c(paste0(input$de_new_group, collapse="_"))
        de_idx$idx_list[[length(de_idx$idx_list) + 1]] <- des$vis@idx[which(groups %in% input$de_new_group)]
        de_idx$group_name <- c(de_idx$group_name,gname)
    })


    output$cur_de_group <- renderUI({
        group <- de_idx$group_name
        group <- group[!group == ""]
        if(length(group) !=0) {
            checkboxGroupButtons(inputId = "remove_group", label = NULL, choices = group, individual = T,  status = "danger",
                                 checkIcon = list(yes = icon("remove", lib = "glyphicon"), no = icon("remove", lib = "glyphicon")))
        } else {
            tagList(
                tags$br(),
                actionLink("howtode", "How to run DE")
            )
        }
    })

    observeEvent(input$howtode, {
        content <- tagList(
            tags$li("First specify sample, then meta class, then which groups/subsets in that class to be used for DE comparison."),
            tags$li("If one group is specified, rest of cells will be treated as background and 1 vs background DE will be performed."),
            tags$li("Specify more than one group by clicking + button. One vs rest DE will be performed for each group."),
            tags$li("Use the green button to set proper DE parameters and downsample cells if necessary.")
        )
        showModal(modalDialog(
            title = "HOW TO RUN DE",
            size = "m",
            content,
            easyClose = TRUE
        ))
    })

    observe({
      req(input$de_metaclass, cmeta$df, des$vis)
        input$de_g1_group
        input$de_g2_group
        isolate({
            groups <- des$meta[[input$de_metaclass]]
            de_idx$idx_list[[1]] <- des$vis@idx[which(groups %in% input$de_g1_group)]
            de_idx$idx_list[[2]] <- des$vis@idx[which(groups %in% input$de_g2_group)]
            de_idx$group_name[1] <-c(paste0(input$de_g1_group, collapse="_"))
            de_idx$group_name[2] <-c(paste0(input$de_g2_group, collapse="_"))
        })
    })

    observe({
        req(input$de_metaclass)
        group <- de_idx$group_name
        group <- group[!group == ""]
        if(length(group) == 1) {
            isolate({
                groups <- des$meta[[input$de_metaclass]]
                de_idx$idx_list[[which(de_idx$group_name == "")[1]]] <- des$vis@idx[which(!groups %in% group)]
            })
        } else {
            isolate({
                for(j in which(de_idx$group_name == "")) de_idx$idx_list[[j]] <- list()
            })
        }
    })

    output$downsample_de_ui <- renderUI({
        group <- de_idx$group_name
        group <- group[!group == ""]
        if(length(group) == 1) {
            dropdownButton2(inputId="downsample_background",
                            numericInput("de_pval_cutoff", "P value cutoff", value = 0.01),
                            numericInput("downsample_gp_num", "Max Cell # in subset", value = 200),
                            numericInput("downsample_bg_num", "Max Cell # in background", value = 1000),
                            actionButton("downsample_de_bg", "Downsample Cells", class = "btn_rightAlign"),
                            circle = T, label ="DE configuration", tooltip=T, right = T,
                            icon = icon("angle-double-down"), size = "sm", status="success", class = "btn_rightAlign")
        } else {
            dropdownButton2(inputId="downsample_group",
                            numericInput("de_pval_cutoff", "P value cutoff", value = 0.01),
                            numericInput("downsample_num", "Max Cell # per Group", value = 200),
                            actionButton("downsample_de", "Downsample Cells", class = "btn_rightAlign"),
                            circle = T, label ="DE configuration", tooltip=T, right = T,
                            icon = icon("angle-double-down"), size = "sm", status="success", class = "btn_rightAlign")
        }

    })

    observeEvent(input$downsample_de,{
        de_idx$idx_list <- lapply(de_idx$idx_list, function(x) sample(x, min(length(x), input$downsample_num), replace=F))
        session$sendCustomMessage(type = "showalert", "Downsample complete.")
    })

    observeEvent(input$downsample_de_bg, {
        de_idx$idx_list <- lapply(1:length(de_idx$idx_list), function(i) {
            if(de_idx$group_name[i] == "") {
                if(length(de_idx$idx_list[[i]])) {
                    sample(de_idx$idx_list[[i]], min(length(de_idx$idx_list[[i]]), input$downsample_bg_num), replace=F)
                }
            } else {
                sample(de_idx$idx_list[[i]], min(length(de_idx$idx_list[[i]]), input$downsample_gp_num), replace=F)
            }
        })
        session$sendCustomMessage(type = "showalert", "Downsample complete.")
    })

    observe({
      req(input$remove_group)
      isolate({
        remove_idx <- which(de_idx$group_name == input$remove_group)
        if(length(remove_idx)) {
          if(remove_idx %in% c(1,2)) {
            de_idx$idx_list[remove_idx] <- NA
            de_idx$group_name[remove_idx] <- ""
            clusters <- as.character(des$meta$Cluster)
            if(remove_idx == 1) reset_idx$g1 <-reset_idx$g1+1
            if(remove_idx == 2) reset_idx$g2 <-reset_idx$g2+1
          } else {
            de_idx$idx_list[remove_idx] <- NULL
            de_idx$group_name <- de_idx$group_name[-remove_idx]
          }
        }
      })
    })


    output$de_sample_ui <- renderUI({
        options <- c(names(usr$clist), names(usr$elist))
        selectInput("de_sample", "Choose Sample:", choices=options)
    })

    observe({
        req(input$de_sample)
        sample <- input$de_sample
        cur_list <- c(usr$clist,usr$elist)
        idx <- cur_list[[sample]]@idx
        cur_meta <-  cmeta$df[idx,]
        if(length(cur_list[[sample]]@cluster) > 0){
            cur_meta$Cluster <- as.character(cur_list[[sample]]@cluster)
        }
        des$meta <- cur_meta
        des$vis <- cur_list[[sample]]
    })

    observe({
        req(des$vis, input$de_proj_type, des$meta)
        req(input$de_proj_type %in% names(des$vis@proj))
        isolate({
            des$proj <- cbind(des$vis@proj[[input$de_proj_type]], des$meta)
        })
    })

    output$de_plot_configure_ui <- renderUI({
        input$de_plot_config_reset
        dropdownButton2(inputId="de_plot_configure",
                        selectInput("de_plot_color_pal", "Color palette", choices=factor_palettes),
                        numericInput("de_plot_alpha_level", "Transparency", min = 0, max = 1, value = 0.01, step = 0.01),
                        numericInput("de_plot_marker_size", "Point Size", min = 0.1, max = 5, value = 2, step = 0.1),
                        selectInput("de_plot_legend_type", "Legend", choices = c("Color Legend", "On-plot Legend", "Both", "None"), selected="On-plot Legend"),
                        circle = T, label ="Configure Plot", tooltip=T, right = T,
                        icon = icon("cog"), size = "xs", status="primary", class = "btn_rightAlign")
    })


    output$de_plot2d_ui <- renderUI({
        if(input$interactive_de_plot) {
            plotlyOutput("de_plotly", height = "450px")
        } else {
            plotOutput("de_plot2d", height = "450px")
        }
    })

    output$de_proj_type_ui <- renderUI({
        req(des$vis)
        options <- names(des$vis@proj)
        options <- options[!options == "PCA"]
        selectInput("de_proj_type", NULL, choices=options)
    })

    output$de_plot2d <- renderPlot({
        req(pp2())
        pp2()
    })

    output$de_plotly <- renderPlotly({
        req(des$vis, des$proj, input$de_metaclass, input$de_plot_color_pal)
        proj <- des$proj
        ds <- paste0("V", 1:2)
        unique_factors <- unique(proj[[input$de_metaclass]])
        factor_color <- get_color_vector(unique_factors, pal=input$de_plot_color_pal, maxCol = 9)
        names(factor_color) <- unique_factors
        factor_color[["unannotated"]] <- "lightgrey"
        plotly::plot_ly(proj, x = as.formula(paste0("~", ds[1])), y = as.formula(paste0("~", ds[2])),
                        marker = list(size = input$de_plot_marker_size),
                        color = as.formula(paste0("~", input$de_metaclass)), colors = factor_color) %>%
            plotly::add_markers(text = proj[[input$de_metaclass]], hoverinfo = 'text') %>%
            hide_legend() %>% layout(xaxis=list(zeroline=F), yaxis=list(zeroline=F), dragmode='select')
    })

    pp2 <- reactive({
        req(des$vis, des$proj, input$de_metaclass, input$de_plot_color_pal, input$de_proj_type)
        idx <- des$vis@idx
        proj <- des$proj
        group <- de_idx$group_name
        group <- group[!group == ""]
        #assign("de_idx1", reactiveValuesToList(de_idx), env=.GlobalEnv)
        if(length(group) == 1) {
            aidx <- de_idx$idx_list[[which(de_idx$group_name!="")]]
        } else {
            aidx <- Reduce(union, de_idx$idx_list)
        }
        proj$alpha <- ifelse(idx %in% aidx, "f", "t")
        if(input$de_plot_legend_type == "Both") {legend=T; onplotText=T} else if(input$de_plot_legend_type == "Color Legend") {legend=T; onplotText=F} else if(input$de_plot_legend_type == "On-plot Legend"){legend=F; onplotText=T} else {legend=T; onplotText=T}
        plotProj(proj, dim_col = c(1,2), group.by=input$de_metaclass, pal=input$de_plot_color_pal, size = input$de_plot_marker_size, plot_title=NULL, legend_title = NULL, na_col = "lightgrey", alpha=proj$alpha, alpha_level=input$de_plot_alpha_level, legend=legend, onplotText = onplotText, onplotTextSize = 3)
    })

    observeEvent(input$run_de,{
        gidx <- which(lapply(de_idx$idx_list, length) != 0)
        if(length(gidx) <= 1) {
            session$sendCustomMessage(type = "showalert", "At least one group must be specified.")
            return()
        }

        if(sum(duplicated(unlist(de_idx$idx_list)))) {
            session$sendCustomMessage(type = "showalert", "Some groups have shared cells, please recheck.")
            return()
        }

        if(any(sapply(de_idx$idx_list[gidx], length) == 0)) {
            session$sendCustomMessage(type = "showalert", "Missing cells from one of the groups.")
            return()
        }

        if(length(unlist(de_idx$idx_list[gidx])) > 1e4) {
            session$sendCustomMessage(type = "showalert", "Too many cells specified. Please downsample first use the green button on the left.")
            return()
        }

        withProgress(message = 'Processing...', {
            incProgress(1/2)
            test_method = "sseq"
            test_group <- de_idx$group_name[gidx]
            test_group[which(test_group == "")] <- "Background"
            test_idx <- de_idx$idx_list[gidx]
            test_clus <- unlist(sapply(1:length(gidx), function(i){
                rep(test_group[i], length(test_idx[[i]]))
            }))
            test_idx <- unlist(test_idx)
            fdata <- data.frame(id = rownames(sexpr), symbol = rownames(sexpr))
            prioritized_genes <- runsSeq(dat=as.matrix(sexpr[, test_idx]), group=test_clus, fdata = fdata, order_by="pvalue", p_cutoff= input$de_pval_cutoff, min_mean = 0, min_log2fc = 0)
            #prioritized_genes <- prioritize_top_genes(gbm[, test_idx],test_clus, test_method, p_cutoff = .01, min_mean = 0, min_log2fc = 0, order_by = "pvalue")
        })
        de_list <- lapply(prioritized_genes, function(x) {
            x %>% dplyr::filter(significant == TRUE) %>% dplyr::select(gene_id, gene_name, common_mean, dispersion, log2fc, p, p_adj)
        })
        de_res$test_idx <- test_idx
        de_res$test_group <- test_group
        de_res$test_clus <- test_clus
        de_res$test_method <- test_method
        de_res$deg <- prioritized_genes
        de_res$de_list <- de_list
    })

    observe({
        req(!is.null(input$de_filter_tf))
        isolate({
            de_res$de_list <- lapply(de_res$deg, function(x) {
                tbl <- x %>% dplyr::filter(significant == TRUE) %>% dplyr::select(gene_id, gene_name, common_mean, dispersion, log2fc, p, p_adj)
                if(input$de_filter_tf) {
                    tbl <- tbl %>%
                        dplyr::filter(gene_name %in% tf_tbl$Name)
                }
                return(tbl)
            })
        })
    })


    output$de_hmap_ui <- renderUI({
        req(de_res$deg)
        if(input$interactive_hmap) {
            plotlyOutput("de_hmaply", height="600px")
        } else {
            plotOutput("de_hmap", height="600px")
        }
    })


    output$hmap_configure_ui <- renderUI({
        input$hmap_config_reset
        dropdownButton2(inputId="hmap_configure",
                        fluidRow(
                            column(6, selectInput("de_hmap_colorBy", "Heatmap Color By", choices = c("Group",cmeta$factor_cols)))
                        ),
                        fluidRow(
                            column(6, selectInput("hmap_color_pal", "Heatmap Color", choices=numeric_palettes)),
                            column(6, numericInput("hmap_numg", "Max #DEG/Group", min=2, max = 500, value = 50, step=1))
                        ),
                        fluidRow(
                            column(6, selectInput("hmap_dscale", "Data Scale", choices=list("scaled log2" = "scaled log2", "log2"="log2"))),
                            column(6, numericInput("hmap_pseudoc", "Pseudocount", min=1e-5, max = 1, value = 1, step=1e-3))
                        ),
                        fluidRow(
                            column(6, numericInput("hmap_limitlow", "Expr Limit Low", value = -2, step=1)),
                            column(6, numericInput("hmap_limithigh", "Expr Limit High", value = 2, step=1))
                        ),
                        circle = T, label ="Configure Heatmap", tooltip=T, right = T,
                        icon = icon("cog"), size = "xs", status="primary", class = "btn_rightAlign")
    })

    observe({
        req(de_res$deg, input$hmap_dscale)
        if(sum(sapply(de_res$de_list, nrow)) == 0) return()
        cur_list <- c(clist,elist)
        #isolate({
            # Color bar on top of heatmap
            de_res$cells_to_plot <- order_cell_by_clusters2(cmeta$df[de_res$test_idx,], de_res$test_clus)
            if(input$de_hmap_colorBy == "Group") {
                de_res$plot_group = NULL
                de_res$group_colour=NULL
            } else {
                sample <- input$de_sample
                idx <- cur_list[[sample]]@idx
                cur_meta <- cmeta$df[idx,]
                cur_meta$Cluster <- cur_list[[sample]]@cluster
                if(input$de_hmap_colorBy %in% cmeta$factor_cols) {
                    pal = input$de_plot_color_pal
                    cur_factor <- cur_meta[[input$de_hmap_colorBy]]
                    de_res$plot_group = cur_factor[match(de_res$test_idx, idx)]
                    unique_factors <- as.character(unique(cur_factor))
                    factor_color <- get_color_vector(unique_factors, pal=pal, maxCol = 9)
                    names(factor_color) <- unique_factors
                } else if(input$de_hmap_colorBy == "BestTime"){
                    pal ="RdYlBu" # Temporary use this for time for heatmap, otherwise could be complicated to implement
                    cur_factor <- factor(cur_meta[[input$de_hmap_colorBy]])
                    de_res$plot_group = cur_factor[match(de_res$test_idx, idx)]
                    unique_factors <- as.character(levels(cur_factor))
                    factor_color <- rev(get_color_vector(unique_factors, pal=pal, maxCol = 9))
                }
                de_res$group_colour = factor_color
            }
            # Color of heatmap
            if(!is.null(input$hmap_color_pal)) {
                de_res$heatmap_color = get_numeric_color(input$hmap_color_pal)
            } else {
                de_res$heatmap_color = colorRampPalette(rev(brewer.pal(n = 7, name = "RdYlBu")))(100)
            }
            if(input$hmap_dscale == "scaled log2"){
                de_res$scale = T
            } else if(input$hmap_dscale == "log2") {
                de_res$scale = F
            }
       # })

    })

    output$de_hmap <- renderPlot({
        req(de_res$de_list)
        shut_device <- dev.list()[which(names(dev.list()) != "quartz_off_screen")]
        if(length(shut_device)) dev.off(which = shut_device) # Make sure ggsave does not change graphic device
        if(sum(unlist(lapply(de_res$deg, function(x)x$significant))) < 2) return()
        withProgress(message="Rendering heatmap..", {
            de_res$hmap<-gbm_pheatmap2(sexpr_nmlog[, de_res$test_idx],
                                       genes_to_plot = de_res$deg,
                                       cells_to_plot=de_res$cells_to_plot,
                                       group=de_res$plot_group,
                                       group_colour=de_res$group_colour,
                                       log=F, pseudocount = input$hmap_pseudoc,
                                       scale = de_res$scale,
                                       heatmap_color = de_res$heatmap_color,
                                       n_genes=input$hmap_numg, fontsize=6, limits=c(input$hmap_limitlow, input$hmap_limithigh))
        })
        grid::grid.draw(de_res$hmap$gtable)
    })

    output$de_hmaply <- renderPlotly({
        req(de_res$de_list)
        #assign("de_res", reactiveValuesToList(de_res), env=.GlobalEnv)
        withProgress(message="Rendering heatmap..", {
            return(
                heatmaply_plot(sexpr_nmlog[, de_res$test_idx],
                               genes_to_plot = de_res$deg,
                               cells_to_plot=de_res$cells_to_plot,
                               group=de_res$plot_group,
                               group_colour=de_res$group_colour,
                               log=F, pseudocount = input$hmap_pseudoc,
                               scale = de_res$scale,
                               heatmap_color = de_res$heatmap_color,
                               n_genes=input$hmap_numg, fontsize=6, limits=c(input$hmap_limitlow, input$hmap_limithigh)) %>%
                plotly::layout(margin = list(l = 60, b = 30))
            )
        })
    })

    output$deg_summary <- DT::renderDataTable({
        req(de_res$de_list)
        tbl<- data.frame(clusters = names(de_res$de_list), number_de_genes = sapply(de_res$de_list, nrow))
        DT::datatable(tbl, rownames=F, options = list(searching=F, paging=F))
    })


    output$deg_tabs <- renderUI({
      do.call(
        what = shiny::tabsetPanel,
        args= purrr::map(de_res$test_group,.f = function(nm){
          shiny::tabPanel(title=nm,
                          DT::renderDataTable({
                            req(de_res$de_list)
                            DT::datatable(de_res$de_list[[nm]], selection = 'single') %>%
                              DT::formatRound(columns=c('common_mean', 'dispersion', 'log2fc', 'p', 'p_adj'), digits=3)
                          })
          )
        })
      )
    })

    output$download_hmap_ui <- renderUI({
        req(de_res$de_list)
        downloadButton("download_hmap", "Download Heatmap", class = "btn_rightAlign")
    })

    output$download_hmap <- downloadHandler(
        filename = function() {
            paste(input$de_sample, "_", paste0(de_idx$group_name, collapse="_vs_"), "_", Sys.Date(), '.pdf', sep='')
        },
        content = function(con) {
            req(de_res$hmap)
            pdf(con, width=7, height=8)
            grid::grid.draw(de_res$hmap$gtable)
            dev.off()
        }
    )

    output$download_de_res_ui <- renderUI({
        req(de_res$deg)
        fluidRow(
            column(6),
            column(3, checkboxInput("de_filter_tf", "Filter for TFs", value = F)),
            column(3, downloadButton("download_de_res", "Download DE Table", class = "btn_rightAlign"))
        )
    })

    output$download_de_res <- downloadHandler(
        filename = function() {
            paste(input$de_sample, "_", paste0(de_idx$group_name, collapse="_vs_"), "_", Sys.Date(), '_deg.xlsx', sep='')
        },
        content = function(con) {
            req(de_res$de_list)
            write.xlsx(de_res$de_list, file=con)
        }
    )


    output$go_ui <- renderUI({
        req(de_res$de_list)
        tagList(
            fluidRow(
                column(4, selectInput("go_type", "Choose Enrichment Type", choices =list("GO-BP" = "BP", "GO-MF" = "MF", "GO-CC" = "CC", "KEGG" = "kegg"))),
                column(8, tags$p(), actionButton("run_go", "Run Enrichment Analysis", class = "btn-info btn_rightAlign"))
            ),
            fluidRow(
              column(12,
                     uiOutput("go_tabs_ui")
              ),
                uiOutput("download_go_res_ui")
            )
        )
    })

    output$go_tabs_ui <- renderUI({
      do.call(
        what = shiny::tabsetPanel,
        args= purrr::map(de_res$test_group,.f = function(nm){
          shiny::tabPanel(title=nm,
                          DT::renderDataTable({
                            req(de_res$enrich_list)
                            if(is.null(de_res$enrich_list[[nm]]) || nrow(de_res$enrich_list[[nm]])==0){
                              return(DT::datatable(data.frame("No enrichment found."), colnames = "", rownames=F))
                            }
                            tbl <- de_res$enrich_list[[nm]] %>% dplyr::select(ID, Description, GeneRatio, BgRatio, pvalue, qvalue, geneID)
                            DT::datatable(tbl, rownames = F, options = list(scrollX = TRUE), selection = 'single') %>%
                              DT::formatRound(columns=c('pvalue', 'qvalue'), digits=3)
                          })
          )
        })
      )
    })


    observeEvent(input$run_go, {
        req(de_res$de_list)
        ## GO
        withProgress(message = 'Enrichment test in progress...', {
            incProgress(1/2)
            de_list <- de_res$de_list
            bg_list <- rownames(sexpr)
            enrich_list <- compute_go(de_list, bg_list, type = input$go_type, organism = "cel")
            de_res$enrich_list <- enrich_list
            de_res$enrich_type <- input$go_type
        })
    })


    output$download_go_res_ui <- renderUI({
        req(de_res$enrich_list)
        downloadButton("download_go_res", "Download Enrichment Table", class = "btn_rightAlign")
    })

    output$download_go_res <- downloadHandler(
        filename = function() {
            paste(input$de_sample, "_", paste0(de_idx$group_name, collapse="_vs_"), "_", Sys.Date(), "_", de_res$enrich_type, '.xlsx', sep='')
        },
        content = function(con) {
            req(de_res$enrich_list)
            write.xlsx(de_res$enrich_list, file=con)
        }
    )


    #################### Cell type marker table #################
    proxy = dataTableProxy('ct_marker_tbl')

    output$ct_marker_tbl <- DT::renderDataTable({
        req(ct$cur_show)
        DT::datatable(ct$cur_show, selection = 'single',
                      extensions=c("Buttons"),
                      options = list(searching=T, scrollX = TRUE,
                                     dom = 'Bfrtip',
                                     buttons = list(list(extend='copy', filename="celltypemarker"),
                                                 list(extend='csv', filename="celltypemarker"),
                                                 list(extend='excel', filename="celltypemarker"),
                                                 list(extend='pdf', filename="celltypemarker"),
                                                 list(extend='print', filename="celltypemarker"))
                                     ),
                      editable = TRUE)
    })

    observe({
        tbl <- ct$tbl
        if(length(input$ct_lineage) && input$ct_lineage != "") {
            tbl <- tbl[grepl(input$ct_lineage, tbl$Lineage_Name, ignore.case = T),]
        }
        if(length(input$ct_cell) && input$ct_cell != "") {
            tbl <- tbl[grepl(input$ct_cell, tbl$Cell, ignore.case = T),]
        }
        if(length(input$ct_tissue) && input$ct_tissue != "") {
            tbl <- tbl[grepl(input$ct_tissue, tbl$Tissue_Cluster, ignore.case = T),]
        }
        if(length(input$ct_marker) && input$ct_marker != "") {
            tbl <- tbl[grepl(input$ct_marker, tbl$Markers_Penn, ignore.case = T) | grepl(input$ct_marker, tbl$Markers_UW, ignore.case = T) | grepl(input$ct_marker, tbl$Possible_other_markers_etc, ignore.case = T),]
        }
        ct$cur_show <- tbl
    })

    observeEvent(input$ct_marker_tbl_cell_edit, {
        info = input$ct_marker_tbl_cell_edit
        str(info)
        i = info$row
        j = info$col
        v = info$value
        ig<-rownames(ct$cur_show[i,, drop=F])
        ct$tbl[ig, j] <- isolate(DT::coerceValue(v, ct$tbl[ig, j]))
        ct$cur_show[i, j] <- isolate(DT::coerceValue(v, ct$cur_show[i, j]))
        DT::replaceData(proxy, ct$cur_show, resetPaging = FALSE)  # important
    })

    gep <- reactiveValues()

    observe({
        req(input$ge_type_choice)
        cur_expr <- as.data.frame(avg_expr[[input$ge_type_choice]])
        if(grepl("Log10", input$ge_data_scale)) {
            cur_expr <- log10(cur_expr+1)
        }
        if(input$ge_order_by == "Variance") {
            val <- cur_expr %>%
                tibble::rownames_to_column("feature") %>%
                dplyr::mutate(variance = apply(cur_expr,1,var)) %>% # Compute variance of feature across sample
                dplyr::arrange(desc(variance))
        } else if(input$ge_order_by == "Fano Factor") {
            val <- cur_expr %>%
                tibble::rownames_to_column("feature") %>%
                dplyr::mutate(average = apply(cur_expr,1,mean)) %>%
                dplyr::mutate(variance = apply(cur_expr,1,var)) %>% # Compute variance of feature across sample
                dplyr::mutate(fano_factor = variance/average) %>%
                dplyr::arrange(desc(fano_factor))
        } else if(input$ge_order_by == "Expression Level") {
            val <- cur_expr %>%
                tibble::rownames_to_column("feature") %>%
                dplyr::mutate(average = apply(cur_expr,1,mean)) %>%
                dplyr::arrange(desc(average))
        }
        rownames(val) <- val$feature
        val <- val %>% dplyr::select(-feature)
        val <- val[,!(colnames(val)%in%c("variance", "average", "fano_factor"))]
        if(grepl("scaled", input$ge_data_scale)) {
            val <- t(scale(t(as.matrix(val))))
        }
        gep$value <- val[1:input$ge_plot_num,]
    })

    output$gep_tbl <- DT::renderDataTable({
        req(gep$value)
        DT::datatable(round(gep$value,2),options = list(scrollX = TRUE), selection = 'single')
    })

    output$download_ge_tbl <- downloadHandler(
        filename = function() {
            paste("expression_profile_", Sys.Date(), '.xlsx', sep='')
        },
        content = function(con) {
            if(input$ge_download_tbl_type == "cur") {
                write.xlsx(gep$value, file=con)
            } else {
                write.xlsx(as.data.frame(avg_expr[[input$ge_type_choice]]), file=con)
            }
        }
    )

    output$ge_hmaply <- renderPlotly({
        req(gep$value)
        return(
            withProgress(message="Rendering heatmap...", {
                groups <- colnames(gep$value)
                group_color <- get_color_vector(groups, "Set1")
                names(group_color) <- groups
                heatmaply_plot(gep$value,
                               genes_to_plot = rownames(gep$value),
                               cells_to_plot=NULL,
                               group=groups,
                               group_colour=group_color,
                               log=F, pseudocount = 0,
                               scale = F,
                               heatmap_color = colorRampPalette(rev(brewer.pal(n = 7, name = "RdYlBu")))(100),
                               n_genes=0, fontsize=6, limits=NULL) %>%
                    plotly::layout(margin = list(l = 60, b = 30))
            })
        )
    })

    output$ge_hmap_ui <- renderUI({
        req(gep$value)
        if(input$interactive_ge_hmap) {
            plotlyOutput("ge_hmaply", height="600px")
        } else {
            plotOutput("ge_hmap", height="600px")
        }
    })
    output$ge_hmap <- renderPlot({
        #shut_device <- dev.list()[which(names(dev.list()) != "quartz_off_screen")]
        #if(length(shut_device)) dev.off(which = shut_device) # Make sure ggsave does not change graphic device
        #assign("test1", gep$value, env = .GlobalEnv)
        withProgress(message="Rendering heatmap..", {
            groups <- colnames(gep$value)
            group_color <- get_color_vector(groups, "Set1")
            names(group_color) <- groups
            gep$hmap<-gbm_pheatmap2(gep$value,
                                    genes_to_plot = rownames(gep$value),
                                    cells_to_plot= NULL,
                                    group= groups,
                                    group_colour=group_color,
                                    log=F, pseudocount = 1,
                                    scale = F,
                                    cluster_rows = T,
                                    cluster_cols = T,
                                    heatmap_color = colorRampPalette(rev(brewer.pal(n = 7, name = "RdYlBu")))(100),
                                    n_genes=0, fontsize=6, limits=NULL)
        })
        grid::grid.draw(gep$hmap$gtable)
    })


    output$download_ge_hmap <- downloadHandler(
        filename = function() {
            paste(make.names(input$ge_type_choice), "_", Sys.Date(), '.pdf', sep='')
        },
        content = function(con) {
            req(gep$hmap)
            pdf(con, width=7, height=8)
            grid::grid.draw(gep$hmap$gtable)
            dev.off()
        }
    )

    output$meta_pie_tt <- renderPlotly({
        tbl<-as.data.frame(table(cmeta$df[["cell.type"]]))
        tbl <- tbl[-which(tbl[,1] %in% c("NA", "Germline", "unannotated")),]
        colnames(tbl) <- c("cell.type", "cell_number")
        pal <- get_color_vector(tbl[["cell.type"]])
        names(pal) <- tbl[["cell.type"]]
        plotly::plot_ly(tbl, labels = as.formula(paste("~", "cell.type")), values = ~cell_number, type = 'pie',textposition = 'inside',
                        textinfo = 'label+percent',
                        insidetextfont = list(color = '#FFFFFF'),
                        marker = list(colors = pal, line = list(color = '#FFFFFF', width = 1))) %>%
            plotly::layout(title = paste("Global composition of", "cell type"),
                           xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                           yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE)) %>% plotly::config(displayModeBar = F)
    })

    output$meta_line_tt <- renderPlotly({
        tbl <- cmeta$df %>%
            filter(!cell.type %in% c("NA", "Germline", "unannotated"))
        tbl1<- tbl %>%
            group_by(embryo.time) %>%
            summarise(countT=n())
        tbl2 <- tbl %>%
            group_by(cell.type, embryo.time) %>%
            summarise(countCT=n())
        tbl2$countT <- tbl1$countT[match(tbl2$embryo.time,tbl1$embryo.time)]
        tbl2$fraction <- tbl2$countCT/tbl2$countT
        pal <- get_color_vector(unique(tbl2[[1]]))
        names(pal) <-unique(tbl2[[1]])
        tbl2 %>%
            plot_ly(x=~embryo.time, y=~fraction,
                    type="scatter",color=~cell.type, colors = pal, mode="lines+markers") %>%
            plotly::layout(title = paste("Temporal composition of", "cell type"))
    })


    output$dynamic_gene_num_barplot <- renderPlot({
        req(input$dynamic_barplot_type, input$dynamic_barplot_bin,input$dynamic_barplot_scale)
        cur_meta <- cmeta$df
        cur_meta$time.bin <- cut(cur_meta$embryo.time,breaks=input$dynamic_barplot_bin)
        #tbl_sum <- cur_meta %>% dplyr::group_by(timebin_slingshot) %>% dplyr::count(Dataset)
        ggplot(cur_meta, aes_string(x="embryo.time", y=input$dynamic_barplot_type, group="time.bin", fill="time.bin")) +
            geom_boxplot()+
            coord_trans(y = input$dynamic_barplot_scale) +
            #scale_y_continuous(limits = c(0, quantile(cur_meta[[input$dynamic_barplot_type]],0.9)))+
            scale_fill_manual(values=rev(get_color_vector(levels(cur_meta$time.bin), "RdYlBu")))+
            theme_bw()
    })



    # Image plot for lineage

    # output$image_color_pal <- renderUI({
    #     req(input$image_colorBy)
    #     if(input$image_colorBy  == "lin16") {
    #         sel1<-selectInput("image_pal", "Palette", choices=factor_palettes)
    #     } else {
    #         sel1 <-
    #     }
    #     return(sel1)
    # })

    output$image_graph_plot_ui <- renderUI({
        req(input$image_ploth)
        plotOutput("image_graph_plot", height = paste0(500/5.5 *input$image_ploth,"px"))
    })

    output$image_graph_plot <- renderPlot({
        req(input$image_colorBy, input$image_pal)
        if(input$image_colorBy == "lin16") {
            pal = lin16_color
        } else {
            pal = input$image_pal
        }
        plotGraph(g_all, color.by=input$image_colorBy, pal=pal, label="name", type = ifelse(input$image_colorBy == "lin16", "factor", "numeric"))
    })



    #### Pseudotime DE result ###
    output$dy_proj_show <- renderPlot({
        req(input$dy_ctype_choice)
        cur_proj<-time_umap_list[[input$dy_ctype_choice]]
        if(is.null(input$dy_ctype_gene)) {
            cur_proj$estimated.embryo.time<-  factor(floor(pmeta[rownames(cur_proj), "raw.embryo.time"]/50) *50)
            embryo.time.bin.colors = gg_color_hue(length(levels(cur_proj$estimated.embryo.time)))
            names(embryo.time.bin.colors) <- levels(cur_proj$estimated.embryo.time)
            plotProj(cur_proj, dim_col = c(1,2), group.by="estimated.embryo.time", pal= embryo.time.bin.colors,  size = 2, plot_title=NULL, legend_title = NULL, na_col = "lightgrey", alpha=NULL, alpha_level=NULL, legend=T, trans = "identity", legend.size=2, legend.position = "top", legend.title = "Estimated\nembryo time\n(minutes)", keyheight=.5)
        } else {
            gene_values <- t(as.matrix(sexpr_nmlog[input$dy_ctype_gene,rownames(cur_proj), drop=F]))
            visualize_gene_expression(gene_values, input$dy_ctype_gene, cur_proj[,c(1,2)],limits=c(0,5),
                                      marker_size = 2, ncol=1,
                                      binary = ifelse(length(input$dy_ctype_gene) == 1, F, T),
                                      pal="rainbow2",
                                      alpha = rep("f", nrow(gene_values)),
                                      alpha_manual = c("f"=1,"t"=0.1),
                                      na_col = "lightgrey")
        }

    })

    output$dy_de_tbl <- DT::renderDataTable({
        req(input$dy_ctype_choice)
        cur_tbl <- time_deg_list[[input$dy_ctype_choice]] %>%
            dplyr::filter(qval <=0.01)
        if(input$dy_filter_tf) {
             cur_tbl <- cur_tbl %>% filter(gene_short_name %in% tf_tbl$Name)
        }
        DT::datatable(cur_tbl, rownames=F, options = list(scrollX = TRUE), selection = 'single') %>%
        DT::formatRound(columns=c('pval', 'qval'), digits=3)
    })

    output$dy_hmap_show <- renderPlot({
        req(input$dy_ctype_choice, input$dy_hmap_ngene > 2, input$dy_hmap_ecut)
        cur_proj<-time_umap_list[[input$dy_ctype_choice]]
        cur_tbl <- time_deg_list[[input$dy_ctype_choice]]
        if(input$dy_filter_tf) {
            tf_idx <- which(cur_tbl$gene_short_name %in% tf_tbl$Name)
            if(length(tf_idx)) {
                cur_tbl <- cur_tbl[tf_idx,]
            } else {
                return()
            }
        }
        cur_tbl <- cur_tbl[1:input$dy_hmap_ngene,]
        cur_proj$estimated.embryo.time<-  factor(floor(pmeta[rownames(cur_proj), "raw.embryo.time"]/50) *50)
        #assign("cur_tbl", cur_tbl, env=.GlobalEnv)
        expr_deg <-all_cds@auxOrderingData$normalize_expr_data[cur_tbl$id, rownames(cur_proj)]
        rownames(expr_deg) <- cur_tbl$gene_short_name

        # Further filter low expression genes
        expr_deg <- expr_deg[Matrix::rowMeans(expr_deg) > input$dy_hmap_ecut,]

        index_list <- order(as.numeric(as.character(cur_proj$estimated.embryo.time)), na.last =NA)

        cell_info <- as.character(rownames(cur_proj))
        ordered_cells <- lapply(index_list, function(x) {
            list(barcode = cell_info[x], ix = x)
        })
        names(ordered_cells) <- cur_proj$estimated.embryo.time
        embryo.time.bin.colors = gg_color_hue(length(levels(cur_proj$estimated.embryo.time)))
        names(embryo.time.bin.colors) <- levels(cur_proj$estimated.embryo.time)
        #assign("expr_deg", expr_deg, env=.GlobalEnv)
        #assign("ordered_cells", ordered_cells, env=.GlobalEnv)
        gbm_pheatmap2(expr_deg, genes_to_plot=rownames(expr_deg),cells_to_plot=ordered_cells,n_genes=5000, fontsize=6, limits=c(-2,2),cluster_rows =T, group_colour=embryo.time.bin.colors, clustering_method = "mcquitty", clustering_distance_rows = "correlation")
    })

    output$download_dy_res <- downloadHandler(
        filename = function() {
            req(input$dy_ctype_choice)
            paste(input$dy_ctype_choice, "_", Sys.Date(), 'dynamic_deg.xlsx', sep='')
        },
        content = function(con) {
            req(input$dy_ctype_choice)
            cur_tbl <- time_deg_list[[input$dy_ctype_choice]] %>%
                dplyr::filter(qval <=0.01)
            if(input$dy_filter_tf) {
                cur_tbl <- cur_tbl %>% filter(gene_short_name %in% tf_tbl$Name)
            }
            write.xlsx(cur_tbl, file=con)
        }
    )
}
