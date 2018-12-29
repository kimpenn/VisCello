
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
            #LiveInputs <- reactiveValuesToList(input)
            #r_state[names(LiveInputs)] <- LiveInputs
            #r_state <- lapply(reactiveValuesToList(input), unclass)
            r_data <- list(
                de_idx = reactiveValuesToList(de_idx),
                de_res = reactiveValuesToList(de_res),
                reset_idx = reactiveValuesToList(reset_idx),
                ct = reactiveValuesToList(ct),
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
                # if (exists("r_state", envir=tmpEnv, inherits=FALSE)) {
                #     assign("r_state", tmpEnv$r_state, envir=.GlobalEnv)
                #     lapply(names(r_state),
                #            function(x) session$sendInputMessage(x, list(value = r_state[[x]]))
                #     )
                # }
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
                    #assign("r_state", lapply(reactiveValuesToList(input), unclass), envir = .GlobalEnv)
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


    if (
        #exists("r_state") && 
        exists("r_data")) {
        de_idx = do.call(reactiveValues, r_data$de_idx)
        de_res = do.call(reactiveValues, r_data$de_res)
        reset_idx = do.call(reactiveValues, r_data$reset_idx)
        ct = do.call(reactiveValues, r_data$ct)
        cmeta = do.call(reactiveValues,r_data$cmeta)
        usr <- do.call(reactiveValues,r_data$usr)
        # lapply(names(r_state),
        #        function(x) session$sendInputMessage(x, list(value = r_state[[x]]))
        # )
        rm(r_data, envir = .GlobalEnv)
    } else {
        #r_state <- list()
        de_idx <- reactiveValues()
        de_idx$idx_list <- list(NA, NA)
        de_idx$group_name <- c("", "")
        de_res <- reactiveValues()
        reset_idx <- reactiveValues()
        cmeta <- reactiveValues(df=pData(all_cds))
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
                        #source = "main_dragselect", event = reactive(plotly::event_data("plotly_selected", source = "main_dragselect")),
                        cmeta = cmeta,
                        showcols_basic = ctype_cols_basic,
                        showcols_advanced = ctype_cols_advanced
    )

    rval2 <- callModule(explorer_server, id="early",
                        sclist = usr,
                        useid = "elist",
                        #source = "early_dragselect", event = reactive(plotly::event_data("plotly_selected", source = "early_dragselect")),
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
        df <- as.data.frame(as.matrix(all_cds@auxOrderingData$normalize_expr_data[input$bp_gene, cur_idx]))
        feature_plot(df, input$bp_gene, plot_by = input$bp_group, meta = cur_meta, palette = "Set1", style = input$bp_plot_type, log_scale = F, legend_pos = "top", textSize = 15, pointSize = 3)
    })



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

    
    
    
    

    output$meta_pie_tt <- renderPlotly({
        tbl<-as.data.frame(table(cmeta$df[["cell.type"]]))
        tbl <- tbl[-which(tbl[,1] %in% c("NA", "Germline", "unannotated")),]
        colnames(tbl) <- c("cell.type", "cell_number")
        pal <- get_factor_color(tbl[["cell.type"]])
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
        pal <- get_factor_color(unique(tbl2[[1]]))
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
            scale_fill_manual(values=rev(get_factor_color(levels(cur_meta$time.bin), "RdYlBu")))+
            theme_bw()
    })


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
            cur_proj$estimated.embryo.time<-  factor(floor(pData(all_cds)[rownames(cur_proj), "raw.embryo.time"]/50) *50)
            embryo.time.bin.colors = gg_color_hue(length(levels(cur_proj$estimated.embryo.time)))
            names(embryo.time.bin.colors) <- levels(cur_proj$estimated.embryo.time)
            plotProj(cur_proj, dim_col = c(1,2), group.by="estimated.embryo.time", pal= embryo.time.bin.colors,  size = 2, plot_title=NULL, legend_title = NULL, na_col = "lightgrey", alpha=NULL, alpha_level=NULL, legend=T, trans = "identity", legend.size=2, legend.position = "top", legend.title = "Estimated\nembryo time\n(minutes)", keyheight=.5)
        } else {
            if(length(input$dy_ctype_gene) > 2) {
                session$sendCustomMessage(type = "showalert", "Do not support more than 2 genes.")
                return()
            }
            gene_values <- t(as.matrix(all_cds@auxOrderingData$normalize_expr_data[input$dy_ctype_gene,rownames(cur_proj), drop=F]))
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
        cur_proj$estimated.embryo.time<-  factor(floor(pData(all_cds)[rownames(cur_proj), "raw.embryo.time"]/50) *50)
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
