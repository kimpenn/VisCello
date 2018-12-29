





de_ui <- function(id) {
    ns <- NS(id)
    tagList(
        wellPanel(
            fluidRow(
                column(3, uiOutput("de_sample_ui")),
                column(3, uiOutput("de_metaclass_ui")),
                uiOutput("de_g1_ui"),
                uiOutput("de_g2_ui")
            ),
            fluidRow(
                column(8, uiOutput("cur_de_group")),
                column(4,
                       actionButton("run_de", "Run DE", class = "btn-info btn_rightAlign"),
                       uiOutput("add_clus_ui"),
                       uiOutput("downsample_de_ui")
                )
            )
        ),
        fluidRow(
            column(5,
                   fluidRow(
                       column(8, uiOutput("de_proj_type_ui")),
                       column(4,
                              circleButton("de_plot_config_reset", icon = icon("undo"), size = "xs", status = "danger btn_rightAlign"),
                              shinyBS::bsTooltip(
                                  "de_plot_config_reset",
                                  title = "Reset plot configuration",
                                  options = list(container = "body")
                              ),
                              uiOutput("de_plot_configure_ui")
                       )
                   ),
                   uiOutput("de_plot2d_ui"),
                   materialSwitch(inputId = "interactive_de_plot", tags$b("interactive"), value = F, status = "success"),
                   tags$br(),
                   DT::dataTableOutput("deg_summary")
            ),
            column(7,
                   fluidRow(
                       column(12,
                              circleButton("hmap_config_reset", icon = icon("undo"), size = "xs", status = "danger btn_rightAlign"),
                              shinyBS::bsTooltip(
                                  "hmap_config_reset",
                                  title = "Reset heatmap configuration",
                                  options = list(container = "body")
                              ),
                              uiOutput("hmap_configure_ui")
                       )
                   ),
                   uiOutput("de_hmap_ui"),
                   fluidRow(
                       column(6, materialSwitch(inputId = "interactive_hmap", tags$b("interactive"), value = F, status = "success")),
                       column(6, uiOutput("download_hmap_ui"))
                   )
            )
        ),
        tags$hr(),
        fluidRow(
            column(12,
                   uiOutput("deg_tabs"),
                   uiOutput("download_de_res_ui")
            )
        ),
        tags$hr(),
        fluidRow(
            column(12,
                   uiOutput("go_ui")
            )
        )
    )
}


de_server <- function(input, output, session){
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
        factor_color <- get_factor_color(unique_factors, pal=input$de_plot_color_pal, maxCol = 9)
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
            cur_cds <- all_cds[, test_idx]
            gene_idx <- Matrix::rowSums(exprs(cur_cds)) > 0
            cur_cds <- cur_cds[gene_idx,]
            feature_data <- fData(cur_cds)[,c("id", "gene_short_name")]
            colnames(feature_data) <- c("id", "symbol")
            prioritized_genes <- runsSeq(dat=as.matrix(exprs(cur_cds)), group=test_clus, fdata = feature_data, order_by="pvalue", p_cutoff= input$de_pval_cutoff, min_mean = 0, min_log2fc = 0)
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
        de_res$feature_data <- feature_data
        de_res$feature_idx <- gene_idx
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
                factor_color <- get_factor_color(unique_factors, pal=pal, maxCol = 9)
                names(factor_color) <- unique_factors
            } else if(input$de_hmap_colorBy == "BestTime"){
                pal ="RdYlBu" # Temporary use this for time for heatmap, otherwise could be complicated to implement
                cur_factor <- factor(cur_meta[[input$de_hmap_colorBy]])
                de_res$plot_group = cur_factor[match(de_res$test_idx, idx)]
                unique_factors <- as.character(levels(cur_factor))
                factor_color <- rev(get_factor_color(unique_factors, pal=pal, maxCol = 9))
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
            de_res$hmap<-gbm_pheatmap2(all_cds@auxOrderingData$normalize_expr_data[de_res$feature_idx, de_res$test_idx],
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
                heatmaply_plot(all_cds@auxOrderingData$normalize_expr_data[de_res$feature_idx, de_res$test_idx],
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
            enrich_list <- compute_go(de_res$de_list, de_res$feature_data[["symbol"]], type = input$go_type, organism = "cel")
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
    
}





