

#' @export
explorer_ui <- function(id) {
    ns <- NS(id)
    tagList(
        uiOutput(ns("eui")) %>% withSpinner(type = 6, size = 2)
    )
}

#' @export
explorer_server <- function(input, output, session, sclist, useid, cmeta = NULL, showcols_basic = NULL, showcols_advanced = NULL){
    ev <- reactiveValues(list = NULL, sample=NULL, vis=NULL, colorBy_state = "less")
    # Reactive variable storing all basic plot parameters
    pvals <- reactiveValues()

    observe({
        factor_cols <- sapply(colnames(cmeta$df)[-1], function(x) {
            ifelse(!is.numeric(cmeta$df[[x]]), x, NA)
        })
        ev$factor_cols <- c(factor_cols[!is.na(factor_cols)], "Cluster")
        ev$meta_custom <- c(ev$factor_cols[!ev$factor_cols %in% showcols_advanced])
    })
    
    output$eui <- renderUI({
        ns <- session$ns
        fluidRow(
            column(4,
                   wellPanel(
                       class = "SidebarControl",
                       uiOutput(ns("input_sample_ui")),
                       uiOutput(ns("proj_type_ui")),
                       uiOutput(ns("proj_colorBy_ui")),
                       selectizeInput(ns("gene_list"), "Search Gene:", choices = NULL, multiple = T),
                       conditionalPanel("input.proj_type == 'PCA-2D'",
                                        ns = ns,
                                        fluidRow(
                                            column(6, selectInput(ns("pca2d_v1"), NULL, choices = paste0("PC",1:8), selected = "PC1")),
                                            column(6, selectInput(ns("pca2d_v2"), NULL, choices = paste0("PC",1:8), selected = "PC2"))
                                        )
                       ),
                       conditionalPanel("input.proj_type == 'PCA-3D'",
                                        ns = ns,
                                        fluidRow(
                                            column(4, selectInput(ns("pca3d_v1"), NULL, choices = paste0("PC",1:8), selected = "PC1")),
                                            column(4, selectInput(ns("pca3d_v2"), NULL, choices = paste0("PC",1:8), selected = "PC2")),
                                            column(4, selectInput(ns("pca3d_v3"), NULL, choices = paste0("PC",1:8), selected = "PC3"))
                                        )
                       ),
                       uiOutput(ns("plot_scalecolor_ui")),
                       uiOutput(ns("g_limit_ui")),
                       uiOutput(ns("data_highlight")),
                       uiOutput(ns("selectCell_panel"))
                   )
            ),
            column(8,
                   fluidRow(
                       column(8),
                       column(4,
                              circleButton(ns("plot_config_reset"), icon = icon("undo"), size = "xs", status = "danger btn_rightAlign"),
                              shinyBS::bsTooltip(
                                  ns("plot_config_reset"),
                                  title = "Reset plot configuration",
                                  options = list(container = "body")
                              ),
                              uiOutput(ns("plot_configure_ui"))
                       )
                   ),
                   uiOutput(ns("plot_ui")) %>% withSpinner(),
                   # fluidRow(
                   #     column(8),
                   #     column(4,
                   #            materialSwitch2(inputId = ns("interactive_2dplot"), tags$b("interactive"), value = F, status = "success")
                   #     )
                   # ),
                   wellPanel(
                       fluidRow(
                           column(3, uiOutput(ns("explore_plotf_ui"))),
                           column(3, numericInput(ns("ploth"), "Plot Height", min=1, value = 7, step=1)),
                           column(3, numericInput(ns("plotw"), "Width [download]", min=1, value = 7, step=1)),
                           column(3, tags$br(), downloadButton(ns("download_explore_plot"), "Download", class = "btn-primary", style="width: 115px"))
                       )
                   )
            )
        )
    })

    output$input_sample_ui <- renderUI({
        ns <- session$ns
        sample_names <- names(ev$list)
        tagList(
            fluidRow(
                column(9, tags$b("Choose Sample:")),
                column(3, pivot_help_UI(ns("choose_sample_info"), title = NULL, label = NULL, icn="cog", type = "link", tooltip = F))
                # column(3,
                #        actionLink(ns("choose_sample_btn"), label = NULL, icon = icon("cog")),
                #        shinyBS::bsModal(id = ns("choose_sample_modal"), "Visualization of cell subsets", ns("choose_sample_btn"), content)
                # )
            ),
            fluidRow(
                column(12, selectInput(ns("input_sample"), NULL, choices=sample_names))
            )
        )
    })

    output$proj_type_ui <- renderUI({
        ns <- session$ns
        req(ev$vis)
        options <- names(ev$vis@proj)
        if("PCA" %in% options) options <- c(options[!options == "PCA"], "PCA-2D", "PCA-3D")
        selectInput(ns("proj_type"), "Choose Projection:", choices=options)
    })

    output$proj_colorBy_ui <- renderUI({
        ns = session$ns
        selectInput(ns("proj_colorBy"), "Color By", choices = c(showcols_basic, ev$meta_custom, "More options..."="moreop"))
    })

    output$plot_scalecolor_ui <- renderUI({
        ns = session$ns
        req(input$proj_colorBy)
        if(input$proj_colorBy == 'Gene Expression') {
            selectInput(ns("log_transform_gene"), "Data scale", choices=list("log2norm"="log2", "raw" = "raw"))
        } else if(!input$proj_colorBy %in% ev$factor_cols){
            selectInput(ns("log_transform"), "Data scale", choices=list("log10"="log10", "identity" = "identity"))
        } else {
            return()
        }
    })
    
    output$plot_configure_ui <- renderUI({
        input$plot_config_reset
        ns <- session$ns
        
        req(input$proj_colorBy)
        if(input$proj_colorBy %in% pmeta_attr$meta_id && !is.null(pmeta_attr$dpal)) {
            default_pal <- pmeta_attr$dpal[which(pmeta_attr$meta_id==input$proj_colorBy)]
        } else {
            default_pal <- NULL
        }
        
        if(input$proj_colorBy == 'Gene Expression') {
            sel<-selectInput(ns("numeric_pal"), "Palette", choices=numeric_palettes, selected=default_pal)
        } else if(input$proj_colorBy %in% ev$factor_cols){
            if(grepl("time.bin", input$proj_colorBy)) {
                sel <- selectInput(ns("numericbin_pal"), "Palette", choices=numeric_bin_color_opt(), selected=default_pal)
            } else {
                sel <- selectInput(ns("factor_pal"), "Palette", choices=factor_color_opt(), selected=default_pal)
            }
        } else {
            sel<- selectInput(ns("numeric_pal"), "Palette", choices=numeric_palettes, selected=default_pal)
        }
        
        dropdownButton2(inputId=ns("plot_configure"),
                        fluidRow(
                            column(6, numericInput(ns("marker_size"), "Point Size", min = 0.1, max = 5, value = 1, step = 0.1)),
                            column(6, numericInput(ns("text_size"), "Text Size", min = 1, max = 5, value = 3, step = 0.1))
                        ),
                        fluidRow(
                            column(6, sel),
                            column(6, selectInput(ns("legend_type"), "Legend", choices=c("Color Legend" = "l", "Onplot Label" = "ol", "Onplot Text" = "ot", "Legend + Label" = "lol", "Legend + Text" = "lot", "None" = "none")))
                        ),
                        numericInput(ns("alpha_level"), "Transparency (for cells not selected)", min = 0, max = 1, value = 0.01, step = 0.01),
                        circle = T, label ="Configure Plot", tooltip=T, right = T,
                        icon = icon("cog"), size = "xs", status="primary", class = "btn_rightAlign")
    })
    

    observe({
        req(input$proj_type, input$proj_colorBy)
        isolate({
            updateSelectInput(session, "numeric_pal", "Palette",selected=lapply(reactiveValuesToList(input), unclass)$numeric_pal )
            updateSelectInput(session, "log_transform_gene", "Data scale", selected=lapply(reactiveValuesToList(input), unclass)$log_transform_gene )
            updateSelectInput(session, "factor_pal", "Palette", selected=lapply(reactiveValuesToList(input), unclass)$factor_pal )
            updateSelectInput(session, "legend_type", "Legend", selected=lapply(reactiveValuesToList(input), unclass)$legend_type )
            updateSelectInput(session, "log_transform", "Data scale", selected=lapply(reactiveValuesToList(input), unclass)$log_transform)
            #updateNumericInput(session, "g_limit", "Expr Cut", value = lapply(reactiveValuesToList(input), unclass)$g_limit)
        })
    })
    
    updateSelectizeInput(session, "gene_list", "Search Gene:", choices = gene_symbol_choices, selected = NULL, server=T)

    output$plot_ui <- renderUI({
        ns <- session$ns
        req(input$proj_type)
        if(!grepl("3D", input$proj_type, ignore.case = T)) {
            # if(input$interactive_2dplot) {
            #     req(pp1_ly())
            #     plotlyOutput(ns("plotly2d"), height = paste0(500/5.5 *input$ploth,"px")) #%>% withSpinner()
            # } else {
                req(pp1())
            tagList(
                plotOutput(ns("plot2d"), height = paste0(500/5.5 *input$ploth,"px"), 
                           brush = brushOpts(
                               id = ns("plot2d_brush")
                           ),
                           hover = hoverOpts(id = ns("plot2d_hover"), delay = 50)), #%>% withSpinner()
                uiOutput(ns("plot2d_tooltip"))
            )
            #}
        } else {
            req(pp1_3d())
            plotlyOutput(ns("plotly3d"), height = paste0(500/5.5 *input$ploth,"px"), width = "100%") #%>% withSpinner()
        }
    })
    
    output$plot2d_tooltip <- renderUI({
        ns <- session$ns
        hover <- input$plot2d_hover
        #assign("hover", hover, env=.GlobalEnv)
        x <- nearPoints(pvals$proj, hover, maxpoints = 1)
        req(nrow(x) > 0)
        if(pvals$plot_class != "expression") {
            y <- as.character(x[[pvals$proj_colorBy]])
            tip <- paste0("<b>", pvals$proj_colorBy, ": </b>", y, "<br/>")
        } else {
            y <- round(pvals$gene_values[rownames(x),, drop=F],3)
            tip <- paste0(sapply(1:ncol(y), function(i) paste0("<b>", colnames(y)[i], ": </b>", y[[i]], "<br/>")), collapse = "")
        }
        req(length(y) > 0)
        style <- paste0("position:absolute; z-index:100; background-color: rgba(245, 245, 245, 0.65); ",
                        "left:", hover$coords_css$x + 2, "px; top:", hover$coords_css$y + 2, "px;",
                        "margin:5px; padding:5px 5px 0px 5px;")
        
        # actual tooltip created as wellPanel
        wellPanel(
            style = style,
            p(HTML(tip))
        )
    })
    

    output$data_highlight <- renderUI({
        req(ev$vis, input$proj_colorBy, !grepl("3D", input$proj_type))
        #req(!input$interactive_2dplot)
        input$gene_list
        ns <- session$ns
        isolate({
            if(input$proj_colorBy %in% ev$factor_cols) {
                factors <- as.character(levels(factor(ev$meta[[input$proj_colorBy]])))
                names(factors) <- factors
                return(
                    selectInput(ns("factor_compo"), "Choose cells belongs to:", choices = factors, multiple = T)
                )
            } else if(input$proj_colorBy != "Gene Expression") {
                num_range <- range(ev$meta[[input$proj_colorBy]])
                return(
                    sliderInput(ns("numeric_range"), label = "Select Range",
                                min = num_range[1], max = num_range[2],
                                value = num_range)
                )
            } else if(input$proj_colorBy == "Gene Expression"){
                curg <- input$gene_list
                if(length(curg)){
                    options <- list(
                        "Select cells that:" = "nulv"
                    )
                    if(length(curg) == 1) {
                        curg_opt <- "1g"
                        names(curg_opt) <- paste0("Express: ", curg)
                    } else {
                        curg_opt <- "mg"
                        names(curg_opt) <- paste0("Co-Express: ", paste(curg, collapse = ", "))
                    }
                    options <- c(options, curg_opt)
                    return(
                        selectInput(ns("cell_expr_gene"), NULL, choices = options, multiple = F)
                    )
                }
            }else {
                return()
            }
        })
    })

    observe({
        ev$list <- sclist[[useid]]
        rval$list <- sclist[[useid]]
    })

    observe({
        sample <- input$input_sample
        req(sample %in% names(ev$list), cmeta$df)
        isolate({
            ev$sample <- sample
            ev$vis <- ev$list[[sample]]
            idx <- ev$vis@idx
            cur_meta <- cmeta$df[ev$vis@idx,]
            if(!is.null(ev$vis@pmeta)) cur_meta <- cbind(cur_meta, ev$vis@pmeta)
            ev$meta <- cur_meta
        })
    })

    observe({
        input$gene_list
        isolate({
            if(!is.null(input$gene_list)){
                if(ev$colorBy_state == "less") {
                    updateSelectInput(session, "proj_colorBy", "Color By", choices = c(showcols_advanced, ev$meta_custom, "Less options..."="lessop"), selected = "Gene Expression")
                } else {
                    updateSelectInput(session, "proj_colorBy", "Color By", choices = c(showcols_advanced, ev$meta_custom, "More options..."="moreop"), selected = "Gene Expression")
                }
            }
        })
    })

    observe({
        req(input$proj_colorBy)
        if(input$proj_colorBy == "lessop") {
            updateSelectInput(session, "proj_colorBy", "Color By", choices = c(showcols_basic, ev$meta_custom, "More options..."="moreop"))
            ev$colorBy_state <- "more"
        } else if(input$proj_colorBy == "moreop") {
            updateSelectInput(session, "proj_colorBy", "Color By", choices = c(showcols_advanced, ev$meta_custom, "Less options..."="lessop"))
            ev$colorBy_state <- "less"
        } else if(input$proj_colorBy != "Gene Expression") {
            updateSelectizeInput(session, "gene_list", "Search Gene:", choices = gene_symbol_choices, selected = NULL, server=T)
        }
    })

    observeEvent(input$run_clust, {
        req(ev$vis)
        idx <- ev$vis@idx
        if(grepl("pca", input$proj_type)){
            showNotification("Clustering can only run with UMAP projection.", type="error", duration=10)
            return()
        }
        withProgress(message = 'Processing...', {
            incProgress(1/2)
            proj<=switch(input$proj_type,
                         umap2dl = ev$vis@umap[[1]]$umap2d,
                         umap2dh = ev$vis@umap[[2]]$umap2d,
                         umap2dj = ev$vis@umap[[3]]$umap2d,
                         umap3dl = ev$vis@umap[[1]]$umap3d,
                         umap3dh = ev$vis@umap[[2]]$umap3d,
                         umap3dj = ev$vis@umap[[3]]$umap3d)
            set.seed(2016)
            clus <- as.factor(louvain_clus(proj[,c(1,2)], k=input$recomp_clus_k, resolution = input$recomp_clus_res))
            # Update cluster for all projections
            ev$vis@pmeta$Cluster<- clus
            ev$list[[ev$sample]] <- ev$vis
        })
    })

    observe({
        req(input$input_sample, input$proj_type, input$proj_colorBy, !input$proj_colorBy %in% c('lessop', 'moreop'))
        # Prevent rendering twice when switching between gene expression and other colorBy
        if(input$proj_colorBy != "Gene Expression" && !is.null(input$gene_list)) {
            return()
        }
        
        plot_col <- if(grepl("3D", input$proj_type)) {paste0("V", 1:3)} else {paste0("V", 1:2)}
        if(grepl("PCA", input$proj_type)) {
            if(!grepl("3D", input$proj_type)) {
                req(input$pca2d_v1)
                plot_col <- c(input$pca2d_v1, input$pca2d_v2)
            } else {
                req(input$pca3d_v1, input$pca3d_v2, input$pca3d_v3)
                plot_col <- c(input$pca3d_v1, input$pca3d_v2, input$pca3d_v3)
            }
            ptype = "PCA"
        } else {
            ptype = input$proj_type
        }
        #isolate({
            req(ptype %in% names(ev$vis@proj))
            proj <- ev$vis@proj[[ptype]]
            idx <- ev$vis@idx
            req(nrow(proj) == nrow(ev$meta))
            proj <- cbind(proj, ev$meta)
        #})
        proj$alpha <- rep("f", length(nrow(proj)))
        
        gene_values <- NULL
        gene_exprlim <- NULL
        factor_color <- NULL
        trans <- NULL
        if(input$proj_colorBy %in% ev$factor_cols) {
            plot_class = "factor"
            if(!is.null(input$factor_compo)) {
                proj$alpha <- ifelse(proj[[input$proj_colorBy]] %in% input$factor_compo, "f", "t")
            }
            if(grepl("time.bin", input$proj_colorBy)) {
                req(input$numericbin_pal)
                factor_color <- get_numeric_bin_color(levels(proj[[input$proj_colorBy]]), palette = input$numericbin_pal)
                names(factor_color) <- levels(proj[[input$proj_colorBy]])
            } else {
                req(input$factor_pal)
                proj[[input$proj_colorBy]] <- as.character(proj[[input$proj_colorBy]])
                proj[[input$proj_colorBy]][is.na(proj[[input$proj_colorBy]])] <- "NA"
                unique_factors <- unique(proj[[input$proj_colorBy]])
                proj[[input$proj_colorBy]] <- factor(proj[[input$proj_colorBy]], levels=unique_factors)
                factor_color <- get_factor_color(unique_factors, pal=input$factor_pal, maxCol = 9)
                names(factor_color) <- unique_factors
            }

            factor_color[["unannotated"]] <- "lightgrey"
            factor_color[["NA"]] <- "lightgrey"
        } else {
            req(input$numeric_pal)
            plot_class <- "numeric"
            if(is.null(input$log_transform)) {
                trans = "identity"
            } else {
                trans = input$log_transform
            }
            if(input$proj_colorBy == "Gene Expression"){
                plot_class <- "expression"
                if(!is.null(input$gene_list)) {
                    if(length(input$gene_list) > 2) {
                        session$sendCustomMessage(type = "showalert", "Do not support more than 2 genes.")
                        return()
                    }
                    req(input$log_transform_gene)
                    if(input$log_transform_gene == "log2") {
                        gene_values <- t(as.matrix(all_cds@auxOrderingData$normalize_expr_data[input$gene_list,idx, drop=F]))
                    } else if(input$log_transform_gene == "raw") {
                        gene_values <- t(as.matrix(exprs(all_cds)[input$gene_list,idx, drop=F]))
                    }
                }
                if(!is.null(input$cell_expr_gene)) {
                    if(input$cell_expr_gene!="nulv") {
                        if(length(gene_values)) { # Fix
                            proj$alpha <- ifelse(rowSums(gene_values > 0) == ncol(gene_values), "f", "t")
                        }
                    }
                }
            } else {
                if(!is.null(input$numeric_range)) {
                    proj$alpha <- ifelse(proj[[input$proj_colorBy]] >= input$numeric_range[1] & proj[[input$proj_colorBy]] <= input$numeric_range[2], "f", "t")
                }
            }
        }
        
        legend=F; onplotAnnot=NULL
        if(!is.null(input$legend_type)) {
            if(input$legend_type == "l") {
                legend=T; onplotAnnot=NULL
            } else if(input$legend_type == "lot") {
                legend=T; onplotAnnot="text"
            } else if(input$legend_type == "lol") {
                legend=T; onplotAnnot="label"
            } else if(input$legend_type == "ot"){
                legend=F; onplotAnnot="text"
            } else if(input$legend_type == "ol"){
                legend=F; onplotAnnot="label"
            }
        } 
        
        pvals$proj <- proj
        pvals$proj_colorBy <- input$proj_colorBy
        pvals$plot_class <- plot_class
        pvals$plot_col <- plot_col
        pvals$factor_color <- factor_color
        pvals$numeric_pal <- input$numeric_pal
        pvals$marker_size <- input$marker_size
        pvals$text_size <- input$text_size
        pvals$factor_compo <- input$factor_compo
        pvals$alpha_level <-input$alpha_level
        pvals$log_transform <- trans
        pvals$gene_values <- gene_values
        pvals$expr_lim <- c(0,input$g_limit)
        pvals$legend = legend
        pvals$onplotAnnot = onplotAnnot
    })
    

    pp1 <- reactive({
        req(pvals$proj)
        assign("pvals", reactiveValuesToList(pvals), env = .GlobalEnv)
        #isolate({
            if(pvals$plot_class == "factor") {
                pp<-plotProj(pvals$proj, dim_col = which(colnames(pvals$proj) %in% pvals$plot_col), group.by=pvals$proj_colorBy, pal=pvals$factor_color, size = pvals$marker_size, plot_title=NULL, legend_title = NULL, na_col = "lightgrey", alpha=pvals$proj$alpha, alpha_level=pvals$alpha_level, legend=pvals$legend, onplotAnnot = pvals$onplotAnnot, onplotAnnotSize = pvals$text_size)
            }
            else if(pvals$plot_class == "numeric") {
                pp<-plotProj(pvals$proj, dim_col = which(colnames(pvals$proj) %in% pvals$plot_col), group.by=pvals$proj_colorBy, pal=pvals$numeric_pal, size = pvals$marker_size, plot_title=NULL, legend_title = NULL, na_col = "lightgrey", alpha=pvals$proj$alpha, alpha_level=pvals$alpha_level, legend=T, trans = pvals$log_transform)
            } else {
                if(is.null(pvals$gene_values)) {
                    pp<-ggplot(pvals$proj, aes_string(pvals$plot_col[1],pvals$plot_col[2])) +
                        geom_point(color="lightgrey", size=pvals$marker_size)+
                        theme_bw() +
                        theme(plot.title = element_text(hjust = 0.5), legend.position = c("top"))+ guides(alpha=F)
                } else {
                    pp<-visualize_gene_expression(pvals$gene_values, colnames(pvals$gene_values), pvals$proj[c(pvals$plot_col[1],pvals$plot_col[2])],
                                                  limits=pvals$expr_lim,
                                                  marker_size = pvals$marker_size, ncol=1,
                                                  binary = ifelse(ncol(pvals$gene_values) == 1, F, T),
                                                  pal=pvals$numeric_pal,
                                                  alpha =pvals$proj$alpha,
                                                  alpha_manual = c("f"=1,"t"=pvals$alpha_level),
                                                  na_col = "lightgrey",
                                                  legend_name = ifelse(input$log_transform_gene == "log2", "Expression\n(log normalized)", "Raw count"))
                }
            }
        #})

        return(pp)
    })

    
    ##### The code for ploting plotly in 2D is no longer used due to user side slow render ######
    pp1_ly <- reactive({
        req(pvals$proj, length(pvals$plot_col) == 2)
        proj <- pvals$proj
        ds <- pvals$plot_col
        marker_size <- pvals$marker_size * 2
        #alpha_manual <- c("f"=1,"t"=pvals$alpha_level)
        if(pvals$plot_class == "factor") {
            #assign("pvals1", reactiveValuesToList(pvals), env=.GlobalEnv)
            plotly::plot_ly(proj, x = as.formula(paste0("~", ds[1])), y = as.formula(paste0("~", ds[2])),
                            text=proj[[pvals$proj_colorBy]],
                            hoverinfo="text",
                            marker = list(size = marker_size),
                            #opacity=alpha_manual[proj$alpha],
                            source = source,
                            key = row.names(proj),
                            color = as.formula(paste0("~", pvals$proj_colorBy)), colors = pvals$factor_color) %>%
                plotly::add_markers() %>%
                layout(legend = list(orientation = 'h'))
        } else if(pvals$plot_class == "numeric") {
            rgb_scale_list<- numeric_rgb_range(col = get_numeric_color(pvals$numeric_pal), zgrey=F)
            if(pvals$log_transform == "log10") {
                color_formula <- as.formula(paste0("~log10(", pvals$proj_colorBy, ")"))
            } else {
                color_formula <- as.formula(paste0("~", pvals$proj_colorBy))
            }
            plotly::plot_ly(proj,
                            x = as.formula(paste0("~", ds[1])), y = as.formula(paste0("~", ds[2])),
                            text=proj[[pvals$proj_colorBy]],
                            hoverinfo="text",
                            source = source,
                            key = row.names(proj),
                            marker = list(size = marker_size,
                                          color = color_formula,
                                          colorscale = rgb_scale_list)) %>%
                plotly::add_markers(opacity=alpha_manual[proj$alpha]) %>%
                layout(legend = list(orientation = 'h'))
        } else {
            if(ncol(pvals$gene_values) > 2) {
                session$sendCustomMessage(type = "showalert", "Do not support more than 2 genes.")
                return()
            }
            visualize_expression_plotly(expr= pvals$gene_values, projection = proj, ds=ds, gene_probes = colnames(pvals$gene_values), limits = pvals$gene_exprlim, marker_size=marker_size, source = source, pal = pvals$numeric_pal)
        }
    })

    output$plot2d <- renderPlot({
        req(pp1())
        pp1()
    })

    output$plotly2d <- renderPlotly({
        req(pp1_ly())
        pp1_ly() %>% hide_legend() %>% layout(xaxis=list(zeroline=F), yaxis=list(zeroline=F), dragmode='select')
    })

    pp1_3d <- reactive({
        req(pvals$proj, length(pvals$plot_col) == 3)
        proj <- pvals$proj
        ds <- pvals$plot_col
        marker_size <- pvals$marker_size * 2
        #alpha_manual <- c("f"=1,"t"=pvals$alpha_level)
        if(pvals$plot_class == "factor") {
            plotly::plot_ly(proj, x = as.formula(paste0("~", ds[1])), y = as.formula(paste0("~", ds[2])), z = as.formula(paste0("~", ds[3])),
                            text=proj[[pvals$proj_colorBy]],
                            hoverinfo="text",
                            marker = list(size = marker_size),
                            #opacity=alpha_manual[proj$alpha],
                            source = source,
                            key = row.names(proj),
                            color = as.formula(paste0("~", pvals$proj_colorBy)), colors = pvals$factor_color) %>%
                plotly::add_markers() %>%
                layout(legend = list(orientation = 'h'))
        } else if(pvals$plot_class == "numeric") {
            rgb_scale_list<- numeric_rgb_range(col = get_numeric_color(pvals$numeric_pal), zgrey=F)
            if(pvals$log_transform == "log10") {
                color_formula <- as.formula(paste0("~log10(", pvals$proj_colorBy, ")"))
            } else {
                color_formula <- as.formula(paste0("~", pvals$proj_colorBy))
            }
            plotly::plot_ly(proj,
                            x = as.formula(paste0("~", ds[1])), y = as.formula(paste0("~", ds[2])), z = as.formula(paste0("~", ds[3])),
                            text=proj[[pvals$proj_colorBy]],
                            hoverinfo="text",
                            source = source,
                            key = row.names(proj),
                            marker = list(size = marker_size,
                                          color = color_formula,
                                          colorscale = rgb_scale_list)) %>%
                plotly::add_markers(opacity=alpha_manual[proj$alpha]) %>%
                layout(legend = list(orientation = 'h'))
        } else {
            visualize_expression_plotly(expr= pvals$gene_values, projection = proj, ds=ds, gene_probes = colnames(pvals$gene_values), limits = pvals$gene_exprlim, marker_size=marker_size, source = source, pal = pvals$numeric_pal)
        }
    })

    output$plotly3d <- renderPlotly({
        req(pp1_3d())
        pp1_3d() %>% hide_legend()
    })

    output$explore_plotf_ui <- renderUI({
        ns <- session$ns
        req(input$proj_type)
        if(!grepl("3D", input$proj_type)){
            choices <- list("png" = "png", "pdf" = "pdf", "eps" = "eps", "tiff" = "tiff")
        } else {
            choices <- list( "html" = "html")
        }
        selectInput(ns("plotf"), "Format", choices = choices, selected = choices[[1]])
    })

    output$download_explore_plot <- downloadHandler(
        filename = function(format = input$plotf) {
            fn_ext<-switch(format,
                           png = '.png',
                           tiff = '.tiff',
                           eps = '.eps',
                           pdf = '.pdf',
                           html = '.html'
            )
            paste('Plot-', Sys.Date(), fn_ext, sep='')
        },
        content = function(con, format = input$plotf) {
            req(input$plotw, input$ploth, format)
            fn_dev<-switch(format,
                           png = 'png',
                           tiff = 'tiff',
                           eps = 'eps',
                           pdf = 'pdf',
                           html = 'html'
            )
            if(fn_dev!='html') {
                req(pp1())
                ggsave(con, plot = pp1(), device = fn_dev, width = input$plotw, height = input$ploth)
                shut_device <- dev.list()[which(names(dev.list()) != "quartz_off_screen")]
                if(length(shut_device)) dev.off(which = shut_device) # Make sure ggsave does not change graphic device
            } else {
                req(pp1_3d())
                htmlwidgets::saveWidget(pp1_3d(), con)
            }
        }
    )

    output$download_data <- downloadHandler(
        filename = function(format = input$selectCell_goal) {
            fn_ext<-switch(format,
                           downcell = '.rds',
                           downmeta = '.csv'
            )
            paste('cedata-', ev$sample, format, "-", Sys.Date(), fn_ext, sep='')
        },
        content = function(con, format = input$selectCell_goal) {
            req(format, length(ev$cells))
            if(format == "downcell") {
                cur_cds <- all_cds[,ev$cells]
                tmp<-ev$meta %>% tibble::rownames_to_column("Cell")
                rownames(tmp) <- tmp$Cell
                pData(cur_cds) <- tmp
                cur_cds@auxOrderingData$normalize_expr_data <- NULL
                saveRDS(cur_cds, con, compress=F) # Not compress so that saving is faster
            } else if(format == "downmeta") {
                write.csv(ev$meta[ev$cells, ], con)
            }
        }
    )


    # Cell Select
    output$selectCell_panel <- renderUI({
        req(length(ev$cells) > 0)
        ns = session$ns
        selected_samples <- ev$cells
        ns <- session$ns
        tagList(
            fluidRow(
                column(12, selectInput(ns("selectCell_goal"), paste("Operation on", length(selected_samples), "cells"), choices = list(
                    "Name selected cell subset" = "addmeta",
                    "Compute new PCA/UMAP with selected cells" = "compdimr",
                    "Download expression data (Monocle cds format) of selected cells" = "downcell",
                    "Download meta data of selected cells" = "downmeta"
                )))
            ),
            conditionalPanel(
                "input.selectCell_goal == 'addmeta'", ns=ns,
                fluidRow(
                    column(6,
                           selectizeInput(ns("selectCell_meta_col"), "Meta Class", choices = ev$meta_custom, options=list(create=T)),
                           shinyBS::bsTooltip(
                               ns("selectCell_meta_col"),
                               title = "Type name and press enter to add a new meta class, delete it use the button on the right",
                               placement="top",
                               options = list(container = "body")
                           )),
                    column(6,
                           tags$br(),actionButton(ns("MetaCol_delete"), "Delete Class", class = "btn-danger btn_leftAlign")
                    )
                ),
                fluidRow(
                    column(6, textInput(ns("selectCell_group_name"), "Name Subset", placeholder="e.g., group 1")),
                    column(6, tags$br(),actionButton(ns("selectCell_add"), "Add Group", class = "btn-info btn_leftAlign"))
                )
            ),
            conditionalPanel(
                "input.selectCell_goal == 'downcell' || input.selectCell_goal == 'downmeta'", ns=ns,
                fluidRow(
                    column(12,
                           downloadButton(ns("download_data"), "Download Data", class = "btn-primary btn_rightAlign")
                    )
                )
            ),
            conditionalPanel(
                "input.selectCell_goal == 'compdimr'", ns=ns,
                fluidRow(
                    column(6,
                           selectInput(ns("compdimr_type"), "Compute:", choices = list("UMAP-2D" = "UMAP-2D", "UMAP-3D" = "UMAP-3D", "PCA" = "PCA"))
                    ),
                    column(6,
                           textInput(ns("compdimr_name"), "Sample name:", placeholder="e.g., Late Neurons")
                    )
                ),
                fluidRow(
                    column(6,
                           numericInput(ns("compdimr_mine"), "Umi >", value=1)
                    ),
                    column(6,
                           numericInput(ns("compdimr_minc"), "in cells", value=10)
                    )
                ),
                fluidRow(
                    column(6,
                           numericInput(ns("compdimr_disp"), "DispRatio", value=.5)
                    ),
                    column(6,
                           numericInput(ns("compdimr_numpc"), "NumPC", value=50, min=2)
                    )
                ),
                fluidRow(
                    column(6,
                           checkboxInput(ns("compdimr_batch"), tags$b("Correct batch"), F)
                    ),
                    column(6, actionButton(ns("compdimr_run"), "Compute", class = "btn-info btn_rightAlign"))
                )
            )
        )
    })

     observe({
         #req(!is.null(input$interactive_2dplot))
         #if(!input$interactive_2dplot) {
             if(!is.null(input$plot2d_brush)) {
                 area_selected<-input$plot2d_brush
                 plot_cols <- which(colnames(pvals$proj) %in% pvals$plot_col)
                 ev$cells <- rownames(ev$meta)[which(pvals$proj[[plot_cols[1]]] >= area_selected$xmin & pvals$proj[[plot_cols[1]]] <= area_selected$xmax & 
                           pvals$proj[[plot_cols[2]]] >= area_selected$ymin & pvals$proj[[plot_cols[2]]] <= area_selected$ymax)]
             } else {
                 req(input$proj_colorBy)
                 if(input$proj_colorBy %in% ev$factor_cols & !is.null(input$factor_compo)) {
                     ev$cells <- rownames(ev$meta)[ev$meta[[input$proj_colorBy]] %in% input$factor_compo]
                 } else if(input$proj_colorBy != "Gene Expression" & !is.null(input$numeric_range)) {
                     filter_std <- ev$meta[[input$proj_colorBy]] >= input$numeric_range[1] & ev$meta[[input$proj_colorBy]] <= input$numeric_range[2]
                     ev$cells <- rownames(ev$meta)[filter_std]
                 } else if(input$proj_colorBy == "Gene Expression") {
                     req(pvals$gene_values, input$cell_expr_gene)
                     if(input$cell_expr_gene!="nulv") {
                         ev$cells <- names(which(rowSums(pvals$gene_values > 0) == ncol(pvals$gene_values)))
                     } else {
                         ev$cells <- NULL
                     }
                 }
             }
         # } else {
         #    req(event()$key)
         #    ev$cells <- unlist(event()$key)
         # }

     })

    # Add by interactive mode
    observeEvent(input$selectCell_add, {
        if(nchar(input$selectCell_meta_col) < 1){
            session$sendCustomMessage(type = "showalert", "Please specify a meta class or create one.")
            return()
        }
        if(!is.na(as.numeric(input$selectCell_meta_col))) {
            session$sendCustomMessage(type = "showalert", "Number name not allowed.")
            return()
        }
        if(nchar(input$selectCell_group_name) < 1){
            session$sendCustomMessage(type = "showalert", "Please specify a name for the cell subset.")
            return()
        }
        if(length(ev$cells)) {
            rval$mclass = input$selectCell_meta_col
            rval$group_name = input$selectCell_group_name
            rval$cells <- ev$cells
        }
        showNotification(paste("New meta class:",  rval$group_name, "added"), type="message", duration=10)
        updateSelectInput(session, "proj_colorBy", "Color By", selected = rval$mclass)
        updateSelectInput(session, "selectCell_meta_col", "Meta Class", selected = rval$mclass)
    })


    observeEvent(input$MetaCol_delete, {
        req(input$selectCell_meta_col, nchar(input$selectCell_meta_col) >= 1)
        rval$mclass = input$selectCell_meta_col
        rval$cells = NULL
        rval$group_name = NULL
        showNotification(paste("Meta class:",  rval$mclass, "deleted"), type="message", duration=10)
    })

    callModule(pivot_help, "cellSelection", title = "Select and define cell groups:", size = "m", content = list(
        tags$li("In interactive 2D plot, you can select cells by drag on the plot."),
        tags$li("You can use the topright plotly menu to switch selection mode to lasso selection."),
        tags$li("Once cells are selected, you can make a new meta class to add annotation to the selected cells."),
        tags$li("First, in 'Meta class', type the new meta class name, press enter."),
        tags$li("Then with the new class selected, enter a name for the selected cell group, press 'Add Group'."),
        tags$li("You can now see the newly added meta class appear in 'Color By' menu."),
        tags$li("You can download the newly annotated cds file, or just download the new metadata.")
    ))

    callModule(pivot_help, "choose_sample_info", title = "Visualize cell subsets:", size = "m", content = list(
        tags$li("'Sample's are cell subsets which enable global and zoom-in exploration of the data."),
        tags$li("The tool contains sets of 'sample's that's generated by the developer and stored as part of the package."),
        tags$li("Users can create their own sample by using the cell selection tool, and running a UMAP/PCA with selected cells."),
        tags$li("You can delete user-created samples with menu below:"),
        tags$hr(),
        uiOutput(session$ns("choose_sample_del_ui"))
    ))

    output$choose_sample_del_ui <- renderUI({
        ns <- session$ns
        sample_names <- names(rval$list)
        fluidRow(
            column(8, selectInput(ns("del_sample"), "Choose Sample", choices=sample_names)),
            column(4, tags$br(),actionButton(ns("del_sample_btn"), "Delete", class = "btn-danger btn_leftAlign"))
        )
    })

    observeEvent(input$del_sample_btn, {
        req(input$del_sample)
        ns <- session$ns
        rval$list[[input$del_sample]] <- NULL
        rval$ustats <- "del"
        showNotification("Sample deleted.", type="message", duration=10)
    })


    output$g_limit_ui <- renderUI({
        ns <- session$ns
        req(input$gene_list)
        #glim <- round(quantile(pvals$gene_values, .95),1)
        fluidRow(
            column(6,
                   numericInput(ns("g_limit"), label = "Expression Cutoff", value = 5, min = 0)
            ),
            column(6,
                   tags$br(),
                   actionLink(ns("gene_histogram_trigger"), label = "Expression Histogram", icon = icon("chevron-right"))
            )
        )
    })

    observeEvent(input$gene_histogram_trigger, {
        ns = session$ns
        showModal(modalDialog(
            title = "Expression Histogram", size = "m",
            plotOutput(ns("gene_histogram_plot")),
            easyClose = TRUE
        ))
    })

    output$gene_histogram_plot <- renderPlot({
        req(pvals$gene_values)
        gname <- colnames(pvals$gene_values)
        hist(pvals$gene_values[,1], xlab=paste0(input$log_transform_gene, "expression"), main = paste0("Expression histogram of gene ", gname))
        abline(v = input$g_limit, col=c("red"), lty=c(2), lwd=c(3))
        text(x = input$g_limit, y = 100, "Expr cut")
    })


    observeEvent(input$compdimr_run, {
        req(ev$cells)
        if(is.null(input$compdimr_name) || input$compdimr_name == "") {
            session$sendCustomMessage(type = "showalert", "Enter a name first.")
            return()
        }
        if(!is.na(as.numeric(input$compdimr_name))) {
            session$sendCustomMessage(type = "showalert", "Number name not allowed.")
            return()
        }
        if(input$compdimr_name %in% c(names(sclist$clist), names(sclist$clist))) {
            session$sendCustomMessage(type = "showalert", "Name already taken.")
            return()
        }
        if(input$compdimr_batch) {
            resform <- "~as.factor(batch) + ~as.factor(batch) * raw.embryo.time"
        } else {
            resform <- NULL
        }

        withProgress(message = 'Processing...', {
            incProgress(1/2)
            set.seed(2018)
            #assign("ev1cells", ev$cells, env=.GlobalEnv)
            cds_oidx <- filter_cds(cds=all_cds[,ev$cells], min_detect=input$compdimr_mine, min_numc_expressed = input$compdimr_minc, min_disp_ratio=input$compdimr_disp)
            #assign("cds1", cds_oidx, env=.GlobalEnv)
            irlba_res <- compute_pca_cds(cds_oidx, num_dim =input$compdimr_numpc, scvis=NULL, use_order_gene = T, residualModelFormulaStr = resform, return_type="irlba")
            pca_proj <- as.data.frame(irlba_res$x)
            rownames(pca_proj) <- colnames(cds_oidx)
            newvis <- new("cvis", idx = match(ev$cells, colnames(all_cds)))
            newvis@proj[["PCA"]] <- pca_proj
            if(grepl("UMAP", input$compdimr_type)) {
                n_component = ifelse(grepl("2D", input$compdimr_type), 2, 3)
                newvis@proj[[paste0(input$compdimr_type, " [", input$compdimr_numpc, "PC]")]]<-compute_umap_pca(pca_proj, num_dim = input$compdimr_numpc, n_component=n_component)
            }
            rval$list[[input$compdimr_name]] <- newvis
            rval$ustats <- "add"
        })
        showNotification("Dimension reduction successfully computed and listed in samples.", type="message", duration=10)
    })

    rval <- reactiveValues(mclass = NULL, cells=NULL, group_name=NULL, ulist = list())
    return(rval)
}






