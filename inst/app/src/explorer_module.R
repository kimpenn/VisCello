

#' @export
explorer_ui <- function(id) {
    ns <- NS(id)
    uiOutput(ns("eui")) %>% withSpinner(type = 3, size = 3, color.background = "white")
}

#' @export
explorer_server <- function(input, output, session, sclist, useid, cmeta = NULL){
    ev <- reactiveValues(list = NULL, sample=NULL, vis=NULL, colorBy_state = "less", cells = NULL, cell_source = NULL)
    # Reactive variable storing all basic plot parameters
    pvals <- reactiveValues()
    
    output$eui <- renderUI({
        ns <- session$ns
        eui <- fluidRow(
            column(4,
                   wellPanel(
                       class = "SidebarControl",
                       uiOutput(ns("input_sample_ui")),
                       uiOutput(ns("proj_type_ui")),
                       conditionalPanel("input.proj_type == 'PCA-2D'",
                                        ns = ns,
                                        fluidRow(
                                            column(6, selectInput(ns("pca2d_v1"), NULL, choices = paste0("PC",1:max_pc_show), selected = "PC1")),
                                            column(6, selectInput(ns("pca2d_v2"), NULL, choices = paste0("PC",1:max_pc_show), selected = "PC2"))
                                        )
                       ),
                       conditionalPanel("input.proj_type == 'PCA-3D'",
                                        ns = ns,
                                        fluidRow(
                                            column(4, selectInput(ns("pca3d_v1"), NULL, choices = paste0("PC",1:max_pc_show), selected = "PC1")),
                                            column(4, selectInput(ns("pca3d_v2"), NULL, choices = paste0("PC",1:max_pc_show), selected = "PC2")),
                                            column(4, selectInput(ns("pca3d_v3"), NULL, choices = paste0("PC",1:max_pc_show), selected = "PC3"))
                                        )
                       ),
                       uiOutput(ns("proj_colorBy_ui")),
                       selectizeInput(ns("gene_list"), "Search Gene:", choices = NULL, multiple = T),
                       uiOutput(ns("plot_scalecolor_ui")),
                       uiOutput(ns("data_highlight"))
                   ),
                   uiOutput(ns("selectCell_panel"))
            ),
            column(8,
                   fluidRow(
                       column(6),
                       column(6,
                              circleButton(ns("plot_config_reset"), icon = icon("undo"), size = "xs", status = "danger btn_rightAlign"),
                              shinyBS::bsTooltip(
                                  ns("plot_config_reset"),
                                  title = "Reset plot configuration",
                                  options = list(container = "body")
                              ),
                              uiOutput(ns("plot_configure_ui")),
                              dropdownButton2(inputId=ns("plot_download"),
                                              fluidRow(
                                                  column(6, numericInput(ns("down_ploth"), "Height", min=1, value = 7, step=1)),
                                                  column(6, numericInput(ns("down_plotw"), "Width", min=1, value = 7, step=1))
                                              ),
                                              fluidRow(
                                                  column(6, uiOutput(ns("explore_plotf_ui"))),
                                                  column(6, tags$br(), downloadButton(ns("download_explore_plot"), "Download", class = "btn-primary", style="width: 115px"))
                                              ),
                                              circle = T, label ="Download Plot", tooltip=T, right = T,
                                              icon = icon("download"), size = "xs", status="success", class = "btn_rightAlign"),
                              uiOutput(ns("g_limit_ui")),
                              uiOutput(ns("v_limit_ui")),
                              uiOutput(ns("factor_pie_ui"))
                       )
                   ),
                   uiOutput(ns("plot_ui")) %>% withSpinner()
            )
        )
        
        
        fui <- tagList(
            wellPanel(
                fluidRow(
                    column(3, uiOutput(ns("bp_sample_ui"))),
                    column(3, selectizeInput(ns("bp_gene"), "Search Gene:", choices = NULL, selected = NULL)),
                    column(3, uiOutput(ns("bp_colorBy_ui"))),
                    column(3, selectInput(ns("bp_log_transform_gene"), "Data scale", choices=list("Log2 normalized count"="log2", "Molecule (UMI) count" = "raw")))
                ),
                uiOutput(ns("bp_include_ui"))
            ),
            fluidRow(
                column(6),
                column(6, 
                       circleButton(ns("bp_plot_config_reset"), icon = icon("undo"), size = "xs", status = "danger btn_rightAlign"),
                       shinyBS::bsTooltip(
                           ns("bp_plot_config_reset"),
                           title = "Reset plot configuration",
                           options = list(container = "body")
                       ),
                       uiOutput(ns("bp_plot_configure_ui")),
                       dropdownButton2(inputId=ns("bp_plot_download"),
                                       fluidRow(
                                           column(6, numericInput(ns("bp_down_ploth"), "Height", min=1, value = 5, step=1)),
                                           column(6, numericInput(ns("bp_down_plotw"), "Width", min=1, value = 7, step=1))
                                       ),
                                       fluidRow(
                                           column(6, selectInput(ns("bp_plotf"), "Format", choices =  list("png","pdf","eps","tiff"))),
                                           column(6, tags$br(), downloadButton(ns("download_bp_plot"), "Download", class = "btn-primary", style="width: 115px"))
                                       ),
                                       circle = T, label ="Download Plot", tooltip=T, right = T,
                                       icon = icon("download"), size = "xs", status="success", class = "btn_rightAlign")
                      )
            ),
            uiOutput(ns("bp_gene_plot_ui")) 
        )
        
        tabsetPanel(
            id = ns("ct_tab"),
            tabPanel(
                value = "eui",
                tags$b("Data Visualization"),
                eui
            ),
            tabPanel(
                value = "fui",
                tags$b("Expression by Group"),
                fui
            )
        )
    })
    
    output$input_sample_ui <- renderUI({
        ns <- session$ns
        sample_names <- names(ev$list)
        selectInput(ns("input_sample"), tags$div("Choose Sample:", pivot_help_UI(ns("choose_sample_info"), title = NULL, label = NULL, icn="question-circle", type = "link", tooltip = F, style = "padding-left:10px;")), choices=sample_names)
    })

    output$proj_type_ui <- renderUI({
        ns <- session$ns
        #assign("ev", reactiveValuesToList(ev), env = .GlobalEnv)
        req(ev$vis)
        options <- names(ev$vis@proj)
        if("PCA" %in% options) options <- c(options[!options == "PCA"], "PCA-2D", "PCA-3D")
        tagList(
            selectInput(ns("proj_type"), "Choose Projection:", choices=options),
            conditionalPanel("1==0", textInput(ns("proj_type_I"), NULL, value = ev$sample))
        )
    })

    output$proj_colorBy_ui <- renderUI({
        ns = session$ns
        selectInput(ns("proj_colorBy"), "Color By", choices = c(showcols_meta, ev$meta_custom))
    })

    output$plot_scalecolor_ui <- renderUI({
        ns = session$ns
        req(input$proj_colorBy, !input$proj_colorBy %in% c("moreop", "lessop"))
        
        if(input$proj_colorBy == 'gene.expr') {
            selectInput(ns("log_transform_gene"), "Data scale", choices=list("Log2 normalized count"="log2", "Molecule (UMI) count" = "raw"))
        } else if(!input$proj_colorBy %in% ev$factor_cols){
            default_scale <- NULL
            if(input$proj_colorBy %in% pmeta_attr$meta_id && !is.null(pmeta_attr$dscale)) {
                default_scale <- pmeta_attr$dscale[which(pmeta_attr$meta_id==input$proj_colorBy)]
                if(is.na(default_scale)) default_scale <- NULL
            } 
            selectInput(ns("log_transform_val"), "Data scale", choices=list("Log10"="log10", "Identity" = "identity"), selected = default_scale)
        } else {
            return()
        }
    })
    
    output$plot_configure_ui <- renderUI({
        input$plot_config_reset
        ns <- session$ns
        
        dropdownButton2(inputId=ns("plot_configure"),
                        fluidRow(
                            column(6, numericInput(ns("marker_size"), "Point Size", min = 0.1, value = 2, step = 0.1)),
                            column(6, numericInput(ns("text_size"), "Text Size", min = 1, value = 3, step = 1))
                        ),
                        fluidRow(
                            column(6, selectInput(ns("color_pal"), "Palette", choices=factor_color_opt())),
                            column(6, selectInput(ns("legend_type"), "Legend", choices=c("Color Legend" = "l", "Onplot Label" = "ol", "Onplot Text" = "ot", "Legend + Label" = "lol", "Legend + Text" = "lot", "None" = "none"), selected = "ot"))
                        ),
                        fluidRow(
                            column(6, numericInput(ns("show_ploth"), "Height (resize window for width)", min=1, value = 7, step=1)),
                            column(6, numericInput(ns("alpha_level"), "Transparency (for cells not selected)", min = 0, max = 1, value = 0.01, step = 0.01))
                        ),
                        circle = T, label ="Configure Plot", tooltip=T, right = T,
                        icon = icon("cog"), size = "xs", status="primary", class = "btn_rightAlign")
    })
    
    observeEvent(input$proj_colorBy, {
        req(!input$proj_colorBy %in% c("moreop", "lessop"))
        if(input$proj_colorBy %in% pmeta_attr$meta_id && !is.null(pmeta_attr$dpal)) {
            default_pal <- pmeta_attr$dpal[which(pmeta_attr$meta_id==input$proj_colorBy)]
        } else {
            default_pal <- NULL
        }
        
        if(input$proj_colorBy == 'gene.expr') {
            updateSelectInput(session, "color_pal", "Palette", choices=numeric_palettes, selected=default_pal)
        } else if(input$proj_colorBy %in% ev$factor_cols){
            if(grepl("time.bin", input$proj_colorBy)) {
                updateSelectInput(session, "color_pal", "Palette", choices=numeric_bin_color_opt(), selected=default_pal)
            } else {
                updateSelectInput(session, "color_pal",  "Palette", choices=factor_color_opt(), selected=default_pal)
            }
        } else {
            updateSelectInput(session, "color_pal",  choices=numeric_palettes, selected=default_pal)
        }
        
    })

    observe({
        req(ev$cells)
        isolate({
            updateSelectInput(session, "selectCell_goal", selected=lapply(reactiveValuesToList(input), unclass)$selectCell_goal)
        })
    })
    
    updateSelectizeInput(session, "gene_list", "Search Gene:", choices = feature_options$features, selected = NULL, server=T)

    output$plot_ui <- renderUI({
        ns <- session$ns
        req(input$proj_type)
        if(!grepl("3D", input$proj_type, ignore.case = T)) {
            req(pp1())
            tagList(
                plotOutput(ns("plot2d"), height = paste0(500/5.5 *input$show_ploth,"px"), 
                           brush = brushOpts(
                               id = ns("plot2d_brush")
                           ),
                           hover = hoverOpts(id = ns("plot2d_hover"), delay = 50)), #%>% withSpinner()
                uiOutput(ns("plot2d_tooltip")),
                tags$p("Hint: Mouse over points to see the detailed annotation. Drag on plots to select cells. Set plot aesthetics (legend etc.) using cog button on topright.")
            )
        } else {
            req(pp1_3d())
            plotlyOutput(ns("plotly3d"), height = paste0(500/5.5 *input$show_ploth,"px"), width = "100%") #%>% withSpinner()
        }
    })
    
    output$plot2d_tooltip <- renderUI({
        ns <- session$ns
        hover <- input$plot2d_hover
        #assign("hover", hover, env=.GlobalEnv)
        x <- nearPoints(pvals$proj, hover, maxpoints = 1)
        req(nrow(x) > 0)
        if(pvals$plot_class != "expression" || is.null(ev$gene_values)) {
            y <- as.character(x[[pvals$proj_colorBy]])
            tip <- paste0("<b>", pvals$legend_title, ": </b>", y, "<br/>")
        } else {
            y <- round(ev$gene_values[rownames(x),, drop=F],3)
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
        req(ev$vis, input$proj_colorBy, !grepl("3D", input$proj_type), !input$proj_colorBy %in% c("moreop", "lessop"))
        #print(paste0("highlight:", input$proj_colorBy))
        input$gene_list
        input$log_transform_val
        
        ns <- session$ns
        proj_colorBy_dh <- input$proj_colorBy # This is necessary!!! See explanation in the observer
        ui1 <- NULL
        if(input$proj_colorBy %in% ev$factor_cols) {
            if(input$proj_colorBy %in% c("cell.type", "cell.subtype")) {
                factors <- names(which(table(ev$meta[[input$proj_colorBy]]) >= 10)) 
            } else {
                factors <- as.character(levels(factor(ev$meta[[input$proj_colorBy]])))
            }
            names(factors) <- factors
            ui1 <- selectInput(ns("factor_compo"), "Choose Cells:", choices = factors, multiple = T)
        } else if(input$proj_colorBy != "gene.expr") {
            #assign("ev",reactiveValuesToList(ev), env = .GlobalEnv)
            num_range <- range(ev$value, na.rm = T)
            num_range[1] <- floor_dec(num_range[1],2)
            num_range[2] <- ceiling_dec(num_range[2],2)
            ui1 <- sliderInput(ns("numeric_range"), label = "Select Range", min = num_range[1], max = num_range[2], value = num_range)
        } else if(input$proj_colorBy == "gene.expr"){
            curg <- gene_symbol_choices[input$gene_list]
            if(length(curg)){
                options <- list(
                    "All cells" = "nulv"
                )
                if(length(curg) == 1) {
                    curg_opt <- "1g"
                    names(curg_opt) <- paste0("Express: ", curg)
                } else {
                    curg_opt <- "mg"
                    names(curg_opt) <- paste0("Co-Express: ", paste(curg, collapse = ", "))
                }
                options <- c(options, curg_opt)
                ui1 <- selectInput(ns("cell_expr_gene"), "Choose Cells:", choices = options, multiple = F)
            }
        }
        return(tagList(
            ui1,
            conditionalPanel("1==0", ns = ns, textInput(ns("proj_colorBy_dh"), label = NULL, value = proj_colorBy_dh)) # This is necessary!!! See explanation in the observer
        ))
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
            if(!is.null(ev$vis@pmeta) && nrow(ev$vis@pmeta) == nrow(cur_meta)) cur_meta <- cbind(cur_meta, ev$vis@pmeta)
            ev$meta <- cur_meta
            ev$factor_cols <- sapply(colnames(ev$meta), function(x) {
                ifelse(!is.numeric(ev$meta[[x]]), x, NA)
            })
            ev$meta_custom <- colnames(ev$meta)[!colnames(ev$meta) %in% showcols_meta]
        })
    })

    observeEvent(input$gene_list, {
        updateSelectInput(session, "proj_colorBy", selected = "gene.expr")
    })

    observeEvent(input$proj_colorBy, {
        if(input$proj_colorBy != "gene.expr") {
            if(!is.null(input$gene_list)) {
                updateSelectInput(session, "gene_list", selected = "")
            }
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

    
    # Data dependent
    observe({
        input$plot_config_reset
        req(input$input_sample, input$proj_type, input$proj_colorBy, !input$proj_colorBy %in% c('lessop', 'moreop'))
        req(input$input_sample == input$proj_type_I) # Sync the two renderUIs
        
        if(!grepl("3D", input$proj_type)) req(input$proj_colorBy_dh == input$proj_colorBy)
        # Prevent rendering twice when switching between gene expression and other colorBy
        if(input$proj_colorBy != "gene.expr" && !is.null(input$gene_list)) {
            return()
        }
        
        if(grepl("PCA", input$proj_type)) {
            if(!grepl("3D", input$proj_type)) {
                req(input$pca2d_v1)
                plot_col <- c(input$pca2d_v1, input$pca2d_v2)
            } else {
                req(input$pca3d_v1, input$pca3d_v2, input$pca3d_v3)
                plot_col <- c(input$pca3d_v1, input$pca3d_v2, input$pca3d_v3)
            }
            ptype = "PCA"
            proj <- ev$vis@proj[[ptype]]
        } else {
            ptype = input$proj_type
            req(ptype %in% names(ev$vis@proj))
            proj <- ev$vis@proj[[ptype]]
            plot_col <- if(grepl("3D", input$proj_type)) {colnames(proj)[1:3]} else {colnames(proj)[1:2]}
        }
        
        req(nrow(proj) == nrow(ev$meta))
        proj <- cbind(proj, ev$meta)
        proj$alpha <- rep("f", length(nrow(proj)))

        gene_values <- NULL
        gene_exprlim <- NULL
        factor_color <- NULL
        trans <- NULL
        limits <- NULL
        factor_breaks <- waiver()
        if(input$proj_colorBy %in% ev$factor_cols) {
            plot_class = "factor"
            if(grepl("time.bin", input$proj_colorBy)) { 
                req(input$color_pal %in% numeric_bin_color_opt())
                factor_color <- get_numeric_bin_color(levels(proj[[input$proj_colorBy]]), palette = input$color_pal)
                names(factor_color) <- levels(proj[[input$proj_colorBy]])
            } else {
                req(input$color_pal %in% factor_color_opt())
                factor_color <- get_factor_color(unique(proj[[input$proj_colorBy]]), pal=input$color_pal, maxCol = 9)
                if(input$proj_colorBy == "to.filter") { # special case
                    factor_color <- rev(factor_color)
                }
                names(factor_color) <- unique(proj[[input$proj_colorBy]])
            }
            factor_color[["unannotated"]] <- "lightgrey"
            
            if(input$proj_colorBy %in% c("cell.type", "cell.subtype")) {
                factor_breaks <- names(which(table(proj[[input$proj_colorBy]]) >= 10)) 
            } else {
                factor_breaks <- names(factor_color)
            }
            factor_breaks <- factor_breaks[factor_breaks != "unannotated"]
            
            if(!is.null(input$factor_compo)) {
                proj$alpha <- ifelse(proj[[input$proj_colorBy]] %in% input$factor_compo, "f", "t")
            }
        } else {
            req(input$color_pal)
            if(input$proj_colorBy == "gene.expr"){
                plot_class <- "expression"
                if(length(input$gene_list) == 1) {
                    req(!is.na(input$g_limit))
                    req(input$g_limit_sample == ev$sample)
                    req(input$g_limit_ds == input$log_transform_gene)
                    req(input$g_limit_gene == input$gene_list)
                    #print(paste0(input$gene_list, ": ", input$g_limit))
                    limits <- c(0,input$g_limit)
                } 
                if(!is.null(input$cell_expr_gene)) {
                    if(input$cell_expr_gene!="nulv") {
                        proj$alpha <- ifelse(rowSums(ev$gene_values > 0) == ncol(ev$gene_values), "f", "t")
                    }
                }
            } else {
                plot_class <- "numeric"
                # !!! IMPORTANT !!! 
                # This evaluates if renderUI for v_limit has been updated, to prevent the plot from rendering twice
                # If not updated, previous v_limit UI's corresponding proj_colorBy will not be the same as current input$proj_colorBy, then reactive is aborted.
                req(ev$value_sample == ev$sample, 
                    input$proj_colorBy_vlim == input$proj_colorBy, 
                    input$v_limit_ds == input$log_transform_val,
                    input$v_limit_sample == ev$sample)
                req(input$v_limit)
                proj[[input$proj_colorBy]] <- ev$value
                limits<-c(min(proj[[input$proj_colorBy]]), input$v_limit)
                if(!is.null(input$numeric_range)) proj$alpha <- ifelse(proj[[input$proj_colorBy]] >= input$numeric_range[1] & proj[[input$proj_colorBy]] <= input$numeric_range[2], "f", "t")
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
        
        ev$proj <- proj # Save an original copy for zoom in
        pvals$proj <- proj
        pvals$proj_colorBy <- input$proj_colorBy
        pvals$legend_title <- pmeta_attr$meta_name[which(pmeta_attr$meta_id == pvals$proj_colorBy)]
        pvals$plot_class <- plot_class
        pvals$plot_col <- plot_col
        pvals$factor_color <- factor_color
        pvals$color_pal <- input$color_pal
        pvals$marker_size <- input$marker_size
        pvals$text_size <- input$text_size
        pvals$factor_compo <- input$factor_compo
        pvals$factor_breaks <- factor_breaks
        pvals$alpha_level <-input$alpha_level
        pvals$limits <- limits
        pvals$legend = legend
        pvals$onplotAnnot = onplotAnnot
        pvals$gene_values <- ev$gene_values
        pvals$log_transform_gene <- input$log_transform_gene
    })
    
    observe({
        req(input$proj_colorBy, input$log_transform_val)
        req(is.numeric(ev$meta[[input$proj_colorBy]]))
        if(input$log_transform_val == "log10") {
            ev$value <- log10(ev$meta[[input$proj_colorBy]] + 1) # +1 ok for pseudo? Be careful for small values! Don't allow log in future
            ev$value_sample <- ev$sample # Use this to sync up reactivity
        } else {
            ev$value <- ev$meta[[input$proj_colorBy]]
            ev$value_sample <- ev$sample
        }
    })
    
    observe({
        req(input$log_transform_gene)
        if(is.null(input$gene_list)) {
            ev$gene_values <- NULL
            return()
        }
        if(length(input$gene_list) > 2) {
            session$sendCustomMessage(type = "showalert", "Do not support more than 2 genes.")
            return()
        }
        if(any(duplicated(input$gene_list))) {
            session$sendCustomMessage(type = "showalert", "Duplicated gene name not allowed.")
            return()
        }
        gnames <- gene_symbol_choices[input$gene_list]
        if(input$log_transform_gene == "log2") {
            gvals <- t(as.matrix(eset@assayData$norm_exprs[gnames,ev$vis@idx, drop=F]))
        } else if(input$log_transform_gene == "raw") {
            gvals <- t(as.matrix(exprs(eset)[gnames,ev$vis@idx, drop=F]))
        }
        colnames(gvals) <- input$gene_list # duplicates?
        ev$gene_values <- gvals
    })

    
    pp_factor <- reactive({
        plotProj(pvals$proj, dim_col = which(colnames(pvals$proj) %in% pvals$plot_col), group.by=pvals$proj_colorBy, pal=pvals$factor_color, size = pvals$marker_size, plot_title=NULL, legend.title = pvals$legend_title, na.col = "lightgrey", alpha=pvals$proj$alpha, alpha_level=pvals$alpha_level, legend=pvals$legend, onplotAnnot = pvals$onplotAnnot, onplotAnnotSize = pvals$text_size, legend.text.size = pvals$text_size*3, ncol=4, breaks = pvals$factor_breaks)
    })    
    
    pp_numeric <- reactive({
        plotProj(pvals$proj, dim_col = which(colnames(pvals$proj) %in% pvals$plot_col), group.by=pvals$proj_colorBy, pal=pvals$color_pal, size = pvals$marker_size, plot_title=NULL, legend_title = pvals$legend_title, na.col = "lightgrey", alpha=pvals$proj$alpha, alpha_level=pvals$alpha_level, legend=T, trans = "identity", limits = pvals$limits)
    })
    
    pp_gene <- reactive({
        if(is.null(pvals$gene_values)) {
            ggplot(pvals$proj, aes_string(pvals$plot_col[1],pvals$plot_col[2])) +
                geom_point(color="lightgrey", size=pvals$marker_size)+
                theme_bw() +
                theme(plot.title = element_text(hjust = 0.5), legend.position = c("top"))+ guides(alpha=F)
        } else {
            visualize_gene_expression(pvals$gene_values, colnames(pvals$gene_values), pvals$proj[c(pvals$plot_col[1],pvals$plot_col[2])],
                                          limits=pvals$limits,
                                          marker_size = pvals$marker_size, ncol=1,
                                          binary = ifelse(ncol(pvals$gene_values) == 1, F, T),
                                          pal=pvals$color_pal,
                                          alpha =pvals$proj$alpha,
                                          alpha_manual = c("f"=1,"t"=pvals$alpha_level),
                                          na.col = "lightgrey",
                                          legend_name = ifelse(pvals$log_transform_gene == "log2", 
                                                               paste0(colnames(pvals$gene_values), " expression\n(log normalized)"), 
                                                               paste0(colnames(pvals$gene_values), " expression\n(raw count)")))
        }
    })

    pp1 <- reactive({
        req(length(pvals$plot_col) == 2, pvals$plot_class)
        assign("pvals", reactiveValuesToList(pvals),env=.GlobalEnv)
        if(pvals$plot_class == "factor") {
            pp_factor()
        } else if(pvals$plot_class == "numeric") {
            pp_numeric()
        } else {
            pp_gene()
        }
    })

    output$plot2d <- renderPlot({
        req(pp1())
        pp1()
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
                            key = row.names(proj),
                            color = as.formula(paste0("~", pvals$proj_colorBy)), colors = pvals$factor_color) %>%
                plotly::add_markers() %>%
                layout(legend = list(orientation = 'h'))
        } else if(pvals$plot_class == "numeric") {
            rgb_scale_list<- numeric_rgb_range(col = get_numeric_color(pvals$color_pal), zgrey=F)
            proj$show_value <- proj[[pvals$proj_colorBy]] # Show original value
            if(!is.null(pvals$limits)) {
                proj[[pvals$proj_colorBy]][proj[[pvals$proj_colorBy]] < pvals$limits[1]] <- pvals$limits[1]
                proj[[pvals$proj_colorBy]][proj[[pvals$proj_colorBy]] > pvals$limits[2]] <- pvals$limits[2]
            }
            plotly::plot_ly(proj,
                            x = as.formula(paste0("~", ds[1])), y = as.formula(paste0("~", ds[2])), z = as.formula(paste0("~", ds[3])),
                            text=proj$show_value,
                            hoverinfo="text",
                            key = row.names(proj),
                            marker = list(size = marker_size,
                                          color = as.formula(paste0("~", pvals$proj_colorBy)),
                                          colorscale = rgb_scale_list)) %>%
                plotly::add_markers(
                    #opacity=alpha_manual[proj$alpha]
                ) %>%
                layout(legend = list(orientation = 'h'))
        } else {
            visualize_expression_plotly(expr= pvals$gene_values, projection = proj, ds=ds, gene_probes = colnames(pvals$gene_values), limits = pvals$limits, marker_size=marker_size, pal = pvals$color_pal)
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
            req(input$down_plotw, input$down_ploth, format)
            fn_dev<-switch(format,
                           png = 'png',
                           tiff = 'tiff',
                           eps = 'eps',
                           pdf = 'pdf',
                           html = 'html'
            )
            if(fn_dev!='html') {
                req(pp1())
                ggsave(con, plot = pp1(), device = fn_dev, width = input$down_plotw, height = input$down_ploth)
                shut_device <- dev.list()[which(names(dev.list()) != "quartz_off_screen")]
                if(length(shut_device)) dev.off(which = shut_device) # Make sure ggsave does not change graphic device
            } else {
                req(pp1_3d())
                htmlwidgets::saveWidget(pp1_3d(), con)
            }
        }
    )
    
    output$download_bp_plot <- downloadHandler(
        filename = function(format = input$bp_plotf) {
            fn_ext<-switch(format,
                           png = '.png',
                           tiff = '.tiff',
                           eps = '.eps',
                           pdf = '.pdf'
            )
            paste('Plot-', Sys.Date(), fn_ext, sep='')
        },
        content = function(con, format = input$bp_plotf) {
            req(input$bp_down_plotw, input$bp_down_ploth, format)
            fn_dev<-switch(format,
                           png = 'png',
                           tiff = 'tiff',
                           eps = 'eps',
                           pdf = 'pdf'
            )
            if(fn_dev!='html') {
                req(bp1())
                ggsave(con, plot = bp1(), device = fn_dev, width = input$bp_down_plotw, height = input$bp_down_ploth)
                shut_device <- dev.list()[which(names(dev.list()) != "quartz_off_screen")]
                if(length(shut_device)) dev.off(which = shut_device) # Make sure ggsave does not change graphic device
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
                cur_eset <- eset[,ev$cells]
                tmp<-ev$meta %>% tibble::rownames_to_column("Cell")
                rownames(tmp) <- tmp$Cell
                pData(cur_eset) <- tmp
                saveRDS(cur_eset, con, compress=F) # Not compress so that saving is faster
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
        isolate({
            if(!is.null(input$selectCell_meta_col)) {
                meta_col_selected<-input$selectCell_meta_col
            } else {
                meta_col_selected<-NULL
            }
            # if(!is.null(input$selectCell_goal)) {
            #     goal_selected<-input$selectCell_goal
            # } else {
            #     goal_selected<-NULL
            # }
        })

        wellPanel(
            class = "SidebarControl",
            fluidRow(
                column(12, selectInput(ns("selectCell_goal"), paste("Operation on", length(selected_samples), "cells chosen by", ev$cell_source), choices = list(
                    "Zoom in to selected cells" = "zoom", 
                    "Name selected cell subset" = "addmeta",
                    "Compute new PCA/UMAP with selected cells" = "compdimr",
                    "Download expression data (ExpressionSet format) of selected cells" = "downcell",
                    "Download meta data of selected cells" = "downmeta"
                )))
            ),
            conditionalPanel(
                "input.selectCell_goal == 'addmeta'", ns=ns,
                fluidRow(
                    column(6,
                           selectizeInput(ns("selectCell_meta_col"), "Meta Class", choices = ev$meta_custom, options=list(create=T), selected = meta_col_selected),
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
                "input.selectCell_goal == 'zoom'", ns=ns,
                fluidRow(
                    column(6,
                           textInput(ns("zoom_name"), "Sample name:", placeholder="optional")
                    ),
                    column(6,
                           tags$br(),
                           actionButton(ns("zoom_in"), "Zoom in", class = "btn-primary btn_rightAlign")
                    )
                ),
                tags$li("Provide a name to create a new visualization (sample)."),
                tags$li("Zoom out by click topright reset button.")
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
                selectInput(ns("compdimr_type"), "Compute:", choices = list("UMAP-2D" = "UMAP-2D", "UMAP-3D" = "UMAP-3D", "PCA" = "PCA")),
                textInput(ns("compdimr_name"), "Sample name:", placeholder="e.g., Late Neurons"),
                numericInput(ns("compdimr_expr_cut"), "Gene cutoff (expressed cell fraction)", value=.01, max = .5, min = 0),
                numericInput(ns("compdimr_numpc"), "NumPC", value=50, min=2),
                selectInput(ns("compdimr_bcol"), label = "Batch col", choices = colnames(cmeta$df)),
                checkboxInput(ns("compdimr_batch"), tags$b("Correct batch"), F),
                actionButton(ns("compdimr_run"), "Compute", class = "btn-info btn_rightAlign")
            )
        )
    })

     observe({
         req(input$plot2d_brush)
         isolate({
             area_selected<-input$plot2d_brush
             plot_cols <- which(colnames(pvals$proj) %in% pvals$plot_col)
             ev$cells <- rownames(pvals$proj)[which(pvals$proj[[plot_cols[1]]] >= area_selected$xmin & pvals$proj[[plot_cols[1]]] <= area_selected$xmax & 
                                                     pvals$proj[[plot_cols[2]]] >= area_selected$ymin & pvals$proj[[plot_cols[2]]] <= area_selected$ymax)]
             ev$cell_source <- "plot selection"
         })
     })
     
     observe({
         req(input$proj_colorBy)
         input$factor_compo
         input$numeric_range
         input$cell_expr_gene
         
         if(input$proj_colorBy %in% ev$factor_cols) {
             req(input$factor_compo)
             ev$cells <- rownames(ev$meta)[ev$meta[[input$proj_colorBy]] %in% input$factor_compo]
         } else if(input$proj_colorBy != "gene.expr") {
             req(input$numeric_range)
             vals <- ev$value
             filter_std <- vals >= input$numeric_range[1] & vals <= input$numeric_range[2]
             ev$cells <- rownames(ev$meta)[filter_std]
         } else if(input$proj_colorBy == "gene.expr") {
             if(is.null(ev$gene_values)) {
                 ev$cells <- NULL
                 return()
             }
             req(input$cell_expr_gene)
             if(input$cell_expr_gene!="nulv") {
                 ev$cells <- names(which(rowSums(ev$gene_values > 0) == ncol(ev$gene_values)))
             } else {
                 ev$cells <- NULL
             }
         }
         ev$cell_source <- input$proj_colorBy 
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
        updateSelectInput(session, "selectCell_goal", selected = "addmeta")
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
        input$plot_config_reset
        req(input$gene_list)
        if(length(input$gene_list) == 1) {
            gvals <- ev$gene_values[,1]
            glim <- round(quantile(gvals[gvals!=0], .975),1)
            if(is.na(glim)) glim = 1
            if(!is.na(glim) && glim < 2) glim = 2  # Minimal max-cut 
            dropdownButton2(inputId=ns("val_cutoff"),
                            width = "500px",
                            plotOutput(ns("gene_histogram_plot")),
                            fluidRow(
                                column(6, numericInput(ns("g_limit"), 
                                                       label = "Expression Cutoff", 
                                                       value = glim, min = 0)),
                                column(6, tags$p("Red line indicate max value for color scale. Default cutoff is set at 97.5th percentile."))
                            ),
                            conditionalPanel("1==0", ns = ns, textInput(ns("g_limit_ds"), label = NULL, value = input$log_transform_gene)),
                            conditionalPanel("1==0", ns = ns, textInput(ns("g_limit_sample"), label = NULL, value = input$input_sample)),
                            conditionalPanel("1==0", ns = ns, textInput(ns("g_limit_gene"), label = NULL, value = input$gene_list)),
                            circle = T, label ="Expression histogram and color scale cutoff", tooltip=T, right = T,
                            icon = icon("chart-bar"), size = "xs", status="info", class = "btn_rightAlign")
        } else {
            return()
            # tagList(
            #     conditionalPanel("1==0", ns = ns, textInput(ns("g_limit_sample"), label = NULL, value = input$input_sample)),
            #     conditionalPanel("1==0", ns = ns, numericInput(ns("g_limit"), label = NULL, value = NA))
            # )
        }
    })

    output$gene_histogram_plot <- renderPlot({
        req(ev$gene_values)
        gname <- colnames(ev$gene_values)
        hist(ev$gene_values[,1], xlab=paste0(input$log_transform_gene, "expression"), main = paste0("Expression histogram of gene ", gname))
        abline(v = input$g_limit, col=c("red"), lty=c(2), lwd=c(3))
    })

    output$v_limit_ui <- renderUI({
        ns <- session$ns
        input$plot_config_reset
        req(!input$proj_colorBy %in% c(ev$factor_cols, 'gene.expr'))
        v_limit <- round(quantile(ev$value, .975, na.rm=T), 1)
        dropdownButton2(inputId=ns("v_cutoff"),
                        width = "500px",
                        plotOutput(ns("value_histogram_plot")),
                        fluidRow(
                            column(6, numericInput(ns("v_limit"), label = "Cutoff", value = v_limit, min = 0)),
                            column(6, tags$p("Red line indicate max value for color scale. Default cutoff is set at 97.5th percentile."))
                        ),
                        conditionalPanel("1==0", ns = ns, textInput(ns("proj_colorBy_vlim"), label = NULL, value = input$proj_colorBy)), # This is necessary!!! See explanation in the observer
                        conditionalPanel("1==0", ns = ns, textInput(ns("v_limit_ds"), label = NULL, value = input$log_transform_val)),
                        conditionalPanel("1==0", ns = ns, textInput(ns("v_limit_sample"), label = NULL, value = input$input_sample)),
                        circle = T, label ="Histogram and color scale cutoff", tooltip=T, right = T,
                        icon = icon("chart-bar"), size = "xs", status="info", class = "btn_rightAlign")
    })
    
    output$value_histogram_plot <- renderPlot({
        req(pvals$plot_class == "numeric")
        vals <- pvals$proj[[pvals$proj_colorBy]]
        hist(vals, xlab=pvals$legend_title, main = paste0("Histogram of ", pvals$legend_title))
        abline(v = input$v_limit, col=c("red"), lty=c(2), lwd=c(3))
    })
    
    
    
    output$factor_pie_ui <- renderUI({
        ns <- session$ns
        req(input$proj_colorBy %in% c(ev$factor_cols))
        dropdownButton2(inputId=ns("factor_pie_drop"),
                        width = "500px",
                        plotOutput(ns("factor_pie_plot")),
                        DT::dataTableOutput(ns("factor_comp_table")),
                        circle = T, label ="Pie chart of factor composition", tooltip=T, right = T,
                        icon = icon("chart-pie"), size = "xs", status="info", class = "btn_rightAlign")
    })
    
    output$factor_comp_table <- DT::renderDataTable({
        compos<-as.data.frame(table(pvals$proj[[pvals$proj_colorBy]]))
        colnames(compos) <- c(pvals$proj_colorBy, "Freq")
        compos$Fraction <- round(compos$Freq/sum(compos$Freq),3)
        DT::datatable(compos, rownames = F)
    })
    
    pp_pie<-reactive({
        req(pvals$proj_colorBy)
        compos<-as.data.frame(table(pvals$proj[[pvals$proj_colorBy]]))
        colnames(compos) <- c(pvals$proj_colorBy, "Freq")
        compos$Fraction <- round(compos$Freq/sum(compos$Freq),3)
        compos$label <- scales::percent(compos$Fraction)
        ggplot(data=compos,aes_string(x=1, y="Fraction", fill=pvals$proj_colorBy))+
            geom_bar(stat="identity", width = 1)+
            coord_polar("y")+
            scale_fill_manual(values=pvals$factor_color) + 
            theme_void()+
            geom_text(aes(label=label),  position = position_stack(vjust = 0.5)) 
    })
    
    output$factor_pie_plot <- renderPlot({
        pp_pie()
    })
    
    observeEvent(input$zoom_in, {
        req(ev$cells)
        if(input$zoom_name == "") {
            pvals$proj <- ev$proj[ev$cells,]
            factor_breaks <- waiver()
            if(pvals$proj_colorBy %in% ev$factor_cols) {
                if(input$proj_colorBy %in% c("cell.type", "cell.subtype")) {
                    factor_breaks <- names(which(table(pvals$proj[[pvals$proj_colorBy]]) >= 10)) 
                } else {
                    factor_breaks <- unique(pvals$proj[[pvals$proj_colorBy]])
                }
                factor_breaks <- factor_breaks[factor_breaks != "unannotated"]
            } 
            pvals$factor_breaks <- factor_breaks
            if(!is.null(ev$gene_values)) pvals$gene_values <- ev$gene_values[ev$cells,, drop=F]
            return()
        }
        if(!is.na(as.numeric(input$zoom_name))) {
            session$sendCustomMessage(type = "showalert", "Number name not allowed.")
            return()
        }
        if(input$zoom_name %in% c(names(sclist$clist), names(sclist$elist))) {
            session$sendCustomMessage(type = "showalert", "Name already taken.")
            return()
        }
        newvis <- new("Cello", idx = match(ev$cells, colnames(eset)))
        newvis@proj[[input$proj_type]] <- pvals$proj[ev$cells, pvals$plot_col]
        rval$list[[input$zoom_name]] <- newvis
        rval$ustats <- "add"
        updateSelectInput(session, "input_sample", selected = input$zoom_name)
    })

    observeEvent(input$compdimr_run, {
        req(ev$cells)
        
        error_I <- 0
        tryCatch({
            #reticulate::import("umap")
        }, warning = function(w) {
        }, error = function(e) {
            error_I <<-1
        })
        
        if(error_I) {
            session$sendCustomMessage(type = "showalert", "UMAP not installed, please install umap to python environment first.")
            return()
        }
        
        if(is.null(input$compdimr_name) || input$compdimr_name == "") {
            session$sendCustomMessage(type = "showalert", "Enter a name first.")
            return()
        }
        if(!is.na(as.numeric(input$compdimr_name))) {
            session$sendCustomMessage(type = "showalert", "Number name not allowed.")
            return()
        }
        if(input$compdimr_name %in% c(names(sclist$clist), names(sclist$elist))) {
            session$sendCustomMessage(type = "showalert", "Name already taken.")
            return()
        }
        if(input$compdimr_batch) {
            resform <- paste0("~", input$compdimr_bcol, "") 
        } else {
            resform <- NULL
        }

        withProgress(message = 'Processing...', {
            incProgress(1/2)
            set.seed(2020)
            #assign("ev1cells", ev$cells, env=.GlobalEnv)
            print(resform)
            cur_eset <- eset[,ev$cells]
            expressed_gene <- rowMeans(exprs(cur_eset) > 0) > input$compdimr_expr_cut
            cur_eset <- cur_eset[expressed_gene,]
            newvis <- new("Cello", name = input$compdimr_name, idx = match(ev$cells, colnames(eset))) 
            newvis <- compute_pca_cello(cur_eset, newvis, num_dim = input$compdimr_numpc, residualModelFormulaStr = resform) # Compute PCA 
            #newvis <- compute_tsne_newvis(cur_eset, newvis, use_dim = input$compdimr_numpc, n_component = 2, perplexity = 30) # Compute t-SNE
            if(input$compdimr_type == "UMAP-2D") {
                newvis <- compute_umap_cello(cur_eset, newvis, use_dim = input$compdimr_numpc, n_component = 2) # Compute UMAP
            } else if(input$compdimr_type == "UMAP-3D"){
                newvis <- compute_umap_cello(cur_eset, newvis, use_dim = input$compdimr_numpc, n_component = 3) # 3D UM
            }
            rval$list[[input$compdimr_name]] <- newvis
            rval$ustats <- "add"
        })
        updateSelectInput(session, "input_sample", selected = input$compdimr_name)
        showNotification("Dimension reduction successfully computed and listed in samples.", type="message", duration=10)
    })

    
    ### Feature Plot ###
    
    output$bp_gene_plot_ui <- renderUI({
        req(input$bp_show_ploth)
        ns <- session$ns
        plotOutput(ns("bp_gene_plot"), height = paste0(500/5.5 *input$bp_show_ploth,"px")) %>% withSpinner()
    })
    
    updateSelectizeInput(session, "bp_gene", "Search Gene:", choices = feature_options$features, server=T)
    
    output$bp_sample_ui <- renderUI({
        ns <- session$ns
        sample_names <- names(ev$list)
        isolate({
            if(!is.null(input$input_sample)) {
                selected <- input$input_sample
            } else {
                selected <- NULL
            }
        })
        selectInput(ns("bp_sample"),"Choose Sample", choices=sample_names, selected = selected)
    })
    
    # The follwoing observers control the syncing between explorer sample input and feature plot sample input
    observe({
        req(!is.null(input$input_sample))
        updateSelectInput(session, "bp_sample", "Choose Sample", selected = input$input_sample)
    })
    
    observe({
        req(!is.null(input$bp_sample))
        updateSelectInput(session, "input_sample", selected = input$bp_sample)
    })
    
    
    # The follwoing observers control the syncing between explorer gene input and feature plot gene input
    observeEvent(input$gene_list, {
        req(length(input$gene_list) == 1)
        if(is.null(input$bp_gene) || input$gene_list != input$bp_gene) {
            updateSelectInput(session, "bp_gene", selected = input$gene_list)
        }
    })
    
    observeEvent(input$bp_gene, {
        req(input$bp_gene, input$bp_gene != "No gene selected")
        if(is.null(input$gene_list) || input$bp_gene != input$gene_list) {
            updateSelectInput(session, "gene_list", selected = input$bp_gene)
        }
    })
    
    
    output$bp_colorBy_ui <- renderUI({
        ns <- session$ns
        selectInput(ns("bp_colorBy"), "Color By:", choices = c(bp_colorBy_choices, "Cluster"))
    })
    
    output$bp_include_ui <- renderUI({
        ns <- session$ns
        req(input$bp_colorBy)
        factors <- names(which(table(ev$meta[[input$bp_colorBy]]) >= 10)) 
        factors <- factors[factors != "unannotated"]
        tagList(
            selectInput(ns("bp_include"), "Include:", choices = factors, selected=factors, multiple = T, width = '100%'),
            conditionalPanel("1==0", textInput(ns("bp_include_I"), NULL, value = ev$sample)) # indicator of rendering state of bp_include
        )
    })
    
    
    output$bp_plot_configure_ui <- renderUI({
        input$bp_plot_config_reset
        ns <- session$ns
        
        req(input$bp_colorBy)
        if(input$bp_colorBy %in% pmeta_attr$meta_id && !is.null(pmeta_attr$dpal)) {
            default_pal <- pmeta_attr$dpal[which(pmeta_attr$meta_id==input$bp_colorBy)]
        } else {
            default_pal <- NULL
        }
        
        if(input$bp_colorBy %in% ev$factor_cols){
            if(grepl("time.bin", input$bp_colorBy)) {
                sel <- selectInput(ns("bp_numericbin_pal"), "Palette", choices=numeric_bin_color_opt(), selected=default_pal)
            } else {
                sel <- selectInput(ns("bp_factor_pal"), "Palette", choices=factor_color_opt(), selected=default_pal)
            }
        } else {
            return()
        }
        
        dropdownButton2(inputId=ns("bp_plot_configure"),
                        fluidRow(
                            column(6, numericInput(ns("bp_downsample"), "Downsample #", min=2, max = 10000, value=500)),
                            column(6, selectInput(ns("bp_plot_type"), "Plot Type", choices = list("Box plot" = "box", "Violin plot" = "violin", "Plot points" = "points")))
                        ),
                        fluidRow(
                            column(6, numericInput(ns("bp_marker_size"), "Point Size", min = 0.1, value = 1, step = 0.1)),
                            column(6, numericInput(ns("bp_text_size"), "Text Size", min = 1, value = 15, step = 1))
                        ),
                        fluidRow(
                            column(6, sel),
                            column(6, selectInput(ns("bp_legend_type"), "Legend", choices=c("Color Legend" = "l", "None" = "none"), selected = "none"))
                        ),
                        fluidRow(
                            column(6, numericInput(ns("bp_xaxis_angle"), "X-axis Label Angle", value = 45, step=1)),
                            column(6, numericInput(ns("bp_show_ploth"), "Plot Height", min=1, value = 5, step=1))
                        ),
                        circle = T, label ="Configure Plot", tooltip=T, right = T,
                        icon = icon("cog"), size = "xs", status="primary", class = "btn_rightAlign")
    })
    
    bp1 <- reactive({
        req(input$bp_colorBy,input$bp_include, length(input$bp_gene) == 1, input$bp_gene != "No gene selected", input$bp_gene %in% feature_options[[1]])
        req(ev$sample == input$bp_sample, ev$sample == input$bp_include_I) # IMPORTANT, this controls the sync between sample choices in the explorer and the featurePlot, and prevent double rendering
        cur_group <- ev$meta[[input$bp_colorBy]]
        # Downsample cells from each cell type
        cur_idx <- unlist(lapply(input$bp_include, function(g) {
            cidx <- which(cur_group==g)
            sample(cidx, min(length(cidx),input$bp_downsample))
        }))
        cur_meta <- ev$meta[cur_idx, input$bp_colorBy, drop=F]
        
        if(grepl("time.bin", input$bp_colorBy)) { 
            req(input$bp_numericbin_pal)
            factor_color <- get_numeric_bin_color(levels(ev$meta[[input$bp_colorBy]]), palette = input$bp_numericbin_pal)
            names(factor_color) <- levels(ev$meta[[input$bp_colorBy]])
        } else {
            req(input$bp_factor_pal)
            factor_color <- get_factor_color(unique(ev$meta[[input$bp_colorBy]]), pal=input$bp_factor_pal, maxCol = 9)
            names(factor_color) <- unique(ev$meta[[input$bp_colorBy]])
        }
        factor_color[["unannotated"]] <- "lightgrey"
        
        colorBy_name <-  pmeta_attr$meta_name[which(pmeta_attr$meta_id == input$bp_colorBy)]
        curg <- gene_symbol_choices[input$bp_gene]
        if(input$bp_log_transform_gene == "log2") {
            df <- as.data.frame(as.matrix(eset@assayData$norm_exprs[curg, ev$vis@idx[cur_idx]]))
        } else {
            df <- as.data.frame(as.matrix(exprs(eset)[curg, ev$vis@idx[cur_idx]]))
        }
        
        # assign("df1", df, env = .GlobalEnv)
        # assign("cur_meta1", cur_meta, env = .GlobalEnv)
        feature_plot(df, input$bp_gene, 
                     group.by = input$bp_colorBy, 
                     meta = cur_meta, 
                     pal = factor_color, 
                     style = input$bp_plot_type, log_scale = F, legend.title = colorBy_name, legend.pos = "right", 
                     text.size = input$bp_text_size, pointSize = input$bp_marker_size, legend = ifelse(input$bp_legend_type == "l", T, F), 
                     breaks = unique(cur_group), axis.text.angle = input$bp_xaxis_angle, 
                     order.by = ifelse(grepl("time",input$bp_colorBy, ignore.case = T), "none", "mean"), 
                     ylab.label = ifelse(input$bp_log_transform_gene == "log2", "Expression (log2 normalized)", "Expression (raw count)")
        )
    })
    
    output$bp_gene_plot <- renderPlot({
        req(bp1())
        bp1()
    })
    
    rval <- reactiveValues(mclass = NULL, cells=NULL, group_name=NULL, ulist = list())
    return(rval)
}






