


#' @export
de_ui <- function(id) {
    ns <- NS(id)
    tagList(
        wellPanel(
            fluidRow(
                column(3, uiOutput(ns("de_sample_ui"))),
                column(3, uiOutput(ns("de_metaclass_ui"))),
                uiOutput(ns("de_g1_ui")),
                uiOutput(ns("de_g2_ui"))
            ),
            fluidRow(
                column(8, uiOutput(ns("cur_de_group"))),
                column(4,
                       actionButton(ns("run_de"), "Run DE", class = "btn-info btn_rightAlign"),
                       uiOutput(ns("add_clus_ui")),
                       uiOutput(ns("downsample_de_ui"))
                )
            )
        ),
        fluidRow(
            column(5,
                   fluidRow(
                       column(8, uiOutput(ns("de_proj_type_ui"))),
                       column(4,
                              circleButton(ns("de_plot_config_reset"), icon = icon("undo"), size = "xs", status = "danger btn_rightAlign"),
                              shinyBS::bsTooltip(
                                  ns("de_plot_config_reset"),
                                  title = "Reset plot configuration",
                                  options = list(container = "body")
                              ),
                              uiOutput(ns("de_plot_configure_ui"))
                       )
                   ),
                   plotOutput(ns("de_plot2d"), height = "450px", hover = hoverOpts(id = ns("plot2d_hover"), delay = 50)),
                   uiOutput(ns("plot2d_tooltip")),
                   tags$p("Hint: Mouse over points to see label."),
                   tags$br(),
                   DT::dataTableOutput(ns("deg_summary"))
            ),
            column(7,
                   fluidRow(
                       column(12,
                              circleButton(ns("hmap_config_reset"), icon = icon("undo"), size = "xs", status = "danger btn_rightAlign"),
                              shinyBS::bsTooltip(
                                  ns("hmap_config_reset"),
                                  title = "Reset heatmap configuration",
                                  options = list(container = "body")
                              ),
                              uiOutput(ns("hmap_configure_ui"))
                       )
                   ),
                   plotOutput(ns("de_hmap"), height="600px") %>% withSpinner(),
                   fluidRow(
                       column(6),
                       column(6, uiOutput(ns("download_hmap_ui")))
                   )
            )
        ),
        tags$hr(),
        fluidRow(
            column(12,
                   uiOutput(ns("deg_tabs")),
                   uiOutput(ns("download_de_res_ui"))
            )
        ),
        tags$hr(),
        fluidRow(
            column(12,
                   uiOutput(ns("go_ui"))
            )
        )
    )
}


#' @export
de_server <- function(input, output, session, sclist = NULL, cmeta = NULL, organism = "mmu"){
    ################################# DE Module ###################################
    
    des <- reactiveValues()
    de_res <- reactiveValues()
    reset_idx <- reactiveValues(g1=0, g2=0)
    de_idx <- reactiveValues(idx_list = list(NA, NA), group = list(NA, NA), group_name = c("", ""))
    
    ############ Interactive UI for adding new groups for comparison ###################
    
    output$de_metaclass_ui <- renderUI({
        ns <- session$ns
        req(des$meta_options)
        selectInput(ns("de_metaclass"), "Meta Class", choices = des$meta_options)
    })
    
    output$de_g1_ui <- renderUI({
        req(input$de_metaclass, des$meta, des$groups)
        ns <- session$ns
        reset_idx$g1
        tagList(
            column(3, selectInput(ns("de_g1_group"), "Group 1", choices = des$groups, multiple = T))
        )
    })
    
    output$de_g2_ui <- renderUI({
        ns <- session$ns
        req(input$de_metaclass, des$meta, des$groups)
        reset_idx$g2
        tagList(
            column(3, selectInput(ns("de_g2_group"), "Group 2", choices = des$groups, multiple = T))
        )
    })
    
    output$add_clus_ui <- renderUI({
        ns <- session$ns
        req(input$de_metaclass, des$meta)
        input$de_new_add
        dropdownButton2(inputId="add_clus",
                        selectInput(ns("de_new_group"), "Group", choices = des$groups, multiple = T),
                        actionButton(ns("de_new_add"), "Add Group", icon = icon("plus-circle"), class = "btn-danger btn_rightAlign"),
                        circle = T, label ="Add Another Group", tooltip=T, right = T,
                        icon = icon("plus"), size = "sm", status="primary", class = "btn_rightAlign")
    })
    
    
    
    observeEvent(input$de_new_add, {
        req(input$de_metaclass, input$de_new_group)
        groups <- des$meta[[input$de_metaclass]]
        gname <- c(paste0(input$de_new_group, collapse="_"))
        de_idx$idx_list[[length(de_idx$idx_list) + 1]] <- des$vis@idx[which(groups %in% input$de_new_group)]
        de_idx$group[[length(de_idx$group) + 1]] <- if(!is.null(input$de_new_group)) input$de_new_group else NA
        de_idx$group_name <- c(de_idx$group_name,gname)
    })
    
    
    output$cur_de_group <- renderUI({
        ns <- session$ns
        group <- de_idx$group_name
        group <- group[!group == ""]
        if(length(group) !=0) {
            checkboxGroupButtons(inputId = ns("remove_group"), label = NULL, choices = group, individual = T,  status = "danger",
                                 checkIcon = list(yes = icon("remove", lib = "glyphicon"), no = icon("remove", lib = "glyphicon")))
        } else {
            tagList(
                tags$br(),
                actionLink(ns("howtode"), "How to run DE")
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
            de_idx$group[[1]] <- if(!is.null(input$de_g1_group)) input$de_g1_group else NA
            de_idx$group[[2]] <- if(!is.null(input$de_g2_group)) input$de_g2_group else NA
            de_idx$group_name[1] <-c(paste0(input$de_g1_group, collapse="_"))
            de_idx$group_name[2] <-c(paste0(input$de_g2_group, collapse="_"))
        })
    })
    
    observe({
        req(input$de_metaclass)
        group_name <- de_idx$group_name
        gid <- which(group_name != "")
        group_name <- group_name[gid]
        if(length(group_name) == 1) {
            isolate({
                cur_group <- de_idx$group[[gid]]
                groups <- des$meta[[input$de_metaclass]]
                de_idx$idx_list[[which(de_idx$group_name == "")[1]]] <- des$vis@idx[which(!groups %in% cur_group)]
            })
        } else {
            isolate({
                for(j in which(de_idx$group_name == "")) de_idx$idx_list[[j]] <- list()
            })
        }
    })
    
    output$downsample_de_ui <- renderUI({
        ns <- session$ns
        group <- de_idx$group_name
        group <- group[!group == ""]
        if(length(group) == 1) {
            dropdownButton2(inputId=ns("downsample_background"),
                            selectInput(ns("de_method"), "DE Method", choices=c("Chi-square (binary)" = "chi", "Mann–Whitney U test" = "mw", "sSeq" = "sseq"), selected = "sseq"),
                            numericInput(ns("de_pval_cutoff"), "FDR cutoff", value = 0.05),
                            numericInput(ns("de_lfc_cutoff"), "LFC cutoff", value = 1),
                            numericInput(ns("downsample_gp_num"), "Max Cell # in subset", value = 200),
                            numericInput(ns("downsample_bg_num"), "Max Cell # in background", value = 1000),
                            actionButton(ns("downsample_de_bg"), "Downsample Cells", class = "btn_rightAlign"),
                            circle = T, label ="DE configuration", tooltip=T, right = T,
                            icon = icon("angle-double-down"), size = "sm", status="success", class = "btn_rightAlign")
        } else {
            dropdownButton2(inputId=ns("downsample_group"),
                            selectInput(ns("de_method"), "DE Method", choices=c("sSeq" = "sseq", "Mann–Whitney U test" = "mw", "Chi-square (binary)" = "chi"), selected = "sseq"),
                            numericInput(ns("de_pval_cutoff"), "FDR cutoff", value = 0.05),
                            numericInput(ns("de_lfc_cutoff"), "LFC cutoff", value = 1),
                            numericInput(ns("downsample_num"), "Max Cell # per Group", value = 200),
                            actionButton(ns("downsample_de"), "Downsample Cells", class = "btn_rightAlign"),
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
                    de_idx$group[remove_idx] <- NA
                    de_idx$group_name[remove_idx] <- ""
                    if(remove_idx == 1) reset_idx$g1 <-reset_idx$g1+1
                    if(remove_idx == 2) reset_idx$g2 <-reset_idx$g2+1
                } else {
                    de_idx$idx_list[remove_idx] <- NULL
                    de_idx$group[remove_idx] <- NA
                    de_idx$group_name <- de_idx$group_name[-remove_idx]
                }
            }
        })
    })
    
    
    output$de_sample_ui <- renderUI({
        ns <- session$ns
        options <- names(sclist$clist)
        selectInput(ns("de_sample"), "Choose Sample:", choices=options)
    })
    
    observe({
        req(input$de_sample)
        sample <- input$de_sample
        cur_list <- sclist$clist
        idx <- cur_list[[sample]]@idx
        cur_meta <-  cmeta$df[idx,]
        des$vis <- cur_list[[sample]]
        if(!is.null(des$vis@pmeta) && nrow(des$vis@pmeta) == nrow(cur_meta)) cur_meta <- cbind(cur_meta, des$vis@pmeta)
        des$meta <- cur_meta
        des$meta_options <- c(de_meta_options, colnames(des$meta)[which(!colnames(des$meta) %in% pmeta_attr$meta_id)])
        #assign("des", reactiveValuesToList(des), env=.GlobalEnv)
    })
    
    observe({
        req(des$vis, input$de_proj_type, des$meta)
        req(input$de_proj_type %in% names(des$vis@proj))
        isolate({
            des$proj <- cbind(des$vis@proj[[input$de_proj_type]], des$meta)
        })
    })
    
    observe({
        req(des$meta, input$de_metaclass)
        isolate({
            factor_breaks <- unique(des$meta[[input$de_metaclass]])
            if(input$de_metaclass %in% c("cell.type", "cell.subtype")) {
                factor_breaks <- names(which(table(des$meta[[input$de_metaclass]]) >= 10)) 
            } 
            factor_breaks <- factor_breaks[factor_breaks != "unannotated"]
            des$groups <- factor_breaks
        })
    })
    
    output$de_plot_configure_ui <- renderUI({
        ns <- session$ns
        input$de_plot_config_reset
        dropdownButton2(inputId=ns("de_plot_configure"),
                        selectInput(ns("de_plot_color_pal"), "Color palette", choices=factor_color_opt()),
                        numericInput(ns("de_plot_alpha_level"), "Transparency", min = 0, max = 1, value = 0.05, step = 0.01),
                        numericInput(ns("de_plot_marker_size"), "Point Size", min = 0.1, max = 5, value = 2, step = 0.1),
                        selectInput(ns("de_plot_legend_type"), "Legend", choices = c("Color Legend" = "l", "On-plot Label" = "ol", "Both" = "lol", "None" = "none"), selected="none"),
                        circle = T, label ="Configure Plot", tooltip=T, right = T,
                        icon = icon("cog"), size = "xs", status="primary", class = "btn_rightAlign")
    })
    
    
    
    output$de_proj_type_ui <- renderUI({
        ns <- session$ns
        req(des$vis)
        options <- names(des$vis@proj)
        options <- options[options != "PCA" & !grepl("3D", options)]
        selectInput(ns("de_proj_type"), NULL, choices=options)
    })
    
    output$de_plot2d <- renderPlot({
        req(pp2())
        pp2()
    })
    
    output$plot2d_tooltip <- renderUI({
        ns <- session$ns
        hover <- input$plot2d_hover
        #assign("hover", hover, env=.GlobalEnv)
        x <- nearPoints(des$proj, hover, maxpoints = 1)
        req(nrow(x) > 0)
        y <- as.character(x[[input$de_metaclass]])
        tip <- y
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
        req(nrow(proj) == length(idx))
        proj$alpha <- ifelse(idx %in% aidx, "f", "t")
        
        legend_type <- input$de_plot_legend_type
        legend=F; onplotAnnot=NULL
        if(!is.null(legend_type)) {
            if(legend_type == "l") {
                legend=T; onplotAnnot=NULL
            } else if(legend_type == "lot") {
                legend=T; onplotAnnot="text"
            } else if(legend_type == "lol") {
                legend=T; onplotAnnot="label"
            } else if(legend_type == "ot"){
                legend=F; onplotAnnot="text"
            } else if(legend_type == "ol"){
                legend=F; onplotAnnot="label"
            }
        } 
        
            factor_color <- get_factor_color(unique(proj[[input$de_metaclass]]), pal=input$de_plot_color_pal, maxCol = 9)
            names(factor_color) <- unique(proj[[input$de_metaclass]])
            factor_color[["unannotated"]] <- "lightgrey"

        
        if(input$de_metaclass %in% c("cell.type", "cell.subtype")) {
            factor_breaks <- names(which(table(proj[[input$de_metaclass]]) >= 10)) 
        } else {
            factor_breaks <- names(factor_color)
        }
        factor_breaks <- factor_breaks[factor_breaks != "unannotated"]
        
        plotProj(proj, dim_col = c(1,2), group.by=input$de_metaclass, pal=factor_color, size = input$de_plot_marker_size, plot_title=NULL, legend.title = NULL, na.col = "lightgrey", alpha=proj$alpha, alpha_level=input$de_plot_alpha_level, legend=legend, onplotAnnot = onplotAnnot, onplotAnnotSize = 3, legend.text.size = 3, breaks = factor_breaks)
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
            test_group <- de_idx$group_name[gidx]
            test_group[which(test_group == "")] <- "Background"
            test_idx <- de_idx$idx_list[gidx]
            test_clus <- unlist(lapply(1:length(gidx), function(i){
                rep(test_group[i], length(test_idx[[i]]))
            }))
            test_idx <- unlist(test_idx)
            cur_cds <- eset[, test_idx]
            
            assign("cur_cds", cur_cds, env =.GlobalEnv)
            assign("test_clus", test_clus, env =.GlobalEnv)
            feature_var <- apply(as.matrix(exprs(cur_cds)), 1,var)
            if(NEG_VAL == T) {
                gene_idx <- complete.cases(as.matrix(exprs(cur_cds))) & feature_var !=0
            } else{
                gene_idx <- Matrix::rowSums(exprs(cur_cds)) > 0 & complete.cases(as.matrix(exprs(cur_cds))) & feature_var !=0
            }

            cur_cds <- cur_cds[gene_idx,]
            feature_data <- fData(cur_cds)

            if(input$de_method == "sseq") {
                test_method = "sseq"
                de_list <- runsSeq(dat=as.matrix(exprs(cur_cds)), group=test_clus, fdata = feature_data, order_by="pvalue", p_cutoff= input$de_pval_cutoff, min_mean = 0, min_log2fc = input$de_lfc_cutoff, id_col = id_col, name_col = name_col)
                de_list <- lapply(de_list, function(x) {
                    if(!identical(id_col, name_col)) {
                        x %>% dplyr::select(gene_id, gene_name, common_mean, dispersion, log2fc, p, p_adj)
                        colnames(x)[c(1,2)] <- c(id_col, name_col)
                    } else {
                        x %>% dplyr::select(gene_id, common_mean, dispersion, log2fc, p, p_adj)
                        colnames(x)[c(1)] <- c(id_col)
                    }
                    return(x)
                })
            } else if(input$de_method == "chi") {
                if(length(unique(test_clus)) > 2) {
                    session$sendCustomMessage(type = "showalert", "Only support pairwise chi-square test.")
                    return()
                }
                test_method = "chi-square"
                de_list <- run_chisq(dat=as.matrix(exprs(cur_cds)), group=test_clus, fdata = feature_data, min_fdr= input$de_pval_cutoff, min_lfc = input$de_lfc_cutoff, detRate=.05, id_col = id_col, name_col = name_col)
            } else if(input$de_method == "mw") {
                if(length(unique(test_clus)) > 2) {
                    session$sendCustomMessage(type = "showalert", "Only support pairwise Mann–Whitney U test.")
                    return()
                }
                test_method = "mw"
                de_list <- run_mw(dat=as.matrix(exprs(cur_cds)), group=test_clus, fdata = feature_data, min_fdr= input$de_pval_cutoff, id_col = id_col, name_col = name_col,  detRate = .05)
            }
        })

        updateSelectInput(session, "de_hmap_colorBy", selected = input$de_metaclass)
        de_res$sample <- input$de_sample
        de_res$metaclass <- input$de_metaclass
        de_res$test_idx <- test_idx
        de_res$test_group <- test_group
        de_res$test_clus <- test_clus
        de_res$test_method <- test_method
        de_res$de_list <- de_list
        de_res$feature_data <- feature_data
        de_res$feature_idx <- gene_idx
    })
    
    de_show <- reactive({
        input$de_filter
        lapply(de_res$de_list, function(x) {
            if(de_res$test_method == "sseq") {
                tbl <- x[,c(id_col, name_col, "common_mean", "dispersion", "log2fc", "p", "p_adj", "significant")]
            } else {
                tbl <- x
            }
            if(input$de_filter == "significant") {
                tbl <- tbl %>% dplyr::filter(significant == TRUE)
            } else { # Add human symbol for GSEA
                if(organism == "mmu") {
                    tbl$human_symbol <- VisCello:::mouse_to_human_symbol(tbl[[name_col]], in.type = "mm", HMD_HumanPhenotype)
                }
            }
            return(tbl)
        })
    })
    
    
    output$hmap_configure_ui <- renderUI({
        ns <- session$ns
        input$hmap_config_reset
        dropdownButton2(inputId=ns("hmap_configure"),
                        fluidRow(
                            column(6, selectInput(ns("de_hmap_colorBy"), "Heatmap Color By", choices = des$meta_options)),
                            column(6, selectInput(ns("de_hmap_scale"), "Data scale", choices = c("Log2 normalized count"="log2", "Molecule (UMI) count" = "raw")))
                        ),
                        fluidRow(
                            column(6, selectInput(ns("hmap_color_pal"), "Heatmap Color", choices=heatmap_palettes)),
                            column(6, numericInput(ns("hmap_numg"), "Max #DEG/Group", min=2, max = 500, value = 30, step=1))
                        ),
                        fluidRow(
                            column(6, selectInput(ns("hmap_dscale"), "Data Scale", choices=list("scaled log" = "scaled log", "log"="log"))),
                            column(6, numericInput(ns("hmap_pseudoc"), "Pseudocount", min=1e-5, max = 1, value = 1, step=1e-3))
                        ),
                        fluidRow(
                            column(6, numericInput(ns("hmap_limitlow"), "Expression z-score cutoff low", value = -2, step=1)),
                            column(6, numericInput(ns("hmap_limithigh"), "Expression z-score cutoff high", value = 2, step=1))
                        ),
                        circle = T, label ="Configure Heatmap", tooltip=T, right = T,
                        icon = icon("cog"), size = "xs", status="primary", class = "btn_rightAlign")
    })
    
    observe({
        req(de_res$de_list, input$hmap_dscale)
        assign("de_res", reactiveValuesToList(de_res), env = .GlobalEnv)
        #assign("des", reactiveValuesToList(des), env=.GlobalEnv)
        if(sum(sapply(de_show(), nrow)) == 0) return()
        cur_list <- sclist$clist
        #isolate({
        # Color bar on top of heatmap
        #assign("de_res", reactiveValuesToList(de_res), env =.GlobalEnv)
        de_res$cells_to_plot <- order_cell_by_clusters2(cmeta$df[de_res$test_idx,], de_res$test_clus)
        sample <- de_res$sample
        idx <- cur_list[[sample]]@idx
        cur_factor <- des$meta[[input$de_hmap_colorBy]]
        unique_factors <- unique(cur_factor)
        de_res$plot_group = cur_factor[match(de_res$test_idx, idx)]
        #print(de_res$plot_group)
        factor_color <- get_factor_color(unique_factors, pal=input$de_plot_color_pal, maxCol = 9)
        names(factor_color) <- unique_factors
        factor_color[["unannotated"]] <- "lightgrey"
        de_res$group_colour = factor_color
        # Color of heatmap
        if(!is.null(input$hmap_color_pal)) {
            de_res$heatmap_color = get_numeric_color(input$hmap_color_pal)
        } else {
            de_res$heatmap_color = colorRampPalette(rev(brewer.pal(n = 7, name = "RdYlBu")))(100)
        }
        if(input$hmap_dscale == "scaled log"){
            de_res$scale = T
        } else if(input$hmap_dscale == "log") {
            de_res$scale = F
        }
    })
    
    output$de_hmap <- renderPlot({
        req(de_res$de_list)
        # shut_device <- dev.list()[which(names(dev.list()) != "quartz_off_screen")]
        # if(length(shut_device)) dev.off(which = shut_device) # Make sure ggsave does not change graphic device
        #if(is.null(de_show())) return()
        withProgress(message="Rendering heatmap..", {
            if(input$de_hmap_scale == "log") {
                dat <- eset@assayData$norm_exprs[de_res$feature_idx, de_res$test_idx]
                if(NEG_VAL) dat <- t(apply(dat, 1, function(x) {x[is.infinite(x) & x < 0 ] <- min(x[!is.infinite(x)]); x[is.infinite(x) & x > 0 ] <- max(x[!is.infinite(x)]); return(x)}))
            } else {
                dat <- exprs(eset)[de_res$feature_idx, de_res$test_idx]
            }
            rownames(dat) <- make.unique(as.character(fData(eset)[[name_col]][match(rownames(dat), rownames(fData(eset)))])) # Deal with non-unique features later!!
            #isolate({
                de_res$hmap<-gbm_pheatmap2(dat,
                                           genes_to_plot = as.character(unlist(lapply(de_show(), function(x) {x[[name_col]][1:min(input$hmap_numg, nrow(x))]}))),
                                           cells_to_plot=de_res$cells_to_plot,
                                           group=de_res$plot_group,
                                           group_colour=de_res$group_colour,
                                           log=F, pseudocount = input$hmap_pseudoc,
                                           scale = de_res$scale,
                                           heatmap_color = de_res$heatmap_color,
                                           n_genes=input$hmap_numg, fontsize=8, limits=c(input$hmap_limitlow, input$hmap_limithigh))
                grid::grid.draw(de_res$hmap$gtable)
            #})
        })
    })
    
    output$deg_summary <- DT::renderDataTable({
        req(de_res$de_list)
        tbl<- data.frame(clusters = names(de_res$de_list), number_de_genes = sapply(de_show(), function(x)sum(x$significant)))
        DT::datatable(tbl, rownames=F, options = list(searching=F, paging=F))
    })
    
    
    output$deg_tabs <- renderUI({
        ns <- session$ns
        do.call(
            what = shiny::tabsetPanel,
            args= purrr::map(de_res$test_group,.f = function(nm){
                shiny::tabPanel(title=nm,
                                DT::renderDataTable({
                                    req(de_show())
                                    if(de_res$test_method == "sseq") {
                                        DT::datatable(de_show()[[nm]], selection = 'single') %>%
                                            DT::formatRound(columns=c('common_mean', 'dispersion', 'log2fc', 'p', 'p_adj'), digits=3)
                                    } else {
                                        DT::datatable(de_show()[[nm]], selection = 'single') 
                                    }
                                })
                )
            })
        )
    })
    
    output$download_hmap_ui <- renderUI({
        ns <- session$ns
        req(de_show())
        downloadButton(ns("download_hmap"), "Download Heatmap", class = "btn_rightAlign")
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
        ns <- session$ns
        #req(de_show())
        fluidRow(
            column(6),
            column(3, selectInput(ns("de_filter"), NULL, choices = c("Show only significant"="significant" ,
                                                                     "Show stats for all genes" = "all"), selected= "significant")),
            column(3, downloadButton(ns("download_de_res"), "Download DE Table", class = "btn_rightAlign"))
        )
    })
    
    output$download_de_res <- downloadHandler(
        filename = function() {
            group_name <- paste0(make.names(de_idx$group_name), collapse="_vs_")
            if(nchar(group_name) > 20) group_name = "custom"
            paste(input$de_sample, "_", group_name, "_", Sys.Date(), '_de', "_", input$de_filter, '.xlsx', sep='')
        },
        content = function(con) {
            req(de_show())
            write.xlsx(de_show(), file=con)
        }
    )
    
    
    output$go_ui <- renderUI({
        ns <- session$ns
        req(de_res$de_list)
        tagList(
            fluidRow(
                column(4, selectInput(ns("go_type"), "Choose Enrichment Type", choices =list("GO-BP" = "BP", "GO-MF" = "MF", "GO-CC" = "CC", "KEGG" = "kegg"))),
                column(8, tags$p(), actionButton(ns("run_go"), "Run Enrichment Analysis", class = "btn-info btn_rightAlign"))
            ),
            fluidRow(
                column(12,
                       uiOutput(ns("go_tabs_ui"))
                ),
                uiOutput(ns("download_go_res_ui"))
            )
        )
    })
    
    output$go_tabs_ui <- renderUI({
        ns <- session$ns
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
            # only compute enrichment for significant deg
            if("significant" %in% colnames(de_res$de_list[[1]])) {
                de_list <- lapply(de_res$de_list, function(x) {
                    x %>% dplyr::filter(significant == TRUE)
                })
            } else {
                de_list <- de_res$de_list
            }
            enrich_list <- compute_go(de_list, bg_list = de_res$feature_data[[name_col]], type = input$go_type, organism = organism, idcol = name_col)
            de_res$enrich_list <- enrich_list
            de_res$enrich_type <- input$go_type
        })
    })
    
    
    output$download_go_res_ui <- renderUI({
        ns <- session$ns
        req(de_res$enrich_list)
        downloadButton(ns("download_go_res"), "Download Enrichment Table", class = "btn_rightAlign")
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





