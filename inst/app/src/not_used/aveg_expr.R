


output$expr_profile_ui <- renderUI({
    tabPanel(tags$b("Expression Profile"),
             tags$br(),
             fluidRow(
                 column(3, selectInput("ge_type_choice", "Choose Profile", choices = list("Tissue Type" = "tissue", "Cell Type" = "celltype", "Early Lineage" = "lineage"))),
                 column(3, selectInput("ge_data_scale", "Data Scale", choices = list("Log10 scaled" = "Log10 scaled", "Log10" = "Log10", "Count" = "Count"))),
                 column(3, selectInput("ge_order_by", "Order By", choices = list("Fano Factor" = "Fano Factor", "Variance" = "Variance", "Expression Level" = "Expression Level"))),
                 column(3, numericInput("ge_plot_num", "Include Gene#", value = 300, step=1, min=2))
             ),
             DT::dataTableOutput("gep_tbl"),
             fluidRow(
                 column(7),
                 column(3,
                        selectInput("ge_download_tbl_type", NULL, choices = list("Download this table (filtered)" = "cur", "Download  entire table" = "all"))
                 ),
                 column(2, downloadButton('download_ge_tbl', 'Download',class = "btn_rightAlign"))
             ),
             hr(),
             #uiOutput("ge_hmap_ui"),
             plotOutput("ge_hmap", height="600px"),
             fluidRow(
                 column(6),
                 #column(6, materialSwitch(inputId = "interactive_ge_hmap", tags$b("interactive"), value = F, status = "success")),
                 column(6, downloadButton("download_ge_hmap", "Download Heatmap", class = "btn_rightAlign"))
             )
    )
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
            group_color <- get_factor_color(groups, "Set1")
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
        group_color <- get_factor_color(groups, "Set1")
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


