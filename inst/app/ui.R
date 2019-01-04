
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#


function() {
    fluidPage(
        div(style="padding: 1px 0px; width: '100%'",
            titlePanel(
                title="", windowTitle="VisCello: C.elegans Embryogenesis Visualizer"
            )
        ),
        navbarPage(
    title = div(
        "C.elegans Embryogenesis",
        div(
         id = "sys_control",
        dropdownButton(
            inputId="load_state_dropdown",
            actionButton("exit_app","Exit App", icon = icon("power-off"), width = "115px", class = "btn-primary", onclick = "setTimeout(function(){window.close();}, 100); "),
            tags$br(),
            fluidRow(column(12,downloadButton("state_save_sc","Save State", icon = icon("save"),class = "btn_leftAlign btn-primary", style="width: 115px"))),
            tags$br(),
            fileInput2('uploadState', NULL, buttonLabel = "Load State", width = "50px", accept = ".rda"),
            uiOutput("refreshOnUpload"),
            circle = T, label ="System Control", tooltip=T, right = T,
            status = "syscontrol",
            icon = icon("cog")
        )
        )
    ),
    position = "fixed-top",
    theme = shinytheme("flatly"),
    # Application title
    tabPanel(
        "Cell Type",
        mainPanel(
            NULL,
            width = 12,
            tags$head(
                tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
            ),
            singleton(tags$script(type="text/javascript", "
                                  $(document).ready(function() {
                                  $('input[type='text'], textarea').attr({autocomplete:'off', spellcheck:false, autocorrect:'off', autocapitalize:'off'});

                                  ")),
            singleton(tags$script(type="text/javascript", "
                                  $(document).ready(function() {
                                  Shiny.addCustomMessageHandler('showalert', function(message) {
                                  alert(message);
                                  });
                                  });
                                  ")
            ),
            explorer_ui("main"),
            
            # tabPanel(
            #     tags$b("Cell Type Marker"),
            #     tags$br(),
            #     fluidRow(
            #         column(3, textInputCode("ct_lineage", "Search Lineage:", placeholder = "lineage name substring")),
            #         column(3, textInputCode("ct_cell", "Search Cell:", placeholder = "cell name substring")),
            #         column(3, textInputCode("ct_tissue", "Search Tissue:", placeholder = "tissue cluster")),
            #         column(3, textInputCode("ct_marker", "Search Gene:", placeholder = "gene name"))
            #     ),
            #     DT::dataTableOutput("ct_marker_tbl")
            # )
            tags$br(),
            tags$br(),
            tags$br(),
            tags$br(),
            tags$br(),
            tags$br(),
            tags$br()
            #downloadButton('download_ct_tbl', 'Download',class = "btn_rightAlign")
        )
    ),
    tabPanel(
        "Early Lineage",
        tabsetPanel(
            tabPanel(
                tags$b("Explorer"),
                width = 12,
                explorer_ui("early")
            ),
            tabPanel(
                tags$b("Marker Imaging"),
                wellPanel(
                    fluidRow(
                        column(3, selectInput("image_colorBy", "Color by", choices = image_colorBy_choices)),
                        column(3, selectInput("image_pal", "Palette", choices=numeric_palettes)),
                        column(3, numericInput("image_ploth", "Plot Height", min=1, value = 10, step=1)),
                        column(3, numericInput("image_plotw", "Plot Width", min=1, value = 7, step=1))
                    )
                ),
                uiOutput("image_graph_plot_ui")
            )
        )
    ),
    tabPanel(
        "Differential Expression"
    ),

    tabPanel(
        "Dynamic Pattern",
        tabsetPanel(
            tabPanel(
                tags$b("Global Dynamic Pattern:"),
                width = 12,
                fluidRow(
                    column(4, selectInput("dynamic_barplot_type", "Plot global pattern:", choices = list("Number of Expressed Genes per Cell" = "num.genes.expressed", "Number of UMI per Cell" = "n.umi"))),
                    column(4, selectInput("dynamic_barplot_scale", "Data scale:", choices = c("log10", "identity"))),
                    column(4, numericInput("dynamic_barplot_bin", "Number of time bin:", value = 30, step = 1, min = 2))
                ),
                plotOutput("dynamic_gene_num_barplot"),
                hr(),
                tags$b("Temporal Composition Change:"),
                tags$br(),
                fluidRow(
                    column(12, plotlyOutput("meta_line_tt"))
                )
            ),
            tabPanel(
                tags$b("Cell Type Differentiation"),
                fluidRow(
                    column(6,
                           fluidRow(
                               column(6, selectInput("dy_ctype_choice", "Cell Type", choices = names(time_umap_list))),
                               column(6, selectizeInput("dy_ctype_gene", "Search Gene:", NULL, multiple = T))
                           ),
                           plotOutput("dy_proj_show")
                    ),
                    column(6,
                           fluidRow(
                               column(6, numericInput("dy_hmap_ngene", "# Genes", value = 50, min = 2, max = 500)),
                               column(6, numericInput("dy_hmap_ecut", "Min Expr", value = 0.1, min = 0))
                           ),
                           plotOutput("dy_hmap_show")
                    )
                ),
                fluidRow(
                    column(12, DT::dataTableOutput("dy_de_tbl"))
                ),
                fluidRow(
                    column(6),
                    column(3, checkboxInput("dy_filter_tf", "Filter for TFs", value = F)),
                    column(3, downloadButton("download_dy_res", "Download DE Table", class = "btn_rightAlign"))
                )


            )
            # tabPanel(
            #     tags$b("Branch Point Analysis")
            # )
        )
    ),
    tabPanel(
        "Tutorial",
        tags$p("To be added.")
    ),
    header = tagList(
        tags$br(),
        tags$br(),
        tags$br(),
        tags$br()
    ),

    footer = tagList(
        tags$br(),
        tags$br(),
        tags$br(),
        tags$br()
    )
            )
        )
}
