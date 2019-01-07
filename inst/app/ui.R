
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
                fluidRow(
                    column(4,
                           wellPanel(
                               selectInput("image_colorBy", "Color by", choices = image_colorBy_choices),
                               selectInput("image_pal", "Palette", choices=image_palettes),
                               numericInput("image_ploth", "Plot Height", min=1, value = 7, step=1),
                               tags$br(),
                               tagList(tags$strong("EPiC Movies: "), tags$a("http://epic.gs.washington.edu/", href="http://epic.gs.washington.edu/")),
                               tagList(tags$strong("EPiC2 Movies: "), tags$a("http://epic.gs.washington.edu/Epic2/", href="http://epic.gs.washington.edu/Epic2/"))
                           ),
                           fluidRow(
                               column(12, tags$p("Expression level summarized from following sources:"))
                           ),
                           DT::dataTableOutput("g_meta_table")
                    ), 
                    column(8, 
                           uiOutput("image_graph_plot_ui")
                    )
                )
            )
        )
    ),
    tabPanel(
        "Differential Expression",
        de_ui("cel")
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
