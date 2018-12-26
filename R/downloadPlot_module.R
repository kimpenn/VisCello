



# downloadPlot_ui <- function(id) {
#     ns <- NS(id)
#     tagList(
#         column(3, uiOutput(ns("plotf_ui"))),
#         column(3, tags$br(), downloadButton(ns("download_plot"), "Download", class = "btn-primary", style="width: 115px"))
#     )
# }
#
# downloadPlot_server <- function(input, output, session, plt, ndim = 2){
#     output$plotf_ui <- renderUI({
#         ns <- session$ns
#         req(ndim)
#         if(ndim == 2){
#             choices <- list("png" = "png", "pdf" = "pdf", "eps" = "eps", "tiff" = "tiff")
#         } else {
#             choices <- list( "html" = "html")
#         }
#         selectInput(ns("plotf"), "Format", choices = choices, selected = choices[[1]])
#     })
#
#     output$download_plot <- downloadHandler(
#         filename = function(format = input$plotf) {
#             fn_ext<-switch(format,
#                            png = '.png',
#                            tiff = '.tiff',
#                            eps = '.eps',
#                            pdf = '.pdf',
#                            html = '.html'
#             )
#             paste('Plot-', Sys.Date(), fn_ext, sep='')
#         },
#         content = function(con, format = input$plotf) {
#             req(input$plotw, input$ploth, format)
#             fn_dev<-switch(format,
#                            png = 'png',
#                            tiff = 'tiff',
#                            eps = 'eps',
#                            pdf = 'pdf',
#                            html = 'html'
#             )
#             if(fn_dev!='html') {
#                 req(plt)
#                 ggsave(con, plot = plt, device = fn_dev, width = input$plotw, height = input$ploth)
#                 shut_device <- dev.list()[which(names(dev.list()) != "quartz_off_screen")]
#                 if(length(shut_device)) dev.off(which = shut_device) # Make sure ggsave does not change graphic device
#             } else {
#                 req(plt)
#                 htmlwidgets::saveWidget(plt, con)
#             }
#         }
#     )
#
# }







