


#' @export
numericInput_1 <- function (inputId, label, value, min = NA, max = NA, step = NA, width = NULL)
{
  inputTag <- tags$input(id = inputId, type = "number", class = "form-control",
                         value = formatNoSci(value), style = "display:inline-block; width:60%;")
  if (!is.na(min))
    inputTag$attribs$min = min
  if (!is.na(max))
    inputTag$attribs$max = max
  if (!is.na(step))
    inputTag$attribs$step = step
  div(class = "form-group shiny-input-container", style = if (!is.null(width))
    paste0("width: 100%;"), label %AND%
      tags$label(label, `for` = inputId, style = "display:inline_block"), inputTag)
}

`%AND%` <- function(x, y) {
  if (!is.null(x) && !is.na(x))
    if (!is.null(y) && !is.na(y))
      return(y)
  return(NULL)
}

formatNoSci <- function(x) {
  if (is.null(x)) return(NULL)
  format(x, scientific = FALSE, digits = 15)
}


# Function adapted from shinyWidgests
#' @export
dropdownButton2 <- function (..., circle = TRUE, status = "default", size = "default",
          icon = NULL, label = NULL, tooltip = FALSE, right = FALSE,
          up = FALSE, width = NULL, inputId = NULL, class = NULL)
{
  size <- match.arg(arg = size, choices = c("default", "lg",
                                            "sm", "xs"))
  if (is.null(inputId)) {
    inputId <- paste0("drop", sample.int(1e+09, 1))
  }
  html_ul <- list(class = paste("dropdown-menu", ifelse(right,
                                                        "dropdown-menu-right", "")), class = "dropdown-shinyWidgets",
                  id = paste("dropdown-menu", inputId, sep = "-"), style = if (!is.null(width)) paste0("width: ",
                                                                                                       htmltools::validateCssUnit(width), ";"), `aria-labelledby` = inputId,
                  lapply(X = list(...), FUN = htmltools::tags$li, style = "margin-left: 10px; margin-right: 10px;"))
  if (circle) {
    html_button <- circleButton(inputId = inputId, icon = icon,
                                status = status, size = size, class = paste("dropdown-toggle", class),
                                `data-toggle` = "dropdown")
  }
  else {
    html_button <- list(class = paste0("btn btn-", status,
                                       " action-button dropdown-toggle "), class = if (size !=
                                                                                       "default") paste0("btn-", size), type = "button",
                        id = inputId, `data-toggle` = "dropdown", `aria-haspopup` = "true",
                        `aria-expanded` = "true", list(icon, label), tags$span(class = "caret"))
    html_button <- do.call(htmltools::tags$button, html_button)
  }
  if (identical(tooltip, TRUE))
    tooltip <- tooltipOptions(title = label, placement = "bottom")
  if (!is.null(tooltip) && !identical(tooltip, FALSE)) {
    tooltip <- lapply(tooltip, function(x) {
      if (identical(x, TRUE))
        "true"
      else if (identical(x, FALSE))
        "false"
      else x
    })
    tooltipJs <- htmltools::tags$script(sprintf("$('#%s').tooltip({ placement: '%s', title: '%s', html: %s, trigger: 'hover' });",
                                                inputId, tooltip$placement, tooltip$title, tooltip$html))
  }
  else {
    tooltipJs <- ""
  }
  dropdownTag <- htmltools::tags$div(class = ifelse(up, "dropup", "dropdown"), 
                                     html_button, 
                                     id = paste("dropdown", inputId, sep = "-"), 
                                     do.call(htmltools::tags$ul, html_ul), 
                                     tooltipJs,
                                     tags$script(sprintf("dropBtn('#%s', %s);", paste("dropdown",
                                                                                      inputId, sep = "-"), "true")))
  attachShinyWidgetsDep(dropdownTag, "dropdown")
}



#' @export
attachShinyWidgetsDep <- function (tag, widget = NULL)
{
  version <- as.character(packageVersion("shinyWidgets")[[1]])
  dep <- htmltools::htmlDependency(name = "shinyWidgets", version = version,
                                   src = c(href = "shinyWidgets"), script = "shinyWidgets-bindings.min.js",
                                   stylesheet = "shinyWidgets.css")
  if (!is.null(widget)) {
    if (widget == "picker") {
      dep <- list(dep, htmltools::htmlDependency(name = "selectPicker",
                                                 version = "1.12.4", src = c(href = "shinyWidgets/selectPicker"),
                                                 script = "js/bootstrap-select.min.js", stylesheet = "css/bootstrap-select.min.css"))
    }
    else if (widget == "awesome") {
      dep <- list(dep, htmltools::htmlDependency(name = "awesome-bootstrap",
                                                 version = "0.2.0", src = c(href = "shinyWidgets/awesomeRadioCheckbox"),
                                                 stylesheet = "css/awesome-bootstrap-checkbox-shiny.css"),
                  htmltools::findDependencies(shiny::icon("rebel"))[[1]])
    }
    else if (widget == "bsswitch") {
      dep <- list(dep, htmltools::htmlDependency(name = "bootstrap-switch",
                                                 version = "3.3.4", src = c(href = "shinyWidgets/switchInput/bootstrap-switch-3.3.4"),
                                                 script = "bootstrap-switch.min.js", stylesheet = "bootstrap-switch.min.css"))
    }
    else if (widget == "sweetalert") {
      dep <- list(dep, htmltools::htmlDependency(name = "sweetAlert",
                                                 version = "0.2.0", src = c(href = "shinyWidgets/sweetAlert"),
                                                 script = c("sweetalert.min.js", "sweetalert-bindings.js")))
    }
    else if (widget == "multi") {
      dep <- list(dep, htmltools::htmlDependency(name = "multi",
                                                 version = "0.3.0", src = c(href = "shinyWidgets/multi"),
                                                 script = "multi.min.js", stylesheet = c("multi.min.css")))
    }
    else if (widget == "jquery-knob") {
      dep <- list(dep, htmltools::htmlDependency(name = "jquery-knob",
                                                 version = "1.2.13", src = c(href = "shinyWidgets/jquery-knob"),
                                                 script = c("jquery.knob.min.js", "knob-input-binding.js")))
    }
    else if (widget == "dropdown") {
      dep <- list(dep, htmltools::htmlDependency(name = "dropdown-patch",
                                                 version = version, src = c(href = "shinyWidgets/dropdown"),
                                                 script = "dropdown-click.js"))
    }
    else if (widget == "sw-dropdown") {
      dep <- list(dep, htmltools::htmlDependency(name = "sw-dropdown",
                                                 version = version, src = c(href = "shinyWidgets/sw-dropdown"),
                                                 script = "sw-dropdown.js", stylesheet = "sw-dropdown.css"))
    }
    else if (widget == "animate") {
      dep <- list(dep, htmltools::htmlDependency(name = "animate",
                                                 version = version, src = c(href = "shinyWidgets/animate"),
                                                 stylesheet = "animate.min.css"))
    }
    else if (widget == "bttn") {
      dep <- list(dep, htmltools::htmlDependency(name = "bttn",
                                                 version = version, src = c(href = "shinyWidgets/bttn"),
                                                 stylesheet = "bttn.min.css"))
    }
    else if (widget == "spectrum") {
      dep <- list(dep, htmltools::htmlDependency(name = "spectrum",
                                                 version = version, src = c(href = "shinyWidgets/spectrum"),
                                                 script = c("spectrum.min.js"), stylesheet = c("spectrum.min.css",
                                                                                               "sw-spectrum.css")))
    }
    else if (widget == "pretty") {
      dep <- list(dep, htmltools::htmlDependency(name = "pretty",
                                                 version = version, src = c(href = "shinyWidgets/pretty-checkbox"),
                                                 stylesheet = "pretty-checkbox.min.css"))
    }
    else if (widget == "nouislider") {
      dep <- list(dep, htmltools::htmlDependency(name = "nouislider",
                                                 version = "11.0.3", src = c(href = "shinyWidgets/nouislider"),
                                                 script = c("nouislider.min.js", "wNumb.js"),
                                                 stylesheet = "nouislider.min.css"))
    }
  }
  htmltools::attachDependencies(tag, dep, append = TRUE)
}


# Credits to https://rdrr.io/github/gadenbuie/regexplain/src/R/shiny_modified_inputs.R
#' @export
textInputCode <- function(inputId, label, value = "", width = NULL,
                          placeholder = NULL) {
    `%AND%` <- getFromNamespace("%AND%", "shiny")
    value <- shiny::restoreInput(id = inputId, default = value)

    shiny::div(class = "form-group shiny-input-container",
               style = if (!is.null(width)) paste0("width: ", shiny::validateCssUnit(width), ";"),
               label %AND% shiny::tags$label(label, `for` = inputId),
               shiny::tags$input(id = inputId, type="text", class="form-control", value=value,
                                 style = 'font-family: "Monaco", "Inconsolata", monospace;',
                                 autocomplete = "off", autocorrect = "off",
                                 autocapitalize = "off", spellcheck = "false",
                                 placeholder = placeholder)
    )
}


#' @export
fileInput2 <- function (inputId, label, multiple = FALSE, accept = NULL, width = NULL,
          buttonLabel = "Browse...", placeholder = "No file selected")
{
    restoredValue <- restoreInput(id = inputId, default = NULL)
    if (!is.null(restoredValue) && !is.data.frame(restoredValue)) {
        warning("Restored value for ", inputId, " has incorrect format.")
        restoredValue <- NULL
    }
    if (!is.null(restoredValue)) {
        restoredValue <- toJSON(restoredValue, strict_atomic = FALSE)
    }
    inputTag <- tags$input(id = inputId, name = inputId, type = "file",
                           style = "display: none;", `data-restore` = restoredValue)
    if (multiple)
        inputTag$attribs$multiple <- "multiple"
    if (length(accept) > 0)
        inputTag$attribs$accept <- paste(accept, collapse = ",")
    div(class = "form-group shiny-input-container", style = if (!is.null(width))
        paste0("width: ", validateCssUnit(width), ";"), label %AND%
            tags$label(label), div(class = "input-group", tags$label(class = "input-group-btn",
                                                                     span(class = "btn btn-primary btn-file", buttonLabel,
                                                                          inputTag)), tags$input(type = "text", class = "form-control",
                                                                                                 placeholder = placeholder, readonly = "readonly")), tags$div(id = paste(inputId,
                                                                                                                                                                         "_progress", sep = ""), class = "progress progress-striped active shiny-file-input-progress",
                                                                                                                                                              tags$div(class = "progress-bar")))
}


#' @export
materialSwitch2 <- function (inputId, label = NULL, value = FALSE, status = "default",
          right = FALSE, inline = FALSE, width = NULL)
{
    value <- shiny::restoreInput(id = inputId, default = value)
    inputTag <- htmltools::tags$input(id = inputId, type = "checkbox")
    if (!is.null(value) && value)
        inputTag$attribs$checked <- "checked"
    msTag <- htmltools::tags$div(class = "form-group shiny-input-container",
                                 style = if (!is.null(width))
                                     paste0("width: ", htmltools::validateCssUnit(width),
                                            ";"), class = if (inline)
                                                "shiny-input-container-inline", style = if (inline)
                                                    "display: inline-block; margin-right: 10px;", tags$div(class = "material-switch",
                                                                                                           if (!is.null(label) & !right)
                                                                                                               htmltools::tags$span(label, style = "padding-right: 10px;"),
                                                                                                           inputTag, htmltools::tags$label(`for` = inputId,
                                                                                                                                           class = paste0("label-", status)), if (!is.null(label) &
                                                                                                                                                                                  right)
                                                                                                                                               htmltools::tags$span(label, style = "padding-left: 5px;")))
    attachShinyWidgetsDep(msTag)
}


#' @export
circleDownloadButton <- function (inputId, icon = NULL, status = "default", size = "default",
          ...)
{
    value <- shiny::restoreInput(id = inputId, default = NULL)
    size <- match.arg(arg = size, choices = c("default", "lg",
                                              "sm", "xs"))
    attachShinyWidgetsDep(htmltools::tags$a(id = inputId, href = "", target = "_blank", download = NA,
                                                 type = "button", style = "outline: none;", `data-val` = value,
                                                 class = paste0("btn btn-", status, " shiny-download-link ",
                                                                ifelse(size == "default", "btn-circle", paste0("btn-circle-",
                                                                                                               size))), tags$span(icon), ...))
}



#' @export
modalActionButton <- function (inputId, label, icon = NULL, width = NULL, ...)
{
    value <- restoreInput(id = inputId, default = NULL)
    tags$button(id = inputId, style = if (!is.null(width))
        paste0("width: ", validateCssUnit(width), ";"), type = "button",
        class = "btn btn-default action-button", `data-val` = value,
        `data-dismiss` = "modal",  # This line is new
        list(icon, label), ...)
}


#' PIVOT help modules, UI
#'
#' @export
pivot_help_UI <- function(id, title, label = NULL, icn = "question-circle", type = "button", tooltip = T, style = NULL){
    ns<- NS(id)
    if(tooltip) {
        tip <- shinyBS::bsTooltip(
            ns("pivot_help"),
            title = title,
            options = list(container = "body")
        )
    } else {
        tip <- NULL
    }
    if(type == "button") {
        btn <-  actionButton(ns("pivot_help"), label = label, icon = icon(icn), style = style)
    } else {
        btn <- actionLink(ns("pivot_help"), label = label, icon = icon(icn), style = style)
    }
    tagList(
        btn,
        tip
    )
}

#' PIVOT help modules, server
#'
#' @export
pivot_help <- function (input, output, session, title, content, size = "m") {
    observeEvent(input$pivot_help, {
        showModal(modalDialog(
            title = title,
            size = size,
            content,
            easyClose = TRUE
        ))
    })
}

#' PIVOT Data Input, UI function
#' @description
#' Modal confirm button from Weicheng Zhu
#' source: https://github.com/mingsnu/shiny-confirm-dialog
#' Note: This was not used in PIVOT. I later switched to shiny modal.
#' @export
modalConfirmDialog = function(id, header = "Confirmation", body = "Are you sure?", footer = list(actionButton("confirmDlgOkBtn", "OK")), size = "m"){
    if(size != "m") {
        modalclass = paste0("modal-dialog modal-", modalsize)
    } else {
        modalclass = "modal-dialog"
    }
    div(id = id, class = "modal fade",
        div(class = modalclass,
            div(class = "modal-content",
                div(class = "modal-header",
                    tags$button(type = "button", class = "close", 'data-dismiss' = "modal", 'aria-hidden' = "true", HTML('&times;')),
                    tags$h4(class = "modal-title", header)
                ),
                div(class = "modal-body",
                    tags$p(body)
                ),
                div(class = "modal-footer",
                    tagList(footer)
                )
            )
        )
    )
}

#' PIVOT Data Input, UI function
#' @description
#' modal trigger button from Weicheng Zhu
#' source: https://github.com/mingsnu/shiny-confirm-dialog
#' Note: This was not used in PIVOT. I later switched to shiny modal.
#' @export
modalTriggerButton = function(inputId, target, label, icon = NULL, type = "button", class = "btn-danger btn_leftAlign"){
    if (!is.null(icon))
        buttonContent <- list(icon, label)
    else buttonContent <- label
    class <- paste("btn action-button", class)
    if(type == "button") {
        tags$button(id = inputId, type = "button", class = class, 'data-toggle' = "modal", 'data-target' = target,
                    buttonContent)
    } else {
        tags$a(id = inputId, href = "#", class = class, 'data-toggle' = "modal", 'data-target' = target,
                    buttonContent)
    }
}

