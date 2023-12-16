#' tune_param UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_tune_param_ui <- function(id, param, grid_type, default_param_vals) {
  ns <- NS(id)
  tagList(
    if (is.numeric(default_param_vals)) {
      bslib::layout_columns(
        shinyWidgets::numericRangeInput(
          ns("param_range"),
          label = param,
          value = default_param_vals
        ),
        if (grid_type == "grid_regular") {
          shiny::numericInput(
            ns("param_levels"),
            "Levels",
            value = 3
          )
        }
      )
    } else if (is.character(default_param_vals)) {
      shiny::selectInput(
        ns("param_select"),
        label = param,
        choices = default_param_vals,
        selected = default_param_vals,
        multiple = T
      )
    }
  )
}

#' tune_param Server Functions
#'
#' @noRd
mod_tune_param_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    levels <-
      shiny::reactive({
        if (!is.null(input$param_select)) {
          length(input$param_select)
        } else {
          input$param_levels
        }
      })



    list(
      range = shiny::reactive(input$param_range),
      trans = shiny::reactive(input$param_trans),
      levels = levels,
      values = shiny::reactive(input$param_select)
    )
  })
}

## To be copied in the UI
# mod_tune_param_ui("tune_param_1")

## To be copied in the server
# mod_tune_param_server("tune_param_1")
