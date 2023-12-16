#' hyperparam UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_hyperparam_ui <- function(id, arg) {
  ns <- NS(id)
  bslib::layout_columns(
    shinyjs::useShinyjs(),
    shinyjs::disabled(shiny::numericInput(
      ns("param_value"),
      arg,
      value = NULL
    )),
    shiny::checkboxInput(
      ns("param_tune"),
      "Tune",
      value = T
    )
  )
}

#' hyperparam Server Functions
#'
#' @noRd
mod_hyperparam_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    shiny::observe({
      shiny::req(!is.null(input$param_tune))

      if (input$param_tune) {
        shinyjs::disable("param_value")
      } else {
        shinyjs::enable("param_value")
      }
    })

    param_value <- shiny::reactive({
      shiny::req(!is.null(input$param_tune))

      if (input$param_tune) {
        tune::tune()
      } else {
        input$param_value
      }
    })


    param_value
  })
}

## To be copied in the UI
# mod_hyperparam_ui("hyperparam_1")

## To be copied in the server
# mod_hyperparam_server("hyperparam_1")
