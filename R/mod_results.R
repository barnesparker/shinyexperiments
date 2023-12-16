#' results UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_results_ui <- function(id) {
  ns <- NS(id)
  tagList(
    shiny::verbatimTextOutput(ns("results_print")),
    bslib::card(
      shiny::plotOutput(ns("results_autoplot"))
    )
  )
}

#' results Server Functions
#'
#' @noRd
mod_results_server <- function(id, results) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    output$results_print <- shiny::renderPrint({
      results()
    })

    output$results_autoplot <- shiny::renderPlot({
      results() |>
        workflowsets::autoplot()
    })
  })
}

## To be copied in the UI
# mod_results_ui("results_1")

## To be copied in the server
# mod_results_server("results_1")
