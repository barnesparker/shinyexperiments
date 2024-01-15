#' metrics UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_metrics_ui <- function(id) {
  ns <- NS(id)
  shiny::tagList(
    bslib::accordion(
      id = ns("metrics_accordion"),
      bslib::accordion_panel(
        "Metrics",
        selectInput(
          inputId = ns("metrics_select"),
          label = "Metrics",
          choices = NULL,
          multiple = T
        )
      )
    ),
    shiny::verbatimTextOutput(ns("metrics_preview")),
    mod_save_object_dialog_ui(ns("save_object_dialog_metrics"))
  )
}

#' metrics Server Functions
#'
#' @noRd
mod_metrics_server <- function(id, model_mode, exp_id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns


    shiny::observe({
      shiny::req(model_mode())

      shiny::updateSelectInput(
        inputId = "metrics_select",
        session = session,
        choices = get_metrics_list(model_mode()),
        selected = get_default_metrics(model_mode())
      )
    })

    reactive_metrics_config <-
      shiny::reactive({
        shiny::req(input$metrics_select)
        metrics_config(
          metrics = as.list(input$metrics_select),
          exp_id = get_golem_config("exp_id")
        )
      })


    reactive_metric_set <-
      shiny::reactive({
        reactive_metrics_config() |>
          build_object_from_config()
      })

    output$metrics_preview <-
      shiny::renderPrint({
        reactive_metric_set()
      })



    mod_save_object_dialog_server(
      "save_object_dialog_metrics",
      "Metrics",
      reactive_metrics_config
    )
  })
}

## To be copied in the UI
# mod_metrics_ui("metrics_1")

## To be copied in the server
# mod_metrics_server("metrics_1")
