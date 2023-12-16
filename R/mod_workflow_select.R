#' workflow_select UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_workflow_select_ui <- function(id) {
  ns <- NS(id)
  tagList(
    shiny::selectInput(
      ns("wflow_set_select"),
      "Select Workflow Set",
      choices = NULL
    ),
    shiny::verbatimTextOutput(
      ns("wflow_set_info")
    ),
    shiny::actionButton(
      ns("wflow_set_train_button"),
      "Train Workflow Set"
    )
  )
}

#' workflow_select Server Functions
#'
#' @noRd
mod_workflow_select_server <- function(id, wflow_sets, resampling_folds) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    shiny::observe({
      shiny::updateSelectInput(
        session,
        "wflow_set_select",
        choices = names(wflow_sets)
      )
    })


    output$wflow_set_info <- shiny::renderPrint({
      wflow_sets[[shiny::req(input$wflow_set_select)]]
    })

    map_results <-
      shiny::reactive({
        results <- workflowsets::workflow_map(
          wflow_sets[[shiny::req(input$wflow_set_select)]],
          fn = "tune_grid",
          resamples = resampling_folds(),
        )
        results
      }) |>
      shiny::bindEvent(input$wflow_set_train_button)


    map_results
  })
}

## To be copied in the UI
# mod_workflow_select_ui("workflow_select_1")

## To be copied in the server
# mod_workflow_select_server("workflow_select_1")
