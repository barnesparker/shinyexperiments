box::use(
  sh = shiny[moduleServer, NS],
  bs = bslib,
  workflowsets
)

#' @export
ui <- function(id) {
  ns <- NS(id)

  sh$tagList(
    sh$selectInput(
      ns("wflow_set_select"),
      "Select Workflow Set",
      choices = NULL
    ),
    sh$verbatimTextOutput(
      ns("wflow_set_info")
    ),
    sh$actionButton(
      ns("wflow_set_train_button"),
      "Train Workflow Set"
    )
  )
}

#' @export
server <- function(id, wflow_sets, resampling_folds) {
  moduleServer(id, function(input, output, session) {
    sh$observe({
      sh$updateSelectInput(
        session,
        "wflow_set_select",
        choices = names(wflow_sets)
      )
    })


    output$wflow_set_info <- sh$renderPrint({
      wflow_sets[[sh$req(input$wflow_set_select)]]
    })

    map_results <-
      sh$reactive({
        results <- workflowsets$workflow_map(
          wflow_sets[[sh$req(input$wflow_set_select)]],
          fn = "tune_grid",
          resamples = resampling_folds(),
        )

        browser()
        results
      }) |>
      sh$bindEvent(input$wflow_set_train_button)


    map_results
  })
}
