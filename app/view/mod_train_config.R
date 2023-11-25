box::use(
  sh = shiny[moduleServer, NS],
  bs = bslib,
  tune,
  yardstick,
  dials
)

#' @export
ui <- function(id) {
  ns <- NS(id)
  bs$accordion(
    id = ns("config_accordion"),
    title = "Train",
    bs$accordion_panel(
      "Tune Grid",
      sh$selectInput(
        ns("tune_grid"),
        "Tune Grid",
        choices = c(
          "Regular" = "grid_regular",
          "Random" = "grid_random",
          "Latin Hypercube" = "grid_latin_hypercube",
          "Max Entropy" = "grid_max_entropy"
        ),
        selected = "grid_latin_hypercube"
      ),

    ),
    bs$accordion_panel(
      "Metrics Configuration",
      sh$selectInput(
        inputId = ns("metrics"),
        label = "Metrics",
        choices = NULL,
        multiple = T
      )
    )
  )
}

#' @export
server <- function(id, model_mode, model_spec) {
  moduleServer(id, function(input, output, session) {

    sh$observe({
      if (model_mode() == "classification") {
        sh$updateSelectInput(
          session = session,
          inputId = "metrics",
          choices = c(
            "Accuracy" = "accuracy",
            "AUC" = "roc_auc",
            "F1" = "f_meas",
            "Kappa" = "kappa",
            "MCC" = "mcc"
          ),
          selected = c("accuracy", "roc_auc")
        )
      } else {
        sh$updateSelectInput(
          session = session,
          inputId = "metrics",
          choices = c(
            "RMSE" = "rmse",
            "RSquared" = "rsq",
            "MAE" = "mae"
          ),
          selected = c("rmse", "rsq")
        )
      }
    })

    reactive_metric_set <-
      sh$reactive({
        do.call(yardstick$metric_set, list(input$metrics))
      })


    reactive_metric_set

  })
}
