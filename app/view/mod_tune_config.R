box::use(
  sh = shiny[moduleServer, NS],
  bs = bslib,
  tune,
  yardstick,
  dials,
  purrr,
  rlang,
  sw = shinyWidgets
)

box::use(
  app/logic/helpers
)

#' @export
ui <- function(id) {
  ns <- NS(id)
  sh$tagList(
    bs$accordion(
      id = ns("config_accordion"),
      open = T,
      title = "Train",
      bs$accordion_panel(
        "Tune Grid",
        sh$uiOutput(ns("model_config_ui")),
        sh$selectInput(
          ns("tune_grid_select"),
          "Tune Grid",
          choices = c(
            "Regular" = "grid_regular",
            "Random" = "grid_random",
            "Latin Hypercube" = "grid_latin_hypercube",
            "Max Entropy" = "grid_max_entropy"
          ),
          selected = "grid_latin_hypercube"
        )
      ),
      bs$accordion_panel(
        "Tune Parameters",
        sh$uiOutput(ns("tune_param_ui"))
      )
      # bs$accordion_panel(
      #   "Metrics Configuration",
      #   sh$selectInput(
      #     inputId = ns("metrics"),
      #     label = "Metrics",
      #     choices = NULL,
      #     multiple = T
      #   )
      # )
    ),
    sh$actionButton(
      ns("save_tune_config_button"),
      "Save Tune Config",
      icon = sh$icon("save")
    )
  )
}

#' @export
server <- function(id, model_mode, model_spec, saved_models, saved_tune_configs) {
  moduleServer(id, function(input, output, session) {
    output$model_config_ui <-
      sh$renderUI({
        sh$selectInput(
          session$ns("model_config"),
          "Select Model Config",
          choices = names(saved_models)
        )
      })

    # sh$observe({
    #   if (model_mode() == "classification") {
    #     sh$updateSelectInput(
    #       session = session,
    #       inputId = "metrics",
    #       choices = c(
    #         "Accuracy" = "accuracy",
    #         "AUC" = "roc_auc",
    #         "F1" = "f_meas",
    #         "Kappa" = "kappa",
    #         "MCC" = "mcc"
    #       ),
    #       selected = c("accuracy", "roc_auc")
    #     )
    #   } else {
    #     sh$updateSelectInput(
    #       session = session,
    #       inputId = "metrics",
    #       choices = c(
    #         "RMSE" = "rmse",
    #         "RSquared" = "rsq",
    #         "MAE" = "mae"
    #       ),
    #       selected = c("rmse", "rsq")
    #     )
    #   }
    # })


    output$tune_param_ui <-
      sh$renderUI({
        tunable_param_list <-
          saved_models[[sh$req(input$model_config)]]$args |>
          purrr$map(rlang$as_label) |>
          purrr$keep(~ .x == "tune()")
        tune_inputs <-
          purrr$map2(
            names(tunable_param_list),
            tunable_param_list,
            \(param, value) {
              default_param_vals <-
                get(param, envir = dials) |>
                formals() |>
                as.list() |>
                helpers$pluck_param() |>
                eval()

              if (is.numeric(default_param_vals)) {
                sw$numericRangeInput(
                  session$ns(paste0(input$model_config, "_", param, "range")),
                  label = paste(param, "Range"),
                  value = default_param_vals
                )
              } else if (is.character(default_param_vals)) {
                sh$selectInput(
                  session$ns(paste0(input$model_config, "_", param, "select")),
                  label = paste(param, "Select"),
                  choices = default_param_vals
                )
              }
            }
          )

        sh$tagList(!!!tune_inputs)
        # if (input$tune_grid_select == "grid_regular") {
        #
        #
        #
        # } else {
        #
        # }
      })

    # reactive_metric_set <-
    #   sh$reactive({
    #     do.call(yardstick$metric_set, list(input$metrics))
    #   })

    reactive_tune_config <-
      sh$reactive({
        do.call(input$tune_grid_select, list())
      })


    sh$observe({
      sh$showModal(
        sh$modalDialog(
          title = "Save Tune Configuration",
          # sh$textInput(ns("tune_config_name"), "Tune Configuration Name"),
          footer = sh$tagList(
            sh$modalButton("Cancel"),
            sh$actionButton(
              session$ns("confirm_save_button"),
              "Save Configuration",
              icon = sh$icon("save")
            )
          )
        )
      )
    }) |>
      sh$bindEvent(input$save_tune_config_button)

    sh$observe({
      sh$removeModal()

      saved_tune_configs[[input$model_config]] <- reactive_tune_config()
    }) |>
      sh$bindEvent(input$confirm_save_button)





    # reactive_metric_set
  })
}
