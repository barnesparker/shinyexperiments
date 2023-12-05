box::use(
  sh = shiny[moduleServer, NS],
  bs = bslib,
  tune,
  yardstick,
  dials,
  purrr,
  rlang,
  sw = shinyWidgets,
  stringr,
  stats
)

box::use(
  app/view/mod_tune_param,
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
          )
          # selected = "grid_latin_hypercube"
        ),
        sh$uiOutput(ns("grid_size_ui"))
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
    sh$verbatimTextOutput(ns("tune_config_preview")),
    sh$actionButton(
      ns("save_tune_config_button"),
      "Save Tune Config",
      icon = sh$icon("save")
    )
  )
}

#' @export
server <- function(id, model_mode, model_spec, saved_models, saved_tune_configs, train_data) {
  moduleServer(id, function(input, output, session) {
    output$model_config_ui <-
      sh$renderUI({
        sh$selectInput(
          session$ns("model_config"),
          "Select Model Config",
          choices = names(saved_models)
        )
      })

    sh$observe({
      sh$freezeReactiveValue(input, "grid_size")
      if (input$tune_grid_select == "grid_regular") {
        sh$updateNumericInput(
          session = session,
          inputId = "grid_size",
          label = "Levels",
          value = 5
        )
      } else {
        sh$updateNumericInput(
          session = session,
          inputId = "grid_size",
          label = "Size",
          value = 20
        )
      }
    })

    output$grid_size_ui <-
      sh$renderUI({
        if (input$tune_grid_select != "grid_regular") {
          sh$numericInput(
            session$ns("grid_size"),
            label = "Max Grid Size",
            value = 20
          )
        }
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

    tunable_param_list <- sh$reactive({
      saved_models[[sh$req(input$model_config)]]$args |>
        purrr$map(rlang$as_label) |>
        purrr$keep(~ .x == "tune()")
    })


    tune_param_inputs <- sh$reactiveValues()

    output$tune_param_ui <-
      sh$renderUI({
        tune_inputs <-
          purrr$map(
            names(tunable_param_list()),
            # tunable_param_list(),
            \(tune_param) {
              default_param_vals <-
                get(tune_param, envir = dials) |>
                formals() |>
                as.list() |>
                helpers$pluck_param() |>
                eval()

              tune_param_id <- paste("mod_tune", tune_param, "range", sep = "_")
              # browser()
              tune_param_ui <- mod_tune_param$ui(
                session$ns(tune_param_id), tune_param, input$tune_grid_select,
                default_param_vals
              )
              tune_param_inputs[[tune_param]] <- mod_tune_param$server(tune_param_id)
              tune_param_ui
            }
          )

        sh$tagList(!!!tune_inputs)
      })

    # reactive_metric_set <-
    #   sh$reactive({
    #     do.call(yardstick$metric_set, list(input$metrics))
    #   })

    reactive_tune_config <-
      sh$reactive({
        tune_param_list <- tune_param_inputs |> sh$reactiveValuesToList()

        tune_param_list <-
          tune_param_list[names(tune_param_list) %in% names(tunable_param_list())] |>
          helpers$de_reactive()

        sh$req(length(tune_param_list) > 0)

        # browser()

        levels_vec <-
          tune_param_list |>
          purrr$map(~purrr$keep(.x, names(.x) == "levels")) |>
          unlist() |>
          sh$req() |>
          stats$setNames(names(tune_param_list))

        tune_param_list <-
          purrr$map2(names(tune_param_list), tune_param_list, \(param, args) {
            args <- args[names(args) != "levels"]
            do.call(param, purrr$compact(args), envir = dials)
          })

        if (input$tune_grid_select == "grid_regular") {
          tune_param_list <- c(tune_param_list, list(levels = levels_vec))
        } else {
          tune_param_list <- c(tune_param_list, list(size = sh$req(input$grid_size)))
        }
        do.call(input$tune_grid_select, tune_param_list, envir = dials)
      })

    output$tune_config_preview <-
      sh$renderPrint({
        reactive_tune_config()
      })


    sh$observe({
      sh$showModal(
        sh$modalDialog(
          title = paste0("Save Tune Configuration for model spec: ", input$model_config),
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
