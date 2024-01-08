#' tune_config UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_tune_config_ui <- function(id) {
  ns <- NS(id)
  tagList(
    bslib::accordion(
      id = ns("config_accordion"),
      open = T,
      title = "Train",
      bslib::accordion_panel(
        "Tune Grid",
        shiny::uiOutput(ns("model_config_ui")),
        shiny::selectInput(
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
        shiny::uiOutput(ns("grid_size_ui"))
      ),
      bslib::accordion_panel(
        "Tune Parameters",
        shiny::uiOutput(ns("tune_param_ui"))
      )
      # bslib::accordion_panel(
      #   "Metrics Configuration",
      #   shiny::selectInput(
      #     inputId = ns("metrics"),
      #     label = "Metrics",
      #     choices = NULL,
      #     multiple = T
      #   )
      # )
    ),
    shiny::verbatimTextOutput(ns("tune_config_preview")),
    # shiny::actionButton(
    #   ns("save_tune_config_button"),
    #   "Save Tune Config",
    #   icon = shiny::icon("save")
    # ),
    mod_save_object_dialog_ui(
      ns("save_object_dialog_tune"),
      "Save Tune Config"
    )
  )
}

#' tune_config Server Functions
#'
#' @noRd
mod_tune_config_server <- function(id, model_mode, model_spec, train_data, exp_id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    exp_board <- get_exp_board()

    saved_models <-
      reactivePoll(1000, session,
        checkFunc = function() {
          if (is.null(exp_id())) return(NULL)

          exp_board |>
            get_saved_objects_list("model", shiny::req(exp_id())) |>
            length()
        },
        valueFunc = function() {
          models <-
            exp_board |>
            get_saved_objects_list("model", shiny::req(exp_id()))

          names(models) <- models |>
            clean_object_names()

          models
        }
      )


    output$model_config_ui <-
      shiny::renderUI({
        shiny::selectInput(
          ns("model_config"),
          "Select Model Config",
          choices = saved_models()
        )
      })

    shiny::observe({
      shiny::freezeReactiveValue(input, "grid_size")
      if (input$tune_grid_select == "grid_regular") {
        shiny::updateNumericInput(
          session = session,
          inputId = "grid_size",
          label = "Levels",
          value = 5
        )
      } else {
        shiny::updateNumericInput(
          session = session,
          inputId = "grid_size",
          label = "Size",
          value = 20
        )
      }
    })

    output$grid_size_ui <-
      shiny::renderUI({
        if (input$tune_grid_select != "grid_regular") {
          shiny::numericInput(
            ns("grid_size"),
            label = "Max Grid Size",
            value = 20
          )
        }
      })

    tunable_param_list <- shiny::reactive({

      shiny::req(input$model_config)
      exp_board <- get_exp_board()
      reactive_model <-
        exp_board |>
        pins::pin_reactive_read(input$model_config)

      reactive_model()$args |>
        purrr::map(rlang::as_label) |>
        purrr::keep(~ .x == "tune()")
    })


    tune_param_inputs <- shiny::reactiveValues()

    output$tune_param_ui <-
      shiny::renderUI({
        box::use(dials)
        tune_inputs <-
          purrr::map(
            names(tunable_param_list()),
            # tunable_param_list(),
            \(tune_param) {
              default_param_vals <-
                get(tune_param, envir = dials) |>
                formals() |>
                as.list() |>
                pluck_param() |>
                eval()

              tune_param_id <- paste("mod_tune", tune_param, "range", sep = "_")
              tune_param_ui <- mod_tune_param_ui(
                ns(tune_param_id), tune_param, input$tune_grid_select,
                default_param_vals
              )
              tune_param_inputs[[tune_param]] <- mod_tune_param_server(tune_param_id)
              tune_param_ui
            }
          )

        shiny::tagList(!!!tune_inputs)
      })

    # reactive_metric_set <-
    #   shiny::reactive({
    #     do.call(yardstick$metric_set, list(input$metrics))
    #   })

    reactive_tune_config <-
      shiny::reactive({
        box::use(dials)
        tune_param_list <- tune_param_inputs |> shiny::reactiveValuesToList()

        tune_param_list <-
          tune_param_list[names(tune_param_list) %in% names(tunable_param_list())] |>
          de_reactive()

        shiny::req(length(tune_param_list) > 0)


        levels_vec <-
          tune_param_list |>
          purrr::map(~ purrr::keep(.x, names(.x) == "levels")) |>
          unlist() |>
          shiny::req() |>
          setNames(names(tune_param_list))

        tune_param_list <-
          purrr::map2(names(tune_param_list), tune_param_list, \(param, args) {
            args <- args[names(args) != "levels"]
            do.call(param, purrr::compact(args), envir = dials)
          })

        if (input$tune_grid_select == "grid_regular") {
          tune_param_list <- c(tune_param_list, list(levels = levels_vec))
        } else {
          tune_param_list <- c(tune_param_list, list(size = shiny::req(input$grid_size)))
        }
        do.call(input$tune_grid_select, tune_param_list, envir = dials)
      })

    output$tune_config_preview <-
      shiny::renderPrint({
        reactive_tune_config()
      })


    mod_save_object_dialog_server("save_object_dialog_tune", "Tune Config", reactive_tune_config, exp_id,
                                  object_name = shiny::reactive(clean_object_names(input$model_config)))






    # reactive_metric_set
  })
}

## To be copied in the UI
# mod_tune_config_ui("tune_config_1")

## To be copied in the server
# mod_tune_config_server("tune_config_1")
