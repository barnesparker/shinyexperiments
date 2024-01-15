#' modeling UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_modeling_ui <- function(id) {
  ns <- NS(id)
  tagList(
    bslib::accordion(
      id = ns("modeling_accordion"),
      open = T,
      bslib::accordion_panel(
        "Model Selection",
        shiny::uiOutput(ns("model_selection_ui")),
        shiny::selectInput(
          ns("engine"),
          "Choose a Model Engine",
          choices = NULL
        )
      ),
      bslib::accordion_panel(
        "Hyperparameters",
        shiny::uiOutput(ns("args_ui"))
      )
    ),
    shiny::verbatimTextOutput(ns("model_preview")),
    mod_save_object_dialog_ui(
      ns("save_object_dialog_model"),
      "Save Model"
    )
  )
}

#' modeling Server Functions
#'
#' @noRd
mod_modeling_server <- function(id, model_mode) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    output$model_selection_ui <-
      shiny::renderUI({
        model_choices <- parsnip::model_db |>
          dplyr::filter(
            mode == model_mode(),
            package == "parsnip"
          ) |>
          dplyr::distinct(model) |>
          dplyr::pull(model)

        shiny::selectInput(
          ns("model_selection"),
          "Choose a Model",
          choices = model_choices,
          selected = "decision_tree"
        )
      })


    output$model_preview <-
      shiny::renderPrint({
        reactive_model_spec()
      })

    mod_save_object_dialog_server("save_object_dialog_model", "Model", reactive_model_config)

    reactive_model_config <-
      shiny::reactive({

        params_list <- shiny::reactiveValuesToList(param_inputs)

        params_list <-
          params_list[names(params_list) %in% available_args()] |>
          de_reactive()

        model_config(
          type = shiny::req(input$model_selection),
          engine = shiny::req(input$engine),
          mode = model_mode(),
          params = params_list,
          exp_id = get_golem_config("exp_id")
        )
      })

    reactive_model_spec <-
      shiny::reactive({
        reactive_model_config() |>
          build_object_from_config()
      })

    shiny::observe({
      # engines <-
      #   shiny::req(input$model_selection) |>
      #   parsnip::show_engines() |>
      #   dplyr::filter(
      #     mode == shiny::req(model_mode()),
      #     !engine %in% c("spark", "brulee")
      #   ) |>
      #   dplyr::pull(engine)

      engines <-
        parsnip::model_db |>
        dplyr::filter(
          mode == shiny::req(model_mode()),
          package == "parsnip",
          model == shiny::req(input$model_selection),
          !engine %in% c("spark", "brulee")
        ) |>
        dplyr::distinct(engine) |>
        dplyr::pull(engine)

      shiny::freezeReactiveValue(input, "engine")
      shiny::updateSelectInput(
        session,
        "engine",
        choices = engines,
        selected = engines[1]
      )
    })



    available_args <-
      shiny::reactive({
        box::use(parsnip)
        model_args <-
          get(shiny::req(input$model_selection), envir = parsnip) |>
          formals()

        # model_args <-
        #   parsnip::model_db |>
        #   dplyr::filter(
        #     mode == shiny::req(model_mode()),
        #     engine == shiny::req(input$engine)
        #   ) |>
        #   # tidyr$unnest(parameters)
        #   dplyr::pull(parameters)


        model_args <- names(model_args[!names(model_args) %in% c("mode", "engine")])

        # shiny::req(length(model_args) > 0)


        model_args
        # model_args[[1]] |> dplyr::pull()
      })

    param_inputs <- shiny::reactiveValues()

    output$args_ui <-
      shiny::renderUI({
        if (length(available_args()) == 0) {
          shiny::tagList()
        } else {
          args_inputs <-
            purrr::map(
              available_args(),
              \(arg) {

                param_id <- paste("mod", input$model_selection, arg, sep = "_")
                param_ui <- mod_hyperparam_ui(ns(param_id), arg)
                param_inputs[[arg]] <- mod_hyperparam_server(param_id)
                param_ui
              }
            )
          shiny::tagList(!!!args_inputs)
        }
      })


    reactive_model_spec
  })
}

## To be copied in the UI
# mod_modeling_ui("modeling_1")

## To be copied in the server
# mod_modeling_server("modeling_1")
