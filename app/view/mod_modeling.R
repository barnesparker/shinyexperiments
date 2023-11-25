box::use(
  sh = shiny[moduleServer, NS],
  bs = bslib,
  parsnip,
  dp = dplyr,
  purrr,
  stringr,
  tune,
  tidyr
)

box::use(
  app/view/mod_hyperparam
)

#' @export
ui <- function(id) {
  ns <- NS(id)
  bs$accordion(
    id = ns("modeling_accordion"),
    open = T,
    sh$tagList(
      bs$accordion_panel(
        "Model Selection",
        sh$uiOutput(ns("model_selection_ui")),
        sh$selectInput(
          ns("engine"),
          "Choose a Model Engine",
          choices = NULL
        )
      ),
      bs$accordion_panel(
        "Hyperparameters",
        sh$uiOutput(ns("args_ui"))
      )
    ),
    sh$verbatimTextOutput(ns("model_preview")),
    sh$actionButton(
      ns("save_model_button"),
      "Save Model",
      icon = sh$icon("save")
    )
  )
}

#' @export
server <- function(id, model_mode = reactive("regression")) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns


    output$model_selection_ui <-
      sh$renderUI({

        model_choices <- parsnip$model_db |>
          dp$filter(
            mode == model_mode(),
            package == "parsnip"
          ) |>
          dp$distinct(model) |>
          dp$pull(model)

        sh$selectInput(
          session$ns("model_selection"),
          "Choose a Model",
          choices = model_choices,
          selected = model_choices[1]
        )
      })


    output$model_preview <-
      sh$renderPrint({
        reactive_model_spec()
      })

    sh$observe({
      sh$showModal(
        sh$modalDialog(
          title = "Save Model",
          sh$textInput(ns("model_name"), "Model Name"),
          footer = sh$tagList(
            sh$modalButton("Cancel"),
            sh$actionButton(
              ns("confirm_save_button"),
              "Save Model",
              icon = sh$icon("save")
            )
          )
        )
      )
    }) |>
      sh$bindEvent(input$save_model_button)

    sh$observe({
      sh$removeModal()

    }) |>
      sh$bindEvent(input$confirm_save_button)

    # function to recursively return all elements of list non reactive
    de_reactive <- function(x) {
      if (is.list(x)) {
        purrr$map(x, de_reactive)
      } else if (sh$is.reactive(x)) {
        x()
      } else {
        x
      }
    }

    reactive_model_spec <-
      sh$reactive({
        params_list <- sh$reactiveValuesToList(param_inputs)

        params_list <-
          params_list[names(params_list) %in% available_args()] |>
          de_reactive()

        # browser()

        do.call(
          sh$req(input$model_selection),
          c(list(mode = model_mode(), engine = sh$req(input$engine)), params_list),
          envir = parsnip
        )
      })

    sh$observe({
      # engines <-
      #   sh$req(input$model_selection) |>
      #   parsnip$show_engines() |>
      #   dp$filter(
      #     mode == sh$req(model_mode()),
      #     !engine %in% c("spark", "brulee")
      #   ) |>
      #   dp$pull(engine)

      engines <-
        parsnip$model_db |>
        dp$filter(
          mode == sh$req(model_mode()),
          package == "parsnip",
          model == sh$req(input$model_selection),
          !engine %in% c("spark", "brulee")
        ) |>
        dp$distinct(engine) |>
        dp$pull(engine)


      sh$updateSelectInput(
        session,
        "engine",
        choices = engines,
        selected = engines[1]
      )
    })



    available_args <-
      sh$reactive({
        model_args <-
          get(sh$req(input$model_selection), envir = parsnip) |>
          formals()

        # model_args <-
        #   parsnip$model_db |>
        #   dp$filter(
        #     mode == sh$req(model_mode()),
        #     engine == sh$req(input$engine)
        #   ) |>
        #   # tidyr$unnest(parameters)
        #   dp$pull(parameters)

        # browser()

        model_args <- names(model_args[!names(model_args) %in% c("mode", "engine")])

        # sh$req(length(model_args) > 0)


        model_args
        # model_args[[1]] |> dp$pull()
      })

    param_inputs <- sh$reactiveValues()

    output$args_ui <-
      sh$renderUI({
        if (length(available_args()) == 0) {
          sh$tagList()
        } else {

          args_inputs <-
            purrr$map(
              available_args(),
              \(arg) {
                # browser()
                # default_param_values <-
                #   get(arg, envir = dials) |>
                #   formals()

                param_id <- paste("mod", input$model_selection, arg, sep = "_")
                param_ui <- mod_hyperparam$ui(ns(param_id), arg)
                param_inputs[[arg]] <- mod_hyperparam$server(param_id)
                param_ui
              }
            )
          sh$tagList(!!!args_inputs)
        }
      })


    reactive_model_spec
  })
}
