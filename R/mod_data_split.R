#' data_split UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_data_split_ui <- function(id) {
  ns <- NS(id)
  shiny::tagList(
    bslib::accordion(
      open = T,
      bslib::accordion_panel(
        "Initial Split",
        shiny::sliderInput(
          ns("split_prop"),
          "Train/Test Split Proportion",
          value = .75,
          min = 0,
          max = 1
        ),
        bslib::layout_columns(
          shiny::numericInput(
            ns("seed"),
            "Seed",
            value = 123,
            min = 1
          ),
          shiny::uiOutput(ns("strata_ui"))
        ),
        shiny::verbatimTextOutput(ns("preview_splits"))
      ),
      bslib::accordion_panel(
        "Resamples",
        bslib::layout_columns(
          shiny::numericInput(
            ns("folds"),
            "Folds",
            value = 5,
            min = 2
          ),
          shiny::numericInput(
            ns("repeats"),
            "Repeats",
            value = 1,
            min = 1
          )
        ),
        shiny::verbatimTextOutput(ns("preview_resamples"))
      ),
    ),
    mod_save_object_dialog_ui(ns("save_object_dialog_split"))
  )
}

#' data_split Server Functions
#'
#' @noRd
mod_data_split_server <- function(id, raw_data, outcome) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns


    # iv <- shinyvalidate::InputValidator$new()
    #
    # iv$add_rule("seed", shinyvalidate::sv_required())
    # iv$add_rule("seed", shinyvalidate::sv_integer())
    #
    # iv$add_rule("folds", shinyvalidate::sv_required())
    # iv$add_rule("folds", shinyvalidate::sv_integer())
    #
    # iv$add_rule("repeats", shinyvalidate::sv_required())
    # iv$add_rule("repeats", shinyvalidate::sv_integer())
    #
    # iv$enable()


    output$strata_ui <- shiny::renderUI({
      shiny::selectInput(
        ns("strata"),
        "Strata",
        choices = c(colnames(raw_data()), "None"),
        selected = outcome()
      )
    })

    reactive_strata <-
      shiny::reactive({
        if (input$strata == "None") {
          NULL
        } else {
          input$strata
        }
      })


    reactive_split <-
      shiny::reactive({
        # req(iv$is_valid())
        set.seed(input$seed)
        raw_data() |>
          rsample::initial_split(
            prop = input$split_prop,
            strata = reactive_strata()
          )
      }) |>
      shiny::debounce(1000)

    output$preview_splits <- shiny::renderPrint({
      shiny::req(reactive_split())
    })

    reactive_resamples <-
      shiny::reactive({
        # req(iv$is_valid())
        set.seed(input$seed)
        rsample::training(reactive_split()) |>
          rsample::vfold_cv(
            v = shiny::req(input$folds),
            repeats = shiny::req(input$repeats),
            strata = reactive_strata()
          )
      })

    output$preview_resamples <- shiny::renderPrint({
      reactive_resamples()
    })

    reactive_split_config <-
      shiny::reactive({
        split_config(
          split_prop = input$split_prop,
          strata = reactive_strata(),
          folds = input$folds,
          repeats = input$repeats,
          seed = input$seed,
          exp_id = get_golem_config("exp_id")
        )
      })


    mod_save_object_dialog_server(
      "save_object_dialog_split",
      type = "Splitting Configuration",
      reactive_split_config
    )


    list(
      reactive_split = reactive_split,
      reactive_resamples = reactive_resamples
    )
  })
}
