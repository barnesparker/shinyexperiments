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
        shiny::numericInput(
          ns("seed"),
          "Seed",
          value = 123,
          min = 1
        ),
        shiny::uiOutput(ns("strata_ui")),
        shiny::verbatimTextOutput(ns("preview_splits"))
      ),
      bslib::accordion_panel(
        "Resamples",
        bslib::layout_columns(
          shiny::numericInput(
            ns("folds"),
            "Folds",
            value = 10,
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
    shiny::actionButton(
      ns("save_split_button"),
      "Save Splitting Configuration"
    )
  )
}

#' data_split Server Functions
#'
#' @noRd
mod_data_split_server <- function(id, raw_data, outcome, saved_split_configs) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

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
        if (shiny::req(input$strata) == "None") {
          NULL
        } else {
          input$strata
        }
      })


    reactive_split <-
      shiny::reactive({
        set.seed(input$seed)
        raw_data() |>
          rsample::initial_split(
            prop = shiny::req(input$split_prop),
            strata = reactive_strata()
          )
      }) |>
      shiny::debounce(1000)

    output$preview_splits <- shiny::renderPrint({
      reactive_split()
    })

    reactive_resamples <-
      shiny::reactive({
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

    shiny::observe({
      shiny::showModal(
        shiny::modalDialog(
          title = "Save Split + Resampling Configuration",
          shiny::textInput(
            ns("split_config_name"),
            "Split Configuration Name",
            value = "split1"
          ),
          footer = shiny::tagList(
            shiny::modalButton("Cancel"),
            shiny::actionButton(
              ns("confirm_save_button"),
              "Save Configuration",
              icon = shiny::icon("save")
            )
          )
        )
      )
    }) |>
      shiny::bindEvent(input$save_split_button)

    shiny::observe({
      shiny::removeModal()
      saved_split_configs[[input$split_config_name]] <- list(
        split_prop = input$split_prop,
        folds = input$folds,
        strata = input$strata,
        repeats = input$repeats,
        seed = input$seed
      )
    }) |>
      shiny::bindEvent(input$confirm_save_button)


    list(
      reactive_split = reactive_split,
      reactive_resamples = reactive_resamples
    )
  })
}

## To be copied in the UI
# mod_data_split_ui("data_split_1")

## To be copied in the server
# mod_data_split_server("data_split_1")
