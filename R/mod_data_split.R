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
  )
}

#' data_split Server Functions
#'
#' @noRd
mod_data_split_server <- function(id, raw_data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    output$strata_ui <- shiny::renderUI({
      shiny::selectInput(
        ns("strata"),
        "Strata",
        choices = c(colnames(raw_data()), "None"),
        selected = "None"
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
