box::use(
  sh = shiny,
  bs = bslib,
  rsample
)

#' @export
ui <- function(id) {
  ns <- sh$NS(id)
  bs$accordion(
    open = T,
    bs$accordion_panel(
      "Initial Split",
      sh$sliderInput(
        ns("split_prop"),
        "Train/Test Split Proportion",
        value = .75,
        min = 0,
        max = 1
      ),
      sh$uiOutput(ns("strata_ui")),
      sh$verbatimTextOutput(ns("preview_splits"))
    ),
    bs$accordion_panel(
      "Resamples",
      bs$layout_columns(
        sh$numericInput(
          ns("folds"),
          "Folds",
          value = 10,
          min = 2
        ),
        sh$numericInput(
          ns("repeats"),
          "Repeats",
          value = 1,
          min = 1
        )
      ),
      sh$verbatimTextOutput(ns("preview_resamples"))
    ),
  )
}

#' @export
server <- function(id, raw_data) {
  sh$moduleServer(id, function(input, output, session) {
    ns <- session$ns

    output$strata_ui <- sh$renderUI({
      sh$selectInput(
        ns("strata"),
        "Strata",
        choices = c(colnames(raw_data), "None")
      )
    })

    reactive_split <-
      sh$reactive({
        raw_data |>
          rsample$initial_split(
            prop = sh$req(input$split_prop),
            strata = sh$req(input$strata)
          )
      })

    output$preview_splits <- sh$renderPrint({
      reactive_split()
    })

    reactive_resamples <-
      sh$reactive({
        rsample$training(reactive_split()) |>
          rsample$vfold_cv(
            v = sh$req(input$folds),
            repeats = sh$req(input$repeats),
            strata = sh$req(input$strata)
          )
      })

    output$preview_resamples <- sh$renderPrint({
      reactive_resamples()
    })

    list(
      reactive_split = reactive_split,
      reactive_resamples = reactive_resamples
    )
  })
}
