box::use(
  sh = shiny[moduleServer, NS],
  bs = bslib,
  shjs = shinyjs,
  tune
)

#' @export
ui <- function(id, arg) {
  ns <- NS(id)
  bs$layout_columns(
    shjs$useShinyjs(),
    shjs$disabled(sh$numericInput(
      ns("param_value"),
      arg,
      value = NULL
    )),
    sh$checkboxInput(
      ns("param_tune"),
      "Tune",
      value = T
    )
  )
}

#' @export
server <- function(id) {
  moduleServer(id, function(input, output, session) {
    sh$observe({

      sh$req(!is.null(input$param_tune))

      if (input$param_tune) {
        shjs$disable("param_value")
      } else {
        shjs$enable("param_value")
      }
    })

    param_value <- sh$reactive({
      sh$req(!is.null(input$param_tune))

      if (input$param_tune) {
        tune$tune()
      } else {
        input$param_value
      }
    })


    param_value
  })
}
