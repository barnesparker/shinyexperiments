box::use(
  sh = shiny[moduleServer, NS],
  bs = bslib,
  workflowsets
)

#' @export
ui <- function(id) {
  ns <- NS(id)
  sh$tagList(
    sh$verbatimTextOutput(ns("results_print")),
    bs$card(
      sh$plotOutput(ns("results_autoplot"))
    )
  )

}

#' @export
server <- function(id, results) {
  moduleServer(id, function(input, output, session) {

    output$results_print <- sh$renderPrint({
      results()
    })

    output$results_autoplot <- sh$renderPlot({
      results() |>
        workflowsets$autoplot()
    })

  })
}
