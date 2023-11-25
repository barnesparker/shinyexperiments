box::use(
  sh = shiny[moduleServer, NS],
  bs = bslib,
  yardstick,
  dials
)

#' @export
ui <- function(id) {
  ns <- NS(id)
  bs$accordion(
    id = ns("metrics_accordion"),
    bs$accordion_panel(
      "Metrics"
    )
  )
}

#' @export
server <- function(id) {
  moduleServer(id, function(input, output, session) {

  })
}
