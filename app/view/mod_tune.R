box::use(
  sh = shiny[moduleServer, NS],
  bs = bslib,
  tune
)

#' @export
ui <- function(id) {
  ns <- NS(id)
  bs$accordion(
    id = ns("tune_accordion"),
    bs$accordion_panel(
      "Tune"
    )
  )
}

#' @export
server <- function(id, model_spec) {
  moduleServer(id, function(input, output, session) {

  })
}
