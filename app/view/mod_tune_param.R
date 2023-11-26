box::use(
  sh = shiny[moduleServer, NS],
  bs = bslib,
  sw = shinyWidgets
)

#' @export
ui <- function(id, param, grid_type, default_param_vals) {
  ns <- NS(id)

  sh$tagList(
    if (is.numeric(default_param_vals)) {
      bs$layout_columns(
        sw$numericRangeInput(
          ns("param_range"),
          label = param,
          value = default_param_vals
        ),
        if (grid_type == "grid_regular") {
          sh$numericInput(
            ns("param_levels"),
            "Levels",
            value = 3
          )
        }
      )
    } else if (is.character(default_param_vals)) {
      sh$selectInput(
        ns("param_select"),
        label = param,
        choices = default_param_vals,
        selected = default_param_vals,
        multiple = T
      )
    }
  )
}

#' @export
server <- function(id) {
  moduleServer(id, function(input, output, session) {
    levels <-
      sh$reactive({
        if (!is.null(input$param_select)) {
          length(input$param_select)
        } else {
          input$param_levels
        }
      })



    list(
      range = sh$reactive(input$param_range),
      trans = sh$reactive(input$param_trans),
      levels = levels,
      values = sh$reactive(input$param_select)
    )
  })
}
