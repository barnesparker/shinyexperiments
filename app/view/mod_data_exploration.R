box::use(
  sh = shiny[moduleServer, NS],
  visdat,
  DE = DataExplorer,
  gt,
  bs = bslib,
  tidyr,
  plotly,
  gg = ggplot2
)

#' @export
ui <- function(id) {
  ns <- NS(id)
  sh$tagList(
    gt$gt_output(ns("data_gt")),
    bs$layout_columns(
      sh$plotOutput(ns("missing_plot")),
      sh$plotOutput(ns("corr_plot"))
    )
  )
}

#' @export
server <- function(id, reactive_training) {
  moduleServer(id, function(input, output, session) {
    output$data_gt <-
      gt$render_gt({
        gt$gt(reactive_training()) |>
          gt$opt_interactive(
            use_search = T
          )
      })

    output$missing_plot <- sh$renderPlot({
      reactive_training() |>
        DE$plot_missing()
    })

    output$corr_plot <- sh$renderPlot({
      reactive_training() |>
        tidyr$drop_na() |>
        DE$plot_correlation(theme_config = list(
          axis.text.x = gg$element_text(angle = 90),
          legend.position = "none"))
    })
  })
}
