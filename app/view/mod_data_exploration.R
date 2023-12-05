box::use(
  sh = shiny[moduleServer, NS],
  DE = DataExplorer,
  rt = reactable,
  bs = bslib,
  tidyr,
  gg = ggplot2,
  thematic,
  DT
)

#' @export
ui <- function(id) {
  ns <- NS(id)
  sh$tagList(
    bs$card(
      # rt$reactableOutput(ns("data_gt")),
      DT$DTOutput(ns("data_table")),
      full_screen = T
    ),
    bs$layout_columns(
      bs$card(
        sh$plotOutput(ns("missing_plot")),
        full_screen = T
      ),
      bs$card(
        sh$plotOutput(ns("corr_plot")),
        full_screen = T
      )
    )
  )
}

#' @export
server <- function(id, reactive_training) {
  moduleServer(id, function(input, output, session) {


    # thematic$thematic_shiny()

    output$data_table <-
      DT$renderDT({
        DT$datatable(
          reactive_training()
        )
      })

    output$missing_plot <- sh$renderPlot({
      reactive_training() |>
        DE$plot_missing() +
        gg$theme_minimal() +
        gg$theme(
          axis.text.y = gg$element_text(size = 10, color = "white"),
        )
    })

    output$corr_plot <- sh$renderPlot({
      reactive_training() |>
        tidyr$drop_na() |>
        DE$plot_correlation() +
        gg$theme_minimal() +
        gg$theme(
          axis.text.y = gg$element_text(size = 10),
        )
    })
  })
}
