#' data_exploration UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_data_exploration_ui <- function(id) {
  ns <- NS(id)
  tagList(
    bslib::card(
      # reactable::reactableOutput(ns("data_gt")),
      DT::DTOutput(ns("data_table")),
      full_screen = T
    ),
    bslib::layout_columns(
      bslib::card(
        bslib::layout_sidebar(
          shiny::plotOutput(ns("missing_plot")),
          sidebar = bslib::sidebar(
            bslib::input_switch(
              ns("missing_only_switch"),
              "Missing Only?",
              value = F
            ),
            open = "closed",
            position = "right"
          )
        ),
        full_screen = T
      ),
      bslib::card(
        shiny::plotOutput(ns("corr_plot")),
        full_screen = T
      )
    )
  )
}

#' data_exploration Server Functions
#'
#' @noRd
mod_data_exploration_server <- function(id, reactive_training) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # thematic$thematic_shiny()

    output$data_table <-
      DT::renderDT({
        reactive_training()
      })

    output$missing_plot <- shiny::renderPlot({
      reactive_training() |>
        DataExplorer::plot_missing(missing_only = input$missing_only_switch, ) +
        ggplot2::theme_minimal()
        # ggplot2::theme(
        #   axis.text.y = ggplot2::element_text(size = 10, color = "white"),
        # )
    })

    output$corr_plot <- shiny::renderPlot({
      reactive_training() |>
        tidyr::drop_na() |>
        DataExplorer::plot_correlation() +
        ggplot2::theme_minimal() +
        ggplot2::theme(
          axis.text.y = ggplot2::element_text(size = 10),
        )
    })
  })
}

## To be copied in the UI
# mod_data_exploration_ui("data_exploration_1")

## To be copied in the server
# mod_data_exploration_server("data_exploration_1")
