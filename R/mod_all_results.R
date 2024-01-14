#' all_results UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_all_results_ui <- function(id) {
  ns <- NS(id)
  tagList(
    bslib::card(
      DT::DTOutput(ns("all_results_table")),
      full_screen = T,
    )
  )
}

#' all_results Server Functions
#'
#' @noRd
mod_all_results_server <- function(id, current_results, cur_exp, exp_id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    exp_board <- get_exp_board()

    cur_results_reactive_val <- shiny::reactiveVal()

    shiny::observe({
      cur_results_reactive_val(
        shiny::req(current_results())
      )
    })

    all_results <-
      shiny::reactivePoll(1000, session,
        checkFunc = function() {
          if (is.null(exp_id())) {
            return(NULL)
          }
          exp_board |>
            get_saved_objects_list("results", exp_id()) |>
            length()
        },
        valueFunc = function() {
          results_list <- get_saved_objects_list(exp_board, "results", shiny::req(exp_id()))
          # shiny::req(results_list)
          out <-
            results_list |>
            purrr::map(
              ~ pins::pin_read(exp_board, .x) |>
                workflowsets::collect_metrics() |>
                dplyr::mutate(
                  experiment = clean_object_names(.x),
                  .before = 1
                )
            ) |>
            purrr::list_rbind() |>
            tidyr::pivot_wider(
              id_cols = -std_err,
              names_from = ".metric",
              values_from = "mean"
            )

          if (!is.null(cur_results_reactive_val())) {
            out <-
              dplyr::bind_rows(
                cur_results_reactive_val(),
                out
              )
          }
          out |>
            dplyr::distinct()
        }
      )

    output$all_results_table <-
      DT::renderDT({
        df <- as.data.frame(cbind(matrix(round(rnorm(50), 3), 10), sample(0:1, 10, TRUE)))

        DT::datatable(
          all_results()
        ) |>
          DT::formatStyle(
            "experiment",
            target = "cell",
            backgroundColor = DT::styleEqual(shiny::req(clean_object_names(cur_exp())),
              values = c("lightblue")
            )
          )
      })
  })
}

## To be copied in the UI
# mod_all_results_ui("all_results_1")

## To be copied in the server
# mod_all_results_server("all_results_1")
