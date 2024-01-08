#' object_table UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_object_table_ui <- function(id) {
  ns <- NS(id)
  tagList(
    reactable::reactableOutput(ns("object_rt"))
  )
}

#' object_table Server Functions
#'
#' @noRd
mod_object_table_server <- function(id, object_type, exp_id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    exp_board <- get_exp_board()

    saved_objects <-
      shiny::reactivePoll(1000, session,
        checkFunc = function() {
          if (is.null(exp_id())) {
            return(NULL)
          }
          get_saved_objects_list(exp_board, object_type, shiny::req(exp_id())) |>
            length()
        },
        valueFunc = function() {
          get_saved_objects_list(exp_board, object_type, shiny::req(exp_id()))
        }
      )

    output$object_rt <- reactable::renderReactable({
      tbl_name <- switch(object_type,
        split = "Splits",
        preproc = "Preprocessors",
        model = "Models",
        metrics = "Metrics",
        experiment = "Experiments"
      )

      object_metas <-
        saved_objects() |>
        purrr::map(
          ~ exp_board |>
            pins::pin_meta(.x)
        )

      object_tbl <-
        c("title", "description", "tags") |>
        purrr::map(
          \(meta) {
            object_metas |>
              purrr::map_chr(
                \(x) {
                  if (meta == "tags") {
                    stringr::str_flatten_comma(x[[meta]])
                  } else {
                    x[[meta]]
                  }
                }
              )
          }
        ) |>
        purrr::set_names(c(tbl_name, "description", "tags")) |>
        tibble::as_tibble()

      if (object_type == "experiment") {
        object_tbl <-
          object_tbl |>
          dplyr::mutate(
            "Has Results" = purrr::map_lgl(
              saved_objects(),
              ~ length(get_results_from_exp(.x)) > 0
            )
          )
      }
      reactable::reactable(
        object_tbl,
        selection = "multiple",
        onClick = "select",
        details = function(index) {
          object_id <- saved_objects()[index]

          clicked_object <-
            exp_board |>
            pins::pin_read(object_id)

          object_meta <-
            exp_board |>
            pins::pin_meta(object_id)

          tune_grid <- NULL

          if (object_type == "model") {
            tune_grid_name <- stringr::str_replace(object_id, "_model_", "_tune_")
            exp_board <- get_exp_board()
            if (tune_grid_name %in% pins::pin_list(exp_board)) {
              tune_grid <- pins::pin_read(exp_board, tune_grid_name)
            }
            msg <- paste(capture.output(
              print(clicked_object), cat("Tune Grid\n"), tune_grid
            ), collapse = "\n")
          } else {
            msg <- paste(capture.output(
              print(clicked_object)
            ), collapse = "\n")
          }


          shiny::div(shiny::tags$pre(msg))
        }
      )
    })

    selected_object_names <- shiny::reactive({
      object_names <- saved_objects()[reactable::getReactableState("object_rt", "selected", session = session)]
      object_names[!is.na(object_names)]
    })



    shiny::reactive({
      selected_object_names() |>
        purrr::map(
          ~ exp_board |>
            pins::pin_read(.x)
        ) |>
        setNames(selected_object_names())
    })
  })
}

## To be copied in the UI
# mod_object_table_ui("object_table_1")

## To be copied in the server
# mod_object_table_server("object_table_1")
