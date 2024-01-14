#' results UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_current_results_ui <- function(id) {
  ns <- NS(id)
  tagList(
    waiter::useWaiter(),
    bslib::card(
      id = "results_card",
      DT::DTOutput(ns("results_table")),
      full_screen = T,
    ),
    bslib::card(
      id = "plot_card",
      shiny::plotOutput(ns("results_autoplot")),
      full_screen = T
    )
  )
}

#' results Server Functions
#'
#' @noRd
mod_current_results_server <- function(id, raw_data, exp_button, selected_experiment, selected_exp_id, exp_id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    exp_board <- get_exp_board()
    split_config <-
      shiny::reactive({
        exp_board |>
          pins::pin_read(selected_experiment()$Splits)
      })

    reactive_split <-
      shiny::reactive({
        build_splits_from_config(split_config(), raw_data())
      })



    resampling_folds <- shiny::reactive({
      build_resamples_from_config(split_config(), reactive_split())
    })


    w <- waiter::Waiter$new(id = c("results_card", "plot_card"))

    results_id <-
      shiny::reactive({
        get_results_from_exp(shiny::req(selected_exp_id()))
      })

    existing_results <-
      shiny::reactive({
        shiny::req(results_id())
        # shiny::req(length(results_id()) > 0)
        if (length(results_id()) > 0 && results_id() %in% pins::pin_list(exp_board)) {
          exp_board |>
            pins::pin_read(results_id())
        } else {
          NULL
        }
      })

    map_results <- shiny::reactiveVal()

    shiny::observe({
      map_results(existing_results())
    })
      # shiny::bindEvent(existing_results())

    # map_results <-
    shiny::observe({
      w$show()
      map_results(
        workflowsets::workflow_map(
          selected_experiment()$wflow_set,
          fn = "tune_grid",
          resamples = resampling_folds(),
          metrics = pins::pin_read(exp_board, selected_experiment()$Metrics),
          seed = split_config()$seed
        )
      )
      w$hide()
    }) |>
      shiny::bindEvent(exp_button())

    # shiny::observe({
    #   map_results(NULL)
    # }) |>
    #   bindEvent(selected_exp_id(), prio)

    results_wide <-
      shiny::reactive({
        workflowsets::collect_metrics(shiny::req(map_results())) |>
          tidyr::pivot_wider(
            id_cols = -std_err,
            names_from = ".metric",
            values_from = "mean"
          ) |>
          dplyr::mutate(
            experiment = clean_object_names(selected_exp_id()),
            .before = 1
          )
      }) |>
      shiny::bindEvent(map_results())



    output$results_table <- DT::renderDT({
      results_wide()
    })

    output$results_autoplot <- shiny::renderPlot({
      shiny::req(map_results()) |>
        workflowsets::autoplot()
    })

    shiny::observe({

      if (length(results_id()) == 0) {
        save_exp_config(
          map_results(),
          exp_id(),
          clean_object_names(selected_exp_id()),
          config_type = "results"
        )
      }
    }) |>
      shiny::bindEvent(map_results())


    results_wide
  })
}

## To be copied in the UI
# mod_current_results_ui("results_1")

## To be copied in the server
# mod_current_results_server("results_1")
