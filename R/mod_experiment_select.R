#' experiment_select UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_experiment_select_ui <- function(id) {
  ns <- NS(id)
  tagList(
    waiter::useWaiter(),
    # waiter::waiterOnBusy(),
    # waiter::autoWaiter(c()),
    shinyjs::useShinyjs(),
    shiny::selectInput(
      ns("exp_select"),
      "Select Experiment",
      choices = NULL
    ),
    bslib::card(
      shiny::verbatimTextOutput(
        ns("exp_info"),
      ),
      full_screen = T,
      max_height = 500
    ),
    # bslib::layout_columns(
    shiny::actionButton(
      ns("run_exp_button"),
      "Run Experiment"
    )
    # mod_save_object_dialog_ui(
    #   ns("save_object_dialog_experiment"),
    #   "Save Results"
    # )
    # )
  )
}

#' experiment_select Server Functions
#'
#' @noRd
mod_experiment_select_server <- function(id, exp_id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    exp_board <- get_exp_board()

    wflow_set_names <-
      shiny::reactivePoll(1000, session,
        checkFunc = function() {
          if (is.null(exp_id())) {
            return(NULL)
          }
          get_saved_objects_list(exp_board, "experiment", exp_id()) |>
            length()
        },
        valueFunc = function() {
          if (is.null(exp_id())) {
            return(NULL)
          }
          get_saved_objects_list(exp_board, "experiment", exp_id())
        }
      )


    shiny::observe({
      shiny::updateSelectInput(
        session,
        "exp_select",
        choices = shiny::req(wflow_set_names()) |>
          setNames(clean_object_names(wflow_set_names()))
      )
    })

    selected_exp <-
      shiny::reactive({
        exp_board |>
          pins::pin_read(shiny::req(input$exp_select))
      })



    output$exp_info <- shiny::renderPrint({
      print(selected_exp())
    })


    # save_exp_enable_var <- shiny::reactiveVal(F)

    # shiny::observe({
    #   if (is.null(map_results())) {
    #     save_exp_enable_var(F)
    #   } else {
    #     save_exp_enable_var(T)
    #     shinyjs::enable("save_object_dialog_experiment-save_results_button")
    #   }
    # })

    # mod_save_object_dialog_server(
    #   "save_object_dialog_experiment",
    #   "Results",
    #   metrics_wide,
    #   exp_id,
    #   clean_object_names(input$exp_select),
    #   save_exp_enable_var
    # )


    list(
      selected_exp = selected_exp,
      exp_button = shiny::reactive(input$run_exp_button),
      selected_exp_id = shiny::reactive(input$exp_select)
      # results = map_results,
      # metrics_wide = metrics_wide
    )
  })
}
