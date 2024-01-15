#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  # Your application server logic

  shiny::observe({
    shiny::showModal(
      mod_data_import_ui("mod_data_import")
    )
  })

  data_import <- mod_data_import_server("mod_data_import")

  reset_exp_id <- function() {
    golem::amend_golem_config("exp_id", NULL)
  }

  raw_data <- shiny::reactive({
    shiny::removeModal()
    golem::amend_golem_config("exp_id", exp_id())
    shiny::onStop(reset_exp_id)
    data_import$dataset()
  }) |>
    bindEvent(data_import$confirm_data_import_button())


  exp_id <- shiny::reactiveVal()

  shiny::observe({
    outcome_hash <- stringr::str_sub(rlang::hash(data_import$outcome()), 1, 5)
    exp_id(paste0(data_import$dataset_hash(), outcome_hash))
  })

  reactive_mode <-
    shiny::reactive({
      if (dplyr::n_distinct(reactive_training()[[data_import$outcome()]], na.rm = T) == 2) {
        "classification"
      } else {
        "regression"
      }
    })

  reactive_training <-
    reactive({
      data_splits$reactive_split() |>
        rsample::training()
    })



  data_splits <- mod_data_split_server(
    "mod_data_split", raw_data, data_import$outcome)



  mod_data_exploration_server("mod_data_exploration_raw", reactive_training)
  mod_data_exploration_server(
    "mod_data_exploration_preproc",
    req(reactive_recipe_prepped()) |>
      recipes::juice() |>
      reactive()
  )



  reactive_recipe_prepped <- mod_preproc_server(
    "mod_preproc", reactive_training, data_import$outcome)

  reactive_model_spec <- mod_modeling_server("mod_modeling", reactive_mode)

  mod_tune_config_server(
    "mod_tune_config",
    reactive_mode,
    reactive_model_spec,
    reactive_training
  )

mod_metrics_server(
  "mod_metrics",
  reactive_mode
)


  # mod_saved_objects_server("mod_saved_objects", reactive_training, exp_id)

  # exp_select <- mod_experiment_select_server("mod_workflow_select", exp_id)

  # results <- mod_current_results_server(
  #   "mod_current_results", raw_data,
  #   exp_select$exp_button, exp_select$selected_exp,
  #   exp_select$selected_exp_id,
  #   exp_id
  # )

  # mod_all_results_server("mod_all_results", results, exp_select$selected_exp_id, exp_id)
}
