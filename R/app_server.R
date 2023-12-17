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

  raw_data <- shiny::reactive({
    shiny::removeModal()
    data_import$dataset_choice()
  }) |>
    bindEvent(data_import$confirm_data_import_button())



  data_import <- mod_data_import_server("mod_data_import")


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

  saved_split_configs <- reactiveValues()


  data_splits <- mod_data_split_server("mod_data_split", raw_data, data_import$outcome, saved_split_configs)





  mod_data_exploration_server("mod_data_exploration_raw", reactive_training)
  mod_data_exploration_server(
    "mod_data_exploration_preproc",
    req(reactive_recipe_prepped()) |>
      recipes::juice() |>
      reactive()
  )


  saved_recipes <- reactiveValues()

  reactive_recipe_prepped <- mod_preproc_server("mod_preproc", reactive_training, saved_recipes, data_import$outcome)

  saved_models <- reactiveValues()

  reactive_model_spec <- mod_modeling_server("mod_modeling", reactive_mode, saved_models)

  saved_tune_configs <- reactiveValues()

  mod_tune_config_server(
    "mod_tune_config", reactive_mode,
    reactive_model_spec,
    saved_models,
    saved_tune_configs,
    reactive_training
  )

  saved_wflowsets <- reactiveValues()

  mod_saved_objects_server("mod_saved_objects", saved_split_configs, raw_data, saved_recipes, saved_models, saved_tune_configs, saved_wflowsets)

  reactive_results <- mod_workflow_select_server("mod_workflow_select", saved_wflowsets, data_splits$reactive_resamples)

  mod_results_server("mod_results", reactive_results)
}
