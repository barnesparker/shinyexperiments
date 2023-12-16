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



  reactive_training <-
    reactive({
      data_splits$reactive_split() |>
        rsample::training()
    })

  data_splits <- mod_data_split_server("mod_data_split", raw_data)





  mod_data_exploration_server("mod_data_exploration_raw", reactive_training)
  mod_data_exploration_server(
    "mod_data_exploration_preproc",
    req(reactive_recipe_prepped()) |>
      recipes::juice() |>
      reactive()
  )


  saved_recipes <- reactiveValues()

  reactive_recipe_prepped <- mod_preproc_server("mod_preproc", reactive_training, saved_recipes)

  reactive_mode <-
    reactive({
      # browser()
      outcome_type <-
        reactive_recipe_prepped()$var_info |>
        dplyr::filter(
          role == "outcome"
        ) |>
        dplyr::pull(type)
      if ("numeric" %in% outcome_type[[1]]) {
        "regression"
      } else {
        "classification"
      }
    })

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

  mod_saved_objects_server("mod_saved_objects", saved_recipes, saved_models, saved_tune_configs, saved_wflowsets)

  reactive_results <- mod_workflow_select_server("mod_workflow_select", saved_wflowsets, data_splits$reactive_resamples)

  mod_results_server("mod_results", reactive_results)
}
