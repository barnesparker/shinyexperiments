box::use(
  sh = shiny,
  bs = bslib,
  bsicons,
  gt,
  rsample,
  recipes,
  dp = dplyr,
  workflows,
  visdat,
  DE = DataExplorer
)

box::use(
  app/view/mod_preproc,
  app/view/mod_data_split,
  app/view/mod_modeling,
  app/view/mod_tune_config,
  app/view/mod_data_exploration,
  app/view/mod_data_import,
  app/view/mod_saved_objects
)

#' @export
ui <- function(id) {
  ns <- sh$NS(id)
  bs$page_navbar(
    title = "Shiny Experiments",
    sidebar = bs$sidebar(
      bs$navset_underline(
        bs$nav_panel(
          "Import",
          mod_data_import$ui(ns("mod_data_import"))
        ),
        bs$nav_panel(
          "Split",
          mod_data_split$ui(ns("mod_data_split"))
        ),
        bs$nav_panel(
          "Preprocess",
          mod_preproc$ui(ns("mod_preproc"))
        ),
        bs$nav_panel(
          "Model",
          mod_modeling$ui(ns("mod_modeling"))
        ),
        bs$nav_panel(
          "Tune",
          mod_tune_config$ui(ns("mod_tune_config"))
        )
      ),
      width = "35%"
    ),
    # bs$input_dark_mode(mode = "light"),
    bs$nav_panel(
      "Workflow Builder",
      icon = sh$icon("screwdriver-wrench"),
      bs$navset_underline(
        bs$nav_panel(
          "Raw Training Data",
          mod_data_exploration$ui(ns("mod_data_exploration_raw"))
        ),
        bs$nav_panel(
          "Preprocessed Data",
          mod_data_exploration$ui(ns("mod_data_exploration_preproc"))
        ),
        bs$nav_panel(
          "Saved Objects",
          mod_saved_objects$ui(ns("mod_saved_objects"))
        )
      )
    ),
    bs$nav_panel(
      "Experiment",
      icon = sh$icon("flask")
    )
  )
}

#' @export
server <- function(id) {
  sh$moduleServer(id, function(input, output, session) {
    raw_data <- mod_data_import$server("mod_data_import")



    reactive_training <-
      sh$reactive({
        data_splits$reactive_split() |>
          rsample$training()
      })

    data_splits <- mod_data_split$server("mod_data_split", raw_data)





    mod_data_exploration$server("mod_data_exploration_raw", reactive_training)
    mod_data_exploration$server("mod_data_exploration_preproc", sh$req(reactive_recipe()) |>
      recipes$prep() |>
      recipes$juice() |> sh$reactive())


    saved_recipes <- sh$reactiveValues()

    reactive_recipe <- mod_preproc$server("mod_preproc", reactive_training, saved_recipes)


    output$train_data_preprocessed_gt <-
      gt$render_gt({
        # reactive_training() |>
        sh$req(reactive_recipe()) |>
          recipes$prep() |>
          recipes$juice() |>
          gt$gt() |>
          gt$opt_interactive(
            use_search = T
          )
      })

    reactive_mode <-
      sh$reactive({
        outcome_type <-
          reactive_recipe()$var_info |>
          dp$filter(
            role == "outcome"
          ) |>
          dp$pull(type)
        if ("numeric" %in% outcome_type[[1]]) {
          "regression"
        } else {
          "classification"
        }
      })

    saved_models <- sh$reactiveValues()

    reactive_model_spec <- mod_modeling$server("mod_modeling", reactive_mode, saved_models)


    saved_tune_configs <- sh$reactiveValues()

    mod_tune_config$server(
      "mod_tune_config", reactive_mode,
      reactive_model_spec,
      saved_models,
      saved_tune_configs,
      reactive_training
    )

    mod_saved_objects$server("mod_saved_objects", saved_recipes, saved_models, saved_tune_configs)

  })
}
