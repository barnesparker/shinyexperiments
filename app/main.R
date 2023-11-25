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
  app/view/mod_train_config,
  app/view/mod_data_exploration,
  app/view/mod_data_import
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
          "Train",
          mod_train_config$ui(ns("mod_train_config"))
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
          "Workflow Preview",
          sh$verbatimTextOutput(ns("workflow_preview"))
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


    reactive_recipe <- mod_preproc$server("mod_preproc", reactive_training)


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

    reactive_model_spec <- mod_modeling$server("mod_modeling", reactive_mode)

    reactive_train_config <- mod_train_config$server(
      "mod_train_config", reactive_mode,
      reactive_model_spec
    )

    reactive_workflow <-
      sh$reactive({
        wf <- workflows$workflow()

        if (sh$isTruthy(reactive_recipe())) {
          wf <- wf |>
            workflows$add_recipe(reactive_recipe())
        }

        if (sh$isTruthy(reactive_model_spec())) {
          wf <- wf |>
            workflows$add_model(reactive_model_spec())
        }
        wf
      })

    # sh$observe({
    # browser()

    # withCallingHandlers(
    #   sh$req(reactive_workflow()),
    #   message = function(m) output$workflow_preview <- sh$renderPrint(m$message)
    # )
    # })

    output$workflow_preview <- sh$renderPrint({
      sh$req(reactive_workflow())
    })
  })
}
