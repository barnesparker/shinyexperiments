box::use(
  sh = shiny,
  bs = bslib,
  bsicons,
  modeldata,
  gt,
  rsample,
  recipes,
  dp = dplyr,
  workflows
)

box::use(
  app/view/mod_preproc,
  app/view/mod_data_split,
  app/view/mod_modeling,
  app/view/mod_train_config
)

#' @export
ui <- function(id) {
  ns <- sh$NS(id)
  bs$page_navbar(
    title = "Shiny Experiments",
    sidebar = bs$sidebar(
      bs$navset_underline(
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
      "Build a Workflow",
      icon = sh$icon("screwdriver-wrench"),
      bs$navset_pill_list(
        bs$nav_panel(
          "Raw Data",
          gt$gt_output(ns("train_data_gt")),
        ),
        bs$nav_panel(
          "Preprocessed Data",
          gt$gt_output(ns("train_data_preprocessed_gt"))
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
    raw_data <- modeldata$penguins

    output$train_data_gt <-
      gt$render_gt({
        gt$gt(reactive_training()) |>
          gt$opt_interactive(
            use_search = T
          )
      })

    data_splits <- mod_data_split$server("mod_data_split", raw_data)

    reactive_training <-
      sh$reactive({
        data_splits$reactive_split() |>
          rsample$training()
      })

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
