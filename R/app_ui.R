#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @import bslib
#' @noRd
app_ui <- function(request) {
  # tagList(
  # Leave this function for adding external resources
  # golem_add_external_resources()
  # Your application UI logic
  page_navbar(
    title = "Shiny Experiments",

    # golem_add_external_resources(),
    id = "nav",
    sidebar = sidebar(
      conditionalPanel(
        "input[['nav']] == 'Build'",
        navset_underline(
          nav_panel(
            "Split",
            mod_data_split_ui("mod_data_split")
          ),
          nav_panel(
            "Preprocess",
            mod_preproc_ui("mod_preproc")
          ),
          nav_panel(
            "Model",
            mod_modeling_ui("mod_modeling")
          ),
          nav_panel(
            "Tune",
            mod_tune_config_ui("mod_tune_config")
          ),
          nav_panel(
            "Evaluate",
            mod_metrics_ui("mod_metrics")
          )
        ),
      ),
      conditionalPanel(
        "input[['nav']] == 'Experiment'",
        navset_underline(
          nav_panel(
            "Run",
            mod_experiment_select_ui("mod_workflow_select")
          )
        )
      ),
      width = "35%"
    ),
    nav_panel(
      "Build",
      icon = icon("screwdriver-wrench"),
      class = "bslib-page-dashboard",
      navset_underline(
        nav_panel(
          "Raw Training Data",
          mod_data_exploration_ui("mod_data_exploration_raw")
        ),
        nav_panel(
          "Preprocessed Data",
          mod_data_exploration_ui("mod_data_exploration_preproc")
        ),
        nav_panel(
          "Saved Objects",
          mod_saved_objects_ui("mod_saved_objects")
        )
      )
    ),
    nav_panel(
      "Experiment",
      icon = icon("flask"),
      class = "bslib-page-dashboard",
      navset_underline(
        nav_panel(
          "Current Results",
          mod_current_results_ui("mod_current_results")
        ),
        nav_panel(
          "All Results",
          mod_all_results_ui("mod_all_results")
        )
      )
    ),
    nav_spacer(),
    nav_item(
      input_dark_mode(mode = "light")
    )
  )
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www",
    app_sys("app/www")
  )

  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "shinyexperiments"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
