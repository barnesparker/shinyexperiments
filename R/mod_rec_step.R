#' rec_step UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_rec_step_ui <- function(id, step_num) {
  ns <- NS(id)
  tagList(
    shiny::selectInput(
      ns("step_select"),
      "Step Type",
      choices = c(
        "Normalize" = "step_normalize",
        "Log Transform" = "step_log",
        "Dummy Encode" = "step_dummy",
        "Impute Mean" = "step_impute_mean",
        "Impute Median" = "step_impute_median",
        "Impute Mode" = "step_impute_mode",
        "Impute Linear" = "step_impute_linear"
      ),
      selected = "step_normalize"
    ),
    shiny::selectizeInput(
      ns("step_predictors"),
      "Predictors",
      multiple = T,
      choices = c(
        "All Numeric Predictors" = "all_numeric_predictors()",
        "All Nominal Predictors" = "all_nominal_predictors()",
        "All Factor Predictors" = "all_factor_predictors()",
        "All Date Predictors" = "all_date_predictors()",
        "All Logical Predictors" = "all_logical_predictors()",
        "All Predictors" = "all_predictors()",
        "All Outcomes" = "all_outcomes()"
      ),
      options = list(
        create = T,
        plugins = list("remove_button")
      )
    ),
    shiny::uiOutput(ns("step_ui"))
  )
}

#' rec_step Server Functions
#'
#' @noRd
mod_rec_step_server <- function(id, step_num, data, pred_cols) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    list(
      func = shiny::reactive(shiny::req(input$step_select)),
      vars = shiny::reactive(c(input$step_predictors, input$custom_predictors))
    )
  })
}
