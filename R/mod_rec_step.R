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
    # shiny::uiOutput(ns("custom_predictors_ui")),
    # shiny::selectInput(
    #   ns("step_predictors"),
    #   "Predictors",
    #   multiple = T,
    #   choices = NULL
    # ),
    # shiny::radioButtons(
    #   ns("step_predictors"),
    #   choices = c("All", "Numeric", "Nominal")
    # ),
    shiny::uiOutput(ns("step_ui"))
  )
}

#' rec_step Server Functions
#'
#' @noRd
mod_rec_step_server <- function(id, step_num, data, pred_cols) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # output$custom_predictors_ui <- shiny::renderUI({
    #   shiny::req(input$step_predictors)
    #   if ("Custom" %in% input$step_predictors) {
    #     shiny::textInput(
    #       ns("custom_predictors"),
    #       "Predictors"
    #     )
    #   }
    # })
    # output$step_ui <- shiny::renderUI({
    #   switch(input$step_select,
    #     "step_normalize" = shiny::numericInput(ns("norm_input"), "Normalize", 0.5),
    #     "step_log" = shiny::numericInput(ns("log_input"), "Log Transform", 0.5)
    #   )
    # })

    # preset_cols <-
    #   shiny::reactive({
    #
    #   })


    # preset_cols <-
    #   shiny::reactive({
    #     shiny::req(input$step_select, data())
    #     cols <- pred_cols[[as.character(step_num - 1)]]
    #     rec_data <- data() |> dplyr::select(dplyr::any_of(cols))
    #     step_select <- input$step_select
    #     dplyr::case_when(
    #       step_select %in% c("step_normalize", "step_log") ~ list(colnames(purrr::keep(rec_data, is.numeric))),
    #       stringr::str_detect(step_select, "impute") ~ list(colnames(purrr::keep(rec_data, ~ is.numeric(.x) & sum(is.na(.x)) > 0)))
    #     ) |>
    #       unlist()
    #   })

    # shiny::observe({
    #   if (!is.null(preset_cols())) {
    #     shiny::freezeReactiveValue(input, "step_predictors")
    #
    #     shiny::updateSelectInput(
    #       session,
    #       "step_predictors",
    #       choices = preset_cols(),
    #       selected = preset_cols()
    #     )
    #   }
    # })


    list(
      func = shiny::reactive(shiny::req(input$step_select)),
      vars = shiny::reactive(c(input$step_predictors, input$custom_predictors))
    )
  })
}

## To be copied in the UI
# mod_rec_step_ui("rec_step_1")

## To be copied in the server
# mod_rec_step_server("rec_step_1")
