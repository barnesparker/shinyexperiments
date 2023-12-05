box::use(
  sh = shiny,
  bs = bslib,
  rc = recipes,
  purrr,
  dp = dplyr,
  stringr
)

#' @export
ui <- function(id, step_num) {
  ns <- sh$NS(id)
  sh$tagList(
    # paste0("Step", step_num),
    sh$selectInput(
      ns("step_select"),
      "Step Type",
      choices = c(
        "Normalize" = "step_normalize",
        "Log Transform" = "step_log",
        "Impute Mean" = "step_impute_mean",
        "Impute Median" = "step_impute_median"
      ),
      selected = "step_normalize"
    ),
    sh$selectInput(
      ns("step_predictors"),
      "Predictors",
      multiple = T,
      choices = NULL
    ),
    # sh$radioButtons(
    #   ns("step_predictors"),
    #   choices = c("All", "Numeric", "Nominal")
    # ),
    sh$uiOutput(ns("step_ui"))
  )
}

#' @export
server <- function(id, step_num, data) {
  sh$moduleServer(id, function(input, output, session) {
    ns <- sh$NS(id)

    rec_args <- sh$reactiveValues()

    # output$step_ui <- sh$renderUI({
    #   switch(input$step_select,
    #     "step_normalize" = sh$numericInput(ns("norm_input"), "Normalize", 0.5),
    #     "step_log" = sh$numericInput(ns("log_input"), "Log Transform", 0.5)
    #   )
    # })



    preset_cols <-
      sh$reactive({
        sh$req(input$step_select, data())
        # browser()
        step_select <- input$step_select
        dp$case_when(
          step_select %in% c("step_normalize", "step_log") ~ list(colnames(purrr$keep(data(), is.numeric))),
          stringr$str_detect(step_select, "impute") ~ list(colnames(purrr$keep(data(), ~ is.numeric(.x) & sum(is.na(.x)) > 0)))
        ) |>
          unlist()
      })

    sh$observe({
      if (!is.null(preset_cols())) {
        sh$freezeReactiveValue(input, "step_predictors")

        sh$updateSelectInput(
          session,
          "step_predictors",
          choices = preset_cols(),
          selected = preset_cols()
        )
      }
    })


    list(
      func = sh$reactive(sh$req(input$step_select)),
      vars = sh$reactive(sh$req(input$step_predictors))
    )
  })
}
