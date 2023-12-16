#' data_import UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_data_import_ui <- function(id) {
  ns <- NS(id)
  box::use(modeldata)
  tagList(
    shiny::selectInput(
      ns("dataset"),
      "Choose a dataset",
      choices = ls(modeldata)[ls(modeldata) != "%>%"],
      selected = "penguins"
    ),
    shiny::verbatimTextOutput(ns("raw_data_glimpse"))
  )
}

#' data_import Server Functions
#'
#' @noRd
mod_data_import_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    box::use(modeldata)
    dataset_choice <-
      shiny::reactive({
        modeldata[[input$dataset]]
      })

    output$raw_data_glimpse <- shiny::renderPrint({
      dplyr::glimpse(dataset_choice())
    })

    dataset_choice
  })
}

## To be copied in the UI
# mod_data_import_ui("data_import_1")

## To be copied in the server
# mod_data_import_server("data_import_1")
