box::use(
  sh = shiny[moduleServer, NS],
  bs = bslib,
  modeldata,
  dp = dplyr
)

#' @export
ui <- function(id) {
  ns <- NS(id)
  sh$tagList(
    sh$selectInput(
      ns("dataset"),
      "Choose a dataset",
      choices = ls(modeldata)[ls(modeldata) != "%>%"],
      selected = "penguins"
    ),
    sh$verbatimTextOutput(ns("raw_data_glimpse"))
  )
}

#' @export
server <- function(id) {
  moduleServer(id, function(input, output, session) {


    dataset_choice <-
      sh$reactive({
        modeldata[[input$dataset]]
      })

    output$raw_data_glimpse <- sh$renderPrint({
      dp$glimpse(dataset_choice())
    })





    dataset_choice
  })

}
