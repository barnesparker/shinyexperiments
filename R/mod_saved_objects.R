#' saved_objects UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_saved_objects_ui <- function(id) {
  ns <- NS(id)
  tagList(
    shinyjs::useShinyjs(),
    mod_object_table_ui(ns("object_table_split")),
    mod_object_table_ui(ns("object_table_preproc")),
    mod_object_table_ui(ns("object_table_model")),
    mod_object_table_ui(ns("object_table_metrics")),
    bslib::layout_columns(
      shiny::actionButton(ns("create_exp_button"), "Create Experiment",
        icon = shiny::icon("bolt")
      ),
      shiny::actionButton(ns("delete_object_button"), "Delete Object(s)",
        icon = shiny::icon("trash")
      )
    ),
    mod_object_table_ui(ns("object_table_exp")),
  )
}

#' saved_objects Server Functions
#'
#' @noRd
mod_saved_objects_server <- function(id, train_data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    exp_board <- get_exp_board()

    selected_splits <- mod_object_table_server(
      "object_table_split", "split"
    )

    selected_recipes <- mod_object_table_server(
      "object_table_preproc", "preproc"
    )
    selected_models <- mod_object_table_server(
      "object_table_model", "model"
    )

    selected_metrics <- mod_object_table_server(
      "object_table_metrics", "metrics"
    )

    selected_exps <- mod_object_table_server(
      "object_table_exp", "experiment"
    )




    shiny::observe({
      shiny::showModal(
        shiny::modalDialog(
          title = "Create Experiment",
          shiny::HTML("You will create an experiment with the following objects:<br><br>"),
          print_selected_objects(
            selected_splits(), selected_recipes(), selected_models(),
            selected_metrics()
          ),
          shiny::HTML("<br><br>"),
          bslib::layout_columns(
            shiny::textInput(ns("exp_name"), "Experiment Name", value = "experiment1"),
            shiny::checkboxInput(ns("cross_checkbox"), "Cross?", value = T)
          ),
          shiny::textAreaInput(
            ns("exp_description"),
            "Description (optional)"
          ),
          shiny::selectizeInput(
            ns("exp_tags"),
            "Add Tags",
            choices = get_exp_tags(),
            multiple = T,
            selected = NULL,
            options = list(
              plugins = list("remove_button"),
              create = T
            )
          ),
          footer = shiny::tagList(
            shiny::modalButton("Cancel"),
            shiny::actionButton(ns("confirm_create_exp"), "Create Experiment")
          )
        )
      )
    }) |>
      shiny::bindEvent(input$create_exp_button)

    shiny::observe({
      shiny::showModal(
        shiny::modalDialog(
          title = "Delete Selected Objects",
          shiny::HTML("Are you sure you want to delete the following objects?:<br><br>"),
          print_selected_objects(
            selected_splits(), selected_recipes(), selected_models(), selected_metrics(),
            selected_exps()
          ),
          footer = shiny::tagList(
            shiny::modalButton("Cancel"),
            shiny::actionButton(ns("confirm_delete_button"), "Delete", icon = shiny::icon("trash"))
          )
        )
      )
    }) |>
      shiny::bindEvent(input$delete_object_button)

    shiny::observe({
      shiny::removeModal()

      pins_to_delete <- c(
        names(selected_recipes()), names(selected_models()),
        names(selected_splits()), names(selected_exps()),
        names(selected_metrics()),
        get_tune_grids_from_models(names(selected_models())),
        get_results_from_exp(names(selected_exps()))
      )
      pins::pin_delete(exp_board, names = pins_to_delete)
    }) |>
      shiny::bindEvent(input$confirm_delete_button)


    shiny::observe({
      if (
        any(
          length(selected_recipes()) == 0,
          length(selected_models()) == 0,
          # length(selected_metrics())
          length(selected_splits()) != 1
        )
      ) {
        shinyjs::disable("create_exp_button")
      } else {
        shinyjs::enable("create_exp_button")
      }
    })

    shiny::observe({
      if (
        any(
          length(selected_recipes()) > 0,
          length(selected_models()) > 0,
          length(selected_splits()) > 0,
          length(selected_metrics()),
          length(selected_exps()) > 0
        )) {
        shinyjs::enable("delete_object_button")
      } else {
        shinyjs::disable("delete_object_button")
      }
    })

    reactive_exp <- shiny::reactive({
      wflow_set <-
        workflowsets::workflow_set(
          preproc = selected_recipes() |> setNames(clean_object_names(names(selected_recipes()))),
          models = selected_models() |> setNames(clean_object_names(names(selected_models()))),
          cross = input$cross_checkbox
        ) |>
        workflowsets::option_add(
          grid = 20
        )

      exp_board <- get_exp_board()
      saved_tune_configs <- get_saved_objects_list(exp_board, "tune", exp_id())

      tune_grids <- get_tune_grids_from_models(names(selected_models()))

      for (grid in tune_grids) {
        for (wflow_id in wflow_set$wflow_id) {
          if (stringr::str_ends(wflow_id, clean_object_names(grid))) {
            wflow_set <- wflow_set |>
              workflowsets::option_add(
                grid = exp_board |> pins::pin_read(grid),
                id = wflow_id
              )
          }
        }
      }

      exp_object <- list(
        Splits = names(selected_splits()),
        Models = names(selected_models()),
        Preprocessors = names(selected_recipes()),
        Metrics = names(selected_metrics()),
        wflow_set = wflow_set
      )

      class(exp_object) <- "experiment"
      exp_object
    })


    shiny::observe({
      shiny::removeModal()


      save_exp_config(reactive_exp(), exp_id(), input$exp_name,
        description = input$exp_description,
        tags = input$exp_tags,
        config_type = "experiment"
      )
    }) |>
      shiny::bindEvent(input$confirm_create_exp)
  })
}
