#' ingest_data_source
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd
ingest_data_source <- S7::new_generic("ingest_data_source", "data_source")


S7::method(ingest_data_source, data_source_pinned) <- function(data_source) {
  data_source@data_board |>
    pins::pin_read(data_source@pin_name)
}


S7::method(ingest_data_source, data_source_demo) <- function(data_source) {
  box::use(modeldata)
  modeldata[[data_source@demo_name]]
}

S7::method(ingest_data_source, data_source_file) <- function(data_source) {
  readr::read_csv(data_source@data_path)
}
