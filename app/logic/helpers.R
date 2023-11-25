box::use(
  purrr,
  recipes,
  sh = shiny
)

#' @export
apply_steps <- function(rec, preproc_steps) {
  sh$req(rec)
  for (step in purrr$compact(preproc_steps)) {
    sh$req(step$vars())
    rec <- do.call(step$func(), list(rec, step$vars()), envir = recipes)
  }
  rec
}
