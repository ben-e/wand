tunable.wand <- function(x, ...) {
  tibble::tibble(
    name = c('batch_size', 'epochs', 'learn_rate', 'stop_iter'),
    call_info = list(
      list(pkg = "dials", fun = "batch_size"),
      list(pkg = "dials", fun = "epochs"),
      list(pkg = "dials", fun = "learn_rate"),
      list(pkg = "dials", fun = "stop_iter")
    ),
    source = "model_spec",
    component = class(x)[class(x) != "model_spec"][1],
    component_id =  "main"
  )
}
