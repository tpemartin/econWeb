update_package <- function(){
  unloadNamespace("econWeb")
  remotes::install_github("tpemartin/econWeb", force=T)
  # library(econWeb)
}
is_rproject <- function(){
  list.files(".") |>
    stringr::str_detect("[.]Rproj$") |>
    any()
}
check_classname <- function(){
  stringr::str_subset(clipr::read_clip(), "/\\* .* \\*/")
}
split_by_cutpoints <- function(origin, cutpoints) {
  seq_along(origin) |>
    cut(c(cutpoints-1,
      length(origin))) ->   levels_cssblock

  origin |>
    split(levels_cssblock)
}
