update_package <- function(){
  devtools::install_github("tpemartin/econWeb", force=T)
}
check_classname <- function(){
  stringr::str_subset(clipr::read_clip(), "/\\* .* \\*/")
}
