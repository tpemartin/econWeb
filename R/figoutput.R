export_fig <- function(fig, tagname="mycard") {
  fig$ui() -> fig_ui

  cssfilename = paste0(tagname,".css")
  fig$csstext |>
    xfun::write_utf8(file.path(".", cssfilename))

  tag_ui <- fig_ui[[1]]
  fig_ui[[1]] |>
    as.character() -> htmlStr
  htmlStr |>
    generate_tagUiText() -> tag_uiText
  stringr::str_replace(tag_uiText,
    "(?<=tag_)ui", tagname) -> tag_uiText
  glue::glue("<<tagname>>_dependency <- function(){
    htmltools::htmlDependency(
      name=\"<<tagname>>\",
      version=\"1.0.0\",
      src=c(file=normalizePath(\".\")),
      style=\"<<cssfilename>>\",
      all_files = F
    )}", .open="<<", .close=">>") -> dep_text
  # dep_text |> clipr::write_clip()

  glue::glue("ui_<<tagname>> <- function(dependency=NULL){
    tagList(tag_<<tagname>>(), <<tagname>>_dependency(), dependency)
  }", .open="<<", .close=">>") -> uiText

  c(tag_uiText, dep_text, uiText, glue::glue("ui_{tagname}() |> browseTag2()")) |>
    paste(collapse = "\n") |> clipr::write_clip()
}
generate_tagUiText <- function(htmlStr){
  library(shiny)
  library(XML)
  library(magrittr)
  library(purrr)
  library(stringr)

  htmlStr %>%
    htmlParse(encoding="UTF-8") -> x
  x %>%
    getNodeSet("/html/body/*") -> .temp

  .temp |>
    purrr::map(
      ~renderNode(.x, prefix = T)
    ) -> .temp2

  .temp3 <- paste(.temp2, collapse = ",\n")
  # browser()
  if(length(.temp2)>1) .temp3 <- paste0("tagList(\n", .temp3, "\n)")

  .temp4 <- glue::glue("tag_ui <-function(){\n <<.temp3>>\n}\n", .open="<<", .close=">>")
  return(.temp4)
}
