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
  tag_uiText |> clipr::write_clip()
  # tag_uiText |> clipr::write_clip()
  glue::glue("tag_dependency <- function(){
    htmltools::htmlDependency(
      name=\"<<tagname>>\",
      version=\"1.0.0\",
      src=c(file=normalizePath(\".\")),
      style=cssfilename,
      all_files = F
    )}", .open="<<", .close=">>") -> dep_text
  dep_text |> clipr::write_clip()

  "ui <- function(){
    tagList(tag_ui(), tag_dependency())
  }" -> uiText

  c(tag_uiText, dep_text, uiText, "ui() |> browseTag2()") |>
    paste(collapse = "\n") |> clipr::write_clip()
}
