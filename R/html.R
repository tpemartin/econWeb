get_div_split_css = function(split_cssx, innerTextx=NULL){
  # split_cssx=split_css[1]
  cssname = names(split_cssx) |>
    stringr::str_remove("\\.")
  css_string=
    do.call(htmltools::css,
      purrr::flatten(split_cssx[[1]]))
  css_string=paste0(
    ".",cssname,"{",css_string,"}"
  )
  function(..., class=NULL){
    tags$div(
      class=cssname, innerTextx, ...
    ) -> tagX
    if(!is.null(class)) htmltools::tagAppendAttributes(tagX, class=class) -> tagX
    tagList(
      tagX,
      tags$style(css_string)
    ) -> tagX2
    return(tagX2)
  }
}
get_div_split = function(split_cssx, innerTextx=NULL){
  # split_cssx=split_css[1]
  cssname = names(split_cssx) |>
    stringr::str_remove("\\.")
  function(..., class=NULL){
    tags$div(
      class=cssname, innerTextx, ...
    ) -> tagX
    if(!is.null(class)) htmltools::tagAppendAttributes(tagX, class=class) -> tagX
    return(tagX)
  }
}
get_cssx <- function(split_cssx){
  cssname = names(split_cssx)
  css_string=
    do.call(htmltools::css,
      purrr::flatten(split_cssx[[1]]))
  css_string=paste0(
    ".",cssname,"{",css_string,"}"
  )
}
get_csstext <- function(split_css) {
  seq_along(split_css) |>
    purrr::map(
      ~{
        get_cssx(split_css[.x])
      }
    ) -> list_css_string
  list_css_string |>
    unlist() |>
    paste(collapse = "\n") ->
    css_text
}

