get_div_split_css = function(split_cssx){
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
      class=cssname,...
    ) -> tagX
    if(!is.null(class)) htmltools::tagAppendAttributes(tagX, class=class) -> tagX
    tagList(
      tagX,
      tags$style(css_string)
    ) -> tagX2
    return(tagX2)
  }
}
