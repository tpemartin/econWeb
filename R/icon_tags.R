#' Generate tag functions for icon svg's saved under a certain folder
#'
#' @param iconFolder A path to the svg folder
#'
#' @return a list of tag functions
#' @export
#'
tags_icon <- function(iconFolder){
  iconFolder <- normalizePath(iconFolder)
  icon_dependency = icon_dependency_generator(iconFolder)
  create_icon_css(iconFolder)

  iconFiles <- list.files(iconFolder, full.names = T)
  classnames = basename(iconFiles) |>
    stringr::str_subset(".svg$") |>
    stringr::str_remove(".svg$")
  require(htmltools)
  purrr::map(seq_along(classnames),
    ~{
      classnameX = classnames[[.x]]
      function(class=NULL, ...) {
        if(!is.null(class)){
          class=paste(classnameX, class)
        } else {
          class=classnameX
        }
        tagList(
          tags$i(class=classnameX, ...),
          icon_dependency()
        )
      }
    }) |> stats::setNames(classnames) -> list_iconTags
  return(list_iconTags)
}


# helpers -----------------------------------------------------------------


create_icon_css = function(iconFolder){
  iconFolder <- normalizePath(iconFolder)
  iconFiles <- list.files(iconFolder, full.names = T)
  css_text <- ""
  for(iconFileX in iconFiles){
    css_text <- c(css_text, create_css(iconFileX))
  }
  css_text |> xfun::write_utf8(
    con=file.path(iconFolder, "icon.css")
  )
}
icon_dependency_generator = function(iconFolder){
  iconFolder <- normalizePath(iconFolder)
  function(){
    htmltools::htmlDependency(
      name="icon",
      version="0.0.1",
      src=c(file=iconFolder),
      stylesheet = "icon.css",
      all_files = T
    )
  }
}
create_css = function(iconfileX){
  svgfile = basename(iconfileX)
  con <- file(iconfileX,"r")
  first_line <- readLines(con,n=1)
  close(con)
  unlist(stringr::str_extract_all(first_line, "width\\=\"[0-9]+\"|height\\=\"[0-9]+\""))->
    width_height
  purrr::map_chr(
    width_height,
    ~{.x |> stringr::str_replace("=", ": ") |>
    stringr::str_remove_all("\"") |> paste0("px;")}) |>
    paste0(collapse="\n") -> css_wh

  css= stringr::str_remove(svgfile, ".svg$")
  glue::glue(".<<css>> {
  background-image: url(<<svgfile>>);
  display: inline-block;
  <<css_wh>>
}", .open="<<", .close=">>")

}

