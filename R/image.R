#' Make svg responsive
#'
#' @param svgfile a filepath
#'
#' @return
#' @export
#'
#' @examples none
make_svg_responsive <- function(svgfile){
  readLines(svgfile, n=1) -> svgline1
  if(stringr::str_detect(svgline1, "(width|height)")){
    svglines <- readLines(svgfile)
    svglines[[1]] |>
      # "width=\"111\" height=\"102\" viewBox=\"0 0 111 102\" fill=\"none\" " |>
      stringr::str_remove_all(
        "(width|height)[\\s=0-9\\\"]+"
      ) -> svglines[[1]]
    svglines |> writeLines(svgfile)
  }
  message('done.')
}
