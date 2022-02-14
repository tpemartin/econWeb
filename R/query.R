#' Generate Query instance for Q&A
#'
#' @return
#' @export
#'
#' @examples none.
Query <- function(){
  q <- list()
  q$css_selectors <- generate_browseURL("https://www.w3schools.com/css/css_selectors.asp")
  q$html_tag <- generate_browseURL("https://developer.mozilla.org/en-US/docs/Web/HTML/Element")

  return(q)
}

generate_browseURL <- function(url){
  function(){
    browseURL(url)
  }
}
