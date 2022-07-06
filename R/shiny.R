#' attach shiny.min.js to html tag
#'
#' @param .tag a shiny tag
#'
#' @return a shiny tag
#' @export
attachShinyJs = function(.tag){
  require(htmltools)
  dep=econWeb::Dependency()
  tagList(
    dep$jquery$onCloud(),
    shinyDep(),
    .tag)
}
shinyDep = function() {
  shinyjsDep = shiny:::shinyDependencies()[[2]]
  shinyjsDep$src = NULL
  shinyjsDep$src$file=system.file("www/shared",package="shiny"
  )
  shinyjsDep
}
