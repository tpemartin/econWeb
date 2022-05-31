
#' Create a list of dependencies generator
#'
#' @return a list
#' @export
#'
Dependency = function(){
  dep = list()
  dep$jquery=list(
    onSite=jquery_onSite,
    onCloud=jquery_onCloud
  )
  dep$materialise$onCloud = materialise_onCloud
  dep$googleFont$onCloud = googleFont_onCloud
  dep$mobile = mobile
  dep$all = function(){
    require(htmltools)
    tagList(
      dep$jquery$onCloud(),
      dep$materialise$onCloud(),
      dep$googleFont$onCloud(),
      dep$mobile()
    )
  }
  dep$noMaterialise = function(){
    require(htmltools)
      tagList(
        dep$jquery$onCloud(),
        dep$googleFont$onCloud(),
        dep$mobile()
      )

  }
  dep
}
googleFont_onCloud <- function(){
  htmltools::htmlDependency(
    name="googlefont", version="1.0.0",
    src= c(href=""),
    stylesheet = "",
    head='<link href="https://fonts.googleapis.com/icon?family=Material+Icons" rel="stylesheet">'
  )
}

jquery_onSite <- function(){
  htmltools::htmlDependency(
    name="jquery", version="3.6.0",
    src=c(file=normalizePath("js/jquery")),
    script="jquery-3.6.0.min.js"
  )
}

jquery_onCloud <- function(){
  htmltools::htmlDependency(
    name="jquery", version="3.6.0",
    src=c(href="https://code.jquery.com"),
    script=list(
      src="jquery-3.6.0.min.js",
      integrity="sha256-/xUj+3OJU5yExlq6GSYGSHk7tPXikynS7ogEvDej/m4=" ,
      crossorigin="anonymous"
    )
  )
}
materialise_onCloud <- function(){
  htmltools::htmlDependency(
    name="materialize", version="1.0.0",
    src=c(href="https://cdnjs.cloudflare.com/ajax/libs/materialize/1.0.0"),
    stylesheet="css/materialize.min.css",
    script="js/materialize.min.js"
  )

}
mobile <- function(){
  htmltools::htmlDependency(
    name="mobile",
    version="1.0.0",
    src=c(file=""),
    meta=list(
      viewport="width=device-width, initial-scale=1.0"
    )
  )
}

