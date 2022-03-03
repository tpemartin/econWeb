fig <- Fig()
fig$export("navbar")
tag_ui <-function(){
  tags$div(class = "navbar",
    tags$div(class = "navbar-logo"),
    tags$div(class = "navbar-more"))
}
navbar_dependency <- function(){
  htmltools::htmlDependency(
    name="navbar",
    version="1.0.0",
    src=c(file=normalizePath(".")),
    style="navbar.css",
    all_files = F
  )}
ui_navbar <- function(dependency=NULL){
  tagList(tag_navbar(), navbar_dependency(), dependency)
}
ui() |> browseTag2()
tag_uiText |> stringr::str_replace("(?<=tag_)ui", tagname)
