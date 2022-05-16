fig <- Fig()
dir.create("inst/assets/css", recursive = T)
fig$export("inst/assets/css/btnPeople")

tag_btnPeople <-function(){
   tags$div(class = "people",
      tags$div(class = "people-status=off",
         tags$div(class = "people-status=off-address")),
      tags$div(class = "people-status=on",
         tags$div(class = "people-status=on-address")),
      tags$div(class = "people-status=return",
         tags$div(class = "people-status=return-Left")))
}
btnPeople_dependency <- function(){
   htmltools::htmlDependency(
      name="econWeb",
      version="1.0.0",
      src=c(file=system.file("/assets/css",  package = "econWeb")),
      style="btnPeople.css",
      all_files = F
   )}
ui_btnPeople <- function(dependency=NULL){
   tagList(tag_btnPeople(), btnPeople_dependency(), dependency)
}
ui_btnPeople() |> econWeb::browseTag2()
