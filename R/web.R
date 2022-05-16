#' Create a web instance
#'
#' @return an environment
#' @export
Web <- function(){
  require(htmltools)
  webExists = exists("web", envir=.GlobalEnv)
  if(!webExists)
    {webname = "web"
    message("will create a object named 'web' in your global environment.")
    # Sys.setenv("econWeb.webname"="web")
  }
  deps = webtheme::generate_webtheme()

  web=new.env()
  web$.tag <- list()

  web$create_page <- function(tag=NULL, destfolder=NULL, filename=NULL){
    if(is.null(destfolder)) destfolder="temp"
    if(!dir.exists(destfolder))
    {  message(
      glue::glue("Creating a folder '{destfolder}' to host the web page."))
       dir.create(destfolder)
    }
    if(is.null(filename)) filename="index.html"
    filepath=file.path(destfolder, filename)
    tagList(
      tag,
      web$.tag
    ) -> web$page

    web$page |>
      htmltools::save_html(filepath)
    message(filepath, "\nis created.")

    web$show_page <- function(){
      browseURL(
        filepath
      )
    }

  }

  web$add_jQuery <- function(){
    dep_jquery =
      htmltools::htmlDependency(
        name = "jquery-mini",
        version = "3.6.0",
        src = list(
          href = "https://code.jquery.com"),
        meta = NULL,
        script = "jquery-3.6.0.min.js",
        stylesheet = NULL, head = NULL,
        attachment = NULL, package = NULL,
        all_files = TRUE)

    append(web$.tag, list(dep_jquery)) -> web$.tag
  }
  web$add_materialise <- function(){
    dep_materialise <-
      htmltools::htmlDependency(
        name = "materialise",
        version = "1.0.0",
        src = list(
        href = "https://cdnjs.cloudflare.com/ajax/libs/materialize/1.0.0"),
        meta = list(viewport = "width=device-width, initial-scale=1.0"),
        script = "js/materialize.min.js", stylesheet = "css/materialize.min.css",
        head = "<link href=\"https://fonts.googleapis.com/icon?family=Material+Icons\" rel=\"stylesheet\">",
        attachment = NULL, package = NULL, all_files = TRUE)
    append(web$.tag, list(dep_materialise)) -> web$.tag
  }
  web$clear_setting <- function(){
    web$.tag <- list()
  }

  # browser()
  return(web)
}
