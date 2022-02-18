#' Generate figma instance
#'
#' @return
#' @export
#'
#' @examples none.
Figma <-  function(){
  figma <- new.env()
  figma$convert2html <- function(){
    figma_Css2Html() -> .html
    invisible(.html)
  }
  figma$convert2R <- function(.html=NULL){
    if(is.null(.html)){
      html <- figma_Css2Html()
    } else {
      html=.html
    }

    html2R(html, prefix = T, withStyle=T) |>
      add_tick2someAttributes() |>
      remove_tagComment() -> rscript
    rscript |>
      clipr::write_clip()

    invisible(rscript)
  }
  figma$create_page <- function(){
    figma$convert2R() -> pageTag_string
    pageTag_expr <- rlang::parse_exprs(pageTag_string)
    rlang::eval_bare(pageTag_expr[[1]]) #-> pageTag
    create_webpageTag(tag_element,filename="figma.html") -> htmlfile
    figma$show_page <- function(){
      browseURL(htmlfile)
    }
    return(figma)
  }
  return(figma)
}

#' Translate clipboard figma css to html
#'
#' @param update_autolayout_margin default =F
#'
#' @return
#' @export
#'
#' @examples none
figma_Css2Html <- function(update_autolayout_margin=T){
  figma2html(update_autolayout_margin) |> clipr::write_clip()
}

# helpers -----------------------------------------------------------------
create_webpageTag <- function(tag=NULL, destfolder=NULL, filename=NULL){
  if(is.null(destfolder)) destfolder="temp"
  if(!dir.exists(destfolder))
  {  message(
    glue::glue("Creating a folder '{destfolder}' to host the web page."))
    dir.create(destfolder)
  }
  if(is.null(filename)) filename="index.html"
  filepath=file.path(destfolder, filename)
  tag -> webpageTag
  webpageTag |>
    htmltools::save_html(filepath)
  message(filepath, "\nis created.")
  return(filepath)
}



