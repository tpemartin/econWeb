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


figma_Css2Html <- function(){
  css <- clipr::read_clip()
  css |>
    stringr::str_which("^/\\*\\s\\.") -> .x
  assertthat::assert_that(
    length(.x) >0,
    msg="There is no class name found. Maybe you forget to name your frame with starting . sign."
  )
  css[.x] |>
    stringr::str_remove_all("^/\\*\\s|\\s\\*/$") -> css_removed_comment
  css_removed_comment |>
    stringr::str_replace_all("^(?<=\\.)([:punct:]+|\\s)", "-") |>
    stringr::str_remove_all("[<>\\=\\.]") -> cssnames0

  pos = seq_along(css)
  pos |> cut(c(.x-1,Inf)) -> groups
  css |> split(groups) |>
    setNames(cssnames0) -> split_css
  purrr::map(
    seq_along(cssnames0),
    ~{
      c(paste0(".",cssnames0[[.x]], " {"),
        paste0("\t", split_css[[.x]]),
        "}")
    }
  ) -> list_css

  css_text = unlist(list_css)

  elementclasses = cssnames0[-1]
  frameclass = cssnames0[[1]]
  purrr::map(
    seq_along(elementclasses),
    ~{

      glue::glue("\t<div class=\"{elementclasses[[.x]]}\">el-{.x}</div>")
    }
  ) |> unlist() -> elementsHTML
  html_text = c(
    glue::glue("<div class=\"{frameclass}\">"),
    paste0("\t", elementsHTML),
    "</div>"
  )
  full_text =
    c(
      "<style>",
      css_text,
      "</style>",
      html_text
    )

  full_text |> clipr::write_clip()

  invisible(full_text)
}

