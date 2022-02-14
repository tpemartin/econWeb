#' Generate assets attachment href when dependency attachment is vector of folder names
#'
#' @param dependency a htmlDepency where src=c(file...)
#'
#' @return
#' @export
#'
#' @examples none.
generate_assets_attachment_href <- function(dependency) {
  assets <- list()
  assets_dep = dependency
  # assets_dep$src$file
  assets$source <- {
    assets_dep$attachment |>
      purrr::map(
        ~{
          list.files(
            file.path(assets_dep$src, .x)
          )
        }
      ) |>
      setNames(assets_dep$attachment)

  }

  assets$folder = file.path(
    "lib",
    paste(assets_dep$name, assets_dep$version, sep="-"))
  assets$href <- {
    assets_dep$attachment |>
      purrr::map(
        ~file.path(assets$folder,.x, assets$source[[.x]])
      ) |>
      setNames(
        assets_dep$attachment
      )
  }
  assets
}
#' Create dependency other than script and link in the head
#'
#' @param name as in htmlDependency
#' @param version as in htmlDependency
#' @param ... other inputs other than src, script and style
#'
#' @return
#' @export
#'
#' @examples
#' dep_ios <- htmlDependency2(
#'   name="ios-app",
#'   version="0.0.1",
#'   meta = list(
#'     `apple-mobile-web-app-capable`="yes",
#'     `apple-mobile-web-app-status-bar-style`="black"
#'   )
#' )
htmlDependency2 <- function(name, version,...){
  require(htmltools)
  if(!dir.exists("assets/js")) dir.create("assets/js", recursive = T)
  "" |> xfun::write_utf8("assets/js/empty.js")
  message("An empty file \"assets/js/empty.js\" is created. Do not delete it.")
  htmlDependency(
    name=name,
    version=version,
    src=c(file="assets/js"),
    script="empty.js",
    ...
  )
}

#' As browseURL but it browse a shiny.tag or html class object
#'
#' @param tag A shiny.tag or html class object
#'
#' @return
#' @export
#'
#' @examples none
browseTag <- function(tag=.Last.value){
  if(!dir.exists("temp")) dir.create("temp")
  htmltools::save_html(
    tag, file="temp/.temp.html"
  )
  browseURL("temp/.temp.html")
}
#' Take copy from clipboard and translate html codes to R codes and write to clipboard for paste
#'
#' @param prefix a logical, default=T, the paste tag will start with tags$
#'
#' @return
#' @export
#'
#' @examples none.
translate_HTML_fromClipboard <- function(prefix = T, withStyle=F) {
  # prefix=T

  html2R(clipr::read_clip(), prefix = prefix, withStyle=withStyle) -> translatedTags
  translatedTags |>
    add_tick2someAttributes() |>
    remove_tagComment() |>
  clipr::write_clip()
}
html2R <- function(htmlStr, prefix = FALSE, withStyle=FALSE) {
  message("adopted from https://github.com/alandipert/html2r")
  library(shiny)
  library(XML)
  library(magrittr)
  library(purrr)
  library(stringr)

  htmlStr %>%
    htmlParse -> x

  styleTag = NULL
  if(withStyle){
    generate_styleTag(x) -> styleTag
  }

  x %>%
    getNodeSet("/html/body/*") -> .temp


  .temp |>
    purrr::map(
      ~renderNode(.x, prefix = prefix)
    ) -> .temp2

  .temp2 = c(unlist(.temp2), styleTag)

  .temp3 <- paste(.temp2, collapse = ",\n")
  # browser()
  if(length(.temp2)>1) .temp3 <- paste0("tagList(\n", .temp3, "\n)")

  .temp4 <- glue::glue("{.temp3} -> tag_element\n\ntag_element |> econWeb::browseTag()")
  return(.temp4)
}


# helpers -----------------------------------------------------------------

generate_styleTag <- function(x) {
  # get all style in header
  x |> XML::getNodeSet("/html/head/style") ->    .style
  purrr::map_chr(
    .style,
    XML::xmlValue
  ) -> styleText
  paste(styleText, collapse="\n\t") -> styleText
  glue::glue("tags$style(\n\"{styleText}\"\n)") -> tag_style
  return(tag_style)
}

makeAttrs <- function(node) {
  attrs <- xmlAttrs(node)
  names(attrs) %>%
    Map(function (name) {
      val <- attrs[[name]]
      paste0(name, ' = ', if (val == "") "NA" else paste0('"', val, '"'))
    }, .)
}

Keep <- function(fun, xs) Map(fun, xs) %>% Filter(Negate(is.null), .)

renderNode <- function(node, indent = 0, prefix = FALSE) {
  if (xmlName(node) == "text") {
    txt <- xmlValue(node)
    if (nchar(trimws(txt)) > 0) {
      paste0('"', trimws(txt), '"')
    }
  } else {
    tagName <- if (prefix) paste0("tags$", xmlName(node)) else xmlName(node)
    newIndent <- indent + length(tagName) + 1
    xmlChildren(node) %>%
      Keep(partial(renderNode, indent = newIndent, prefix = prefix), .) %>%
      append(makeAttrs(node), .) %>%
      paste(collapse = str_pad(",\n", width = newIndent, side = c("right"))) %>%
      trimws(which = c("left")) %>%
      paste0(tagName, "(", ., ")")
  }
}
add_tick2someAttributes <- function(translatedTags) {
  stringr::str_extract_all(
    translatedTags, "[a-z]+[-:][a-z]+(?=\\s*=)"
  ) -> origins
  if(length(origins[[1]])==0) return(translatedTags)
  pattern <- origins <- unique(origins[[1]])
  pattern <- paste0("`",origins,"`")
  names(pattern) = origins
  stringr::str_replace_all(
    translatedTags,
    pattern
  )
}
remove_tagComment <- function(translatedTags) {
  paste0(translatedTags, collapse = "\n") -> translatedTags
  stringr::str_extract_all(
    translatedTags, "tags\\$comment\\(\\)"
  )
  pattern1 = "(?<=,)\\s*tags\\$comment\\(\\)\\s*,"
  stringr::str_remove_all(
    translatedTags, pattern1
  ) -> translatedTags
  pattern2 = ",\\s*tags\\$comment\\(\\)\\s*(?=\\))"
  stringr::str_remove_all(
    translatedTags, pattern2
  ) -> translatedTags
  return(translatedTags)
}


