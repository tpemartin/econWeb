#' Generate assets attachment href when dependency attachment is vector of folder names
#'
#' @param dependency a htmlDepency where src=c(file...)
#'
#' @return a list
#' @export
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

#' As browseURL but it browse a shiny.tag or html class object
#'
#' @param tag A shiny.tag or html class object
#'
#' @return none.
#' @export
#'
browseTag <- function(tag=.Last.value){
  if(!dir.exists("temp")) dir.create("temp")
  htmltools::save_html(
    tagList(tag, dep_mobile()), file="temp/.temp.html"
  )
  browseURL("temp/.temp.html")
}

#' save html with mobile dependency
#'
#' @param tag a shiny tag
#' @param file a filepath
#'
#' @return none.
#' @export
save_html2 <-  function(tag=.Last.value, file){
  htmltools::save_html(
    htmltools::tagList(tag, dep_mobile()), file=file)
}

#' As browseURL but it browse a shiny.tag or html class object in RStudio Viewer
#'
#' @param tag A shiny.tag or html class object
#'
#' @return none.
#' @export
#'
browseTag2 <- function(tag=.Last.value){
  if(!dir.exists("temp")) dir.create("temp")
  servr::daemon_stop()
  htmltools::save_html(
    htmltools::tagList(tag, dep_mobile()), file=file.path("temp","temp.html")
  )
  ss <- servr::httd("temp", host="0.0.0.0") # 0.0.0.0 is required for mobile browsing the desktop content
  # ss$port
  rstudioapi::viewer(glue::glue("http://127.0.0.1:{ss$port}/temp.html"))
}
#' Take copy from clipboard and translate html codes to R codes and write to clipboard for paste
#'
#' @param prefix a logical, default=T, the paste tag will start with tags$
#'
#' @return to clipboard
#' @export
#'
translate_HTML_fromClipboard <- function(prefix = T, withStyle=F, styleTagCss=NULL) {
  # prefix=T
  clipr::read_clip() -> html
  html |> paste(collapse = "\n") -> html_string
  stringr::str_extract_all(
    html_string,
    "(?<=\\<style\\>)((.|\\n)*)(?=\\</style\\>)"
  ) -> css
  if(css!=""){
    styleTagCss= generate_styleTagFromCss(css)
  }
  html2R(html, prefix = prefix, withStyle=withStyle, styleTagCss=styleTagCss) -> translatedTags
  translatedTags |>
    add_tick2someAttributes() |>
    remove_tagComment()  |>
  clipr::write_clip()
}
html2R <- function(htmlStr, prefix = FALSE, withStyle=FALSE, styleTagCss=NULL) {
  message("adopted from https://github.com/alandipert/html2r")
  library(shiny)
  library(XML)
  library(magrittr)
  library(purrr)
  library(stringr)

  htmlStr %>%
    htmlParse(encoding="UTF-8") -> x

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

  .temp2 = c(styleTagCss, unlist(.temp2), styleTag)

  .temp3 <- paste(.temp2, collapse = ",\n")
  # browser()
  if(length(.temp2)>1) .temp3 <- paste0("tagList(\n", .temp3, "\n)")

  .temp4 <- glue::glue("{.temp3} -> tag_element\n\ntag_element |> econWeb::browseTag2()")
  return(.temp4)
}


# helpers -----------------------------------------------------------------
generate_styleTagFromCss <- function(css) {
  unlist(css) |> paste0(collapse = "\n") -> css_string
  paste0(
    "tags$style('\n", css_string, "\n')"
  ) -> style_tag
}
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

dep_mobile <- function(){
  htmltools::htmlDependency(
    name="temp",
    version="1.0.0",
    src=c(file="assets/"),
    meta=list(
      viewport="width=device-width, initial-scale=1.0"
    )
  ) -> dep_mobile
}
