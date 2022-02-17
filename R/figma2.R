#' Convert figma CSS in clipboard to an R environment object
#'
#' @param update_autolayout_margin a logical, default=F. T means update inside auto layout margin to be half of its original value.
#' @return an environment with `create_cssFile`, `div` and `style` objects.
#' @export
#'
#' @examples none
create_rfig <- function(update_autolayout_margin=F){
  fig <- new.env()
  figma2R(update_autolayout_margin) -> list_tag_funs
  list_tag_funs$div |> purrr::map(
    eval
  ) -> fig$div

  eval(list_tag_funs$style) -> fig$style
  list_tag_funs$css -> cssRules
  fig$create_cssFile <- function(cssfile="rfig.css"){
    cssRules |> xfun::write_utf8(cssfile)
    message(glue::glue("{cssfile} is created."))
    fig$includeCSS <- function(){
      htmltools::includeCSS(cssfile)
    }
  }
  return(fig)
}
figma2html <- function(update_autolayout_margin=F){
  css <- clipr::read_clip()
  css |>
    stringr::str_which("^/\\*\\s\\.") -> .x
  assertthat::assert_that(
    length(.x) >0,
    msg="There is no class name found. Maybe you forget to name your frame with starting . sign."
  )

  css |> create_cssList(update_autolayout_margin, .x) -> list_css

  purrr::map(names(list_css), ~glue::glue("<div class='{.x}'>\n</div>")) -> list_div

  paste("<style>",
    convert_listCss2stringCss(list_css),
    "</style>", sep="\n") -> style_tag
  c(style_tag,
    unlist(list_div))
}
figma2R <- function(update_autolayout_margin=F){
  css <- clipr::read_clip()
  css |>
    stringr::str_which("^/\\*\\s\\.") -> .x
  assertthat::assert_that(
    length(.x) >0,
    msg="There is no class name found. Maybe you forget to name your frame with starting . sign."
  )

  css |>
    create_cssList(update_autolayout_margin, .x) -> list_css
  list_css |>
    convert_listCss2stringCss() -> css_string
  expr_tagStyle <- rlang::expr({
    function(){
      tags$style(!!css_string)
    }})
  generate_div_funExpr_list(names(list_css)) -> list_expr_tagDiv

  list(
    style=expr_tagStyle,
    div=list_expr_tagDiv,
    css=css_string
  )
}
create_cssList <- function(css, update_autolayout_margin, .x){
  css |> stringr::str_which("^/\\*") -> whichIsDivision
  if(update_autolayout_margin){
    css |> stringr::str_which(stringr::fixed("/* Inside auto layout */")) -> inside_autoLayout_starts
    purrr::map(
      inside_autoLayout_starts,
      ~{
        get_cssblock_positionIndices(c(whichIsDivision, length(css)+1), .x) -> locs
        locs}
    ) -> .temp

    update_insideAutoLayout_margin(css, .temp) -> css
  }


  css |> get_list_css(.x) -> list_css
  # list_css |> rename_duplicated_cssnames() -> list_css
  # if(isThereJustifyContent(list_css)){
  #   list_css |> purrr::map(remove_autolayout_all_margin) -> list_css
  # } else {
  #   list_css |> purrr::map(remove_autolayout_order0_margin) -> list_css
  # }

  return(list_css)
}
convert_listCss2stringCss <- function(list_css){
  # unlist(list_css) |> paste(collapse="\n") -> css_content
  list_css |> unlist() |> paste(collapse="\n") -> css_string
  return(css_string)
}
# div_list = generate_div_funExpr_list(c("aaa","bbb"))

generate_div_funExpr_list <- function(cssnames){
  purrr::map(
    cssnames,
    ~generate_div_funExpr(.x)
  ) |> setNames(cssnames)
}
generate_div_funExpr <- function(cssname){
  rlang::expr({function(..., class=NULL){
    tags$div(
      class=!!cssname,...
    ) -> tagX
    if(!is.null(class)) htmltools::tagAppendAttributes(tagX, class=class) -> tagX
    return(tagX)
  }})
}
get_cssnames <- function(cssX){
  cssX |>
    stringr::str_remove_all("^/\\*\\s|\\s\\*/$") -> css_removed_comment
  css_removed_comment |>
    stringr::str_replace_all("^(?<=\\.)([:punct:]+|\\s)", "-") |>
    stringr::str_remove_all("[<>\\=\\.]") -> cssnames0
  cssnames0 |> rename_duplicated_cssnames() -> cssnames0
  return(cssnames0)
}
create_css_string <- function(list_css){

  purrr::map(seq_along(list_css),
    ~{
      cssX = list_css[.x]
      cssname = names(cssX)
      stylingRules = unlist(cssX) |> paste(collapse = "\n")
      paste0(".", cssX, " {\n", stylingRules, "\n}")
    }) -> list_css_string
  unlist(list_css_string)
}
# css[.x] |> get_cssnames() -> cssnames0
get_list_css <- function(css, .x){
  css[.x] |> get_cssnames() -> cssnames0

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
  ) |>
    setNames(cssnames0) -> list_css
  # list_css <- rename_duplicated_cssnames(list_css)
  return(list_css)
}
create_rfig_globalEnv <- function(){
  .GlobalEnv$rfig <- create_rfig()
}
remove_autolayout_order0_margin <- function(rules){
  if(any(stringr::str_detect(rules, stringr::fixed("/* Inside auto layout */"))) &&
      any(stringr::str_detect(rules,"order: 0"))){
    stringr::str_subset(rules, "\\bmargin\\b:", negate = T) -> rules
  }
  return(rules)
}
isThereJustifyContent <- function(list_css)
{
  purrr::map_lgl(
    list_css,
    ~{
      flag_space_between <- stringr::str_detect(.x,stringr::fixed("justify-content: space-between;")) |> any()
    }
  ) |> any()
}
remove_autolayout_all_margin <- function(rules){
  if(any(stringr::str_detect(rules, stringr::fixed("/* Inside auto layout */")))){
    stringr::str_subset(rules, "\\bmargin\\b:", negate = T) -> rules
  }
  return(rules)
}
rename_duplicated_cssnames <- function(names_css){
  tb <- table(names_css)
  which(tb>1) -> whichHasDuplicatedName
  tb[whichHasDuplicatedName] -> tb_duplicate

  for(.x in seq_along(tb_duplicate)){
    whichAreDuplicated <- which(names_css == names(tb_duplicate[.x]))
    names_css[whichAreDuplicated] <-
      paste(names(tb_duplicate[.x]),1:tb_duplicate[.x], sep="-")
  }
  return(names_css)
}
get_cssblock <- function(css, pattern, whichIsDivision){
  stringr::str_which(stringr::fixed(pattern)) -> start_autolayout

  css[start_autolayout:end_autolayout]
}
isThereAutoLayout <- function(css){
  any(stringr::str_detect(css, stringr::fixed("/* Auto layout */")))
}
get_cssblock_positionIndices <- function(whichIsDivision, start_autolayout) {
  whichIsDivision |> subset(whichIsDivision> start_autolayout) |>
    min() -> end_autolayout; end_autolayout -1 -> end_autolayout
  return(start_autolayout:end_autolayout)
}
update_insideAutoLayout_margin <- function(css, .temp){
  for(.x in seq_along(.temp)){
    cssblock <- css[.temp[[.x]]]
    cssblock |> stringr::str_which("^margin") -> .xx
    .temp[[.x]][[.xx]] -> margin_loc
    if(.x==1) margin_update = obtain_insideAutoLayout_marginUpdate(cssblock)
    css[[margin_loc]] <- margin_update
  }
  return(css)
}
obtain_insideAutoLayout_marginUpdate <- function(cssblock){
  cssblock |> stringr::str_subset("^margin") |>
    stringr::str_extract_all("[\\.0-9]+") |> unlist() |>
    as.numeric() -> margin
  margin <- margin/2

  paste0(margin,"px") |> paste(collapse=" ") -> margin_cssvalue
  paste("margin:", margin_cssvalue) |> paste0(";") -> margin_css
  margin_css
}
