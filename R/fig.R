#' Initiator for fig
#'
#' @param attachmentSrc A path to attachment files folder.
#' @return
#' @export
#'
#' @examples none
Fig <- function(attachmentSrc=NULL) {
  fig <- structure(new.env(), class="fig")
  fig$css <- list(
    original=clipr::read_clip()
  )
  fig$css$original |>
    stringr::str_which(
      "/\\* (Auto layout|Inside auto layout|identical to box height) \\*/"
    )  -> whichIsAutoLayout

  fig$css$original |>
    stringr::str_which(
      "/\\* .* \\*/"
    ) -> whichIsAllComments
  setdiff(whichIsAllComments,
    whichIsAutoLayout) ->
    whichNotAutoLayout

  fig$css$original[whichNotAutoLayout] |>
    stringr::str_remove_all("/\\*\\s+|\\s*\\*+/") -> cssnames

  fig$css$original |>
    split_by_cutpoints(
      whichNotAutoLayout
    ) |>
    setNames(cssnames) -> list_css
  list_css |>
    purrr::map(split_cssX_byAutolayout) -> split_css

  split_css |>
    rename_split_css() -> split_css

  pc_map = map_parent_child(split_css)

  split_css |>
    correct_insideAutoLayoutMargin(pc_map) -> split_css

  split_css |>
    correct_insideAutoLayoutConstraint(pc_map) -> split_css

  fig$split_css <- split_css
  fig$split_innerContent <-
    vector("list", length(fig$split_css)) |>
    setNames(names(
      fig$split_css
    ))

  fig$DOM <- pc_map
  fig$div <- split_css2div(fig$split_css, fig$split_innerContent)
  fig$update_div <- function(){
    fig$div <- split_css2div(fig$split_css, fig$split_innerContent)
  }

  get_divInputFunction <- function(dep, split_div){
    function(){
      if(is.null(dep)) {
        return(list(NULL))
      }
      # dep |> purrr::map(
      #   .GlobalEnv$input[[.x]]()
      # ) -> list_inputCalls
      # do.call(
      #   split_div[[.x]]
      # )
      purrr::map(
        dep,
        ~do.call(fig$div[[.x]],list(fig$div_input[[.x]]())))
    }
  }

  fig$csstext <- get_csstext(
    fig$split_css
  )
  fig$update_css <- function(){
    get_csstext(
      fig$split_css
    ) -> fig$csstext
  }
  fig |> get_setOfFamilies() ->
    setOfFamilies

  fig$ui <- {


    fig$DOM |> purrr::map(
      ~get_divInputFunction(.x, fig$div)
    ) -> fig$div_input

    function(dependency=NULL){
      require(htmltools)
      tag_style =
        tags$style(
          fig$csstext
        )
      get_listTagUI(
        setOfFamilies, fig
      ) -> list_tagUI
      # do.call(fig$div[[1]], fig$div_input[[1]]()) -> tag_ui
      tagList(
        list_tagUI,
        tag_style,
        dependency
      )
    }
  }


  fig$update_ui <- function(){
    fig$DOM |> purrr::map(
      ~get_divInputFunction(.x, fig$div)
    ) -> fig$div_input
    fig$ui <- function(dependency=NULL){
      require(htmltools)
      tag_style =
        tags$style(
          fig$csstext
        )
      get_listTagUI(
        setOfFamilies, fig
      ) -> list_tagUI
      # do.call(fig$div[[1]], fig$div_input[[1]]()) -> tag_ui
      tagList(
        list_tagUI,
        tag_style,
        dependency
      )
    }
  }
  export_fig <- generate_export_fig(attachmentSrc)
  fig$export <- function(tagname="fig"){
    fig |> export_fig(tagname=tagname)
  }
  return(fig)
}


# helpers -----------------------------------------------------------------


get_setOfFamilies <- function(fig) {
  look4family <- function(familySet){
    # familySet=whichHasChild[[1]]
    fig$DOM[familySet] |> unlist() -> partOfFamily
    numNewMembers = setdiff(partOfFamily, familySet)
    if(length(numNewMembers)==0){
      return(familySet)
    } else {
      familySet=c(familySet, partOfFamily) |> unique()
      look4family(familySet)
    }
  }
  fig$DOM |>
    purrr::map_lgl(
      ~length(.x)!=0
    ) |> which() -> whichHasChild
  setOfFamilies = list()
  flag_stillLeft = T
  it=0; max.it=100;
  while(flag_stillLeft && it <= max.it){
    familySet =
      look4family(whichHasChild[[1]])
    whichHasChild = setdiff(
      whichHasChild, familySet)
    setOfFamilies=
      append(setOfFamilies, list(familySet))
    flag_stillLeft <- length(whichHasChild) !=0
    it=it+1
  }
  return(setOfFamilies)
}
get_listTagUI <- function( setOfFamilies, fig) {
  purrr::map(
    seq_along(setOfFamilies),
    ~{
      familyX <- setOfFamilies[[.x]]
      familyX
      do.call(
        fig$div[[familyX[[1]]]],
        fig$div_input[[familyX[[1]]]]()
      ) -> tagUI
      tagUI
    }
  ) -> list_tagUI
  list_tagUI |> as.tags()
}
