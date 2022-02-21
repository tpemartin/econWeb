Fig <- function() {
  fig <- new.env()
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
  fig$split_innerText <-
    vector("list", length(fig$split_css)) |>
    setNames(names(
      fig$split_css
    ))

  fig$DOM <- pc_map
  fig$div <- split_css2div(fig$split_css, fig$split_innerText)
  fig$update_div <- function(){
    fig$div <- split_css2div(fig$split_css, fig$split_innerText)
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
  fig$ui <- {
    fig$DOM |> purrr::map(
      ~get_divInputFunction(.x, fig$div)
    ) -> fig$div_input

    function(){
      tag_style =
        tags$style(
          fig$csstext
        )
      do.call(fig$div[[1]], fig$div_input[[1]]()) -> tag_ui
      tagList(
        tag_ui,
        tag_style
      )
    }
  }


  fig$update_ui <- function(){
    fig$DOM |> purrr::map(
      ~get_divInputFunction(.x, fig$div)
    ) -> fig$div_input
    fig$ui <- function(){
      do.call(fig$div[[1]], fig$div_input[[1]]())
    }
  }
  return(fig)
}
