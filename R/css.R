split_cssX_byAutolayout <- function(cssX) {
  cssX |> split_css_by("/\\* Auto layout \\*/") -> list_autolayout
  list_autolayout$remain |> split_css_by("/\\* Inside auto layout \\*/") -> list_insideAutolayout

  list(
    auto_layout=list_autolayout$extract,
    inside_autoLayout=list_insideAutolayout$extract,
    remain=list_insideAutolayout$remain
  ) -> list_cssblocks

  list_cssblocks |>
    purrr::map(get_list_styles) -> split_cssX
  split_cssX
}


# helper ------------------------------------------------------------------
get_list_styles <- function(cssX) {
  if(is.null(cssX)) return(NULL)
  cssX |>
    stringr::str_extract(
      "[^:]*"
    ) -> stylenames
  cssX |>
    stringr::str_extract(
      "(?<=:\\s)[^:;]*"
    ) -> stylevalues
  list_styles <- {
    whichIsNotNA <- which(!is.na(stylevalues))
    stylevalues |>
      na.omit() |>
      as.list() |>
      setNames(
        stylenames[whichIsNotNA]
      )
  }

}
split_css_by <- function(cssX, pattern="/\\* Auto layout \\*/"){
  cssX |>
    stringr::str_which(
      pattern
    )  -> whichIsAutoLayout

  if(length(whichIsAutoLayout)==0) return(
    list(
      extract = NULL,
      remain = cssX
    )
  )
  whichIsEmpty <-
    c(which(cssX == ""), length(cssX))

  which(whichIsEmpty > whichIsAutoLayout) |> min() -> whichloc_end

  whichIsEmpty[[whichloc_end]] -> whichEnds

  list(
    extract = cssX[whichIsAutoLayout:whichEnds],
    remain =
      cssX[-c(whichIsAutoLayout:whichEnds)]
  )
}
split_css2div <- function(split_css,split_innerText=NULL){
  innerTextx = NULL
  seq_along(split_css) |>
    purrr::map(
      ~{
        if(!is.null(split_innerText)) innerTextx=split_innerText[[.x]]
        get_div_split(split_css[.x], innerTextx)
        # get_div_split_css(split_css[.x], innerTextx)
      }
    ) |>
    setNames(names(split_css))
}
map_parent_child <- function(split_css){
  map_pc <- vector('list', length(split_css))
  map_pc
  css_belong <-
    vector("integer", length(split_css))
  cssnames = names(split_css)
  seq_cssnames2check = seq_along(split_css)
  for(.x in seq_along(cssnames)){
    # map_pc[[.x]]=NULL
    cssnameX <- cssnames[[.x]]
    # cssnameX
    stringr::str_which(
      cssnames,#[seq_cssnames2check],
      paste0(cssnameX,"\\-[^\\-]+$")
    ) -> whichBelong2X
    css_belong[whichBelong2X] = .x
    map_pc[[.x]]=whichBelong2X
    # if(length(whichBelong2X)==0) next
    # css_belong[seq_cssnames2check[whichBelong2X]] = .x
    # map_pc[[.x]] =  seq_cssnames2check[whichBelong2X]
    # seq_cssnames2check |> setdiff(whichBelong2X) -> seq_cssnames2check
    # if(length(seq_cssnames2check)==0) break
  }
  names(map_pc) <- names(split_css)
  return(map_pc)
}
correctMargin_targetAL_childX <- function(children_css, targetAL_children) {
  numberOfChildren <- length(targetAL_children)
  if(numberOfChildren==1){
    children_css[[1]]$inside_autoLayout$margin <- NULL
    return(children_css)
  }
  for(.x in seq_along(targetAL_children)){
    children_css[[.x]]$inside_autoLayout$margin |>
      stringr::str_extract_all(
        "[0-9]+"
      ) |>
      unlist() |>
      as.numeric() -> margin_old
    margin_new = margin_old/numberOfChildren
    paste0(margin_new,"px") |>
      paste(collapse = " ") ->
      children_css[[.x]]$inside_autoLayout$margin
  }
  return(children_css)
}
correct_insideAutoLayoutMargin <- function(split_css, pc_map) {
  pc_map |> purrr::map_lgl(
    ~{!is.null(.x)}
  ) -> pick_hasChildren
  seq_along(pc_map) |> purrr::map_lgl(
    ~{
      !is.null(split_css[[.x]]$auto_layout)
    }
  ) -> pick_autolayout

  autolayoutChildrenLocations <-
    pc_map[pick_autolayout]

  # correct margin
  for(.x in seq_along(autolayoutChildrenLocations)){
    autolayoutChildrenLocations[[.x]] ->
      targetAL_children
    split_css[targetAL_children] |>
      correctMargin_targetAL_childX(targetAL_children) ->
      split_css[targetAL_children]
  }
  return(split_css)
}
correct_insideAutoLayoutConstraint <- function(split_css, pc_map) {
  seq_along(pc_map) |> purrr::map_lgl(
    ~{
      !is.null(split_css[[.x]]$auto_layout)
    }
  ) -> pick_autolayout

  whichIsAutolayout <- which(pick_autolayout)
  split_css[whichIsAutolayout] ->
    css_autolayout
  for(.x in whichIsAutolayout){
    split_css[[.x]]$auto_layout$`flex-direction` -> flex_direction
    pc_map[[.x]] ->
      children_cssLocations
    for(.y in children_cssLocations){
      flag_alignSelf <- split_css[[.y]]$inside_autoLayout$`align-self` =="stretch"
      if(isTRUE(flag_alignSelf)){
        switch(
          flex_direction,
          "row"={
            split_css[[.y]]$remain$height <- NULL
            split_css
          },
          "column"={
            split_css[[.y]]$remain$width <- NULL
            split_css
          }
        ) -> split_css
      }
    }

  }
  return(split_css)
}
rename_split_css <- function(split_css) {
  cssnames <- names(split_css)
  tb_cssnames <- table(cssnames)
  tb_cssnames |>
    subset(tb_cssnames!=1) ->
    tbDup_cssnames
  if(length(tbDup_cssnames)==0) return(split_css)
  dupIndices = c()
  for(.x in seq_along(tbDup_cssnames)){
    tbDup_cssnames[.x] |>names() -> namex
    whichIsDup <- which(
      cssnames==namex
    )
    paste(
      namex, c("", 1:(tbDup_cssnames[[.x]]-1)), sep="") ->
      names(split_css)[whichIsDup]

    dupIndices=c(dupIndices, whichIsDup)
  }
  split_css =
    append(split_css[-dupIndices], split_css[dupIndices])
  return(split_css)
}
