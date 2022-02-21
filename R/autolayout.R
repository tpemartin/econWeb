fix_autolayout_itemWidthHeight <- function(list_css){
  list_css |>
    purrr::map_lgl(
      ~{
        any(stringr::str_detect(.x, "/\\* Auto layout \\*/"))
      }
    ) -> pick_autoLayout
  whichIsAutoLayout <- which(pick_autoLayout)

  for(.x in whichIsAutoLayout){
    # if(.x %in% c(1, 12, 22)) browser()
    tryCatch(
      {updateCSS_autoLayout(list_css, .x)
      },
      error=function(e){
        warning(.x)
        list_css
      }
    ) -> list_css
    # list_css <- updateCSS_autoLayout(list_css, .x)
  }
  list_css
}
updateCSS_autoLayout <- function(list_css, loc_autolayout) {
  css_AL = list_css[[loc_autolayout]]
  css_AL |>
    get_flexDirection() -> flex_direction

  classname_AL = names(list_css)[[loc_autolayout]]

  list_css |>
    names() |>
    stringr::str_which(
      paste0(classname_AL,"-[^\\-]*$")
    ) -> whichIsItem

  assertthat::assert_that(
    length(whichIsItem)!=0,
    msg=glue::glue("{classname_AL}'s does not have any autolay out items.")
  )
  list_css_nested = list_css[whichIsItem]

  list_css_nested |>
    update_autolayoutItemMargins() ->
    list_css_nested
  flag_insideAL = T
  iter=0
  max_iter=length(list_css_nested)
  cssX=list_css_nested[[1]]
  while(flag_insideAL && iter < max_iter){

    cssX |>
      get_cssValueByKey("flex\\-grow") -> flex_grow
    if(length(flex_grow) !=0 && flex_grow!="0"){
      cssX |> update_cssX_forFlexGrow(flex_direction) -> cssX
    }

    cssX |>
      get_cssValueByKey("align\\-self") -> align_self
    if(length(align_self) !=0 && align_self=="stretch"){
      cssX |> update_cssX_forAlignSelf(flex_direction) -> cssX
    }

    cssX -> list_css_nested[[iter+1]]

    iter=iter+1
    if(iter+1 > length(list_css_nested)){
      flag_insideAL = F

    } else {
      list_css_nested[[iter+1]] -> cssX
      cssX |>
        stringr::str_detect("/\\* Inside auto layout \\*/") |> any() -> flag_insideAL
    }

  }
  list_css_nested -> list_css[whichIsItem]

  return(list_css)
}
get_cssValueByKey <- function(css, key){
  pattern = paste0(
    "(?<=", key, ":\\s)[^;]*"
  )
  stringr::str_extract(css, pattern)|>
    na.omit()
}
get_flexDirection <- function(css_AL){
  css_AL |>
    stringr::str_extract("(?<=flex\\-direction:\\s)(column|row)") |>
    na.omit()
}
get_styleLocation <- function(css, style) {
  stringr::str_which(
    css,
    glue::glue("\\b{style}\\b:")
  )
}
update_cssX_forFlexGrow <- function(cssX, flex_direction){
  switch(
    flex_direction,
    "column"={
      cssX |>
        get_styleLocation("height") -> whichIsHeight
      if(length(whichIsHeight)!=0){
      cssX[[whichIsHeight]] <- NA
      cssX |> na.omit() -> cssX
      }
      cssX
    },
    "row"={
      cssX |>
        get_styleLocation("width") -> whichIsWidth
      if(length(whichIsWidth)!=0){
        cssX[[whichIsWidth]] <- NA
        cssX |> na.omit() -> cssX
      }

      cssX
    }
  )
}
update_cssX_forAlignSelf <- function(cssX, flex_direction){
  switch(
    flex_direction,
    "row"={
      cssX |>
        get_styleLocation("height") -> whichIsHeight
      cssX[[whichIsHeight]] <- NA
      cssX |> na.omit() -> cssX
      cssX
    },
    "column"={
      cssX |>
        get_styleLocation("width") -> whichIsWidth
      cssX[[whichIsWidth]] <- NA
      cssX |> na.omit() -> cssX
      cssX
    }
  )
}
update_autolayoutItemMargins <- function(list_css_nested) {
  for(.x in seq_along(list_css_nested)){
    list_css_nested[[.x]]-> cssX
    if(.x==1){
      cssX |>
        get_cssValueByKey("margin") -> oldMargins
    }
    oldMargins |>
      obtain_newMargins(length(list_css_nested))-> newMargins

    cssX |> update_cssXmargins(newMargins) ->
      list_css_nested[[.x]]
  }
  return(list_css_nested)
}


obtain_newMargins <- function(oldMargin, len) {
  stringr::str_extract_all(unlist(oldMargin),"[0-9]+") |> unlist() |> as.numeric() -> margins
  margins/len -> newMargins
  paste0(newMargins, "px", collapse=" ") -> newMargins
  newMargins
}
update_cssXmargins <- function(cssX, newMargins){
  stringr::str_which(cssX,
    "\\bmargin\\b:") -> whichIsKey
  if(length(whichIsKey)!=0){
    cssX[whichIsKey] |>
      stringr::str_replace(
        "(?<=margin:\\s)[^;]+", newMargins
      ) -> cssX[whichIsKey]
  }
  return(cssX)
}
resolve_flexConflict <- function(list_css){
  for(.x in seq_along(list_css)){
    list_css[[.x]] |> resolve_flexConflict_cssX() -> list_css[[.x]]
  }
  return(list_css)
}

resolve_flexConflict_cssX <- function(cssX){
  # list_css$`cardContent-teacher-word` -> cssX

  if(any(stringr::str_detect(cssX, "display: flex;"))){
    stringr::str_which(cssX, "flex: none;") -> whichIsConflict
    if(length(whichIsConflict)!=0){
      cssX[whichIsConflict] <-
        paste0("/**", cssX[whichIsConflict],"**/")
    }

  }
  cssX

}
resolve_flexPositionConflict <- function(list_css){
  for(.x in 2:length(list_css)){
    # print(.x)
    # if(.x==13) browser()
    list_css[[.x]] |> resolve_flexConflict_cssX() -> list_css[[.x]]
    list_css[[.x]] |> resolve_flexPositionAbsolute_cssX() ->
      list_css[[.x]]
  }
  return(list_css)
}
resolve_flexPositionAbsolute_cssX <- function(cssX){
  flag_detectFont <- any(stringr::str_detect(cssX, "font.+:"))
  flag_detectLRTB <- any(stringr::str_detect(cssX, "(left|right|top|bottom): .+px"))
  flag_detectAbsolute <- any(stringr::str_detect(cssX, "position: absolute"))
  if((flag_detectFont || flag_detectLRTB) && flag_detectAbsolute){
    whichIsAbolute <- stringr::str_which(cssX, "position: absolute")
    cssX[whichIsAbolute] <-
      paste0("/**", cssX[whichIsAbolute],"**/")
  }
  return(cssX)


}
