fix_autolayout_itemWidthHeight <- function(list_css){
  list_css |>
    purrr::map_lgl(
      ~{
        any(stringr::str_detect(.x, "/\\* Auto layout \\*/"))
      }
    ) -> pick_autoLayout
  whichIsAutoLayout <- which(pick_autoLayout)

  for(.x in whichIsAutoLayout){
    list_css <- updateCSS_autoLayout(list_css, .x)
  }
  list_css
}
updateCSS_autoLayout <- function(list_css, loc_autolayout) {
  css_AL = list_css[[loc_autolayout]]
  css_AL |>
    get_flexDirection() -> flex_direction


  flag_insideAL = T
  iter=0
  max_iter=length(list_css)-loc_autolayout
  cssX=list_css[[loc_autolayout+iter+1]]
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

    cssX -> list_css[[loc_autolayout+iter+1]]

    iter=iter+1
    if(loc_autolayout+iter+1 > length(list_css)){
      flag_insideAL = F

    } else {
      list_css[[loc_autolayout+iter+1]] -> cssX
      cssX |>
        stringr::str_detect("/\\* Inside auto layout \\*/") |> any() -> flag_insideAL
    }

  }
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
      cssX[[whichIsHeight]] <- NA
      cssX |> na.omit() -> cssX
      cssX
    },
    "row"={
      cssX |>
        get_styleLocation("width") -> whichIsWidth
      cssX[[whichIsWidth]] <- NA
      cssX |> na.omit() -> cssX
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
