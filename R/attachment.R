generate_export_fig <- function(attachmentSrc){
  attachment_dependencyTxt <-
    generate_attachmentDependency(attachmentSrc)
  function(fig, tagname="mycard") {
    fig$ui() -> fig_ui
    tagbasename =
      stringr::str_remove(
        basename(tagname), ".css$")

    cssfilename = paste0(tagbasename,".css")
    tagdirname = dirname(tagname)
    if(!dir.exists(tagdirname))
      dir.create(tagdirname, recursive = T)
    cssfilepath=file.path(
      tagdirname, cssfilename
    )

    fig$csstext |>
      xfun::write_utf8(file.path(".", cssfilename))
    tagname=tagbasename
    tag_ui <- fig_ui[[1]]
    fig_ui[[1]] |>
      as.character() -> htmlStr
    htmlStr |>
      generate_tagUiText() -> tag_uiText
    tag_uiText |> clipr::write_clip()
    # tag_uiText |> clipr::write_clip()
    glue::glue("tag_dependency <- function(){
      htmltools::htmlDependency(
        name=\"<<tagname>>\",
        version=\"1.0.0\",
        src=c(file=normalizePath(\"./<<tagdirname>>\")),
        style=\"<<cssfilename>>\",
        all_files = F
      )}", .open="<<", .close=">>") -> tag_dependencyTxt
    # dep_text |> clipr::write_clip()



    if(is.null(attachmentSrc)){
"ui <- function(){
  tagList(tag_ui(), tag_dependency())
}" -> uiText
      c(tag_uiText, tag_dependencyTxt, uiText, "ui() |> econWeb::browseTag2()") |>
        paste(collapse = "\n") -> txt4clipboard
    } else {
      "ui <- function(){
      tagList(tag_ui(), tag_dependency(), attachment_dependency())
    }" -> uiText
      c(tag_uiText, tag_dependencyTxt, attachment_dependencyTxt, uiText, "ui() |> econWeb::browseTag2()") |>
        paste(collapse = "\n") -> txt4clipboard
    }
    txt4clipboard |> clipr::write_clip()
  }
}
generate_attachmentDependency <- function(attachmentSrc){
  if(!is.null(attachmentSrc)){
    glue::glue("attachment_dependency <- function(){
    htmltools::htmlDependency(
      name=\"attachment\",
      version=\"1\",
      src=c(file=normalizePath(\"<<attachmentSrc>>\")),
      attachment=\"\",
      all_files = T
    )}", .open="<<", .close=">>") -> attachmentDependencyTxt
  } else {
    NULL-> attachmentDependencyTxt
  }
  return(attachmentDependencyTxt)
}
