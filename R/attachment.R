generate_export_fig <- function(attachmentSrc){
  attachment_dependencyTxt <-
    generate_attachmentDependency(attachmentSrc)
  function(fig, tagname="mycard", dependencyUseProjectname=TRUE) {
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
      xfun::write_utf8(file.path(".", cssfilepath))
    tagname=tagbasename
    tag_ui <- fig_ui[[1]]
    fig_ui[[1]] |>
      as.character() -> htmlStr
    htmlStr |>
      generate_tagUiText() -> tag_uiText
    stringr::str_replace(tag_uiText,
      "(?<=tag_)ui", tagname) -> tag_uiText

    if(is_rproject()
      && in_instFolder(tagdirname)
      && dependencyUseProjectname){
      projname = basename(rstudioapi::getActiveProject())
      sysfilepath = stringr::str_extract(tagdirname, "(?<=inst).*")
      srcfile_expr = rlang::expr(
        system.file(
          !!sysfilepath, package=!!projname
        )
      )
      depname = projname
    } else {
      srcfile_expr = rlang::expr(
        normalizePath(!!tagdirname)
      )
      depname = tagname
    }
    srcfile = paste(rlang::expr_deparse(srcfile_expr), collapse = "")
    glue::glue("<<tagname>>_dependency <- function(){
      htmltools::htmlDependency(
        name=\"<<depname>>\",
        version=\"1.0.0\",
        src=c(file=<<srcfile>>),
        style=\"<<cssfilename>>\",
        all_files = F
      )}", .open="<<", .close=">>") -> tag_dependencyTxt
    # dep_text |> clipr::write_clip()



    if(is.null(attachmentSrc)){
      glue::glue("ui_<<tagname>> <- function(dependency=NULL){
    tagList(tag_<<tagname>>(), <<tagname>>_dependency(), dependency)
  }", .open="<<", .close=">>") -> uiText
      c(tag_uiText, tag_dependencyTxt, uiText, glue::glue("ui_{tagname}() |> econWeb::browseTag2()")) |>
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
in_instFolder <- function(tagdirname) {
  filepath_pattern1 = file.path("\\.","inst")
  filepath_pattern2 = "inst"
  pattern = paste0("^(",filepath_pattern1,"|",filepath_pattern2, ")")
  tagdirname |>
    stringr::str_detect(pattern)
}
