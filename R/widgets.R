#' Scaffold widget from web ui module files
#'
#' @param widgetname a character
#' @param modulename a character (default=NULL) representing "R/modulename.R" module file. If omit, will look for "R/widgetname.R" as the module file and change it to "R/widgetname_support.R" to avoid widget file name conflict.
#'
#' @return none
#' @export
#'
#' @examples none
createWidget = function(widgetname, modulename=NULL){

  projectname = rstudioapi::getActiveProject() |> basename()
  if(is.null(modulename)){
    fromname=file.path("R",
      paste0(widgetname,".R")) |>
      normalizePath()
    file.rename(
      from=fromname,
      to=stringr::str_replace(
        fromname, ".R$", "_support.R")
    )
    modulename=paste0(widgetname,"_support")
  }
  htmlwidgets::scaffoldWidget(name=widgetname)

  copyDependenciesFromAssets(projectname)
  generate_widgetYAML(
    projectname=projectname,
    modulename=modulename
  ) -> moduleYAML
  moduleYAML |>
    xfun::write_utf8(
      con=file.path(
        "inst/htmlwidgets",
        paste0(widgetname, ".yaml"))
    )
}

copyDependenciesFromAssets <- function(projectname=NULL) {
  if(is.null(projectname)) rstudioapi::getActiveProject() |> basename() -> projectname
  widgetsAssetsFolder = file.path(
    normalizePath("./inst/htmlwidgets"),
    projectname)
  dir.create(
    widgetsAssetsFolder, recursive = T)
  fromdir = normalizePath("./assets")
  fromdirfiles = list.files(fromdir, recursive = T)
  todir = widgetsAssetsFolder
  tofiles = file.path(widgetsAssetsFolder, fromdirfiles)
  for(.x in seq_along(fromdirfiles)){
    todirX=dirname(tofiles[[.x]])
    if(!dir.exists(todirX)) dir.create(todirX, recursive = T)
    file.copy(
      from=file.path(fromdir, fromdirfiles[[.x]]),
      to = dirname(tofiles[[.x]]),
      overwrite = T
    )
  }
}
generate_widgetYAML <- function(projectname=NULL, modulename=NULL) {
  if(is.null(projectname)) rstudioapi::getActiveProject() |> basename() -> projectname
  if(is.null(modulename)) {
    allObjs=ls("package:usrApp")
  } else {
    tempenv = new.env()
    source(
      file.path("R", paste0(modulename, ".R")),
      local=tempenv
    )
    allObjs = ls(envir=tempenv)
  }

  list_deps = stringr::str_subset(allObjs, "_dependency$")

  purrr::map(
    list_deps,
    ~{
      callX <- call(.x)
      eval(callX)}
  ) -> allDependencies
  allDependencies |> purrr::map(class) |>
    purrr::map_lgl(~any(.x=="html_dependency")) -> pick_htmlDependency
  allDependencies[!pick_htmlDependency] |>
    purrr::flatten() -> allDependenciesI
  allDependenciesI |>
    append(allDependencies[pick_htmlDependency]) ->
    allDependenciesII

  allDependenciesII |>
    htmltools::resolveDependencies() -> uniqueDependencies

  for(.i in seq_along(uniqueDependencies)){
    purrr::map_lgl(
      uniqueDependencies[[.i]],
      ~{is.null(.x)}
    ) -> pick_null
    uniqueDependencies[[.i]] <- uniqueDependencies[[.i]][!pick_null]
    assetspath = normalizePath("assets")
    widgetPath = file.path("htmlwidgets", projectname)
    uniqueDependencies[[.i]]$src$file |>
      stringr::str_replace(assetspath, widgetPath) ->
      uniqueDependencies[[.i]]$src
  }
  yaml_dependencies =
    list(dependencies=uniqueDependencies) |>
    yaml::as.yaml()
}
