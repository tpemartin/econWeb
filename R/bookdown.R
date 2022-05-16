#' Initiate Bookdown yml builder
#'
#' @return an environment
#' @export
#'
Bookdown <- function(){
  bookdown <- new.env()
  rstudioapi::getActiveProject() |> basename() -> projectName
  bookdown$current_bookdownYML <-
    get_current_bookdownYML()
    # list(
    #   book_filename=projectName,
    #   delete_merged_file=TRUE,
    #   language=list(
    #     ui=list(
    #       chapter_name="Chapter "
    #     )
    #   )
    # ) |> purrr::keep(function(.x){!is.null(.x)})
  # bookdown$current_bookdownYML |>
  #   yaml::as.yaml() |> clipr::write_clip()
  bookdown$book_filename <- book_filename(bookdown)
  bookdown$delete_merged_file <- delete_merged_file(bookdown)
  bookdown$before_chapter_script <- before_chapter_script(bookdown)
  bookdown$after_chapter_script <- after_chapter_script(bookdown)
  bookdown$output_dir <- output_dir(bookdown)
  bookdown$create_bookdown_yml <- create_bookdownYML(bookdown)
  bookdown$rmd_files <- rmd_files(bookdown)
  # bookdown$rmd_dir <- rmd_dir(bookdown)

  return(bookdown)
}


# helpers -----------------------------------------------------------------


component_generator <- function(component_name, input_name){
  function(bookdown){
    glue::glue("function(<<input_name>>){
      bookdown$current_bookdownYML$<<component_name>>=<<input_name>>
    }", .open="<<", .close=">>") -> funexpr_text
    rlang::parse_expr(funexpr_text) -> funexpr
    rlang::eval_bare(funexpr)
  }
}
rmd_dir <- component_generator("rmd_dir","rmd_dir")


book_filename <- function(bookdown){function(book_filename=NULL){
  if(is.null(book_filename)){
    rstudioapi::getActiveProject() |> basename() -> book_filename
  }
  bookdown$current_bookdownYML$book_filename =  book_filename
}}
delete_merged_file <- function(bookdown){function(flag_delete=T){
  bookdown$current_bookdownYML$delete_merged_file <- flag_delete
}}
before_chapter_script <- function(bookdown){function(Rscript_files){
  bookdown$current_bookdownYML$before_chapter_script = Rscript_files
}}
after_chapter_script <- function(bookdown){function(Rscript_files){
  bookdown$current_bookdownYML$after_chapter_script = Rscript_files
}}
output_dir <- function(bookdown){function(output_dir){
  bookdown$current_bookdownYML$output_dir = output_dir
}}
create_bookdownYML <- function(bookdown){function(){
  file.rename(
    from = "_bookdown.yml",
    to = "_bookdown_bakcup.yml"
  )
  bookdown$current_bookdownYML |>
    yaml::as.yaml() |>
    xfun::write_utf8("_bookdown.yml")
}}
bookdownInGlobalEnv <- function(){
  .GlobalEnv$bookdown <- Bookdown()
}
get_current_bookdownYML <- function(){
  yaml::read_yaml(file = "_bookdown.yml")
}
rmd_files <- function(bookdown){function(rmd_files){
  .rmd_files = bookdown$current_bookdownYML$rmd_files
  if(is.null(.rmd_files)){
    .rmd_files <- rmd_files
  } else {
    .rmd_files <- unique(c(.rmd_files, rmd_files))
  }

  .rmd_files -> bookdown$current_bookdownYML$rmd_files
}}
