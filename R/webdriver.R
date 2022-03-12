#' chromedriver webdrive session starter
#'
#' @return an environment with $start_session() to start a webdriver session connected to chromedriver; with $kill_chrome() to kill chromedriver process.
#' @export
#'
#' @examples none.
webdriverChromeSession <- function() {
  sessionNew = new.env()
  sessionNew$p <- processx::process$new("chromedriver", stdout="|")
  sessionNew$kill_chrome = sessionNew$p$kill
  # sessionNew$p$read_output() -> stdout

  # browser()

  sessionNew$start_session = function(){
    webdriver::Session$new(
      port=get_port(
        sessionNew$p$read_output()
      )) -> sessionx
    allmethods = names(sessionx)
    purrr::walk(
      allmethods,
      ~assign(.x, sessionx[[.x]], envir = sessionNew
    ))
  }
  sessionNew}

get_port <- function(stdout) {
  stdout |>
    stringr::str_extract("(?<=(on\\sport\\s))[0-9]+") |> as.integer() -> port
  port
}
# {  names(session) -> allmethods
#   sessionNew = new.env()
#   # .x=allmethods[[1]]
#   # sessionNew$go("https://www.ntpu.edu.tw")
#   sessionNew$kill_chromedriver = p$kill
#   return(sessionNew)
# }
#
