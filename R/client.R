get_2ch_menu <- function(menu_url) {  
  a <- xml2::read_html(menu_url) %>%
    html_nodes("a")
  tibble(name = a %>% html_text(), url = a %>% html_attr("href")) %>%
    distinct(url, .keep_all = TRUE) %>%
    mutate(board_name= stringr::str_match(url, "/(\\w+)/$") %>% `[`(,2))
}

read_url <- function(server_url, board_name, subpath = "") {
  
  stringr::str_c("http://2ch.sc/test/read.cgi", board_name, subpath, sep = "/")
}

parse_thread_title <- function(title) {
  stringr::str_match(title, "^(\\d): (.*) \\((\\d+)\\)$")
}

get_2ch_threads <- function(board_name) {
  url <- get_2ch_menu() %>% filter(board_name== !!board_name) %>% `$`(url)
  if (identical(url, character(0))) {
    stop(stringr::str_c(board_name, " is not exists."))
  }
  a <- xml2::read_html(stringr::str_c(url, "subback.html")) %>%
    html_nodes("small#trad > a")
  tibble(subpath = a %>% html_attr("href") %>% stringr::str_remove("/l\\d+$"),
         url = read_url(board_name, subpath),
         title = a %>% html_text()) %>%
    select(title, url, subpath)
}

read_2ch_thread <- function(url) {
  text <- xml2::read_html(url)
  tibble(
    dt = text %>% html_nodes("dl.thread > dt") %>% html_text(),
    dd = text %>% html_nodes("dl.thread > dd") %>% html_text()
  )
}

NchClient <- R6Class("NchClient", list(
  initialize = function(server_url = "http://2ch.sc") {
    ## stopifnot
    self$server_url = server_url
  },
  get_menu = function() {
    if (is.null(self$menu)) {
      self$menu = get_2ch_menu(self$menu_url)
    }
    self$menu
  }
))

## Local Variables:
## ess-r-package--project-cache: (nchr . "~/Projects/Rpackages/nchr/")
## End:
