#' Get 2ch menu from 'bbsmenu.html'
#'
#' @param server_url A string.
#' @param encoding A string.
#' @return The menu object.
#' @examples
#' get_2ch_menu("http://2ch.sc")
get_2ch_menu <- function(server_url, encoding = "Shift_JIS") {
  menu_url <- stringr::str_c(server_url, "bbsmenu.html", sep = "/")
  a <- xml2::read_html(menu_url, encoding) %>%
    html_nodes("a")
  tibble(
    name = a %>% html_text(),
    url = a %>% html_attr("href")) %>%
    distinct(url, .keep_all = TRUE) %>%
    mutate(board_id = stringr::str_match(url, "/(\\w+)/$") %>% `[`(,2))
}

make_read_url <- function(server_url, board_id, thread_id = "") {
  stringr::str_c(server_url, "test/read.cgi", board_id, thread_id, sep = "/")
}

parse_thread_title <- function(title) {
  stringr::str_match(title, "^\\d: (.*) (\\[\\d+\\] )?\\((\\d+)\\)$")[1,]
}

get_2ch_threads <- function(menu, board_id, encoding = "Shift_JIS") {
  board_url <- menu %>% filter(board_id == !!board_id) %>% `$`(url)
  if (identical(board_url, character(0))) {
    stop(stringr::str_c("Board '", board_id, "' is not exists."))
  }

  server_url <- stringr::str_c("http://", urltools::domain(board_url))

  a <- xml2::read_html(stringr::str_c(board_url, "subback.html"), encoding) %>%
    html_nodes("small#trad > a")
  tibble(thread_id = a %>% html_attr("href") %>% stringr::str_remove("/l\\d+$"),
         url = make_read_url(server_url, board_id, thread_id),
         title = a %>% html_text()) %>%
    select(title, url, thread_id)
}

read_2ch_thread <- function(url) {
  text <- xml2::read_html(url)
  tibble(
    dt = text %>% html_nodes("dl.thread > dt") %>% html_text(),
    dd = text %>% html_nodes("dl.thread > dd") %>% html_text()
  )
}


#' nch clinet R6 class
#'
#' @docType class
#' @export
#' @format An \code{\link{R6Class}} generator object
#' @keywords data
#' @importFrom tidyr unnest
NchClient <- R6Class("NchClient",
  public = list(
    server_url = NULL,
    initialize = function(server_url = "http://2ch.sc") {
      ## stopifnot
      self$server_url = server_url
    },
    threads = function(board_id) {
      threads <- private$.threadsCache %>% filter(board_id == !!board_id)
      if (nrow(threads) == 0) { ## not cached
        th <- get_2ch_threads(self$menu, board_id)
        private$.threadsCache =
          add_row(private$.threadsCache,
            board_id = board_id,
            threads = list(th))
      }
      private$.threadsCache %>% filter(board_id == !!board_id) %>% unnest
    }
  ),
  active = list(
    menu = function(value) {
      if (missing(value)) {
        if (is.null(private$.menuCache)) {
          private$.menuCache = get_2ch_menu(self$server_url)
        }
        private$.menuCache
      } else {
        stop("Can't set `$menu`", call. = FALSE)
      }
    }
  ),
  private = list(
    .menuCache = NULL,
    .threadsCache = tibble(board_id = character(0), threads = list(tibble()))
  )
)
