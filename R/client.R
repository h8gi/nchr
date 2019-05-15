menu_url <- "http://2ch.sc/bbsmenu.html"

a <- read_html(menu_url) %>%
  html_nodes("a")

tibble(name = a %>% html_text(), url = a %>% html_attr("href")) %>%
  distinct(url, .keep_all = TRUE)

