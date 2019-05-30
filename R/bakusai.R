#'
area_code_table <- 1:12
names(area_code_table) <- c(
  '北海道', '東北', '関東', '甲信越', '東海', '北陸',
  '関西', '山陽', '四国', '九州', '沖縄', '山陰'
)

area_menu <- function(acode) {
  if (is.character(acode)) {
    acode <- area_code_table[[acode]]
  }
  url <- stringr::str_c('http://bakusai.com/areamenu/acode=', acode)
  a <- xml2::read_html(url) %>% html_nodes('div.ctg_box > ul > li > a')
  tibble(href = a %>% html_attr('href'),
         title = a %>% html_text(trim = TRUE))
}

BakusaiClient <- R6Class('BakusaiClient',
  public = list(
    server_url = NULL,
    initialize = function(server_url = 'http://bakusai.com') {
      self$server_url = server_url
    }
  )
)

