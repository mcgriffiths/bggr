#' Get Data From BGG Game Page
#'
#' @param id character vector of BGG game IDs
#'
#' @return data frame of basic information with one line for each game
#' @export
#'
#' @examples
#' parse_games(c('1', '42'))
#' @importFrom rlang .data
parse_games <- function(id){
  games_xml <- httr::GET('https://boardgamegeek.com',
                    path = '/xmlapi2/thing',
                    query = list('id' = paste(id, collapse = ','),
                                 'stats' = '1'),
                    httr::accept_xml())

  items <- httr::content(games_xml) %>%
    xml2::xml_find_all('item')

  attrs <- c('yearpublished',
             'minplayers',
             'maxplayers',
             'playingtime',
             'minplaytime',
             'maxplaytime',
             'minage',
             'statistics/ratings/usersrated',
             'statistics/ratings/average',
             'statistics/ratings/bayesaverage',
             'statistics/ratings/stddev',
             'statistics/ratings/owned',
             'statistics/ratings/numweights',
             'statistics/ratings/averageweight',
             'statistics/ratings/ranks/rank[@name="boardgame"]'
  )

  col_names <- stringr::str_replace(attrs, 'statistics/ratings/','')
  col_names[15] <- 'rank'
  col_names <- c('id', col_names)

  games_data <- purrr::map(attrs, ~items %>%
                      xml2::xml_find_all(paste0("//",.x)) %>%
                      xml2::xml_attr('value') %>%
                      as.numeric()) %>%
    dplyr::bind_cols(id = id, .data) %>%
    purrr::set_names(col_names)

  games_data
}
