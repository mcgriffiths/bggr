#' Get Comments From BGG Game Page
#'
#' @param id character vector of BGG game IDs
#'
#' @return data frame of comments
#' @export
#'
#' @examples
#'
#' @importFrom rlang .data
get_comments <- function(id){

  games_xml <- httr::GET('https://boardgamegeek.com',
                         path = '/xmlapi2/thing',
                         query = list('id' = paste(id, collapse = ','),
                                      'comments' = '1'),
                         httr::accept_xml())

  total_comments <- httr::content(games_xml) %>%
    xml2::xml_find_all('item/comments') %>%
    xml2::xml_attr('totalitems')

  pages <- max(ceiling(as.numeric(total_comments)/100))

  purrr::map_df(1:pages, ~get_comments_page(id, .))
}

#' Get Comments From BGG Game Page
#'
#' @param id character vector of BGG game IDs
#' @param page page number
#' @return data frame of comments
#' @export
#'
#' @examples
#'
#' @importFrom rlang .data
get_comments_page <- function(id, page){
  print(page)
  success <- F
  while(!success){
    games_xml <- httr::GET('https://boardgamegeek.com',
                         path = '/xmlapi2/thing',
                         query = list('id' = paste(id, collapse = ','),
                                      'comments' = '1',
                                      'page' = page),
                         httr::accept_xml())

    success <- games_xml$status_code == 200

  }

  items <- httr::content(games_xml) %>%
    xml2::xml_find_all('item/comments') %>%
    purrr::set_names(id)

  parse_item <- function(item){

    comments <- item %>%
      xml2::xml_find_all('comment')

    if(length(comments) == 0) {
      return(NULL)
    } else {

      comments_df <- comments %>%
        xml2::xml_attrs() %>%
        purrr::map_df(dplyr::bind_rows)

      return(comments_df)
    }
  }

  items %>%
    purrr::map_df(parse_item, .id = 'id')

}


#' Get comments for a given page of search results
#'
#' @param params parameters for advanced search
#' @param page page of advanced search
#'
#' @return comments dataframe for the search results
#' @export
#'
#' @examples
get_comments_search_page <- function(params, page){
  games <- get_advsearch_page(params, page)
  comments <- get_comments(games$objectid)
  comments <- games %>%
    select(objectid, name) %>%
    left_join(comments, by = c('objectid' = 'id')) %>%
    select(id = objectid, name, username, rating, comment = value) %>%
    as_tibble()
  return(comments)
}
