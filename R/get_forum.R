#' Retrieve BGG Advanced Search Results
#'
#' @param forum_id The forum to retrieve.
#'
#' @return A data.frame containing the query results.
#' @export
#'
#' @examples
#' 
get_forum <- function(forum_id) {
  forum_xml <- httr::GET('https://boardgamegeek.com',
                         path = '/xmlapi2/forum',
                         query = list('id' = forum_id),
                         httr::accept_xml())
  
  num_threads <- httr::content(forum_xml) %>%
    xml2::xml_find_all('//forum') %>%
    xml2::xml_attr('numthreads') %>%
    as.numeric
  
  num_pages <- ceiling(num_threads/50)
  
  purrr::map(1:num_pages, ~get_forum_page(forum_id, .)) %>%
    purrr::map_df(dplyr::bind_rows)
}

#' Retrieve Page of Advanced Search Results
#'
#' @param forum_id The forum to retrieve.
#' @param page The page number  to retrieve.
#'
#' @return A data.frame containing the query results.
#' @export
#'
#' @examples
#' get_forum_page(forum_id = "8", page = "1")
get_forum_page <- function(forum_id, page) {
  print(page)
  forum_xml <- httr::GET('https://boardgamegeek.com',
                         path = '/xmlapi2/forum',
                         query = list('id' = forum_id,
                                      'page' = page),
                         httr::accept_xml())
  
  items <- httr::content(forum_xml) %>%
    xml2::xml_find_all('//thread')
  
  thread_data <- xml2::xml_attrs(items) %>%
    purrr::map_df(dplyr::bind_rows)
  
  thread_data
}
