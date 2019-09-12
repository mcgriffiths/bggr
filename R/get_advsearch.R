#' Retrieve BGG Advanced Search Results
#'
#' @param params A named list containing the parameters for the search.
#'
#' @return A data.frame containing the query results.
#' @export
#'
#' @examples
#' get_advsearch(list("q" = "Catan"))
get_advsearch <- function(params) {
  result <- get_advsearch_page(params, 1)
  for(i in 2:100){

    page_result <- get_advsearch_page(params, i)
    if(length(page_result) > 0){
      result <- rbind(result, page_result)
    } else {
      break
    }
    Sys.sleep(2)
  }
  result
}

#' Retrieve Page of Advanced Search Results
#'
#' @param params A named list containing the parameters for the search.
#' @param page The page number of search results to retrieve.
#'
#' @return A data.frame containing the query results.
#' @export
#'
#' @examples
get_advsearch_page <- function(params, page) {
  url <- httr::modify_url("https://boardgamegeek.com", path = paste0("/search/boardgame/page/", page))
  query <- list('action' = 'search', 'advsearch' = '1', 'objecttype' = 'boardgame')
  query <- c(query, params)

  resp <- GET(url, query = query)
  if (http_type(resp) != "application/json") {
    stop("API did not return json", call. = FALSE)
  }

  parsed <- jsonlite::fromJSON(content(resp, "text"), simplifyVector = TRUE)

  parsed$items
}

bgg_xml <- GET('https://boardgamegeek.com',
    accept_xml(),
    path = '/xmlapi2/thing',
    query = list('id' = paste(10001:11000, collapse = ','),
                 'stats' = '1'))


url <- modify_url('https://boardgamegeek.com',
                  path = '/xmlapi2/thing',
                  query = list('id' = paste(1001:2323, collapse = ','),'stats' = '1')
                  )

items <- content(bgg_xml) %>%
  xml_find_all('item')
