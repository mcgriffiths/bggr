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
  if(nrow(result) < 100){
    result
  } else {
    for(i in 2:50){
      page_result <- get_advsearch_page(params, i)
      result <- rbind(result, page_result)
      if(nrow(page_result) < 100){
        break
      } 
      Sys.sleep(2)
    }
    dplyr::distinct(result)
  }
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
#' get_advsearch_page(params = list("q" = "Catan"), page = "1")
get_advsearch_page <- function(params, page) {
  url <- httr::modify_url("https://boardgamegeek.com", path = paste0("/search/boardgame/page/", page))
  query <- list('action' = 'search', 'advsearch' = '1', 'objecttype' = 'boardgame')
  query <- c(query, params)

  resp <- httr::GET(url, query = query)
  if (httr::http_type(resp) != "application/json") {
    stop("API did not return json", call. = FALSE)
  }
  

  parsed <- jsonlite::fromJSON(httr::content(resp, "text"), simplifyVector = TRUE)

  parsed$items
}
