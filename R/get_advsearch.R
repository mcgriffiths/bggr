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
