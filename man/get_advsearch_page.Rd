% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_advsearch.R
\name{get_advsearch_page}
\alias{get_advsearch_page}
\title{Retrieve Page of Advanced Search Results}
\usage{
get_advsearch_page(params, page)
}
\arguments{
\item{params}{A named list containing the parameters for the search.}

\item{page}{The page number of search results to retrieve.}
}
\value{
A data.frame containing the query results.
}
\description{
Retrieve Page of Advanced Search Results
}
\examples{
get_advsearch_page(params = list("q" = "Catan"), page = "1")
}
