#' Weights attached to source vertices and destination vertices.
#'
#' A dataset containing the weights attached to traversing from sources to destinations.
#'
#' @format A data frame with 18 rows and 3 variables:
#' \describe{
#'   \item{v1}{id of the source vertices}
#'   \item{v2}{id of the destination vertices}
#'   \item{w}{weight of the path taken}
#'   ...
#' }
#' @source \url{https://en.wikipedia.org/wiki/Dijkstra\%27s_algorithm}
"wiki_graph"
