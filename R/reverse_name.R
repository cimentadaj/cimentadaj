
#' reverse_name
#'
#' @param x a named vector
#'
#' @return same vector with the vector names as elements and elements as names
#' @export
#'
#' @examples reverse_name(c(a = 'b', c = 'b'))
reverse_name <- function(x) {
  stats::setNames(names(x), x)
}
