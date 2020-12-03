#' Determine if the given integer is even or odd
#'
#' @param number Integer
#'
#' @return "even" or "odd"
#' @export
#'
#' @examples
#' parity(3) # odd
#' parity(4) # even
parity <- function(number) {
  list(parity = if (as.integer(number) %% 2 == 0) "even" else "odd")
}

#' A nullary function that returns the current version of R
#'
#' @return character
#' @export
#'
#' @examples
#' hello()
hello <- function() {
  list(response = paste("Hello from", version$version.string))
}
