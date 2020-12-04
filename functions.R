#' Determine if the given integer is even or odd
#'
#' @param number Integer
#'
#' @return list(parity = "even" or "odd")
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
#' @return list(response = character)
#' @export
#'
#' @examples
#' hello()
hello <- function() {
  list(response = paste("Hello from", version$version.string))
}

#' Wait 5 seconds, then return the time
#'
#' @return list(time = double)
#'
#' @examples
#' wait()
wait <- function() {
  Sys.sleep(5)
  list(time = Sys.time())
}
