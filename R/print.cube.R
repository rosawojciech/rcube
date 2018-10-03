#' Printing cube
#'
#' Prints cube in console
#'
#' @param x - cube object
#' @param ... - not used
#' @return plain text
#'
#' @examples
#' cube <- createCube()
#' print(cube)
#'
#' @export
#' @export
print.cube <- function(x, ...)
  {
  N <- x$size
  cat(paste0("Cube size ",N,"x",N,"x",N))

}
