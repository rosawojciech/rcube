#' Scrambling cube
#'
#' Scrambling cube
#'
#' @param cube - cube object to scramble
#' @param times - how many random moves should be done on cube. Default is 0 which means N*10 moves where N is the size of the cube.
#' @return cube
#'
#' @examples
#' cube <- createCube()
#' set.seed(1)
#' cube <- scramble(cube)
#' bigcube <- createCube(N = 15)
#' set.seed(1)
#' bigcube <- scramble(bigcube)
#' @export
scramble <- function(cube,times = 0) {
  N <- cube$size
  if(times == 0) times <- 10*N
  mo <- c("R","L","U","D","F","B","r","l","u","d","f","b")
  mov <- sample(mo,size = times,replace = T)
  move <- paste0(mov,sample(1:(N-1),size = times, replace = T),collapse = " ")
  cube <- twistCube(cube,move)
  return(cube)
}

