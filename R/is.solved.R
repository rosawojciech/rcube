#' Testing if cube is solved
#'
#' Function returns TRUE if cube is solved (each side contains exactly one colour) and FALSE otherwise.
#'
#' @param cube - cube object to be tested
#' @return TRUE/FALSE
#'
#' @examples
#' ## Create new cube:
#' cube <- createCube(3)
#' ## And it is solved:
#' is.solved(cube) # TRUE
#' ## Now, test how many times repeating LFRB moves will bring back initial state:
#' cube <- twistCube(cube,'LFRB')
#' i <- 1
#' while(!is.solved(cube))
#' {
#' cube <- twistCube(cube,'LFRB')
#' i <- i + 1
#' }
#' print(i) # 315
#' ## Check one more time if this is a solution:
#' is.solved(twistCube(cube,'LFRB',315)) # TRUE
#' ## Check if really 314 moves and 316 moves don't give solution:
#' is.solved(twistCube(cube,'LFRB',314)) || is.solved(twistCube(cube,'LFRB',316)) # FALSE
#' @export
is.solved <- function(cube){
  kostka <- cube$cube
  N <- sqrt(length(kostka)/3)/2
  suma <- 0
    for(i in 1:3) #wiersz
      for(j in 1:4) #kolumna
  {
        tmp <- kostka[((i-1)*N+1):(i*N),((j-1)*N+1):(j*N)]
        suma <- suma + sum(abs(tmp - median(tmp)))
  }
  return(suma==0)
}
