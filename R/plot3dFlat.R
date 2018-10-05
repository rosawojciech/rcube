#' Plotting cube in 2D
#'
#' Plotting cube in 2D, but holds 3D advantages
#'
#' @param cube - cube object
#' @return plot
#'
#' @examples
#' cube <- createCube()
#' plot3dFlat(cube) # generates plot of solved cube
#' # Plotting 'checkerboard' pattern using pipe:
#' require(magrittr)
#' createCube() %>% twistCube("(LLFFRRBB) x3") %>% plot3dFlat()
#' @export

plot3dFlat <- function(cube) {
  N <- cube$size
  plot(NULL,xlim = c(-2*N,2*N), ylim = c(-(2*N+1),2*N-1),asp = 1,axes = FALSE,xlab = "",ylab = "")

  k <- 0
  top <- t(cube$cube[N:1,(2*N):(N+1)])
  for(i in 0:(N-1)) for(j in 0:(N-1))
  { k <- k +1
  draw_facelet4(i/(4/3)-j,i/(4/3),cube$scheme[c(top)][k])
  }
  k <- 0
  front <- cube$cube[(2*N):(N+1),(N+1):(2*N)]
  for(i in (-N+1):0) for(j in (-N+1):0) {
    k <- k+1
    draw_facelet5(i,j-1,cube$scheme[c(front)][k])}
  k <- 0
  left <- cube$cube[(2*N):((N+1)),1:N]
  for(i in (-N+1):0) for(j in (-N+1):0) {
    k <- k+1
    draw_facelet5(i-N,j-1,cube$scheme[c(left)][k])}

  k <- 0
  down <- cube$cube[(3*N):((2*N+1)),(N+1):(2*N)]
  for(i in (-N+1):0) for(j in (-N+1):0) {
    k <- k+1
    draw_facelet5(i,j-1-N,cube$scheme[c(down)][k])}

  k <- 0
  right <- cube$cube[(2*N):((N+1)),(2*N+1):(3*N)]
  for(i in 0:(N-1)) for(j in 0:(N-1))
  { k <- k +1
  draw_facelet6(i/(4/3)+1,i/(4/3)+j-N,cube$scheme[c(right)][k])
  }

  k <- 0
  bottom <- cube$cube[(2*N):((N+1)),(3*N+1):(4*N)]
  for(i in (-N+1):0) for(j in (-N+1):0) {
    k <- k+1
    draw_facelet5(i+1.75*N,j-1+N/(4/3),cube$scheme[c(bottom)][k])}

}

