#' Plotting cube in 3D
#'
#' Plotting cube in 3D
#'
#' @param cube - cube object
#' @param sides - string parameter determining which side of cube should be plotted, correct values are: top, bottom, and both (default).
#' @param rotate - string defaulting initial rotating of cube. Correct are strings containing characters: o, O, p, P. Default is 'O'
#' @return plot
#'
#' @examples
#' cube <- createCube()
#' plot3dCube(cube) # generates plot of solved cube
#' cube <- twistCube(cube,"(LLFFRRBB) x3")
#' plot3dCube(cube) # plotted 'chess' pattern
#'
#' @export
plot3dCube <- function(cube, sides = "both", rotate = "O") {
  ocube <- cube
  cube <- twistCube(cube,rotate)
  if(sides == "both") par(mfrow=c(1,2))
if(sides %in% c("top","both"))  plot3Dtop(cube)
   if(sides %in% c("bottom","both") ) plot3Dbottom(cube)
  return(ocube)
}

