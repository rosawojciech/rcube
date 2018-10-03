#' Plotting cube
#'
#' Plots cube in 2D
#'
#' @param x - cube object
#' @param ... - not used
#' @return plot
#'
#' @examples
#' cube <- createCube()
#' plot(cube)
#'
#' @export
plot.cube <- function(x, ...) {
 projekt <- x$cube
 kolory <- x$scheme
    wym_w <- dim(projekt)[1]
    wym_s <- dim(projekt)[2]
    par(mar=c(0,0,0,0))
    plot(1, type="n", axes=F, xlab="", ylab="",xlim = c(0.15,wym_s+0.35),ylim = c(0,wym_w+0.35),asp = 1)
    for(i in 1:(length(kolory))) {
      pkty <- as.data.frame(which(projekt==i,arr.ind = TRUE))
      pkty$row <- wym_w - pkty$row + 1
      if(nrow(pkty)>0)
        with(pkty, {
          ## Girth is diameter in inches
          symbols(col,row, squares = rep(1,times= nrow(pkty)), inches = FALSE,
                  bg = kolory[i], fg = "black",add= TRUE)
        })

    }
return(x)
    }
