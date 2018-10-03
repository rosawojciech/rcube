#' Translating notation
#'
#' Translating notation
#'
#' @param moves - cube object
#' @param from - Singmaster
#' @return moves
#'
#' @examples
#' cube <- createCube()
#' cube <- twistCube(cube, moves = translate("U R2 F B R B2 R U2 L B2 R U' D' R2 F R' L B2 U2 F2 "))
#' # Superflip pattern, https://en.wikipedia.org/wiki/Superflip
#' plot3dCube(cube)
#' @export
translate <- function(moves, from = "singmaster") {
  fr <- c("r'","l'","u'","d'","f'","b'",
          "r","l","u","d","f","b",
          "R'","L'","U'","D'","F'","B'",
          "F2 ","R2 ","U2 ","L2 ","B2 ","D2 ",
          "M'","E'","S'","X'","Y'","Z'",
          "M2","E2","S2",
          "M","E","S","X","Y","Z")
  to <- c("L2:3","R2:3","D2:3","U2:3","B2:3","F2:3",
          "R1:2","L1:2","U1:2","D1:2","F1:2","B1:2",
          "r","l","u","d","f","b",
          "FF ","RR ","UU ","LL ","BB ","DD ",
          "l2 l2","d2 d2","f2 f2",
          "l2","d2","f2","p","o","op",
          "L2","D2","F2","P","O","PO"
          )
  for(i in 1:length(fr))
    moves <- gsub(fr[i],to[i],moves)
  return(moves)
}

