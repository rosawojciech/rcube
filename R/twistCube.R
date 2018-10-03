#' Twist cube
#'
#' Twist the cube by given string of moves and number of times.
#' @param cube - cube object
#' @param moves - string parameter
#' Syntax: The main QTM clockwise movements are the same as in the Singmasters notation: "U", "D", "F", "B", "R", "L". However moves from HTM such as U2 is not move of upper layer by 180 degrees (it will be explained further).
#' Counter clockwise moves are denoted by lowercase letters: "u", "d", "f", "b", "r", "l".
#' Rotations of the cube are denoted by "O" (rotate cube horizontally, "o" means rotation horizontally in different direction); and "P" (rotate cube vertically, "p" means rotation vertically in different direction).
#' Repetitions of the moves: there are several ways to repeat given sequence of moves. The simplest way is to copy commands. The most effective way to do this is using parameter times. However, in some cases it is useful to repeat only parts of sequence of moves - then we could use bracketing terms and operator times "x".
#' @param times - integer (default is 1). Number of repetitions of moves.
#' @return cube - cube object
#'
#' @examples
#' # Create classic Rubik's cube:
#' c <- createCube()
#' # Check moves LL FF RR BB
#' c <- twistCube(c,"LLFFRRBB")
#' # Check if LFRB repeated 316 times is cycle:
#' c <- twistCube(c,"(LFRB)x316")
#' is.solved(c)
#' # TRUE
#' # Twisted chicken feet pattern:
#' c <- createCube()
#' c <- twistCube(c,positions[21,"moves"])
#' plot3dCube(c)
#' # The same pattern using pipe %>% from magrittr package
#' require(magrittr)
#' c <- createCube() %>% twistCube(positions[21,"moves"]) %>% plot3dCube()
#' # Rubik's Revenge
#' c <- createCube(N = 4) %>% plot3dCube()
#' # Rotating only one edge, which is not allowed on a classic cube:
#' c %>% twistCube(positions[30,"moves"]) %>% plot3dCube()
#' # Creating Professor's Cube
#' c <- createCube(N = 5) %>% plot3dCube()
#' # Rotating and moving edges:
#' c %>% twistCube("(u3RUrFrfRU3)x12") %>% plot3dCube()
#' # Moving and rotating edges part 2:
#' c <- createCube(5) %>% twistCube("((R1:2)x2 BBUU (L1:2)x2 UU rr2
#' UU RR2 UUFF RR2 FF ll2 BB (R1:2)x2 )x2 dd") %>% plot3dCube()
#' # Hearts pattern on a cube sized 13x13x13:
#' c <- createCube(13) %>% twistCube("OP U2
#' l4:5 R4:5 u2 L4:5 r4:5 U3
#' l3:6 R3:6 u3 L3:6 r3:6 U4
#' l2:4 R2:4 l6:8 u4 L2:4 r2:4 L6:8 U5
#' l2:3 R2:3 l7 u5 L2:3 r2:3 L7 U6
#' l2:3 R2:3 u6 L2:3 r2:3 U7
#' l2:4 R2:4 u7 L2:4 r2:4 U8
#' l3:5 R3:5 u8 L3:5 r3:5 U9
#' l4:6 R4:6 u9 L4:6 r4:6 d4 l5:9 D4
#' L5:9 d3 l6:8 D3
#' L6:8 d2 l7 D2 L7") %>% plot3dCube()
#' # Creating octa cube
#' c <- createCube(N = 4, mode = "octa") %>%  plot3dCube()
#' # Rotating centers which is not visible on a classic cube (URL algorithm):
#' c %>% twistCube("(URL uurl)x2") %>% plot3dCube()
#' # Creating void cube 8x8x8
#' c <- createCube(N = 8,mode = "void") %>% plot3dCube()
#' @import magrittr
#' @export
twistCube <- function(cube,moves = "", times = 1){
  cube$cube <- kostka.obrot(cube$cube,moves,times)
  cube$moves <- paste(cube$moves,moves)
  return(cube)
}
