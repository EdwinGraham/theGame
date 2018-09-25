#####  playTheGame  #####
#' Function to play The Game in R.
#' @description Interactive function to play The Game
#' using console and plot window.
#' @usage playTheGame()
#' @author Edwin Graham (edwingraham1984@gmail.com)
#' @examples playTheGame()
#' @export

playTheGame <- function(){
  numPlayers <- as.integer(readline("How many players (2-4?) "))
  
  if (numPlayers < 2L | numPlayers > 4L) stop("Invalid number of players.")
  
  pos <- setupGame(numPlayers)
  
  displayGame(pos)
  
  cat(paste0("Order of play is: ",
             paste0(sapply(seq(1, numPlayers),
                           function(n) playerCol(n, name=TRUE)),
                    collapse = ", ")), "\n")
  
  while (! gameOver(pos)){
    move <- gsub(" ", "", toupper(readline("What move are you playing? ")))
    tryMove <- try(nextPos(pos, move))
    if (! "try-error" %in% class(tryMove)) pos <- tryMove
  }
}

