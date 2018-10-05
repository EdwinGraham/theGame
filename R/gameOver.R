# Function to determine that the game is over
gameOver <- function(pos){
  # What turn are we on?
  turn <- sum(pos$nodes$counters) + 1L
  
  # Everyone gets at least one move
  if (turn <= pos$numPlayers) return(FALSE)
  
  # If only one player has pieces left, they win
  if (length(setdiff(unique(pos$nodes$owner), 0L)) <= 1) return(TRUE)
  
  # Otherwise, game on!
  return(FALSE)
}