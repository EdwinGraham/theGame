# Function to get to next position
nextPos <- function(pos, move){
  # Validation check
  if (! move %in% rownames(pos$nodes)) stop("Move does not exist.")
  if (! pos$nodes[move, "owner"] %in% c(0L, pos$nextPlayer)){
    stop("Invalid move.")
  }
  if (pos$nextPlayer < 0) stop("Game is already over.")

  # What turn are we on?
  turn <- sum(pos$nodes$counters) + 1L

  # Add one to board in correct position
  pos$nodes[move, "counters"] <- pos$nodes[move, "counters"] + 1L

  # Set owner of position to player making move
  pos$nodes[move, "owner"] <- pos$nextPlayer

  # For each node, find connecting nodes
  graph <- sapply(rownames(pos$nodes), function(x){
    c(pos$edges[which(pos$edges[, 1]==x), 2],
      pos$edges[which(pos$edges[, 2]==x), 1])
  })

  # Values for toppling to connected positions
  toppleVals <- sapply(graph, length)

  # Repeatedly resolve topples
  while (any(pos$nodes$counters >= toppleVals)){
    toppling <- names(which(pos$nodes$counters >= toppleVals))
    for (x in toppling){
      # Remove toppled counters
      pos$nodes[x, "counters"] <- pos$nodes[x, "counters"] - toppleVals[[x]]
      # If no counters left, player loses control of position
      if (pos$nodes[x, "counters"] == 0) pos$nodes[x, "owner"] <- 0L
      # For each connecting position add 1 counter and claim for current player
      for (y in graph[[x]]){
        pos$nodes[y, "counters"] <- pos$nodes[y, "counters"] + 1
        pos$nodes[y, "owner"] <- pos$nextPlayer
      }
    }
    # If one player has all the tokens
    if (gameOver(pos)) break
  }

  # Next non-eliminated player
  if (gameOver(pos)){
    pos$nextPlayer <- -pos$nextPlayer # If game is over
  } else{
    # Otherwise switch to next player
    pos$nextPlayer <- pos$nextPlayer %% pos$numPlayers + 1L
    # Once all players have a turn, check for eliminated players
    # If eliminated, move to next player
    if(turn > pos$numPlayers){
      while (! any(pos$nodes$owner==pos$nextPlayer)){
        pos$nextPlayer <- pos$nextPlayer %% pos$numPlayers + 1L
      }
    }
  }

  # Display game
  print(displayGame(pos))

  # Return new position
  return(pos)
}
