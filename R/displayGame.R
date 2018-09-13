# Function for displaying game state
displayGame <- function(pos){
  # Check game is over / which player plays next
  if(pos$nextPlayer < 0){
    subTitle <- paste0(playerCol(-pos$nextPlayer, name=TRUE), " wins!")
  } else{
    subTitle <- paste0(playerCol(pos$nextPlayer, name=TRUE), " to play")
  }

  # Setup blank plot
  plot(c(-3.5, 3.5),
       c(-3.5, 3.5),
       type = "n",
       asp = 1,
       axes = FALSE,
       xlab = "",
       ylab = "",
       main = "The Game",
       sub = subTitle)

  # Draw connecting lines first
  for (i in seq(1, nrow(pos$edges))){
    lines(x = pos$nodes[pos$edges[i, ], "posX"],
          y = pos$nodes[pos$edges[i, ], "posY"])
  }

  # Function to draw a circle
  draw.circle <- function(centre, radius, col=NA){
    theta <- seq(0, 2 * pi, length = 200)
    polygon(x = centre[1] + radius * cos(theta),
            y = centre[2] + radius * sin(theta),
            col = col)
  }

  # Colours for circles
  colours <- sapply(pos$nodes$owner, playerCol)

  # Draw the circles
  for (i in seq(1, nrow(pos$nodes))){
    draw.circle(as.numeric(pos$nodes[i, c("posX", "posY")]),
                0.3,
                col=colours[i])
  }

  # Help text
  for(i in seq(-3, 3)){
    text(i, -3.5, LETTERS[i+4])
    text(i, 3.5, LETTERS[i+4])
    text(-3.5, i, i+4)
    text(3.5, i, i+4)
  }

  # Counters
  for (i in seq(1, nrow(pos$nodes))){
    text(x=pos$nodes$posX[i],
         y=pos$nodes$posY[i],
         labels=pos$nodes$counters[i],
         col="white")
  }

  # Record the plot we created
  p <- recordPlot()

  # return the plot
  return(p)
}
