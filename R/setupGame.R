# Function to setup initial game state
setupGame <- function(numPlayers=2L){
  # nodes
  nodesNames <- c("A1", "A4", "A7",
                  "B2", "B4", "B6",
                  "C3", "C4", "C5",
                  "D1", "D2", "D3",
                  "D5", "D6", "D7",
                  "E3", "E4", "E5",
                  "F2", "F4", "F6",
                  "G1", "G4", "G7")

  nodes <- data.frame(posX = c(rep(-3, 3),
                               rep(-2, 3),
                               rep(-1, 3),
                               rep(0, 6),
                               rep(1, 3),
                               rep(2, 3),
                               rep(3, 3)),
                      posY = as.integer(substr(nodesNames, 2, 2))-4,
                      counters = 0L,
                      owner = 0L)

  rownames(nodes) <- nodesNames

  # edges
  edges <- do.call("rbind",
                   list(c("A1", "A4"),
                        c("A1", "B2"),
                        c("A1", "D1"),
                        c("A4", "A7"),
                        c("A4", "B4"),
                        c("A7", "B6"),
                        c("A7", "D7"),
                        c("B2", "B4"),
                        c("B2", "C3"),
                        c("B2", "D2"),
                        c("B4", "B6"),
                        c("B4", "C4"),
                        c("B6", "C5"),
                        c("B6", "D6"),
                        c("C3", "C4"),
                        c("C3", "D3"),
                        c("C4", "C5"),
                        c("C5", "D5"),
                        c("D1", "D2"),
                        c("D1", "G1"),
                        c("D2", "D3"),
                        c("D2", "F2"),
                        c("D3", "E3"),
                        c("D5", "D6"),
                        c("D5", "E5"),
                        c("D6", "D7"),
                        c("D6", "F6"),
                        c("D7", "G7"),
                        c("E3", "E4"),
                        c("E3", "F2"),
                        c("E4", "E5"),
                        c("E4", "F4"),
                        c("E5", "F6"),
                        c("F2", "F4"),
                        c("F2", "G1"),
                        c("F4", "F6"),
                        c("F4", "G4"),
                        c("F6", "G7"),
                        c("G1", "G4"),
                        c("G4", "G7")))

  return(list(nextPlayer = 1L,
              nodes = nodes,
              edges = edges,
              numPlayers = numPlayers))
}
