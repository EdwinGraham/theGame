# Player colours
playerCol <- function(playerNum){
  if (playerNum == 0L){
    return(list(name = "White",
                col = "#FFFFFF",
                text = "#FFFFFF"))
  } else if (playerNum == 1L){
    return(list(name = "Red",
                col = "#EA1744",
                text = "#FFFFFF"))
  } else if (playerNum == 2L){
    return(list(name = "Blue",
                col = "#3454D1",
                text = "#FFFFFF"))
  } else if (playerNum == 3L){
    return(list(name = "Green",
                col = "#4CB944",
                text = "#FFFFFF"))
  } else if (playerNum == 4L){
    return(list(name = "Yellow",
                col = "#FFD23F",
                text = "#000000"))
  } else if (playerNum == 5L){
    return(list(name = "Pink",
                col = "#F6A2F2",
                text = "#000000"))
  } else if (playerNum == 6L){
    return(list(name = "Cyan",
                col = "#45BEFC",
                text = "#000000"))
  } else if (playerNum == 7L){
    return(list(name = "Purple",
                col = "#8715AF",
                text = "#FFFFFF"))
  } else if (playerNum == 8L){
    return(list(name = "Orange",
                col = "#E4572E",
                text = "#FFFFFF"))
  } else if (playerNum == 9L){
    return(list(name = "Teal",
                col = "#197278",
                text = "#FFFFFF"))
  } else if (playerNum == 10L){
    return(list(name = "Light",
                col = "#EFEFF7",
                text = "#000000"))
  } else if (playerNum == 11L){
    return(list(name = "Brown",
                col = "#85522F",
                text = "#FFFFFF"))
  } else if (playerNum == 12L){
    return(list(name = "Dark",
                col = "#070707",
                text = "#FFFFFF"))
  } else stop("That's too many!")
}