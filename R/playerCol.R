# Player colours
playerCol <- function(playerNum, name=FALSE){
  if(playerNum==0L){
    if(name) return("White")
    return("#FFFFFF")
  } else if(playerNum==1L){
    if(name) return("Red")
    return("#DE3C4B")
  } else if(playerNum==2L){
    if(name) return("Blue")
    return("#1446A0")
  } else if(playerNum==3L){
    if(name) return("Green")
    return("#20A39E")
  } else if(playerNum==4L){
    if(name) return("Dark")
    return("#464546")
  } else stop("Too many players")
}
