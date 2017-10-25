
Monty_Hall<-function(){
  x <-sample(1:3,1)
  if (x == 1){
  door <<- c("car","goat","goat")
} else if (x == 2){
  door <<- c("goat","car","goat")
} else{
  door <<- c("goat","goat","car")
}
}

Monty_Hall()

Play_Game <- function(n = 1,stick = TRUE){ #n is number of simulation and stick to your choice
  Wins <<- 0
  Loss <<- 0
  for (i in 1:n){
      Orig_Choice <- sample(1:3,1)
      z <- c(1,2,3)
      z <- z[z!=Orig_Choice]
      New_Choice <- z[sample(1:2,1)]
      Monty_Hall()
      if(stick == TRUE){
        ifelse((door[Orig_Choice] == "car"), Wins <- Wins +1, Loss <- Loss +1)
      }else{
        ifelse((door[New_Choice] == "car"), Wins <- Wins +1, Loss <- Loss +1)
      }
  }
  return(paste("Wins:", Wins/n))
  }
 
Play_Game(n = 1, stick = FALSE)

