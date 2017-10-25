
Monty_Hall<-function(){
  prize <<-sample(1:3,1)
  if (prize == 1){
  door <<- c("car","goat","goat")
} else if (prize == 2){
  door <<- c("goat","car","goat")
} else{
  door <<- c("goat","goat","car")
}
}

ifelse((Orig_Choice == prize), Wins <- Wins + 1, Loss <- Loss + 1)
ifelse((New_Choice == prize), Wins <- Wins + 1, Loss <- Loss + 1)
Play_Game <- function(n = 1,stick = TRUE){ #n is number of simulation and stick to your choice
  Wins <- 0
  Loss <- 0
  for (i in 1:n){
    Orig_Choice <- sample(1:3,1) #Select a door
    z <- 1:3
    prize <-sample(1:3,1) #hide the car behind the door
    
    if(Orig_Choice != prize){
      reveal <-(z[-c(Orig_Choice,prize)]) #Reveal a door with no prize
    }else{
      reveal <-sample(z[-c(prize,Orig_Choice)],1)
    }
    
    # Deciding if we stick with our door or switch
    if(stick == TRUE){
      select <- Orig_Choice
    }else{
      select <- z[-c(reveal,Orig_Choice)]
    }
    
    if(select == prize){
      Wins <- Wins +1
    }else{
      Loss <- Loss +1
    }

    
  }
  cat(paste("Win: ", Wins/n*100,'%\nLoss:',Loss/n*100,'%\n'),sep="")

}

plot(1:10);for(i in 1:10){points(10-i,i);Sys.sleep(1)}


Play_Game(n = 100,stick = TRUE)

z[-c(1,1)]
