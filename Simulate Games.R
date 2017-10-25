library(ggplot2)

Play_Game <- function(n = 1,stick = TRUE){ #n is number of simulation and stick to your choice
  Wins <- 0
  Loss <- 0
  output <<-matrix(ncol=2, nrow =n)
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
    output[i,] <<- c(i, Wins/n)
    output <<- data.frame(output)
    names(output)<<-c('games','wins')

    
  }
  cat(paste("Win: ", Wins/n*100,'%\nLoss:',Loss/n*100,'%\n'),sep="")

}



Play_Game(n = 1000,stick = TRUE)

library(ggplot2)
names(output)<-c('games','wins')
ggplot(data=output, aes(x=games, y=wins)) +
  geom_line()+
  geom_point()
