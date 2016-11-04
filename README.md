# QLearning

This repository implements (Q-Learning)[http://artint.info/html/ArtInt_265.html], a model-free form of reinforcement learning in R.


**To download**: In your R console, type:

```R
install.packages("devtools")
require(devtools)
install_github("labressler/QLearning")
require(QLearning)
```

Currently, the R functions that this framework supports require two things:

-A line where a player must choose an action in the form "player <- 'Choose'". For example, if we are storing a player's action in a variable called "player1", the code for choosing a player's action must be:

```R
player1 <- 'Choose'
```

-A return value of the "reward" after each play-through. This can be a simple as "1 for a win, 0 for a loss".


**Example:** 

Let's set up a simple card game where four players are dealt one card out of a 8-card deck. Each players antes 1 dollar, each player has the chance to "call" for 2 extra dollars or "fold" for nothing, and we act last. The player with the highest card who didn't fold wins the entire money in the pot. Also, we assume that all other players will "call" with a 2 or higher.

```R
cardgame <- function()
{
  playercards <- sample(1:8,4) #distribute the cards, we're player one
  ourcard <- playercards[1] #our card
  playertotals <- rep(-1,4) #including the antes
  playersinpot <- vector()
  for (player in 2:4) #other 3 players go first
  {
    if (playercards[player]>=2)
    {
      playertotals[player] <- (-3)
      playersinpot <- append(playersinpot,player)
    }
  }
  #the next line is where we want to choose our action
  player1 <- 'Choose' 
  if (player1=="Call")
  {
    playertotals[1] <- (-3)
    playersinpot <- append(playersinpot,1)
  }
  potsize <- -1*(sum(playertotals)) #the amount in the pot is how much the players put in
  playercards[!(1:4 %in% playersinpot)] <- 0 #get rid of everyone who folded
  winner <- which.max(playercards) #winner is the person with the highest card who didn't fold
  playertotals[winner] <- playertotals[winner]+potsize
  return(playertotals[1]) #return how much we won
}
```

We want to figure out the expected payoff of *"Call"* and *"Fold"* with a certain card (which is the variable "ourcard"). So, our **state** is which card we hold, and our **possible actions** are *("Call","Fold")*. So, using this package, let's train our player by playing the game 50000 times:

```{r,include=TRUE}
strat <- qlearn(game="cardgame",statevars="ourcard",possibleactions=c("Call","Fold"),playername="player1",numiter=25000) #make sure each function and variable name is a string


strat #output
```

Our row names represent the card that our player is dealt, while the column names represent the possible actions. 

Now that we have a reasonable strategy, we can tell our player to play the games 50000 more times, this time using our previous strategy as a baseline:

```{r,include=FALSE}
qlearn(game="cardgame",statevars="ourcard",possibleactions=c("Call","Fold"),playername="player1",numiter=25000,prevstrategy=strat) 
```


