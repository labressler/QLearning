\name{qlearn}
\alias{qlearn}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ qlearn
}
\description{
  Input a \emph{game} that has variables \emph{statevars} (which the player can keep track of). The player can perform any of \emph{possibleactions}. The output matrix will give the expected value of each action (column) in each state (row).
}
\usage{
qlearn(game, statevars, possibleactions, playername="P1",
  numiter=1000, prevstrategy=NULL, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{game
}{Name of the game to be played/learned.}
  \item{statevars
}{A vector of the states to be monitored inside \emph{game}. These are the conditions under which we the player has to make his decision.}
\item{possibleactions
}{A vector of the names of the possible actions inside \emph{game}. This should be a list of every possible action that can be taken, regardless of state.}
\item{playername
}{The name of the variable that holds the name for the player's action inside \emph{game}. See \strong{Details}.}
\item{numiter
}{Number of iterations of \emph{game}. Defaults to 50.}
\item{prevstrategy
}{Reward matrix returned by a previous \emph{qlearn} function; serves as a starting point. Defaults to a blank reward matrix.}
\item{...
}{Additional arguments to be passed to \emph{game}.}
}
\details{
At some point in game, there must be a line of the format

\code{playername <- 'Choose'}

where playername is substituted with the paramater "playername". This line should be at the point where the user wants to have the player choose an action. Since playername defaults to "P1", it sufficient to put the line:

\code{P1 <- 'Choose'}

somewhere in the function.
}
\value{
A matrix describing the expected reward values of performing a certain action (columns) in a certain state (rows).
}
\references{
http://labressler.github.io/analytics
}
\author{
Liam Bressler
%%  ~~who you are~~
}
\note{
Contact at liam.bressler@yale.edu
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

%%\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
%%}
\examples{
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
  playercards[!(1:4 \%in\% playersinpot)] <- 0 #get rid of everyone who folded
  winner <- which.max(playercards) #winner is the person with the highest card who didn't fold
  playertotals[winner] <- playertotals[winner]+potsize
  return(playertotals[1]) #return how much we won
}

strat <- qlearn(game="cardgame",statevars="ourcard",possibleactions=c("Call","Fold"),
  playername="player1",numiter=25000) #make sure each function and variable name is a string

strat

}


% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ reinforcementlearning }% use one of  RShowDoc("KEYWORDS")
\keyword{ machinelearning }% __ONLY ONE__ keyword per line
\keyword{ optimize }
