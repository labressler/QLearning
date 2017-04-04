\name{qlearningupdate}
\alias{qlearningupdate}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ qlearningupdate
}
\description{
  This repository implements \href{http://artint.info/html/ArtInt_265.html}{Q-Learning}, a model-free form of reinforcement learning in R.
}
\usage{
qlearningupdate(q, currentstate, currentaction, currentreward, nextstate=NULL,
  rewardcount=.5, gamma=.25)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{q
}{Input state/action matrix.}
  \item{currentstate
}{Current state of the game. Does not have to match any of the state for \emph{q}.}
\item{currentaction
}{Action to take.}
\item{currentreward
}{Reward for \emph{currentaction} in current iteration.}
\item{nextstate
}{State that the game is in after taking \emph{currentaction}.}
\item{rewardcount
}{Regularization constant for reward.}
\item{gamma
}{Learning rate constant for Q-Learning.}
}
\details{
For internal use for \emph{qlearn}.
}
\value{
An updated state/action matrix.
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
%%\examples{


%%}


% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ reinforcementlearning }% use one of  RShowDoc("KEYWORDS")
\keyword{ machinelearning }% __ONLY ONE__ keyword per line
\keyword{ optimize }
