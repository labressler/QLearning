\name{qlearningaction}
\alias{qlearningaction}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ qlearningaction
}
\description{
  This repository implements \href{http://artint.info/html/ArtInt_265.html}{Q-Learning}, a model-free form of reinforcement learning in R.
}
\usage{
qlearningaction(q, currentstate, exploration=.5)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{q
}{Input state/action matrix.}
  \item{currentstate
}{Current state of the game. Does not have to match any of the state for \emph{q}.}
\item{exploration
}{The probability of choosing a random state, rather than the one with the highest EV. Default 0.5.}
}
\details{
For internal use for \emph{qlearn}.
}
\value{
An action to take, taken from the possible actions of \emph{q}.
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
