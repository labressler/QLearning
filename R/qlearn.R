qlearn <- function(game,statevars,possibleactions,playername="P1",numiter=50,...)
{
  initial <- rep(1/length(possibleactions),length(possibleactions))
  names(initial) <- possibleactions
  initial <- as.data.frame(t(initial))
  newgame <- get(game)
  ngbody <- as.list(body(newgame))
  nglist <- which(grepl("Choose",as.character(ngbody),ignore.case=T) & grepl(playername,as.character(ngbody),ignore.case=T))
  body(newgame)[[nglist]] <- substitute({
    statevec <<- vector()
    for (jj in statevars)
    {
      statevec <<- append(statevec,get(jj))
    }
    currentstate <<- paste(statevec,collapse=" ")
    assign(playername,qlearningaction(q=initial,currentstate=currentstate))
    curraction1 <<- get(playername)
  })
  for (ii in 1:numiter)
  {
    rew <- as.numeric(newgame(...))
    initial <- qlearningupdate(initial,currentstate,curraction1,rew)
  }
  return (initial[-(1),])
}
