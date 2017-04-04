globalVariables(c("allstatevec","allactionvec","statevec"))

qlearn <- function(game,statevars,possibleactions,playername="P1",numiter=1000,prevstrategy=NULL,...)
{
  allstatevec <- vector()
  allactionvec <- vector()
  statevec <- vector()
  rm(allstatevec)
  rm(allactionvec)
  rm(statevec) #Since I need these three variables to be empty early in the function for "substitute", this is a very hacky way to deal with global variable assignment
  if (is.null(prevstrategy))
  {
    initial <- rep(1/length(possibleactions),length(possibleactions))
    names(initial) <- possibleactions
    initial <- as.data.frame(t(initial))
    initial <- initial[-(1),]
  } else {
    initial <- prevstrategy
  }
  newgame <- get(game)
  ngbody <- as.list(body(newgame))
  nglist <- which(grepl("Choose",as.character(ngbody),ignore.case=T) & grepl(playername,as.character(ngbody),ignore.case=T))
  funcclass <- class(body(newgame)[[nglist]])
  while(funcclass!="<-")
  {
    nglist <- append(nglist,which(grepl("Choose",body(newgame)[[nglist]])))
    funcclass <- class(body(newgame)[[nglist]])
  }
  body(newgame)[[nglist]] <- substitute({
    statevec <<- vector()
    for (jj in statevars)
    {
      statevec <<- append(statevec,get(jj))
    }
    currentstate <<- paste(statevec,collapse=" ")
    allstatevec <<- append(allstatevec,currentstate)
    assign(playername,qlearningaction(q=initial,currentstate=currentstate))
    allactionvec <<- append(allactionvec,get(playername))
  })
  for (ii in 1:numiter)
  {
    statevec <<- vector()
    allstatevec <<- vector()
    allactionvec <<- vector()
    #assign("statevec",vector(),envir=environment(qlearn))
    #assign("allstatevec",vector(),envir=environment(qlearn))
    #assign("allactionvec",vector(),envir=environment(qlearn))
    rew <- as.numeric(newgame(...))
    for (statenum in 1:length(allstatevec))
    {
      curstate <- allstatevec[statenum]
      curraction1 <- allactionvec[statenum]
      #initial <- qlearningupdate(initial,curstate,curraction1,rew)
      initial <- qlearningupdate(initial,curstate,curraction1,rew,rewardcount=.5/length(allstatevec))
    }
  }
  return (initial[order(rownames(initial)),])
}
