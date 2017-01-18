qlearn <- function(game,statevars,possibleactions,playername="P1",numiter=1000,prevstrategy=NULL,...)
{
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

qlearningupdate <- function(q,currentstate,currentaction,currentreward,nextstate=NULL,rewardcount=.5,gamma=.25)
{
  rndn <- runif(1,0,1)
  if ((nrow(q) > 0) & (currentstate %in% rownames(q)))
  {
    q[currentstate,currentaction] <- (1-rewardcount)*q[currentstate,currentaction] + rewardcount*currentreward
  } else {
    qvec <- rep(0.5,ncol(q))
    if(nrow(q)>0)
    {
      q <- rbind.data.frame(q,qvec)
    } else {
      q[1,] <- as.numeric(qvec)
    }
    rownames(q)[nrow(q)] <- currentstate
    if(nchar(currentstate)==4)
    {
      #print(paste0(currentstate,currentreward))
    }
    #q[currentstate,currentaction] <- (q[currentstate,currentaction] + discount*currentreward)/(1+discount)
    q[currentstate,currentaction] <- (1-rewardcount)*q[currentstate,currentaction] + rewardcount*currentreward
  }
  return(q)
}

qlearningaction <- function(q,currentstate,exploration=.5)
{
  rndn <- runif(1,0,1)
  if (currentstate %in% rownames(q))
  {
    vec <- unlist(q[currentstate,])
    vecs <- exp(log(vec/(1-vec)))
    vecs <- vecs/mean(vecs)
    if (rndn<exploration)
    {
      #statenum <- sample.int(ncol(q),1,prob=exp((-1)/((vec+3/length(vec)/8)/(mean(vec)+3/8))))
      #statenum <- sample.int(ncol(q),1,prob=exp(vec/mean(abs(vec))/2))
      statenum <- sample.int(ncol(q),1,prob=exp(vec/mean(vecs)/2))
      retstate <- colnames(q)[statenum]
      return(retstate)
    } else {
      statenum <- which(vec==max(vec))
      statenum <- sample(statenum,1)
      retstate <- colnames(q)[statenum]
      return(retstate)
    }
  } else {
    statenum <- sample.int(ncol(q),1)
    retstate <- colnames(q)[statenum]
    return(retstate)
  }
}
