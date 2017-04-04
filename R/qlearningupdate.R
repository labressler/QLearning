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
