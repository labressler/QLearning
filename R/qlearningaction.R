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
