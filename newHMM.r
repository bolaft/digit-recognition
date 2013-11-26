
initHMM = function(States, Symbols, startProbs=NULL, transProbs=NULL,
                   emissionProbs=NULL)
{
  nStates    = length(States)
  nSymbols   = length(Symbols)
  S          = rep(1/nStates,nStates)
  T          = 0.5*diag(nStates) + array(0.5/(nStates),c(nStates,nStates))
  E          = array(1/(nSymbols),c(nStates,nSymbols))
  names(S)   = States
  dimnames(T)= list(from=States,to=States)
  dimnames(E)= list(states=States,symbols=Symbols)
  if(!is.null(startProbs)){S[]  = startProbs[]}
  if(!is.null(transProbs)){T[,] = transProbs[,]}
  if(!is.null(emissionProbs)){E[,] = emissionProbs[,]}
  return(list(States=States,Symbols=Symbols,startProbs=S,transProbs=T,
              emissionProbs=E))
}

forward = function(hmm, observation)
{
  hmm$transProbs[is.na(hmm$transProbs)]       = 0
  hmm$emissionProbs[is.na(hmm$emissionProbs)] = 0
  nObservations  = length(observation)
  nStates    = length(hmm$States)
  f          = array(NA,c(nStates,nObservations))
  dimnames(f)= list(states=hmm$States,index=1:nObservations)
  # Init
  for(state in hmm$States)
  {
    f[state,1] = log(hmm$startProbs[state]) + log(hmm$emissionProbs[state,observation[1]])
  }
  # Iteration
  for(k in 2:nObservations)
  {
    for(state in hmm$States)
    {
      logsum = -Inf
      for(previousState in hmm$States)
      {
        temp   = f[previousState,k-1] + log(hmm$transProbs[previousState,state])
       # if(temp > 0){
      #    cat("Prob:", k, ":",f[previousState,k-1],"+",log(hmm$transProbs[previousState,state]))
      #  }
        if(temp > - Inf)
        {
          #logsum = temp + log(1 + exp(logsum - temp ))  # since log(exp(a)+exp(b)) = a + log(1+exp(b-a))
          logsum = logadd(logsum,temp)
        }
      }
      f[state,k] = log(hmm$emissionProbs[state,observation[k]]) + logsum
    }
  }
  return(f)
}

backward = function(hmm, observation)
{
  hmm$transProbs[is.na(hmm$transProbs)]       = 0
  hmm$emissionProbs[is.na(hmm$emissionProbs)] = 0
  nObservations  = length(observation)
  nStates    = length(hmm$States)
  b          = array(NA,c(nStates,nObservations))
  dimnames(b)= list(states=hmm$States,index=1:nObservations)
  # Init
  for(state in hmm$States)
  {
    b[state,nObservations] = log(1)
  }
  # Iteration
  for(k in (nObservations-1):1)
  {
    for(state in hmm$States)
    {
      logsum = -Inf
      for(nextState in hmm$States)
      {
        temp   = b[nextState,k+1] + log(hmm$transProbs[state,nextState]*hmm$emissionProbs[nextState,observation[k+1]])
        if(temp > - Inf)
        {
          #logsum = temp + log(1 + exp(logsum-temp))
          logsum = logadd(logsum,temp)
        }
      }
      b[state,k] = logsum
    }
  }
  return(b)
}

baumWelch = function(hmm, observation, maxIterations=100, delta=1E-9, pseudoCount=0)
{
  tempHmm = hmm
  tempHmm$transProbs[is.na(hmm$transProbs)]       = 0
  tempHmm$emissionProbs[is.na(hmm$emissionProbs)] = 0
  diff = c()
  for(i in 1:maxIterations)
  {
    # Expectation Step (Calculate expected Transitions and Emissions)
    bw = baumWelchRecursion(tempHmm, observation)
    T  = bw$TransitionMatrix
    E  = bw$EmissionMatrix
    #    print(T)
    #    print(E)
    # Pseudocounts
    T[!is.na(hmm$transProbs)]    = T[!is.na(hmm$transProbs)]    + pseudoCount
    E[!is.na(hmm$emissionProbs)] = E[!is.na(hmm$emissionProbs)] + pseudoCount
    # Maximization Step (Maximise Log-Likelihood for Transitions and Emissions-Probabilities)
    T = (T/apply(T,1,sum))
    E = (E/apply(E,1,sum))
    d = sqrt(sum((tempHmm$transProbs-T)^2)) + sqrt(sum((tempHmm$emissionProbs-E)^2))
    diff = c(diff, d)
    tempHmm$transProbs    = T
    tempHmm$emissionProbs = E
    #    print(T)
    #    print(E)
    if(d < delta)
    {
      break
    }
  }
  tempHmm$transProbs[is.na(hmm$transProbs)]       = NA
  tempHmm$emissionProbs[is.na(hmm$emissionProbs)] = NA
  return(list(hmm=tempHmm,difference=diff))
}

baumWelchList = function(hmm, LObservation, maxIterations=100, delta=1E-9, pseudoCount=0)
{
  tempHmm = hmm
  diff = c()
  
  for(k in 1:maxIterations){
    tempHmm$transProbs[is.na(hmm$transProbs)]       = 0
    tempHmm$emissionProbs[is.na(hmm$emissionProbs)] = 0
    E = matrix(rep(0,length(tempHmm$emissionProbs)), nrow=dim(tempHmm$emissionProbs))
    T = matrix(rep(0,length(tempHmm$transProb)), nrow=dim(tempHmm$transProb))
    for(i in 1:(dim(LObservation)[1]))
    {
      #print(LObservation[i,])
      # Expectation Step (Calculate expected Transitions and Emissions)
      bw = baumWelchRecursion(tempHmm, LObservation[i,])
      T  = (bw$TransitionMatrix + T)
      E  = (bw$EmissionMatrix + E)
      
    }
    #print(T)
    #print(E)
    # Pseudocounts
    T[!is.na(hmm$transProbs)]    = T[!is.na(hmm$transProbs)]    + pseudoCount
    E[!is.na(hmm$emissionProbs)] = E[!is.na(hmm$emissionProbs)] + pseudoCount
    T[is.na(hmm$transProbs)]    = 0
    E[is.na(hmm$emissionProbs)] = 0
    
    # Maximization Step (Maximise Log-Likelihood for Transitions and Emissions-Probabilities)
    
    T = (T/apply(T,1,sum))
    E = (E/apply(E,1,sum))
    d = sqrt(sum((tempHmm$transProbs-T)^2)) + sqrt(sum((tempHmm$emissionProbs-E)^2))
    diff = c(diff, d)
    tempHmm$transProbs    = T
    tempHmm$emissionProbs = E
    
    #cat("Emm post div:",E)
    tempHmm$transProbs[is.na(hmm$transProbs)]       = NA
    tempHmm$emissionProbs[is.na(hmm$emissionProbs)] = NA
    #print( tempHmm$transProbs)
    if(d < delta)
    {
      cat("early stop (d < delta)\n")
      break
    }
  }
  return(list(hmm=tempHmm,difference=diff))
}

baumWelchRecursion = function(hmm, observation)
{
  TransitionMatrix    = hmm$transProbs
  TransitionMatrix[,] = 0
  EmissionMatrix      = hmm$emissionProbs
  EmissionMatrix[,]   = 0 
  f = forward(hmm,  observation)
  b = backward(hmm, observation)
  probObservations = f[1,length(observation)]
  for(i in 2:length(hmm$States))
  {
    j1 = f[i,length(observation)]
    if(j1 > - Inf)
    {
      #probObservations = j1 + log(1+exp(probObservations-j1))
      probObservations = logadd(probObservations,j1)
    }
  }
  for(x in hmm$States)
  {
    for(y in hmm$States)
    {
      temp = f[x,1] + log(hmm$transProbs[x,y]) +
        log(hmm$emissionProbs[y,observation[1+1]]) + b[y,1+1]
      for(i in 2:(length(observation)-1))
      {
        j2 = f[x,i] + log(hmm$transProbs[x,y]) +
          log(hmm$emissionProbs[y,observation[i+1]]) + b[y,i+1]
        #cat("temp= ",temp,"j2=",j2,"\n")
        if(! is.na(j2) && j2 > - Inf)
        {
          #temp = j2 + log(1+exp(temp-j2))
          temp = logadd(temp,j2)
        }
      }
      temp = exp(temp - probObservations)
      #     cat("temp = ",temp,"\n")
      TransitionMatrix[x,y] = temp
    }
  }
  for(x in hmm$States)
  {
    for(s in hmm$Symbols)
    {
      temp = -Inf
      for(i in 1:length(observation))
      {
        if(s == observation[i])
        {
          j = f[x,i] + b[x,i]
          if(j > - Inf)
          {
            #temp = j + log(1+exp(temp-j))
            temp = logadd(temp,j)
          }
        }
      }
      temp = exp(temp - probObservations)
      EmissionMatrix[x,s] = temp
    }
  }
  return(list(TransitionMatrix=TransitionMatrix,EmissionMatrix=EmissionMatrix))
}
#   compute loglikelihood
loglikelihood <- function(hmm, obs)
{
  
  f <- forward(hmm,obs)
  #cat("f:", dim(f), " obs:", length(obs))
  #print(f)
  loglike = f[1,length(obs)]
  for(i in 2:length(hmm$States))
  {
    t = f[i,length(obs)]
    if(t > - Inf)
    {
      loglike <- logadd(loglike, t)
      #loglike = t + log(1+exp(loglike-t))  # log(exp(a)+exp(b)) = a + log(1+exp(b-a))
    } 
  }
  return(loglike)
}
# logadd <- function(a,b)
# {
#   if (a <= b)
#     return(b+log1p(exp(a-b)))
#   else return(a+log1p(exp(b-a)))
# }

logaddMat <- function(a,b)
{
  res = a
  d <- dim(a)
  for(i in 1:d[1]){
    for(j in 1:d[2]){
      res[i,j] <- logadd(a[i,j], b[i,j])
    }
  }
  return (res)
}
