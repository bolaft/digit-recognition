# stroke
stroke <-function(x0=-1, y0=-1, x1=1, y1=1, N=10) 
{
  return(matrix(
    c(seq(x0, x1, length=N), 
    seq(y0, y1, length=N)), 
    ncol=2
  ))
}

# simu_symbol
simu_symbol <- function() 
{
  digit_1 <- rbind(
    stroke(-0.3, 0.5, 0.3, 1.0, 10), 
    stroke(0.3, 0.9, 0.3, -1.0, 20)
  )
  
  dimnames(digit_1) <- list(num=1:nrow(digit_1), point=c("x", "y"))
  plot(digit_1, type="l", col="red", xlim=c(-1, 1), ylim=c(-1, 1))
  points(digit_1)
  
  digit_4 <- rbind(
    stroke(0.2, 1.0, -0.8, -0.3, 10), 
    stroke(-0.85, -0.32, 0.5, -0.1, 10), 
    stroke(0.3, 0.1, 0.2, -1.0, 10)
  )
  
  dimnames(digit_4) <- list(num=1:nrow(digit_4), point=c("x", "y"))
  plot(digit_4, type="l", col="red", xlim=c(-1, 1), ylim=c(-1, 1))
  points(digit_4)
  
  digit_6 <- rbind(
    stroke(0.25, 0.85, -0.4, -0.2, 10),
    stroke(-0.4, -0.2, 0, -1, 7),
    stroke(0, -1, 0.3, -0.5, 7),
    stroke(0.3, -0.5, -0.1, 0.3, 6)
  )
  
  dimnames(digit_6) <- list(num=1:nrow(digit_6), point=c("x", "y"))
  plot(digit_6, type="l", col="red", xlim=c(-1, 1), ylim=c(-1, 1))
  points(digit_6)
  
  return(list(d1=digit_1, d2=digit_4, d3=digit_6))
}

# compute_symbol
compute_symbol <- function (trace, nr=5, nc=3)
{
  LUT <- matrix(1:(nr*nc), nrow=nr, ncol=nc, byrow=T)
  NB <- length(trace[,"x"])
  
  Ix <- pmax(pmin(1+floor((trace[,"x"]-(-1))*nc/2), rep(nc, NB)), rep(1,NB))
  Iy <- pmax(pmin(1+floor((trace[,"y"]-(-1))*nr/2), rep(nr, NB)), rep(1,NB))
  
  return(LUT[matrix(c(Iy, Ix), ncol=2)])
}

# compute_symbol_dir
compute_symbol_dir <- function (trace, nangle=8)
{
  NB <- length(trace[,"x"])
  delta <- trace
  delta[1:(NB-1),] <- delta[2:NB,]
  delta <- delta - trace
  delta[NB,] <- delta[NB-1,]
  angle <- atan2(delta[,"y"], delta[,"x"]) + pi/nangle
  angle[angle < 0] <- angle[angle < 0] + 2*pi
  angle <- pmin(1 + floor(angle*nangle/(2*pi)), nangle)
  
  return(angle)
}

# initHMMDigit
initHMMDigit <- function(nsymbols=15, nstates=3, uniform=F)
{
  states <- c(paste("Tier", 1:nstates))
  symbols <- 1:nsymbols
  startProbs <- c(1, c(rep(0, nstates-1)))
  
  if (uniform) distr <- c(0.5, 0.5) else  distr <- c(1 - nstates / 30, nstates / 30)
  
  transProbs <- matrix(c(
    rep(c(distr, rep(0.0, nstates-1)), nstates-1), 1.0
    ), nrow=nstates, ncol=nstates, byrow=T
  )
  
  emissionProbs <- matrix(
    rep(1/nsymbols, nsymbols * nstates),
    nrow=nstates, ncol=nsymbols, byrow=T
  )
  
  hmm <- initHMM(states, symbols, startProbs, transProbs, emissionProbs)
}

# load_files
load_files <- function(type="5_3", train=T) 
{
  observations <- sapply(
    paste("data/", if (train) "Train_" else "Test_", "compute_symbol_", type, "Digit", 0:9, ".txt", sep=""), 
    Load_Obs
  )
  
  names(observations) <- paste("o", 0:9, sep="")
  
  classes <- sapply(0:9, function(x) rep(x, nrow(observations[[x + 1]])))
  
  names(classes) <- paste("c", 0:9, sep="")
  
  list(observations=as.list(observations), classes=as.list(classes))
}

# train_models
train_models <- function(type, nsymbols, uniform=F, nstates=3) 
{
  files <- load_files(type=type, train=T)
  
  models <- mclapply(
    mc.cores=detectCores(), 
    files$observations, 
    function(x) baumWelchList(
      LObservation = x,
      hmm = initHMMDigit(nsymbols=nsymbols, nstates=nstates, uniform=uniform)
    )
  )
  
  names(models) <- paste("m", 1:length(files$observations) - 1, sep="")
  
  list(models=models, sequences=files)
}

logadd <- function(min, max)
{
  if (min == -Inf & max == -Inf) {
    return(min)
  } else if (min <= max) {
    return(max + log1p(exp(min - max)))
  } else {
    return(min + log1p(exp(max - min)))
  }
}

# classify <- function(hmm, obs) 
# {
#   loglike <- -Inf
#   
#   for (model in hmm){
#     loglike <- c(loglike, loglikelihood(model$hmm, obs))
#   }
# }

# classifyJ <- function(hmms,obs)
# {
#   max <- -Inf
#   class <- 0
#   k <- 0
#   for (i in seq(1,10,by=1)){
#     fwd <- forward(hmms[i]$hmm,obs)
#     fwdPb <- -Inf
#           j in 1:length(hmms[i]$hmm$States)){
#         fwdPb <- logadd(fwdPb,fwd[j,length(obs)])
#     }
#     if (fwdPb > max){
#       max <- fwdPb
#       class <- k
#     }
#     k <- k + 1
#   }
#   return(class)
# }

recoJ <- function(type, nstates, uniform=F)
{
  classes = NULL
  
  o <- loadAll(paste("data/Test_compute_symbol_", type, "Digit", sep=""))
  
#   load(paste("models", if (uniform) "uniform" else "optimal", nstates, type, sep="/"))
  
  for(i in 1:length(o$obs[,1])){
    classes <- c(classes, classifyJEdit(models_optimal_5_4, o$obs[i,]))
  }
  
  print("Confusion Matrix :")
  print(table(o$cl, classes))
  
  print("Recognition Rate :")
  print((sum(diag(table(o$cl,classes)))/length(o$obs[,1])) * 100)
}

classifyJEdit <- function(hmms, obs)
{
  max <- -Inf
  class <- 0
  k <- 0
  for (i in hmm){
    fwd <- forward(i$hmm,obs)
    fwdPb <- -Inf
    for(j in 1:length(i$hmm$States)){
      fwdPb <- logadd(fwdPb,fwd[j,length(obs)])
    }
    if (fwdPb > max){
      max <- fwdPb
      class <- k
    }
    k <- k + 1
  }
  return(class)
}

# reco_digit <- function(probs) {
#   return (which(probs == max(probs)) - 1)
# }

# classify <- function(hmm, obs) 
# {
#   probs <- sapply(
#     hmm,
#     function(x) loglikelihood(x$hmm, obs)
#   )
# }

# confusion_matrix <- function(hmm, type)
# {
#   o <- load_files(type=type, train=F)
#   
#   cm <- sapply(
#     o$observations,
#     function(x) classify(hmm, x)
#   )
#   
#   for (col in 1:ncol(cm)){  
#     bestrow = which.max(cm[,col])
#     
#     for(row in 1:nrow(cm)){
#       if (row == bestrow){
#         cm[row, col] = 1
#       } else {
#         cm[row, col] = 0
#       }
#     }
#   }
#   
#   return(cm)
# }

# confusion_matrix <- function(hmm, type)
# {
#   o <- load_files(type=type, train=F)
#   
#   cm <- sapply(
#     o$observations,
#     function(x) classify(hmm, x)
#   )
#   
#   print(cm)
# }

# confusion_matrix <- function(hmm, type)
# {
#   o <- load_files(type=type, train=F)
#   
#   probs <- mclapply(
#     mc.cores=detectCores(), 
#     o$observations,
#     function(x) classify(hmm, x)
#   )
#   
#   digits <- mclapply(
#     mc.cores=detectCores(),
#     probs,
#     function(x) reco_digit(x)
#   )
#   
#   cm <- table(
#     c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9),
#     digits
#   )
# }

# recognition <- function(obs_path, models){
#   res = NULL
#   myObs <- loadAll(obs_path)
#   
#   for(i in 1:length(myObs$obs[,1])){
#     res <- c(res,classifyJ(models,myObs$obs[i,]))
#   }
#   print("Confusion matrix :")
#   print(table(myObs$cl,res))
#   
#   print("Recognition rate :")
#   print((sum(diag(table(myObs$cl,res)))/length(myObs$obs[,1]))*100)
# }