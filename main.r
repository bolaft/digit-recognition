setwd("/home/soufian/signal")

require("parallel")

source("newHMM.r")
source("usefullTools.r")
source("functions.r")

types <- list(c("5_3", 15), c("5_4", 20), c("dir_8", 8), c("dir_16", 16))
uniform <- F
states <- c(4, 5, 6, 7)

# for (nstates in states){
#   for (type in types){
#     nsymbols <- as.integer(type[2])
#     
#     models <- train_models(type=type[1], nsymbols=nsymbols, uniform=uniform, nstates=nstates)
#     save(models, file=paste("models", if (uniform) "uniform" else "optimal", nstates, type[1], sep="/"))
#     print(paste("Done with:", type[1], nsymbols, "optimal", nstates))
#   }
# }

# obs <- load_files(type="5_3", train=F)
# # 
# classes <- sapply(
#   obs$observations,
#   function(x) classify(models_optimal_5_3, x)
# )

load("models/uniform/3/models_uniform_5_4")

# for (nstates in states){
#   for (type in types){
#     recoJ(type[1], nstates, uniform=uniform)
#   }
# }

print("########### DONE ###########")