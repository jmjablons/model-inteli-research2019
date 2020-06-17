### get the surface ###
# get optimal parameters step by step

# dependency --------------------------------------------------------------
library(dplyr)

# 1 choose model ----------------------------------------------------------

#{model <- ...}

# 2 pregenerate parameter values ------------------------------------------

init <- expand.grid(alpha = seq(0,1,.01), beta = seq(0,5,.1))

init = lapply(seq_len(nrow(init)), function(i) unlist(init[i,]))

# 3 run the model on initials ---------------------------------------------
surface <- list()

surface$basic <- init
surface$basic$nll <- apply(init, 1, function(x){model(par = x, a = dhero)})

surface$fictitious <- init
surface$fictitious$nll <- apply(init, 1, function(x){model(par = x, a = dhero)})