### get the surface ###
# get optimal parameters step by step

# dependency --------------------------------------------------------------
library(dplyr)

# 1 choose model ----------------------------------------------------------

#{model <- ...}

# 2 pregenerate parameter values ------------------------------------------

init <- expand.grid(alpha = seq(0,1,.01), beta = seq(0,5,.1), 
                    bdecay = seq(0,1,.01))
init = lapply(seq_len(nrow(init)), function(i) unlist(init[i,]))

# 3 run the model on initials ---------------------------------------------
surface <- list()

surface$basic <- init
surface$basic$nll <- apply(init, 1, function(x){model(par = x, a = dhero)})

surface$fictitious <- init
surface$fictitious$nll <- apply(init, 1, function(x){model(par = x, a = dhero)})

#surface$`bdecay*` <- init
#surface$`bdecay*`$nll <- apply(init, 1, function(x){model(par = x, a = dhero)})

# surf.a = util_surface(tag.vec = with(manimal, {
#   tag[substance == "alcohol"]})) %>% 
#   do.call(rbind,.) %>% as_tibble() 
# 
# {
# tstart = Sys.time()
# surf.test.qdecay2 <- util_surface(tag.vec = hero) %>% 
#   do.call(rbind,.) %>% as_tibble()
# tfinish = Sys.time()
# }
# surf.s = util_surface(tag.vec = with(manimal, {
#   tag[substance == "saccharin"]})) %>% 
#   do.call(rbind,.) %>% as_tibble() 
# 
# surf.as = util_surface(tag.vec = with(manimal, {
#   tag[substance == "alcohol+saccharin"]})) %>% 
#   do.call(rbind,.) %>% as_tibble() 
# 
# surf.w = util_surface(tag.vec = with(manimal, {
#   tag[substance == "water"]})) %>% 
#   do.call(rbind,.) %>% as_tibble() 
