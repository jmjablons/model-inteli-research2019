### get the surface ###
# get optimal parameters step by step

# dependency --------------------------------------------------------------
library(dplyr)

# 1 choose model ----------------------------------------------------------

#{model <- ...}

# 2 pregenerate parameter values ------------------------------------------

init <- expand.grid(alpha = seq(0,1,.01), beta = seq(0,5,.1), bdecay = seq(0,1,.01))
init = lapply(seq_len(nrow(init)), function(i) unlist(init[i,]))

# 3 run the model on initials ---------------------------------------------
surface <- list()

util$surface <- function(tag.vec = NULL, a = subset(dmodel, tag %in% hero)) {
  if(is.null(tag.vec)){tag.vec = unique(a$tag)} 
  lapply(tag.vec, function(m){
    dmouse = a[a$tag %in% m,]
    do.call(rbind, lapply(init, function(x){
      c(tag = as.numeric(m), x, nll = model(par = x, a = dmouse))}))})}

surface$basic <- init
surface$basic$nll <- apply(init, 1, function(x){model(par = x, a = dhero)})

surface$fictitious <- init
surface$fictitious$nll <- apply(init, 1, function(x){model(par = x, a = dhero)})

surface$`bdecay*` <- init
surface$`bdecay*`$nll <- apply(init, 1, function(x){model(par = x, a = dhero)})

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

# 4 check results ---------------------------------------------------------

# 5 plot surface ----------------------------------------------------------
# library(ggplot2)
# library(patchwork)
# 
# plot_surface <- function(a, fn = mean){a %>% group_by(alpha, beta) %>%
#         summarise(measure = do.call(what = fn, args = list(nll))) %>%
#         ggplot(aes(alpha, beta, z = measure)) +
#         geom_raster(aes(fill = measure))+
#       geom_contour(colour = 'white', binwidth = 10)+
#       theme_publication}
# 
# p1 <- plot_surface(surf.a, fn = median)
# 
# p2 <- plot_surface(surf.as, fn = median)
# 
# p3 <- plot_surface(surf.s, fn = median)
# 
# p4 <- plot_surface(surf.w, fn = median)
# 
# (p1 | p2) / (p3 | p4)

# check optim -------------------------------------------------------------
# soptimx::optimx(par = c(.1, 1, .01), fn = model, 
#                a = filter(dmodel, tag == manimal$tag[4]), 
#                method = c("Nelder-Mead", "BFGS","CG", "SANN", "ucminf", 
#                           "bobyqa", "nmkb"))
