# dependency --------------------------------------------------------------
library(dplyr)

# SETTUP ------------------------------------------------------------------
setwd('~/phDCompute/let_model_go/')

# variables ---------------------------------------------------------------
#pubmodel <- list()
remodel <- list()

init <- list(default = c(0.05, 0.15, 0.35, 0.55, 0.75, 0.95),
             beta = seq(0.25, 5, by = 1))

# custom ------------------------------------------------------------------
getaic <- function(n.parameters, nll) {2 * nll + 2 * n.parameters}

#listnth <- function(inp, n){sapply(inp, `[`, n)}

getoptimal_ <- function(dataset, list.parameters){ #one parameter
  out = c(tag = as.numeric(unique(dataset$tag)), 
          par = 0, value = Inf, maxPar = 0)
  for(i in list.parameters){
    value = model(i, dataset)
    out[4] = i
    if(value < out[3]){
      out[2] = i
      out[3] = value}}
  out}

getoptimal <- function(input.data, list.parameters) {
  optim.results <- list()
  sample.size = nrow(list.parameters)
  val = Inf
  mouse = as.numeric(unique(input.data$tag))
  temp <- apply(list.parameters, 1, function(x){
    optim(par = x, fn = model, a = input.data)})
  best.optim <- temp[[which.min(lapply(temp,function(x) {unlist(x$value)}))]]
  c(tag = mouse, unlist(best.optim))}

wrapmodel <- function(list.parameters, a = dmodel, tags = NULL) {
  if(is.null(tags)){tags = as.character(unique(a$tag))}
  progressbar <- txtProgressBar(0, length(tags), char = '*', style = 3)
  output = list()
  for(m in seq_along(tags)) {
    dmouse = a[a$tag == tags[m], ]
    output[[m]] = getoptimal(dmouse, list.parameters)
    setTxtProgressBar(progressbar, m)}
  close(progressbar)
  do.call(rbind, output)}

util_wrap <- function(name, ...){
  initial <- expand.grid(...)
  #initial = as.list(initial)
  wrapmodel(initial) %>% as_tibble() %>%
    mutate(name = name, tag = as.character(tag),
           aic = getaic(length(initial), value))}

# BASIC ONES --------------------------------------------------------------
# basic -------------------------------------------------------------------
model <- function(par, a) {
  a = a[with(a, order(start)), ]
  nll = 0
  if (par[1] < 0 | par[1] > 1 |
      par[2] < 0 | par[2] > 50) {
    nll = Inf
  } else {
    Q = c(0, 0)
    P <- vector()
    rewards = a$dooropened
    sides = ceiling(a$corner / 2)
    for (i in seq_along(sides)) {
      r = rewards[i]
      s = sides[i]
      P = exp(par[2] * Q) / sum(exp(par[2] * Q))
      if(P[s] < .001){P[s] = .001}
      if(P[s] > .999){P[s] = .999}
      nll = -log(P[s]) + nll
      pe = r - Q[s]
      Q[s] = Q[s] + (par[1] * pe)}}#}
  nll}

remodel[["basic"]] <- util_wrap("basic", alpha = init$default, 
                               beta = init$beta)

# dual --------------------------------------------------------------------
model <- function(par, a) {
  a = a[with(a, order(start)), ]
  nll = 0
  if (par[1] < 0 | par[1] > 1 |
      par[2] < 0 | par[2] > 50 |
      par[3] < 0 | par[3] > 1) {
    nll = Inf
  } else {
    Q = c(0, 0)
    P <- vector()
    rewards = a$dooropened
    sides = ceiling(a$corner / 2)
    for (i in seq_along(sides)) {
      r = rewards[i]
      s = sides[i]
      P = exp(par[2] * Q) / sum(exp(par[2] * Q))
      if(P[s] < .001){P[s] = .001}
      if(P[s] > .999){P[s] = .999}
      nll = -log(P[s]) + nll
      pe = r - Q[s]
      if (r == 1) {Q[s] = Q[s] + (par[1] * pe)
      } else {Q[s] = Q[s] + (par[3] * pe)}}}
  nll}

remodel[["dual"]] <- util_wrap("dual", alpha.pos = init$default, 
                              beta = init$beta, alpha.neg = init$default)

# attention ---------------------------------------------------------------
model <- function(par, a) {
  a = a[with(a, order(start)), ]
  nll = 0
  if (par[1] < 0 | par[1] > 1 |
      par[2] < 0 | par[2] > 50 |
      par[3] < 0 | par[3] > 1) {
    nll = Inf
  } else {
    Q = c(0, 0)
    P <- vector()
    rewards = a$dooropened
    sides = ceiling(a$corner / 2)
    alpha.zero = par[1]
    for (i in seq_along(sides)) {
      r = rewards[i]
      s = sides[i]
      P = exp(par[2] * Q) / sum(exp(par[2] * Q))
      if(P[s] < .001){P[s] = .001}
      if(P[s] > .999){P[s] = .999}
      nll = -log(P[s]) + nll
      pe = r - Q[s]
      alpha = par[3] * abs(pe) + (1 - par[3]) * alpha.zero
      Q[s] = Q[s] + (alpha * pe)}}
  nll}

remodel[["attention"]] <- 
  util_wrap("attention", alpha.zero = init$default, 
            beta = init$beta, ni= init$default)

# fictitious --------------------------------------------------------------
model <- function(par, a) {
  a = a[with(a, order(start)), ]
  nll = 0
  if ((par[1] < 0 | par[1] > 1) |
      (par[2] < 0 | par[2] > 50)) {
    nll = Inf
  } else {
    Q = c(0, 0)
    P <- vector()
    rewards = a$dooropened
    sides = ceiling(a$corner / 2)
    for (i in seq_along(sides)) {
      r = rewards[i]
      s = sides[i]
      P = exp(par[2] * Q) / sum(exp(par[2] * Q))
      if(P[s] < .001){P[s] = .001}
      if(P[s] > .999){P[s] = .999}
      nll = -log(P[s]) + nll
      pe = r - Q[s]
      Q[s] = Q[s] + (par[1] * pe)
      Q[-s] = Q[-s] - (par[1] * pe)}}
  nll}

remodel[["fictitious"]] <- util_wrap("fictitious", 
                                    alpha = init$default,
                                    beta = init$beta)

# hybrid ------------------------------------------------------------------
model <- function(par, a) {
  a = a[with(a, order(start)), ]
  nll = 0
  if (par[1] < 0 | par[1] > 1 |
      par[2] < 0 | par[2] > 50 |
      par[3] < 0 | par[3] > 1) {
    nll = Inf
  } else {
    Q = c(0, 0)
    P <- vector()
    rewards = a$dooropened
    sides = ceiling(a$corner / 2)
    for (i in seq_along(sides)) {
      r = rewards[i]
      s = sides[i]
      P = exp(par[2] * Q) / sum(exp(par[2] * Q))
      if(P[s] < .001){P[s] = .001}
      if(P[s] > .999){P[s] = .999}
      nll = -log(P[s]) + nll
      pe = r - Q[s]
      if (r == 1) {
        Q[s] = Q[s] + (par[1] * pe)
        Q[-s] = Q[-s] - (par[1] * pe)
      } else {
        Q[s] = Q[s] + (par[3] * pe)
        Q[-s] = Q[-s] - (par[3] * pe)}}}
  nll}

remodel[["hybrid"]] <- util_wrap("hybrid",alpha.pos = init$default,
                                beta =  init$beta,
                                alpha.neg = init$default)

# forgetful ---------------------------------------------------------------
model <- function(par, a) {
  a = a[with(a, order(start)), ]
  nll = 0
  if (par[1] < 0 | par[1] > 1 |
      par[2] < 0 | par[2] > 50 |
      par[3] < 0 | par[3] > 1) {
    nll = Inf
  } else {
    Q = c(0, 0)
    P <- vector()
    rewards = a$dooropened
    sides = ceiling(a$corner / 2)
    for (i in seq_along(sides)) {
      r = rewards[i]
      s = sides[i]
      P = exp(par[2] * Q) / sum(exp(par[2] * Q))
      if(P[s] < .001){P[s] = .001}
      if(P[s] > .999){P[s] = .999}
      nll = -log(P[s]) + nll
      pe = r - Q[s]
      Q[s] = Q[s] + par[1] * pe
      delta = 0.5 - Q[s]
      Q[s] = Q[s] + par[3] * delta
      Q[-s] = Q[-s] + par[3] * delta}}
  nll}

remodel[["forgetful"]] <- util_wrap("forgetful",
                                   alpha = init$default,
                                   beta = init$beta,
                                   epsilon = init$default)

#  TIME-DEPENDENT ---------------------------------------------------------
# q-decay -----------------------------------------------------------------
model <- function(par, a) {
  a = a[with(a, order(start)), ]
  nll = 0
  if (par[1] < 0 | par[1] > 1 |
      par[2] < 0 | par[2] > 50 | 
      par[3] < 0 | par[3] > 1) {
    nll = Inf
  } else {
    Q = c(0, 0)
    t = c(0, 0)
    P <- vector()
    rewards = a$dooropened
    sides = ceiling(a$corner/2)
    nows.start = a$start
    nows.end = a$end
    intervals = a$intervalb
    intervals[1] = 0
    for (i in seq_along(sides)) {
      r = rewards[i]
      s = sides[i]
      now.start = nows.start[i]
      now.end = nows.end[i]
      t = intervals[i]
      t = ifelse(t > 660, 660, t)
      Q = exp( -(t) * par[3] ) * Q
      P = exp(par[2] * Q) / sum(exp(par[2] * Q))
      if(P[s] < .001){P[s] = .001}
      if(P[s] > .999){P[s] = .999}
      nll = -log(P[s]) + nll
      pe = r - Q[s]
      Q[s] = Q[s] + (par[1] * pe)}}
  nll}

remodel[["q-decay"]] <- util_wrap("q-decay",
                               alpha = init$default,
                               beta = init$beta,
                               storage = init$default)

# q-decay split -----------------------------------------------------------
# model <- function(par, a) {
#   a = a[with(a, order(start)), ]
#   nll = 0
#   if (par[1] < 0 | par[1] > 1 |
#       par[2] < 0 | par[2] > 50 | 
#       par[3] < 0 | par[3] > 1 |
#       par[4] < 0 | par[4] > 1) {
#     nll = Inf
#   } else {
#     Q = c(0, 0)
#     t = c(0, 0)
#     P <- vector()
#     rewards = a$dooropened
#     sides = ceiling(a$corner/2)
#     nows.start = a$start
#     nows.end = a$end
#     intervals = a$intervalb
#     intervals[1] = 0
#     rew = c(0,0)
#     for (i in seq_along(sides)) {
#       r = rewards[i]
#       s = sides[i]
#       now.start = nows.start[i]
#       now.end = nows.end[i]
#       t = intervals[i]
#       t = ifelse(t > 660, 660, t)
#       decay = ifelse(rew == 1, par[3], par[4])
#       rew[s] = r
#       Q = exp( -(t) * decay ) * Q
#       P = exp(par[2] * Q) / sum(exp(par[2] * Q))
#       if(P[s] < .001){P[s] = .001}
#       if(P[s] > .999){P[s] = .999}
#       nll = -log(P[s]) + nll
#       pe = r - Q[s]
#       Q[s] = Q[s] + (par[1] * pe)}}
#   nll}
# 
# remodel[["q-decay+"]] <- util_wrap("q-decay+",
#                                alpha = init$default,
#                                beta = init$beta,
#                                storage.pos = init$default,
#                                storage.neg = init$default)

# q-decay fictitious ------------------------------------------------------
model <- function(par, a) {
  a = a[with(a, order(start)), ]
  nll = 0
  if (par[1] < 0 | par[1] > 1 |
      par[2] < 0 | par[2] > 50 | 
      par[3] < 0 | par[3] > 1) {
    nll = Inf
  } else {
    Q = c(0, 0)
    t = c(0, 0)
    P <- vector()
    rewards = a$dooropened
    sides = ceiling(a$corner/2)
    nows.start = a$start
    nows.end = a$end
    intervals = a$intervalb
    intervals[1] = 0
    for (i in seq_along(sides)) {
      r = rewards[i]
      s = sides[i]
      now.start = nows.start[i]
      now.end = nows.end[i]
      t = intervals[i]
      t = ifelse(t > 660, 660, t)
      Q = exp( -(t) * par[3] ) * Q
      P = exp(par[2] * Q) / sum(exp(par[2] * Q))
      if(P[s] < .001){P[s] = .001}
      if(P[s] > .999){P[s] = .999}
      nll = -log(P[s]) + nll
      pe = r - Q[s]
      Q[s] = Q[s] + (par[1] * pe)
      Q[-s] = Q[-s] - (par[1] * pe)}}
  nll}

remodel[["q-decay*"]] <- util_wrap("q-decay*",
                                 alpha = init$default,
                                 beta = init$beta,
                                 storage = init$default)

# relational --------------------------------------------------------------
# model <- function(par, a) {
#   a = a[with(a, order(start)), ]
#   nll = 0
#   if (par[1] < 0 | par[1] > 1 |
#       par[2] < 0 | par[2] > 50 |
#       par[3] < 0 | par[3] > 1 |
#       par[4] < 0 | par[4] > 7) {
#     nll = Inf
#   } else {
#     Q = c(0, 0)
#     P <- vector()
#     b0 = 0
#     rewards = a$dooropened
#     sides = ceiling(a$corner / 2)
#     previous = lag(sides)
#     previous[1] = 1
#     intervals = a$intervalb
#     intervals[1] = 0
#     for (i in seq_along(sides)) {
#       r = rewards[i]
#       s = sides[i]
#       w = previous[i]
#       t = intervals[i]
#       P = exp(par[2] * Q) / sum(exp(par[2] * Q))
#       P = ifelse(P < .001, .001, P)
#       P = ifelse(P > .999, .999, P)
#       b0 = log(P[w] / (1 - P[w]))
#       if(t > 660) t = 660
#       P[w] = exp(b0 + par[3] * t - par[4]) / 
#         (1 + exp(b0 + par[3] * t - par[4]))
#       P[-w] = 1 - P[w]
#       if(P[s] < .001){P[s] = .001}
#       if(P[s] > .999){P[s] = .999}
#       nll = -log(P[s]) + nll
#       pe = r - Q[s] 
#       Q[s] = Q[s] + (par[1] * pe)}}
#   nll}
# 
# remodel[["relational"]] <- 
#   util_wrap("relational", alpha = init$default, beta = init$beta,
#             iota = init$default, rho = init$default)
# 
# # relational + fictitious -------------------------------------------------
# model <- function(par, a) {
#   a = a[with(a, order(start)), ]
#   nll = 0
#   if (par[1] < 0 | par[1] > 1 |
#       par[2] < 0 | par[2] > 50 |
#       par[3] < 0 | par[3] > 1 |
#       par[4] < 0 | par[4] > 7) {
#     nll = Inf
#   } else {
#     Q = c(0, 0)
#     P <- vector()
#     b0 = 0
#     rewards = a$dooropened
#     sides = ceiling(a$corner / 2)
#     previous = lag(sides)
#     previous[1] = 1
#     intervals = a$intervalb
#     intervals[1] = 0
#     for (i in seq_along(sides)) {
#       r = rewards[i]
#       s = sides[i]
#       w = previous[i]
#       t = intervals[i]
#       P = exp(par[2] * Q) / sum(exp(par[2] * Q))
#       P = ifelse(P < .001, .001, P)
#       P = ifelse(P > .999, .999, P)
#       b0 = log(P[w] / (1 - P[w]))
#       if(t > 660) t = 660
#       P[w] = exp(b0 + par[3] * t - par[4]) / 
#         (1 + exp(b0 + par[3] * t - par[4]))
#       P[-w] = 1 - P[w]
#       if(P[s] < .001){P[s] = .001}
#       if(P[s] > .999){P[s] = .999}
#       nll = -log(P[s]) + nll
#       pe = r - Q[s] 
#       Q[s] = Q[s] + (par[1] * pe)
#       Q[-s] = Q[-s] - (par[1] * pe)}}
#   nll}
# 
# remodel[["relational*"]] <- 
#   util_wrap("relational*", alpha = init$default, beta = init$beta,
#             iota = init$default, rho = init$default)

# beta-decay --------------------------------------------------------------
model <- function(par, a) {
  a = a[with(a, order(start)), ]
  nll = 0
  if (par[1] < 0 | par[1] > 1 |
      par[2] < 0 | par[2] > 50 | 
      par[3] < 0 | par[3] > 1) {
    nll = Inf
  } else {
    Q = c(0, 0)
    t = 0
    P <- vector()
    rewards = a$dooropened
    sides = ceiling(a$corner/2)
    intervals = a$intervalb
    intervals[1] = 0
    beta.zero = par[2]
    for (i in seq_along(sides)) {
      r = rewards[i]
      s = sides[i]
      t = intervals[i]
      t = ifelse(t > 660, 660, t)
      beta = exp( -t * par[3] ) * beta.zero
      P = exp(beta * Q) / sum(exp(beta * Q))
      if(P[s] < .001){P[s] = .001}
      if(P[s] > .999){P[s] = .999}
      nll = -log(P[s]) + nll
      pe = r - Q[s]
      Q[s] = Q[s] + (par[1] * pe)}}
  nll}

remodel[["b-decay"]] <- 
  util_wrap("b-decay", alpha = init$default,
            beta = init$beta, bdecay = init$default)

# beta-decay split --------------------------------------------------------
# model <- function(par, a) {
#   a = a[with(a, order(start)), ]
#   nll = 0
#   if (par[1] < 0 | par[1] > 1 |
#       par[2] < 0 | par[2] > 50 | 
#       par[3] < 0 | par[3] > 1 |
#       par[4] < 0 | par[4] > 1) {
#     nll = Inf
#   } else {
#     Q = c(0, 0)
#     date = a$start[1]
#     t = 0
#     P <- vector()
#     rewards = a$dooropened
#     sides = ceiling(a$corner/2)
#     intervals = a$intervalb
#     intervals[1] = 0
#     beta.zero = par[2]
#     rew = 0
#     for (i in seq_along(sides)) {
#       r = rewards[i]
#       s = sides[i]
#       t = intervals[i]
#       t = ifelse(t > 660, 660, t)
#       decay = ifelse(rew == 1, par[3], par[4])
#       beta = exp( -t * decay ) * beta.zero
#       P = exp(beta * Q) / sum(exp(beta * Q))
#       if(P[s] < .001){P[s] = .001}
#       if(P[s] > .999){P[s] = .999}
#       nll = -log(P[s]) + nll
#       pe = r - Q[s]
#       Q[s] = Q[s] + (par[1] * pe)
#       rew = r}}
#   nll}
# 
# remodel[["b-decay+"]] <- 
#   util_wrap("b-decay+", alpha = init$default,
#             beta = init$beta,
#             bdecay.pos = init$default,
#             bdecay.neg = init$default)

# beta-decay fictitious ---------------------------------------------------
model <- function(par, a) {
  a = a[with(a, order(start)), ]
  nll = 0
  if (par[1] < 0 | par[1] > 1 |
      par[2] < 0 | par[2] > 50 | 
      par[3] < 0 | par[3] > 1) {
    nll = Inf
  } else {
    Q = c(0, 0)
    date = rep(a$start[1], 2)
    t = 0
    P <- vector()
    rewards = a$dooropened
    sides = ceiling(a$corner/2)
    intervals = a$intervalb
    intervals[1] = 0
    beta.zero = par[2]
    for (i in seq_along(sides)) {
      r = rewards[i]
      s = sides[i]
      t = intervals[i]
      t = ifelse(t > 660, 660, t)
      beta = exp( -t * par[3] ) * beta.zero
      P = exp(beta * Q) / sum(exp(beta * Q))
      if(P[s] < .001){P[s] = .001}
      if(P[s] > .999){P[s] = .999}
      nll = -log(P[s]) + nll
      pe = r - Q[s]
      Q[s] = Q[s] + (par[1] * pe)
      Q[-s] = Q[-s] - (par[1] * pe)}}
  nll}

remodel[["b-decay*"]] <- 
  util_wrap("b-decay*", alpha = init$default,
            beta = init$beta,
            bdecay = init$default)

#  CONTROL ----------------------------------------------------------------
# noisy win-stay ----------------------------------------------------------
model <- 
  function(par, a) {
    a = a[with(a, order(start)), ]
    nll = 0
    if (par[1] < 0.001 | par[1] > 1.999) {
      nll = Inf
    } else {
      P <- vector()
      rewards = a$dooropened
      sides = ceiling(a$corner/2)
      previous.rewards = lag(rewards)
      previous.sides = lag(sides)
      for (i in seq_along(sides)) {
        r = previous.rewards[i]
        c = previous.sides[i]
        k = sides[i]
        stay = c == k
        win = r == 1
        if(i == 1){ P[k] = .5
        } else {
          if((win & stay) | (!win & !stay)){ 
            P[k] = 1 - par[1]/2 
            if(P[k] < .001){P[k] = .001}
            if(P[k] > .999){P[k] = .999}} 
          if((win & !stay)| (!win & stay)){ 
            P[k] = 0 + par[1]/2 
            if(P[k] < .001){P[k] = .001}
            if(P[k] > .999){P[k] = .999}}}
        nll = -log(P[k]) + nll}}
    nll}

remodel[["noisywinstay"]] <- 
  (function(initials, a = dmodel) {
    tags = as.character(levels(as.factor(a$tag)))
    output = list()
    for (m in seq_along(tags)) {
      dmouse = a[a$tag == tags[m], ]
      output[[m]] = getoptimal_(dmouse, initials)}
    do.call(rbind, output)})(seq(0.001, 1.999, 0.001)) %>% as_tibble() %>%
  mutate(name = "noisywinstay", tag = as.character(tag), aic = getaic(1, value))

# random ------------------------------------------------------------------
remodel[["random"]] <- dmodel %>%
  group_by(tag) %>%
  summarise(n = length(corner)) %>%
  mutate(name = "random", aic = getaic(0, n * -log(0.5)))

print("done: publikacja")
saveRDS(remodel_publikacja, 'result/remodel_publikacja.rds')
#rm(dmodel)
#rm(remodel_publikacja)
