# 2019-11-28
# judyta
# model choices

# dependency --------------------------------------------------------------
library(dplyr)

# result ------------------------------------------------------------------
rmodel <- list()
fig <- list()

# variables ---------------------------------------------------------------
initials.beta = seq(0.25, 15, by = 0.50)
initials.default = seq(0.05, 1, by = 0.05)
initials.primitive = seq(0, 1, by = 0.05)

# basic -------------------------------------------------------------------
model <- 
  function(par, a) {
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
      #if (is.finite(s) & is.finite(r)) {
        P = exp(par[2] * Q) / sum(exp(par[2] * Q))
        if(P[s] < .001){P[s] = .001}
        if(P[s] > .999){P[s] = .999}
        nll = -log(P[s]) + nll
        pe = r - Q[s]
        Q[s] = Q[s] + (par[1] * pe)}}#}
  nll}

{name = "basic"
  initial <- expand.grid(
      alpha = initials.default,
      beta = initials.beta) %>%
    as.list()
  rmodel[[name]] <- getModelMice(list.parameters = initial, A = dmodel) %>%
    mutate(name = name, aic = calculateAIC(length(initial), as.numeric(value)))}

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

{name = "dual"
  initial <- expand.grid(
      alpha.pos = initials.default,
      beta =  initials.beta,
      alpha.neg = initials.default) %>% 
    as.list()
  rmodel[[name]] <- getModelMice(initial) %>%
    mutate(name = name,
           aic = calculateAIC(length(initial), as.numeric(value)))}

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

{name = "attention"
  initial <- expand.grid(
      alpha.zero = initials.default,
      beta = initials.beta,
      ni= initials.default) %>%
    as.list()
  rmodel[[name]] <- getModelMice(initial) %>%
    mutate(name = name,
           aic = calculateAIC(length(initial), as.numeric(value)))}


# attention cumulative ----------------------------------------------------
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
    alpha = par[1]
    for (i in seq_along(sides)) {
      r = rewards[i]
      s = sides[i]
      P = exp(par[2] * Q) / sum(exp(par[2] * Q))
      if(P[s] < .001){P[s] = .001}
      if(P[s] > .999){P[s] = .999}
      nll = -log(P[s]) + nll
      pe = r - Q[s]
      alpha = par[3] * abs(pe) + (1 - par[3]) * alpha
      Q[s] = Q[s] + (alpha * pe)}}
  nll}

{name = "attention+"
  initial <- expand.grid(
    alpha.first = initials.default,
    beta = initials.beta,
    ni= initials.default) %>%
    as.list()
  rmodel[[name]] <- getModelMice(initial) %>%
    mutate(name = name,
           aic = calculateAIC(length(initial), as.numeric(value)))}

# fictitious --------------------------------------------------------------
model <- 
  function(par, a) {
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

{name = "fictitious"
  initial <- expand.grid(
      alpha = initials.default,
      beta = initials.beta) %>%
    as.list()
  rmodel[[name]] <- getModelMice(initial) %>%
    mutate(name = name,
           aic = calculateAIC(length(initial), as.numeric(value)))}

# hybrid ------------------------------------------------------------------
model <- 
  function(par, a) {
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

{name = "hybrid"
  initial <- expand.grid(
      alpha.pos = initials.default,
      beta =  initials.beta,
      alpha.neg = initials.default) %>% 
    as.list()
  rmodel[[name]] <- getModelMice(initial) %>%
    mutate(name = name,
           aic = calculateAIC(
             length(initial), as.numeric(value)))}

# hybrid+ -----------------------------------------------------------------
#TODO nie przechodzi, wywyala blad o braku P
#Error in if (any(P < 0.001)) { : missing value where TRUE/FALSE needed 
# model <- 
#   function(par, a) {
#     a = a[with(a, order(start)), ]
#     nll = 0
#     if (par[1] < 0 | par[1] > 1 |
#         par[2] < 0 | par[2] > 50 |
#         par[3] < 0 | par[3] > 1 |
#         par[4] < 0 | par[4] > 1 |
#         par[5] < 0 | par[5] > 1) {
#       nll = Inf
#     } else {
#       Q = c(0, 0)
#       P <- vector()
#       rewards = a$dooropened
#       sides = ceiling(a$corner / 2)
#       for (i in seq_along(sides)) {
#         r = rewards[i]
#         s = sides[i]
#         if (is.finite(s) & is.finite(r)) {
#           P = exp(par[2] * Q) / sum(exp(par[2] * Q))
#           if(P[s] < .001){P[s] = .001}
#           if(P[s] > .999){P[s] = .999}
#           nll = -log(P[s]) + nll
#           pe = r - Q[s]
#           if (r == 1) {
#             Q[s] = Q[s] + (par[1] * pe)
#             Q[-s] = Q[-s] - (par[4] * pe)
#           } else {
#             Q[s] = Q[s] + (par[3] * pe)
#             Q[-s] = Q[-s] - (par[5] * pe)}}}}
#     nll}
# 
# {name = "hybrid+"
#   initial <- expand.grid(
#     alpha.pos.chosen = initials.default,
#     beta =  initials.beta,
#     alpha.neg.chosen = initials.default,
#     alpha.pos.nonchosen = initials.default,
#     alpha.neg.nonchosen = initials.default) %>% 
#     as.list()
#   rmodel[[name]] <- getModelMice(initial) %>%
#     mutate(name = name,
#            aic = calculateAIC(length(initial), as.numeric(value)))}
#
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

{name = "forgetful"
  initial <- expand.grid(
      alpha = initials.default,
      beta = initials.beta,
      epsilon = initials.default) %>% 
    as.list()
  rmodel[[name]] <- getModelMice(initial) %>%
    mutate(name = name, aic = calculateAIC(length(initial), as.numeric(value)))}

# forgetful+ --------------------------------------------------------------
# #TODO nie przechodzi, blad
# #Error in if (any(P < 0.001)) { : missing value where TRUE/FALSE needed 
# model <- function(par, a) {
#   a = a[with(a, order(start)), ]
#   nll = 0
#   if (par[1] < 0 | par[1] > 1 |
#       par[2] < 0 | par[2] > 50) {
#     nll = Inf
#   } else {
#     Q = c(0, 0)
#     P <- vector()
#     rewards = a$dooropened
#     sides = ceiling(a$corner / 2)
#     intervals = a$intervalb
#     intervals[intervals > 120] = 120
#     ep = 0
#     for (i in seq_along(sides)) {
#       r = rewards[i]
#       s = sides[i]
#       t = intervals[i]
#       if (is.finite(s) & is.finite(r)) {
#         ep = ifelse(t > exp(1) & !is.na(t), 1 - 1/log(t), NA)
#         P = exp(par[2] * Q) / sum(exp(par[2] * Q))
#         if(P[s] < .001){P[s] = .001}
#         if(P[s] > .999){P[s] = .999}
#         nll = -log(P[s]) + nll
#         pe = r - Q[s]
#         Q[s] = Q[s] + par[1] * pe
#         delta = 0.5 - Q[s]
#         Q[s] = Q[s] + ep * delta
#         Q[-s] = Q[-s] + ep * delta}}}
#   nll}
# 
# {name = "forgetful+"
#   initial <- expand.grid(
#     alpha = initials.default,
#     beta = initials.beta) %>% 
#     as.list()
#   rmodel[[name]] <- getModelMice(initial) %>%
#     mutate(name = name,
#            aic = calculateAIC(length(initial), as.numeric(value)))}
#
# decay -------------------------------------------------------------------
model <- function(par, a) {
    a = a[with(a, order(start)), ]
    nll = 0
    if (par[1] < 0 | par[1] > 1 |
        par[2] < 0 | par[2] > 50 | 
        par[3] < 0 | par[3] > 1) {
      nll = Inf
    } else {
      tempQ = c()
      Q = c(0, 0)
      date = rep(a$start[1], 2)
      t = c(0, 0)
      P <- vector()
      rewards = a$dooropened
      sides = ceiling(a$corner/2)
      nows = a$start
      for (i in seq_along(sides)) {
        r = rewards[i]
        s = sides[i]
        now = nows[i]
        t = as.numeric(difftime(now, date, units = 'mins'))
        date[s] = now
          Q[s] = exp( -(t[s]) * par[3] ) * Q[s]
          tempQ = exp( -(t[-s]) * par[3] ) * Q[-s]
          P[s] = exp(par[2] * Q[s]) / 
            (sum(exp(par[2] * Q[s]), exp(par[2] * tempQ)))
          if(P[s] < .001){P[s] = .001}
          if(P[s] > .999){P[s] = .999}
          nll = -log(P[s]) + nll
          pe = r - Q[s]
          Q[s] = Q[s] + (par[1] * pe)}}
    nll}

{name = "decay"
  initial <- expand.grid(
      alpha = initials.default,
      beta = initials.beta,
      storage = initials.primitive) %>%
    as.list()
  rmodel[[name]] <- getModelMice(initial) %>%
    mutate(name = name, aic = calculateAIC(length(initial), as.numeric(value)))}

# decay fictitious --------------------------------------------------------
model <- function(par, a) {
  a = a[with(a, order(start)), ]
  nll = 0
  if (par[1] < 0 | par[1] > 1 |
      par[2] < 0 | par[2] > 50 |
      par[3] < 0 | par[3] > 1) {
    nll = Inf
  } else {
    tempQ = c()
    Q = c(0, 0)
    date = rep(a$start[1], 2)
    t = c(0, 0)
    P <- vector()
    rewards = a$dooropened
    sides = ceiling(a$corner/2)
    nows = a$start
    rew = c(0,0)
    for (i in seq_along(sides)) {
      r = rewards[i]
      s = sides[i]
      now = nows[i]
      t = as.numeric(difftime(now, date, units = 'mins'))
      date[s] = now
        Q[s] = exp( -(t[s]) * par[3] ) * Q[s]
        tempQ = exp( -(t[-s]) * par[3] ) * Q[-s]
        P[s] = exp(par[2] * Q[s]) / 
          (sum(exp(par[2] * Q[s]), exp(par[2] * tempQ)))
        if(P[s] < .001){P[s] = .001}
        if(P[s] > .999){P[s] = .999}
        nll = -log(P[s]) + nll
        pe = r - Q[s]
        Q[s] = Q[s] + (par[1] * pe)
        Q[-s] = Q[-s] + (par[1] * pe)}}
  nll}

{name = "decay*"
  initial <- expand.grid(
    alpha = initials.default,
    beta = initials.beta,
    storage = initials.primitive) %>%
    as.list()
  rmodel[[name]] <- getModelMice(initial) %>%
    mutate(name = name, aic = calculateAIC(length(initial), as.numeric(value)))}

# decay split fictitious --------------------------------------------------
model <- function(par, a) {
  a = a[with(a, order(start)), ]
  nll = 0
  if (par[1] < 0 | par[1] > 1 |
      par[2] < 0 | par[2] > 50 |
      par[3] < 0 | par[3] > 1 |
      par[4] < 0 | par[4] > 1) {
    nll = Inf
  } else {
    tempQ = c()
    Q = c(0, 0)
    date = rep(a$start[1], 2)
    t = c(0, 0)
    P <- vector()
    rewards = a$dooropened
    sides = ceiling(a$corner/2)
    nows = a$start
    rew = c(0,0)
    for (i in seq_along(sides)) {
      r = rewards[i]
      s = sides[i]
      now = nows[i]
      t = as.numeric(difftime(now, date, units = 'mins'))
      date[s] = now
        decay = ifelse(rew == 1, par[3], par[4])
        Q[s] = exp( -(t[s]) * decay[s] ) * Q[s]
        tempQ = exp( -(t[-s]) * decay[-s] ) * Q[-s]
        P[s] = exp(par[2] * Q[s]) / 
          (sum(exp(par[2] * Q[s]), exp(par[2] * tempQ)))
        if(P[s] < .001){P[s] = .001}
        if(P[s] > .999){P[s] = .999}
        nll = -log(P[s]) + nll
        pe = r - Q[s]
        Q[s] = Q[s] + (par[1] * pe)
        Q[-s] = Q[-s] + (par[1] * pe)
        rew[s] = r}}
  nll}

{name = "decay+"
  initial <- expand.grid(
    alpha = initials.default,
    beta = initials.beta,
    storage.pos = initials.primitive,
    storage.neg = initials.primitive) %>%
    as.list()
  rmodel[[name]] <- getModelMice(initial) %>%
    mutate(name = name, aic = calculateAIC(length(initial), as.numeric(value)))}

# decay split fictitious alpha_nonchosen ----------------------------------
# #Error in if (P[s] < 0.001) { : missing value where TRUE/FALSE needed 
# model <- function(par, a) {
#   a = a[with(a, order(start)), ]
#   nll = 0
#   if (par[1] < 0 | par[1] > 1 |
#       par[2] < 0 | par[2] > 50 |
#       par[3] < 0 | par[3] > 1 |
#       par[4] < 0 | par[4] > 1 |
#       par[5] < 0 | par[5] > 1) {
#     nll = Inf
#   } else {
#     tempQ = c()
#     Q = c(0, 0)
#     date = rep(a$start[1], 2)
#     t = c(0, 0)
#     P <- vector()
#     rewards = a$dooropened
#     sides = ceiling(a$corner/2)
#     nows = a$start
#     rew = c(0,0)
#     for (i in seq_along(sides)) {
#       r = rewards[i]
#       s = sides[i]
#       now = nows[i]
#       t = as.numeric(difftime(now, date, units = 'mins'))
#       date[s] = now
#       if (is.finite(s) & is.finite(r)) {
#         decay = ifelse(rew == 1, par[3], par[4])
#         Q[s] = exp( -(t[s]) * decay[s] ) * Q[s]
#         tempQ = exp( -(t[-s]) * decay[-s] ) * Q[-s]
#         P[s] = exp(par[2] * Q[s]) / 
#           (sum(exp(par[2] * Q[s]), exp(par[2] * tempQ)))
#         if(P[s] < .001){P[s] = .001}
#         if(P[s] > .999){P[s] = .999}
#         nll = -log(P[s]) + nll
#         pe = r - Q[s]
#         Q[s] = Q[s] + (par[1] * pe)
#         Q[-s] = Q[-s] + (par[5] * pe)
#         rew[s] = r}}}
#   nll}
# 
# {name = "decay+nonchosen"
#   initial <- expand.grid(
#     alpha.chosen = initials.default,
#     beta = initials.beta,
#     storage.pos = initials.primitive,
#     storage.neg = initials.primitive,
#     alpha.nonchosen = initials.default) %>%
#     as.list()
#   rmodel[[name]] <- getModelMice(initial) %>%
#     mutate(name = name, aic = calculateAIC(length(initial), as.numeric(value)))}
#
# decay threshold ---------------------------------------------------------
model <- function(par, a) {
    a = a[with(a, order(start)), ]
    nll = 0
    if (par[1] < 0 | par[1] > 1 |
        par[2] < 0 | par[2] > 50 |
        par[3] < 0 | par[3] > 1 |
        par[4] < 0 | par[4] > 1 |
        par[5] < 1 | par[5] > 60) {
      nll = Inf
    } else {
      tempQ = c()
      Q = c(0, 0)
      date = rep(a$start[1], 2)
      t = c(0, 0)
      P <- vector()
      rewards = a$dooropened
      sides = ceiling(a$corner/2)
      nows = a$start
      rew = c(0,0)
      for (i in seq_along(sides)) {
        r = rewards[i]
        s = sides[i]
        now = nows[i]
        t = as.numeric(difftime(now, date, units = 'mins'))
        date[s] = now
          decay = ifelse(rew == 1, par[3], par[4])
          if(t[s] > par[5]){ 
            Q[s] = exp( -(t[s]) * decay[s] ) * Q[s]}
          tempQ = ifelse(t[-s] > par[5], 
            exp( -(t[-s]) * decay[-s] ) * Q[-s],
            Q[-s])
          P[s] = exp(par[2] * Q[s]) / 
            (sum(exp(par[2] * Q[s]), exp(par[2] * tempQ)))
          if(P[s] < .001){P[s] = .001}
          if(P[s] > .999){P[s] = .999}
          nll = -log(P[s]) + nll
          pe = r - Q[s]
          Q[s] = Q[s] + (par[1] * pe)
          rew[s] = r}}
    nll}

{name = "decay++"
  initial <- expand.grid(
      alpha = initials.default,
      beta = initials.beta,
      storage.pos = initials.primitive,
      storage.neg = initials.primitive,
      forget.after = seq(1, 10, by = 1)) %>%
    as.list()
  rmodel[[name]] <- getModelMice(initial) %>%
    mutate(name = name, aic = calculateAIC(length(initial), as.numeric(value)))}

# decay dual --------------------------------------------------------------
# #TODO mniej punktow poczatkowych initials
# # Error: cannot allocate vector of size 1.4 Gb
# # 4. expand.grid(alpha.pos = initials.default, 
# #                beta = initials.beta, 
# #                storage.pos = initials.default, 
# #                storage.neg = seq(0.05,1, by = 0.05), 
# #                storage.neutral = initials.default,
# #             alpha.neg = initials.default)
# # 3. eval(lhs, parent, parent)
# # 2. eval(lhs, parent, parent)
# # 1. expand.grid(alpha.pos = initials.default, 
# #                beta = initials.beta, 
# #                storage.pos = initials.default, 
# #                storage.neg = initials.default, 
# #                storage.neutral = initials.default,
# #             alpha.neg = initials.default) %>% as.list()
# model <- function(par, a) {
#   a = a[with(a, order(start)), ]
#   nll = 0
#   if (par[1] < 0 | par[1] > 1 |
#       par[2] < 0 | par[2] > 50 |
#       par[3] < 0 | par[3] > 1 |
#       par[4] < 0 | par[4] > 1 |
#       par[5] < 0 | par[5] > 1 |
#       par[6] < 0 | par[6] > 1) {
#     nll = Inf
#   } else {
#     temp = nrow(a)
#     tempQ = c()
#     Q = c(0, 0)
#     date = rep(a$start[1], 2)
#     t = c(0, 0)
#     P <- vector()
#     rewards = a$dooropened
#     sides = ceiling(a$corner/2)
#     nows = a$start
#     for (i in seq_along(sides)) {
#       r = rewards[i]
#       s = sides[i]
#       now = nows[i]
#       t = as.numeric(difftime(now, date, units = 'mins'))
#       date[s] = now
#       if (is.finite(s) & is.finite(r)) {
#         if(r == 1){
#           Q[s] = exp( -(t[s]) * par[3] ) * Q[s]  
#           tempQ = exp( -(t[-s]) * par[5] ) * Q[-s]
#         } else {
#           Q[s] = exp( -(t[s]) * par[4] ) * Q[s]  
#           tempQ = exp( -(t[-s]) * par[5] ) * Q[-s]}
#         P[s] = exp(par[2] * Q[s]) / 
#           (sum(exp(par[2] * Q[s]), exp(par[2] * tempQ)))
#         if(P[s] < .001){P[s] = .001}
#         nll = -log(P[s]) + nll
#         pe = r - Q[s]
#         if(r == 1){
#           Q[s] = Q[s] + (par[1] * pe)  
#         }else{
#           Q[s] = Q[s] + (par[6] * pe)}}}}
#   nll}
# 
# {name = "decay++"
#   initial <- expand.grid(
#     alpha.pos = initials.default,
#     beta = initials.beta,
#     storage.pos = initials.default,
#     storage.neg = initials.default,
#     storage.neutral = initials.default,
#     alpha.neg = initials.default) %>%
#     as.list()
#   rmodel[[name]] <- getModelMice(initial) %>%
#     mutate(name = name,
#            aic = calculateAIC(length(initial), as.numeric(value)))}
#
# blank decay -------------------------------------------------------------
model <- function(par, a) {
  a = a[with(a, order(start)), ]
  nll = 0
  if (par[1] < 0 | par[1] > 50 |
      par[2] < 0 | par[2] > 1) {
    nll = Inf
  } else {
    Q = c(0, 0)
    date = rep(a$start[1], 2)
    t = c(0, 0)
    P <- vector()
    rewards = a$dooropened
    sides = ceiling(a$corner/2)
    nows = a$start
    for (i in seq_along(sides)) {
      r = rewards[i]
      s = sides[i]
      now = nows[i]
      t = as.numeric(difftime(now, date, units = 'mins'))
      date[s] = now
        Q[s] = exp( -(t[s]) * par[2] ) * Q[s]  
        tempQ = exp( -(t[-s]) * par[2] ) * Q[-s]
        P[s] = exp(par[1] * Q[s]) / 
          (sum(exp(par[1] * Q[s]), exp(par[1] * tempQ)))
        if(P[s] < .001){P[s] = .001}
        if(P[s] > .999){P[s] = .999}
        nll = -log(P[s]) + nll
        pe = r - Q[s]
        Q[s] = Q[s] + pe}}
  nll}

{name = "decayblank"
  initial <- expand.grid(
    beta = initials.beta,
    storage = initials.primitive) %>%
    as.list()
  rmodel[[name]] <- getModelMice(initial) %>%
    mutate(name = name, aic = calculateAIC(length(initial), as.numeric(value)))}

# blank decay+ ------------------------------------------------------------
model <- function(par, a) {
  a = a[with(a, order(start)), ]
  nll = 0
  if (par[1] < 0 | par[1] > 50 |
      par[2] < 0 | par[2] > 1 |
      par[3] < 0 | par[3] > 1) {
    nll = Inf
  } else {
    Q = c(0, 0)
    date = rep(a$start[1], 2)
    t = c(0, 0)
    P <- vector()
    rewards = a$dooropened
    sides = ceiling(a$corner/2)
    nows = a$start
    rew = c(0,0)
    for (i in seq_along(sides)) {
      r = rewards[i]
      s = sides[i]
      now = nows[i]
      t = as.numeric(difftime(now, date, units = 'mins'))
      date[s] = now
        decay = ifelse(rew == 1, par[2], par[3])
        Q[s] = exp( -(t[s]) * decay[s] ) * Q[s]  
        tempQ = exp( -(t[-s]) * decay[-s] ) * Q[-s]
        P[s] = exp(par[1] * Q[s]) / 
          (sum(exp(par[1] * Q[s]), exp(par[1] * tempQ)))
        if(P[s] < .001){P[s] = .001}
        if(P[s] > .999){P[s] = .999}
        nll = -log(P[s]) + nll
        pe = r - Q[s]
        Q[s] = Q[s] + pe
        rew[s] = r}}
  nll}

{name = "decay+blank"
  initial <- expand.grid(
    beta = initials.beta,
    storage.pos = initials.primitive,
    storage.neg = initials.primitive) %>%
    as.list()
  rmodel[[name]] <- getModelMice(initial) %>%
    mutate(name = name, aic = calculateAIC(length(initial), as.numeric(value)))}

# reproval ----------------------------------------------------------------
# version 2
model <- function(par, a) {
    a = a[with(a, order(start)), ]
    nll = 0
    if (par[1] < 0 | par[1] > 1 |
        par[2] < 0 | par[2] > 50 |
        par[3] < 0 | par[3] > 1 |
        par[4] < 0 | par[4] > 7) {
      nll = Inf
    } else {
      Q = c(0, 0)
      P <- vector()
      b0 = 0
      rewards = a$dooropened
      sides = ceiling(a$corner / 2)
      previous = lag(sides)
      previous[1] = 1
      intervals = a$intervalb
      intervals[1] = 0
      for (i in seq_along(sides)) {
        r = rewards[i]
        s = sides[i]
        w = previous[i]
        t = intervals[i]
          P = exp(par[2] * Q) / sum(exp(par[2] * Q))
          if(P[s] < .001){P[s] = .001}
          if(P[s] > .999){P[s] = .999}
          b0 = log(P[w] / (1 - P[w]))
          if(t > 660) t = 660
          P[w] = exp(b0 + par[3] * t - par[4]) / 
            (1 + exp(b0 + par[3] * t - par[4]))
          P[-w] = 1 - P[w]
          if(P[s] < .001){P[s] = .001}
          if(P[s] > .999){P[s] = .999}
          nll = -log(P[s]) + nll
          pe = r - Q[s] 
          Q[s] = Q[s] + (par[1] * pe)}}
    nll}

{name = "reproval"
  initial <- expand.grid(
      alpha = initials.default,
      beta = initials.beta,
      iota = initials.primitive,
      iota = initials.default) %>% 
    as.list()
  rmodel[[name]] <- getModelMice(initial) %>%
    mutate(name = name, aic = calculateAIC(length(initial), as.numeric(value)))}


# puzzlement --------------------------------------------------------------
model <- function(par, a) {
  a = a[with(a, order(start)), ]
  nll = 0
  if (par[1] < 0 | par[1] > 1 |
      par[2] < 0 | par[2] > 50 | 
      par[3] < 0 | par[3] > 1) {
    nll = Inf
  } else {
    tempQ = c()
    Q = c(0, 0)
    date = rep(a$start[1], 2)
    t = c(0, 0)
    P <- vector()
    rewards = a$dooropened
    sides = ceiling(a$corner/2)
    nows = a$start
    beta.zero = par[2]
    for (i in seq_along(sides)) {
      r = rewards[i]
      s = sides[i]
      now = nows[i]
      t = as.numeric(difftime(now, date, units = 'mins'))
      date[s] = now
        beta = exp( -(t[s]) * par[3] ) * beta.zero
        P = exp(beta * Q) / sum(exp(beta * Q))
        if(P[s] < .001){P[s] = .001}
        if(P[s] > .999){P[s] = .999}
        nll = -log(P[s]) + nll
        pe = r - Q[s]
        Q[s] = Q[s] + (par[1] * pe)}}
  nll}

{name = "puzzlement"
  initial <- expand.grid(
    alpha = initials.default,
    beta = initials.beta,
    bdecay = initials.primitive) %>%
    as.list()
  rmodel[[name]] <- getModelMice(initial) %>%
    mutate(name = name, aic = calculateAIC(length(initial), as.numeric(value)))}


# puzzlement split --------------------------------------------------------
model <- function(par, a) {
  a = a[with(a, order(start)), ]
  nll = 0
  if (par[1] < 0 | par[1] > 1 |
      par[2] < 0 | par[2] > 50 | 
      par[3] < 0 | par[3] > 1 |
      par[4] < 0 | par[4] > 1) {
    nll = Inf
  } else {
    Q = c(0, 0)
    date = rep(a$start[1], 2)
    t = c(0, 0)
    P <- vector()
    rewards = a$dooropened
    sides = ceiling(a$corner/2)
    nows = a$start
    rew = c(0,0)
    beta.zero = par[2]
    for (i in seq_along(sides)) {
      r = rewards[i]
      s = sides[i]
      now = nows[i]
      t = as.numeric(difftime(now, date, units = 'mins'))
      date[s] = now
      decay = ifelse(rew == 1, par[3], par[4])
      beta = exp( -(t[s]) * decay ) * beta.zero
      P = exp(beta * Q) / sum(exp(beta * Q))
      if(P[s] < .001){P[s] = .001}
      if(P[s] > .999){P[s] = .999}
      nll = -log(P[s]) + nll
      pe = r - Q[s]
      Q[s] = Q[s] + (par[1] * pe)
      rew[s] = r}}
  nll}

{name = "puzzlement+"
  initial <- expand.grid(
    alpha = initials.default,
    beta = initials.beta,
    bdecay.pos = initials.primitive,
    bdecay.neg = initials.primitive) %>%
    as.list()
  rmodel[[name]] <- getModelMice(initial) %>%
    mutate(name = name, aic = calculateAIC(length(initial), as.numeric(value)))}

# puzzlement fictitious ---------------------------------------------------
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
    t = c(0, 0)
    P <- vector()
    rewards = a$dooropened
    sides = ceiling(a$corner/2)
    nows = a$start
    beta.zero = par[2]
    for (i in seq_along(sides)) {
      r = rewards[i]
      s = sides[i]
      now = nows[i]
      t = as.numeric(difftime(now, date, units = 'mins'))
      date[s] = now
      beta = exp( -(t[s]) * par[3] ) * beta.zero
      P = exp(beta * Q) / sum(exp(beta * Q))
      if(P[s] < .001){P[s] = .001}
      if(P[s] > .999){P[s] = .999}
      nll = -log(P[s]) + nll
      pe = r - Q[s]
      Q[s] = Q[s] + (par[1] * pe)
      Q[-s] = Q[-s] - (par[1] * pe)}}
  nll}

{name = "puzzlement*"
  initial <- expand.grid(
    alpha = initials.default,
    beta = initials.beta,
    bdecay = initials.primitive) %>%
    as.list()
  rmodel[[name]] <- getModelMice(initial) %>%
    mutate(name = name, aic = calculateAIC(length(initial), as.numeric(value)))}

# random ------------------------------------------------------------------
{name = "zero"
rmodel[[name]] <- dmodel %>%
  group_by(tag) %>%
  summarise(n = length(which(is.finite(corner)&is.finite(dooropened)))) %>%
  mutate(name = name, aic = calculateAIC(0, n * -log(0.5)))}

# bandit4arm --------------------------------------------------------------
dmodel4arm <- dall
dmodel4arm$dooropened[dmodel4arm$rp == 0 & dmodel4arm$dooropened == 1] = 0.5

model <- 
  function(par, a) {
    a = a[with(a, order(start)), ]
    nll = 0
    if (par[1] < 0 | par[1] > 1 |
        par[2] < 0 | par[2] > 50) {
      nll = Inf
    } else {
      Q = c(0, 0, 0, 0)
      P <- vector()
      rewards = a$dooropened
      sides = a$corner
      for (i in seq_along(sides)) {
        r = rewards[i]
        s = sides[i]
          P = exp(par[2] * Q) / sum(exp(par[2] * Q))
          if(P[s] < .001){P[s] = .001}
          if(P[s] > .999){P[s] = .999}
          nll = -log(P[s]) + nll
          pe = r - Q[s]
          Q[s] = Q[s] + (par[1] * pe)}}
    nll}

{name = "basic4arm"
  initial <- expand.grid(
    alpha = initials.default,
    beta = initials.beta) %>%
    as.list()
  rmodel[[name]] <- getModelMice(list.parameters = initial, 
                                 A = subset(dmodel4arm, visitduration > 2)) %>%
    mutate(name = name, aic = calculateAIC(length(initial), as.numeric(value)))}

# noisy win-stay ----------------------------------------------------------
#   doi: 10.7554/eLife.49547

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
          if((win & stay) | (!win & !stay)){ P[k] = 1 - par[1]/2 } 
          if((win & !stay)| (!win & stay)){ P[k] = 0 + par[1]/2 }}
        nll = -log(P[k]) + nll}}
    nll}

{name = "noisywinstay"
  initial <- c(epsilon = seq(0.001, 1.999, 0.1))
  rmodel[[name]] <- (function(parameters, a = dmodel) {
    animal.tags = as.character(levels(as.factor(a$tag)))
    progressbar <- txtProgressBar(
      min = 0, max = length(animal.tags), char = '*', style = 3)
    output = list()
    for (m in seq_along(animal.tags)) {
      setTxtProgressBar(progressbar, m)
      dmouse = a[a$tag == animal.tags[m], ]
      output[[m]] = optimalizeParameters_(dmouse, parameters)}
    close(progressbar)
    dplyr::bind_rows(output)})(initial) %>%
    mutate(name = name, aic = calculateAIC(1, value))}

# noisy win-stay + interval -----------------------------------------------
model <- 
  function(par, a) {
    a = a[with(a, order(start)), ]
    nll = 0
    if (par[1] < 0.001 | par[1] > 1.999 |
        par[2] < 0.001 | par[2] > 1.999) {
      nll = Inf
    } else {
      P <- vector()
      rewards = a$dooropened
      sides = ceiling(a$corner/2)
      interval = a$intervalb
      previous.rewards = lag(rewards)
      previous.sides = lag(sides)
      for (i in seq_along(sides)) {
        r = previous.rewards[i]
        c = previous.sides[i]
        k = sides[i]
        t = interval[i]
        stay = c == k
        win = r == 1
        if(i == 1){ P[k] = .5
        } else {
          param = ifelse(t > 5, par[1], par[2])
          if((win & stay) | (!win & !stay)){ P[k] = 1 - param/2 } 
          if((win & !stay)| (!win & stay)){ P[k] = param/2 }}
        nll = -log(P[k]) + nll}}
    nll}

{name = "noisywinstay+"
  initial <- expand.grid(
    epsilonshort = seq(0.001, 1.9, 0.1),
    epsilonlong = seq(0.001, 1.9, 0.1)) %>%
    as.list()
  rmodel[[name]] <- (getModelMice(list.parameters = initial) %>%
                       mutate(name = name, 
                              aic = calculateAIC(length(initial), 
                                                 as.numeric(value))))}
