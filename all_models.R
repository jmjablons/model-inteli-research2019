# 2019-11-28
# judyta

# custom ------------------------------------------------------------------
getaic <- function(n.parameters, nll) {2 * nll + 2 * n.parameters}
listnth <- function(inp, n){sapply(inp, `[`, n)}

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
  sample.size = length(list.parameters[[1]])
  val = Inf
  mouse = as.numeric(unique(input.data$tag))
  for (i in seq_along(sample.size)) {
    temp = optim(par = listnth(list.parameters, i), 
                 fn = model, a = input.data)
    if(temp$value < val){
      val = temp$value
      best.optim <- temp}}
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

# dependency --------------------------------------------------------------
library(dplyr)

# result ------------------------------------------------------------------
rmodel <- list()

# variables ---------------------------------------------------------------
init <- list(
  default = seq(0.05, 1, by = 0.05),
  beta = seq(0.25, 15, by = 0.50),
  primitive = seq(0, 1, by = 0.05))

# bandit4arm
dmodel4arm <- dall
dmodel4arm$dooropened[dmodel4arm$rp == 0 & dmodel4arm$dooropened == 1] = 0.5

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

{name = "basic"
  initial <- expand.grid(
      alpha = initials.default,
      beta = initials.beta) %>% as.list()
  rmodel[[name]] <- wrapmodel(initial) %>% as_tibble() %>%
    mutate(name = name, tag = as.character(tag), 
           aic = getaic(length(initial), value))}

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
    alpha.neg = initials.default) %>% as.list()
  rmodel[[name]] <- wrapmodel(initial) %>% as_tibble() %>%
    mutate(name = name, tag = as.character(tag), 
           aic = getaic(length(initial), value))}

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
  rmodel[[name]] <- wrapmodel(initial) %>% as_tibble() %>%
    mutate(name = name, tag = as.character(tag), 
           aic = getaic(length(initial), value))}


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
  rmodel[[name]] <- wrapmodel(initial) %>% as_tibble() %>%
    mutate(name = name, tag = as.character(tag), 
           aic = getaic(length(initial), value))}

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

{name = "fictitious"
  initial <- expand.grid(
    alpha = initials.default,
    beta = initials.beta) %>%
    as.list()
  rmodel[[name]] <- wrapmodel(initial) %>% as_tibble() %>%
    mutate(name = name, tag = as.character(tag), 
           aic = getaic(length(initial), value))}

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

{name = "hybrid"
  initial <- expand.grid(
    alpha.pos = initials.default,
    beta =  initials.beta,
    alpha.neg = initials.default) %>% 
    as.list()
  rmodel[[name]] <- wrapmodel(initial) %>% as_tibble() %>%
    mutate(name = name, tag = as.character(tag), 
           aic = getaic(length(initial), value))}

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
  rmodel[[name]] <- wrapmodel(initial) %>% as_tibble() %>%
    mutate(name = name, tag = as.character(tag), 
           aic = getaic(length(initial), value))}

# decay -------------------------------------------------------------------
# model <- function(par, a) {
#   a = a[with(a, order(start)), ]
#   nll = 0
#   if (par[1] < 0 | par[1] > 1 |
#       par[2] < 0 | par[2] > 50 | 
#       par[3] < 0 | par[3] > 1) {
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
#     for (i in seq_along(sides)) {
#       r = rewards[i]
#       s = sides[i]
#       now = nows[i]
#       t = as.numeric(difftime(now, date, units = 'mins'))
#       t = ifelse(t > 660, 660, t)
#       date[s] = now
#       Q[s] = exp( -(t[s]) * par[3] ) * Q[s]
#       tempQ = exp( -(t[-s]) * par[3] ) * Q[-s]
#       P[s] = exp(par[2] * Q[s]) / 
#         (sum(exp(par[2] * Q[s]), exp(par[2] * tempQ)))
#       if(P[s] < .001){P[s] = .001}
#       if(P[s] > .999){P[s] = .999}
#       nll = -log(P[s]) + nll
#       pe = r - Q[s]
#       Q[s] = Q[s] + (par[1] * pe)}}
#   nll}

# decay fix ---------------------------------------------------------------
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
    nows.start = a$start
    nows.end = a$end
    for (i in seq_along(sides)) {
      r = rewards[i]
      s = sides[i]
      now.start = nows.start[i]
      now.end = nows.end[i]
      t = as.numeric(difftime(now.start, date, units = 'mins'))
      t = ifelse(t > 660, 660, t)
      date[s] = now.end
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

{name = "decayfix"
  initial <- expand.grid(
    alpha = initials.default,
    beta = initials.beta,
    storage = initials.primitive) %>%
    as.list()
  rmodel[[name]] <- wrapmodel(initial) %>% as_tibble() %>%
    mutate(name = name, tag = as.character(tag), 
           aic = getaic(length(initial), value))}


# decay true --------------------------------------------------------------
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
    nows.start = a$start
    nows.end = a$end
    for (i in seq_along(sides)) {
      r = rewards[i]
      s = sides[i]
      now.start = nows.start[i]
      now.end = nows.end[i]
      t = as.numeric(difftime(now.start, date, units = 'mins'))
      t = ifelse(t > 660, 660, t)
      date[s] = now.end
      Q = exp( -(t) * par[3] ) * Q
      P = exp(par[2] * Q) / sum(exp(par[2] * Q))
      if(P[s] < .001){P[s] = .001}
      if(P[s] > .999){P[s] = .999}
      nll = -log(P[s]) + nll
      pe = r - Q[s]
      Q[s] = Q[s] + (par[1] * pe)}}
  nll}

{name = "decaytrue"
  initial <- expand.grid(
    alpha = initials.default,
    beta = initials.beta,
    storage = initials.primitive) %>%
    as.list()
  rmodel[[name]] <- wrapmodel(initial) %>% as_tibble() %>%
    mutate(name = name, tag = as.character(tag), 
           aic = getaic(length(initial), value))}

# decay fictitious --------------------------------------------------------
# model <- function(par, a) {
#   a = a[with(a, order(start)), ]
#   nll = 0
#   if (par[1] < 0 | par[1] > 1 |
#       par[2] < 0 | par[2] > 50 |
#       par[3] < 0 | par[3] > 1) {
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
#       t = ifelse(t > 660, 660, t)
#       date[s] = now
#       Q[s] = exp( -(t[s]) * par[3] ) * Q[s]
#       tempQ = exp( -(t[-s]) * par[3] ) * Q[-s]
#       P[s] = exp(par[2] * Q[s]) / 
#         (sum(exp(par[2] * Q[s]), exp(par[2] * tempQ)))
#       if(P[s] < .001){P[s] = .001}
#       if(P[s] > .999){P[s] = .999}
#       nll = -log(P[s]) + nll
#       pe = r - Q[s]
#       Q[s] = Q[s] + (par[1] * pe)
#       Q[-s] = Q[-s] + (par[1] * pe)}}
#   nll}


# decay true fictitious ---------------------------------------------------
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
    nows.start = a$start
    nows.end = a$end
    for (i in seq_along(sides)) {
      r = rewards[i]
      s = sides[i]
      now.start = nows.start[i]
      now.end = nows.end[i]
      t = as.numeric(difftime(now.start, date, units = 'mins'))
      t = ifelse(t > 660, 660, t)
      date[s] = now.end
      Q = exp( -(t) * par[3] ) * Q
      P = exp(par[2] * Q) / sum(exp(par[2] * Q))
      if(P[s] < .001){P[s] = .001}
      if(P[s] > .999){P[s] = .999}
      nll = -log(P[s]) + nll
      pe = r - Q[s]
      Q[s] = Q[s] + (par[1] * pe)
      Q[-s] = Q[-s] - (par[1] * pe)}}
  nll}

{name = "decaytrue*"
  initial <- expand.grid(
    alpha = initials.default,
    beta = initials.beta,
    storage = initials.primitive) %>%
    as.list()
  rmodel[[name]] <- wrapmodel(initial) %>% as_tibble() %>%
    mutate(name = name, tag = as.character(tag), 
           aic = getaic(length(initial), value))}


# decay fix fictitious ----------------------------------------------------
# model <- function(par, a) {
#   a = a[with(a, order(start)), ]
#   nll = 0
#   if (par[1] < 0 | par[1] > 1 |
#       par[2] < 0 | par[2] > 50 |
#       par[3] < 0 | par[3] > 1) {
#     nll = Inf
#   } else {
#     tempQ = c()
#     Q = c(0, 0)
#     date = rep(a$start[1], 2)
#     t = c(0, 0)
#     P <- vector()
#     rewards = a$dooropened
#     sides = ceiling(a$corner/2)
#     nows.start = a$start
#     nows.end = a$end
#     rew = c(0,0)
#     for (i in seq_along(sides)) {
#       r = rewards[i]
#       s = sides[i]
#       now.start = nows.start[i]
#       now.end = nows.end[i]
#       t = as.numeric(difftime(now.start, date, units = 'mins'))
#       t = ifelse(t > 660, 660, t)
#       date[s] = now.end
#       Q[s] = exp( -(t[s]) * par[3] ) * Q[s]
#       tempQ = exp( -(t[-s]) * par[3] ) * Q[-s]
#       P[s] = exp(par[2] * Q[s]) / 
#         (sum(exp(par[2] * Q[s]), exp(par[2] * tempQ)))
#       if(P[s] < .001){P[s] = .001}
#       if(P[s] > .999){P[s] = .999}
#       nll = -log(P[s]) + nll
#       pe = r - Q[s]
#       Q[s] = Q[s] + (par[1] * pe)
#       Q[-s] = Q[-s] + (par[1] * pe)}}
#   nll}
# 
# {name = "decayfix*"
#   initial <- expand.grid(
#     alpha = initials.default,
#     beta = initials.beta,
#     storage = initials.primitive) %>%
#     as.list()
#   rmodel[[name]] <- wrapmodel(initial) %>% as_tibble() %>%
#     mutate(name = name, tag = as.character(tag), 
#            aic = getaic(length(initial), value))}


# decay fix split ---------------------------------------------------------
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
    nows.start = a$start
    nows.end = a$end
    rew = c(0,0)
    for (i in seq_along(sides)) {
      r = rewards[i]
      s = sides[i]
      now.start = nows.start[i]
      now.end = nows.end[i]
      t = as.numeric(difftime(now.start, date, units = 'mins'))
      t = ifelse(t > 660, 660, t)
      date[s] = now.end
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
      rew[s] = r}}
  nll}

{name = "decayfix+"
  initial <- expand.grid(
    alpha = initials.default,
    beta = initials.beta,
    storage.pos = initials.primitive,
    storage.neg = initials.primitive) %>%
    as.list()
  rmodel[[name]] <- wrapmodel(initial) %>% as_tibble() %>%
    mutate(name = name, tag = as.character(tag), 
           aic = getaic(length(initial), value))}


# decay fix split dummy ---------------------------------------------------
model <- function(par, a) {
  a = a[with(a, order(start)), ]
  nll = 0
  if (par[1] < 0 | par[1] > 1 |
      par[2] < 0 | par[2] > 50 |
      par[3] < 0 | par[3] > 1 |
      par[4] < 0 | par[4] > 1) {
    nll = Inf
  } else {
    #par[4] = par[3] ##not-working##
    tempQ = c()
    Q = c(0, 0)
    date = rep(a$start[1], 2)
    t = c(0, 0)
    P <- vector()
    rewards = a$dooropened
    sides = ceiling(a$corner/2)
    nows.start = a$start
    nows.end = a$end
    rew = c(0,0)
    for (i in seq_along(sides)) {
      r = rewards[i]
      s = sides[i]
      now.start = nows.start[i]
      now.end = nows.end[i]
      t = as.numeric(difftime(now.start, date, units = 'mins'))
      t = ifelse(t > 660, 660, t)
      date[s] = now.end
      decay = ifelse(rew == 1, par[3], par[3])
      Q[s] = exp( -(t[s]) * decay[s] ) * Q[s]
      tempQ = exp( -(t[-s]) * decay[-s] ) * Q[-s]
      P[s] = exp(par[2] * Q[s]) / 
        (sum(exp(par[2] * Q[s]), exp(par[2] * tempQ)))
      if(P[s] < .001){P[s] = .001}
      if(P[s] > .999){P[s] = .999}
      nll = -log(P[s]) + nll
      pe = r - Q[s]
      Q[s] = Q[s] + (par[1] * pe)
      rew[s] = r}}
  nll}

{name = "decaydummy"
  initial <- expand.grid(
    alpha = initials.default,
    beta = initials.beta,
    storage.one = initials.primitive,
    storage.dummy = initials.primitive) %>%
    as.list()
  rmodel[[name]] <- wrapmodel(initial) %>% as_tibble() %>%
    mutate(name = name, tag = as.character(tag), 
           aic = getaic(length(initial), value))}

# decay split fictitious --------------------------------------------------
# model <- function(par, a) {
#   a = a[with(a, order(start)), ]
#   nll = 0
#   if (par[1] < 0 | par[1] > 1 |
#       par[2] < 0 | par[2] > 50 |
#       par[3] < 0 | par[3] > 1 |
#       par[4] < 0 | par[4] > 1) {
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
#       t = ifelse(t > 660, 660, t)
#       date[s] = now
#       decay = ifelse(rew == 1, par[3], par[4])
#       Q[s] = exp( -(t[s]) * decay[s] ) * Q[s]
#       tempQ = exp( -(t[-s]) * decay[-s] ) * Q[-s]
#       P[s] = exp(par[2] * Q[s]) / 
#         (sum(exp(par[2] * Q[s]), exp(par[2] * tempQ)))
#       if(P[s] < .001){P[s] = .001}
#       if(P[s] > .999){P[s] = .999}
#       nll = -log(P[s]) + nll
#       pe = r - Q[s]
#       Q[s] = Q[s] + (par[1] * pe)
#       Q[-s] = Q[-s] + (par[1] * pe)
#       rew[s] = r}}
#   nll}

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
    nows.start = a$start
    nows.end = a$end
    rew = c(0,0)
    for (i in seq_along(sides)) {
      r = rewards[i]
      s = sides[i]
      now.start = nows.start[i]
      now.end = nows.end[i]
      t = as.numeric(difftime(now.start, date, units = 'mins'))
      t = ifelse(t > 660, 660, t)
      date[s] = now.end
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

{name = "decayfix++"
  initial <- expand.grid(
    alpha = initials.default,
    beta = initials.beta,
    storage.pos = initials.primitive,
    storage.neg = initials.primitive,
    forget.after = seq(1, 10, by = 1)) %>%
    as.list()
  rmodel[[name]] <- wrapmodel(initial) %>% as_tibble() %>%
    mutate(name = name, tag = as.character(tag), 
           aic = getaic(length(initial), value))}

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
    rho = initials.default) %>% 
    as.list()
  rmodel[[name]] <- wrapmodel(initial) %>% as_tibble() %>%
    mutate(name = name, tag = as.character(tag), 
           aic = getaic(length(initial), value))}

# puzzlement --------------------------------------------------------------
# model <- function(par, a) {
#   a = a[with(a, order(start)), ]
#   nll = 0
#   if (par[1] < 0 | par[1] > 1 |
#       par[2] < 0 | par[2] > 50 | 
#       par[3] < 0 | par[3] > 1) {
#     nll = Inf
#   } else {
#     Q = c(0, 0)
#     date = a$start[1]
#     t = 0
#     P <- vector()
#     rewards = a$dooropened
#     sides = ceiling(a$corner/2)
#     nows = a$start
#     beta.zero = par[2]
#     for (i in seq_along(sides)) {
#       r = rewards[i]
#       s = sides[i]
#       now = nows[i]
#       t = as.numeric(difftime(now, date, units = 'mins'))
#       t = ifelse(t > 660, 660, t)
#       date = now
#       beta = exp( -t * par[3] ) * beta.zero
#       P = exp(beta * Q) / sum(exp(beta * Q))
#       if(P[s] < .001){P[s] = .001}
#       if(P[s] > .999){P[s] = .999}
#       nll = -log(P[s]) + nll
#       pe = r - Q[s]
#       Q[s] = Q[s] + (par[1] * pe)}}
#   nll}

# puzzlement fixed --------------------------------------------------------
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

{name = "puzzlementfix"
  initial <- expand.grid(
    alpha = initials.default,
    beta = initials.beta,
    bdecay = initials.primitive) %>%
    as.list()
  rmodel[[name]] <- wrapmodel(initial) %>% as_tibble() %>%
    mutate(name = name, tag = as.character(tag), 
           aic = getaic(length(initial), value))}


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
    date = a$start[1]
    t = 0
    P <- vector()
    rewards = a$dooropened
    sides = ceiling(a$corner/2)
    intervals = a$intervalb
    intervals[1] = 0
    beta.zero = par[2]
    rew = 0
    for (i in seq_along(sides)) {
      r = rewards[i]
      s = sides[i]
      t = intervals[i]
      t = ifelse(t > 660, 660, t)
      decay = ifelse(rew == 1, par[3], par[4])
      beta = exp( -t * decay ) * beta.zero
      P = exp(beta * Q) / sum(exp(beta * Q))
      if(P[s] < .001){P[s] = .001}
      if(P[s] > .999){P[s] = .999}
      nll = -log(P[s]) + nll
      pe = r - Q[s]
      Q[s] = Q[s] + (par[1] * pe)
      rew = r}}
  nll}

{name = "puzzlementfix+"
  initial <- expand.grid(
    alpha = initials.default,
    beta = initials.beta,
    bdecay.pos = initials.primitive,
    bdecay.neg = initials.primitive) %>%
    as.list()
  rmodel[[name]] <- wrapmodel(initial) %>% as_tibble() %>%
    mutate(name = name, tag = as.character(tag), 
           aic = getaic(length(initial), value))}


# puzzlement dual ---------------------------------------------------------
# model <- function(par, a) {
#   a = a[with(a, order(start)), ]
#   nll = 0
#   if (par[1] < 0 | par[1] > 1 |
#       par[2] < 0 | par[2] > 1 | 
#       par[3] < 0 | par[3] > 50 |
#       par[4] < 0 | par[4] > 1) {
#     nll = Inf
#   } else {
#     Q = c(0, 0)
#     date = a$start[1]
#     t = 0
#     P <- vector()
#     rewards = a$dooropened
#     sides = ceiling(a$corner/2)
#     nows = a$start
#     beta.zero = par[2]
#     rew = 0
#     for (i in seq_along(sides)) {
#       r = rewards[i]
#       s = sides[i]
#       now = nows[i]
#       t = as.numeric(difftime(now, date, units = 'mins'))
#       t = ifelse(t > 660, 660, t)
#       date = now
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

{name = "puzzlementfix*"
  initial <- expand.grid(
    alpha = initials.default,
    beta = initials.beta,
    bdecay = initials.primitive) %>%
    as.list()
  rmodel[[name]] <- wrapmodel(initial) %>% as_tibble() %>%
    mutate(name = name, tag = as.character(tag), 
           aic = getaic(length(initial), value))}

# puzzlement split fictitious ---------------------------------------------
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
#     date = rep(a$start[1], 2)
#     t = c(0, 0)
#     P <- vector()
#     rewards = a$dooropened
#     sides = ceiling(a$corner/2)
#     nows = a$start
#     rew = c(0,0)
#     beta.zero = par[2]
#     for (i in seq_along(sides)) {
#       r = rewards[i]
#       s = sides[i]
#       now = nows[i]
#       t = as.numeric(difftime(now, date, units = 'mins'))
#       t = ifelse(t > 660, 660, t)
#       date[s] = now
#       decay = ifelse(rew == 1, par[3], par[4])
#       beta = exp( -(t[s]) * decay ) * beta.zero
#       P = exp(beta * Q) / sum(exp(beta * Q))
#       if(P[s] < .001){P[s] = .001}
#       if(P[s] > .999){P[s] = .999}
#       nll = -log(P[s]) + nll
#       pe = r - Q[s]
#       Q[s] = Q[s] + (par[1] * pe)
#       Q[-s] = Q[-s] - (par[1] * pe)
#       rew[s] = r}}
#   nll}

# beta attention ----------------------------------------------------------

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
    sides = ceiling(a$corner/2)
    beta.zero = par[2]
    beta = 1
    for (i in seq_along(sides)) {
      r = rewards[i]
      s = sides[i]
      P = exp(beta * Q) / sum(exp(beta * Q))
      if(P[s] < .001){P[s] = .001}
      if(P[s] > .999){P[s] = .999}
      nll = -log(P[s]) + nll
      pe = r - Q[s]
      beta = par[3] * (abs(pe) * beta.zero) + (1 - par[3]) * beta.zero
      Q[s] = Q[s] + (par[1] * pe)}}
  nll}

{name = "betadown"
  initial <- expand.grid(
    alpha = initials.default,
    beta0 = initials.beta,
    bdecay = initials.primitive) %>% as.list()
  rmodel[[name]] <- wrapmodel(initial) %>% as_tibble() %>%
    mutate(name = name, tag = as.character(tag), 
           aic = getaic(length(initial), value))}

# beta down step ----------------------------------------------------------

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
    contin = a$contingency
    con = contin[1]
    step = 0
    for (i in seq_along(sides)) {
      if(contin[i] != con){
        step = 0}
      r = rewards[i]
      s = sides[i]
      now = nows[i]
      date[s] = now
      beta = exp(-step * par[3]) * beta.zero
      P = exp(beta * Q) / sum(exp(beta * Q))
      if(P[s] < .001){P[s] = .001}
      if(P[s] > .999){P[s] = .999}
      nll = -log(P[s]) + nll
      pe = r - Q[s]
      Q[s] = Q[s] + (par[1] * pe)
      con = contin[i]
      step = step + 1}}
  nll}

{name = "betadown-"
  initial <- expand.grid(
    alpha = initials.default,
    beta0 = initials.beta,
    bdecay = initials.primitive) %>%
    as.list()
  rmodel[[name]] <- wrapmodel(initial) %>% as_tibble() %>%
    mutate(name = name, tag = as.character(tag), 
           aic = getaic(length(initial), value))}

# bandit4arm --------------------------------------------------------------
model <- function(par, a) {
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
  rmodel[[name]] <- wrapmodel(initial, subset(dmodel4arm, visitduration > 2)) %>% 
    as_tibble() %>% mutate(name = name, tag = as.character(tag), 
                           aic = getaic(length(initial), value))}

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
  initial <- seq(0.001, 1.999, 0.001)
  rmodel[[name]] <- 
    temp <-(function(initials, a = dmodel) {
    tags = as.character(levels(as.factor(a$tag)))
    progressbar <- txtProgressBar(0, length(tags), char = '*', style = 3)
    output = list()
    for (m in seq_along(tags)) {
      setTxtProgressBar(progressbar, m)
      dmouse = a[a$tag == tags[m], ]
      output[[m]] = getoptimal_(dmouse, initials)}
    close(progressbar)
    do.call(rbind, output)})(initial) %>% as_tibble() %>%
    mutate(name = name, tag = as.character(tag), aic = getaic(1, value))}

# noisy win-stay + interval -----------------------------------------------
model <- function(par, a) {
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
    epsilonshort = seq(0.001, 1.999, 0.01),
    epsilonlong = seq(0.001, 1.999, 0.01)) %>%
    as.list()
  rmodel[[name]] <- wrapmodel(initial) %>% as_tibble() %>%
    mutate(name = name, tag = as.character(tag), 
           aic = getaic(length(initial), value))}

# random ------------------------------------------------------------------
{name = "zero"
rmodel[[name]] <- dmodel %>%
  group_by(tag) %>%
  summarise(n = length(which(is.finite(corner)&is.finite(dooropened)))) %>%
  mutate(name = name, aic = getaic(0, n * -log(0.5)))}

# custom ------------------------------------------------------------------

optimraw <- function(tags = manimal$tag, 
                     a = dmodel, 
                     grid = expand.grid(alpha = seq(0, 1, 0.01), 
                                        beta = seq(0,10, 0.1), 
                                        nll = NA, tag = NA)){
  lapply(tags, function(m){dmouse <- a[a$tag == m,]
  grid[,3] <- apply(grid, 1, function(x){model(c(x[1], x[2]), dmouse)})
  grid[,4] <- m
  grid})}

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

surfacebasic <- optimraw()

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

surfacefictitious <- optimraw()

# puzzlement --------------------------------------------------------------
# model <- function(par, a) {
#   a = a[with(a, order(start)), ]
#   nll = 0
#   if (par[1] < 0 | par[1] > 1 |
#       par[2] < 0 | par[2] > 50 | 
#       par[3] < 0 | par[3] > 1) {
#     nll = Inf
#   } else {
#     Q = c(0, 0)
#     date = rep(a$start[1], 2)
#     t = c(0, 0)
#     P <- vector()
#     rewards = a$dooropened
#     sides = ceiling(a$corner/2)
#     nows = a$start
#     beta.zero = par[2]
#     for (i in seq_along(sides)) {
#       r = rewards[i]
#       s = sides[i]
#       now = nows[i]
#       t = as.numeric(difftime(now, date, units = 'mins'))
#       t = ifelse(t > 660, 660, t)
#       date[s] = now
#       beta = exp( -(t[s]) * par[3] ) * beta.zero
#       P = exp(beta * Q) / sum(exp(beta * Q))
#       if(P[s] < .001){P[s] = .001}
#       if(P[s] > .999){P[s] = .999}
#       nll = -log(P[s]) + nll
#       pe = r - Q[s]
#       Q[s] = Q[s] + (par[1] * pe)}}
#   nll}
# 
# surfacepuzzlement <- (function(tags = manimal$tag, a = dmodel, 
#                                grid = expand.grid(alpha = seq(0, 1, 0.01), 
#                                                   betazero = seq(0,10, 0.1), 
#                                                   bdecay = seq(0, 1, 0.01),
#                                                   nll = NA, tag = NA)){
#   lapply(tags, function(m){dmouse <- a[a$tag == m,]
#   grid[,3] <- apply(grid, 1, function(x){model(c(x[1], x[2], x[3]), dmouse)})
#   grid[,4] <- m
#   grid})})()


# surprise ----------------------------------------------------------------

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
    nows.start = a$start
    nows.end = a$end
    rew = c(0,0)
    beta.zero = par[2]
    decay = c(0,0)
    for (i in seq_along(sides)) {
      r = rewards[i]
      s = sides[i]
      now.start = nows.start[i]
      now.end = nows.end[i]
      t = as.numeric(difftime(now.start, date, units = 'mins'))
      t = ifelse(t > 660, 660, t)
      date[s] = now.end
      beta = exp( -t * decay ) * beta.zero
      P = exp(beta[s] * Q) / sum(exp(beta[s] * Q))
      if(P[s] < .001){P[s] = .001}
      if(P[s] > .999){P[s] = .999}
      nll = -log(P[s]) + nll
      pe = r - Q[s]
      Q[s] = Q[s] + (par[1] * pe)
      decay = ifelse(rew == 1, par[3], par[4])
      rew[s] = r}}
  nll}

{name = "favourite"
  initial <- expand.grid(
    alpha = initials.default,
    beta = initials.beta,
    bdecay.pos = initials.primitive,
    bdecay.neg = initials.primitive) %>%
    as.list()
  rmodel[[name]] <- wrapmodel(initial) %>% as_tibble() %>%
    mutate(name = name, tag = as.character(tag), 
           aic = getaic(length(initial), value))}
