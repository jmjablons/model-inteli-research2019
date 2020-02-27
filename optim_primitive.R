# Sys.time()
# surface <- list(rep(NA, 505101))
# sapply(manimal$tag, function(m){
#   dmouse = dmodel[dmodel$tag == m,]
#   tag = as.numeric(unique(dmouse$tag))
#   par1 = -0.01
#   i = 0
#   while(par1 < 1){
#     par1 = par1 + 0.01
#     par2 = -0.01
#     while(par2 < 50){
#       par2 = par2 + 0.01
#       i = i + 1
#       nll = model(c(alpha = par1, beta = par2), dmouse)
#       surface[[i]] <- c(tag = tag, alpha = par1, beta = par2, value = nll)}}})
# Sys.time()
## noep. see: r inferno

# custom ------------------------------------------------------------------

optimraw <- function(tags = with(manimal, {tag[substance == "saccharin"]}), 
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

surfacepuzzlement <- optimraw()

# general -----------------------------------------------------------------

