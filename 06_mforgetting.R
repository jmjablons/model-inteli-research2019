# forgetful by Collins and Frank ------------------------------------------

# reference in publication

modelNLL <- 
  function(par, A) {
  A = A[with(A, order(StartDateTime)),]
  nll = 0
  if (par[1] < 0 | par[1] > 1 | 
      par[2] < 0 | par[2] > 50 |
      par[3] < 0 | par[3] > 1) {
    nll = Inf
  } else {
    Q = c(0,0)
    P <- vector()
    Rewards = A$RewardDoor
    Sides = ceiling(A$Corner/2)
    for (i in seq_along(Sides)) {
      r = Rewards[i]
      s = Sides[i]
      P[1] = exp(par[2] * Q[1]) /
        (sum(exp(par[2] * Q[1]),
             exp(par[2] * Q[2])))
      P[2] = exp(par[2] * Q[2]) / 
        (sum(exp(par[2] * Q[1]), 
             exp(par[2] * Q[2])))
      nll = -log(P[s]) + nll
      if (is.finite(s) & is.finite(r)) {
        pe = r - Q[s]
        Q[s] = Q[s] + par[1] * pe
        Q[s] = Q[s] + par[3] * (0.5 - Q[s])
        Q[-s] = Q[-s] + par[3] * (0.5 - Q[-s])
      } 
    }
  }
  nll
}

tInitials <- 
  expand.grid(
    alpha = seq(0.05, 1, by = 0.05),
    beta = seq(0.25, 15, by = 0.25),
    epsilon = seq(0.05, 1, by = 0.05)) %>% 
  as.list()

mForgetful <- 
  getModelMice(tInitials)

mForgetful = 
  merge(mForgetful, animal.info, all.x = TRUE) %>% 
  as_tibble()

mForgetful$AIC <- 
  apply(mForgetful, 1, function(x) {
    calculateAIC(
      n.parameters = length(tInitials), 
      NLL = as.numeric(x[(length(tInitials) + 2)]))
  })