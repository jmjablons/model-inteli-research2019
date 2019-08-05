# attention 
# larning rate dependent on prediction error

modelNLL <- 
  function(par, A) {
    A = A[with(A, order(StartDateTime)), ]
    nll = 0
    if ((par[1] < 0 | par[1] > 1) |
        (par[2] < 0 | par[2] > 50) |
        (par[3] < 0 | par[3] > 1)) {
      nll = Inf
    } else {
      Q = c(0, 0)
      P <- vector()
      rewards = A$RewardDoor
      sides = ceiling(A$Corner / 2)
      alpha = par[1]
      for (i in seq_along(sides)) {
        r = rewards[i]
        s = sides[i]
        if (is.finite(s) & is.finite(r)) {
          P[1] = exp(par[2] * Q[1]) /
            (sum(exp(par[2] * Q[1]), 
                 exp(par[2] * Q[2])))
          P[2] = exp(par[2] * Q[2]) / 
            (sum(exp(par[2] * Q[1]), 
                 exp(par[2] * Q[2])))
          nll = -log(P[s]) + nll
          pe = r - Q[s]
          alpha = par[3] * abs(pe) + (1 - par[3]) * alpha
          Q[s] = Q[s] + (alpha * pe)
        } 
      }
    }
    nll
  }

tInitials <- 
  expand.grid(
    alpha.zero = seq(0.05, 1, by = 0.05),
    beta = seq(0.25, 15, by = 0.25),
    ni= seq(0.05, 1, by = 0.05)) %>%
  as.list()

mAttention <- 
  getModelMice(tInitials) #be carreful!

mAttention = 
  merge(mAttention, iAnimals, all.x = TRUE) %>%
  as_tibble()

mAttention$AIC <- 
  apply(mAttention, 1, function(x) {
    calculateAIC(n.parameters = length(tInitials), 
                 NLL = as.numeric(x[(length(tInitials) + 2)]))
  })
