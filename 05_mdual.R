#learning rate split into two

modelNLL <- 
  function(par, A) {
    A = A[with(A, order(StartDateTime)),]
    nll = 0
    if (par[1] < 0 | par[1] > 1 |
        par[2] < 0 | par[2] > 50 |
        par[3] < 0 | par[3] > 1) {
      nll = Inf
    } else {
      Q = c(0, 0)
      P <- vector()
      rewards = A$RewardDoor
      sides = ceiling(A$Corner / 2)
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
          if (r == 1) { #if rewarded
            Q[s] = Q[s] + (par[1] * pe)
          } else {
            Q[s] = Q[s] + (par[3] * pe)
          }
        }
      }
    }
    nll
  }

# set initial values
tInitials <- 
  expand.grid(
    alpha.pos = seq(0.05, 1, by = 0.05),
    beta =  seq(0.25, 15, by = 0.25),
    alpha.neg = seq(0.05, 1, by = 0.05)
  ) %>%
  as.list()

mDual <- 
  getModelMice(tInitials)

mDual = 
  merge(mDual, iAnimals, all.x = TRUE) %>% 
  as_tibble()

mDual$AIC <- 
  apply(
    mDual, 1, function(x) {
      calculateAIC(
        n.parameters = length(tInitials), 
        NLL = as.numeric(x[(length(tInitials) + 2)]))
    })
