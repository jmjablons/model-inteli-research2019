# DECAY -------------------------------------------------------------------

#my favourite 
# Q value decays as a function of interval
#   note: interval is calculated on the go

modelNLL <- 
  function(par, A) {
  A = A[with(A, order(StartDateTime)), ]
  nll = 0
  if (par[1] < 0 | par[1] > 1 |
      par[2] < 0 | par[2] > 50 |
      par[3] < 0 | par[3] > 1) {
    nll = Inf
  } else {
    temp = nrow(A)
    tempQ = c()
    Q = c(0, 0)
    date = rep(A$StartDateTime[1], 2)
    t = c(0, 0)
    P <- vector()
    for (i in seq_along(1:temp)) {
      r = A$RewardDoor[i]
      s = ceiling(A$Corner[i] / 2)
      t[1] = as.numeric(
        difftime(A$StartDateTime[i], 
                 date[1], units = 'mins')
      )
      t[2] = as.numeric(
        difftime(A$StartDateTime[i], 
                 date[2], units = 'mins')
      )
      date[s] = A$StartDateTime[i]
      if (is.finite(s) & is.finite(r)) {
        P[s] = exp(par[2] * Q[s]) / 
          (sum(exp(par[2] * Q[s]), 
               exp(par[2] * tempQ)))
        nll = -log(P[s]) + nll
        pe = r - Q[s]
        Q[s] = Q[s] + (par[1] * pe)
        Q[s] = exp( -(t[s]) * par[3] ) * Q[s]
        tempQ = exp( -(t[-s]) * par[3] ) * Q[-s]
      }
    }
  }
  nll
  }

tInitials <- 
  expand.grid(
    alpha = seq(0.05, 1, by = 0.05),
    beta = seq(0.25, 15, by = 0.25),
    storage = seq(0.05, 1, by = 0.05)
  ) %>% as.list()

mDecay <- 
  getModelMice(tInitials)

mDecay = merge(mDecay, iAnimals, all.x = TRUE) %>% 
  as_tibble()

mDecay$AIC <-
  apply(mDecay, 1, function(x) {
    calculateAIC(
      n.parameters = length(tInitials),
      NLL = as.numeric(x[(length(tInitials) + 2)]))
  })
