#model basic

modelNLL <- 
  function(par, A) {
  A = A[with(A, order(StartDateTime)), ]
  nll = 0
  if (par[1] < 0 | par[1] > 1 |
      par[2] < 0 | par[2] > 50) {
    # limitations
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
        P[1] = 
          exp(par[2] * Q[1]) / 
          (sum(exp(par[2] * Q[1]), exp(par[2] * Q[2])))
        # softmax
        P[2] = 
          exp(par[2] * Q[2]) / 
          (sum(exp(par[2] * Q[1]), exp(par[2] * Q[2])))
        nll = -log(P[s]) + nll
        pe = r - Q[s]
        Q[s] = Q[s] + (par[1] * pe) 
        #theres the core equation
      } 
    }
  }
  nll
}

# Set initial values
#   note: 
#     number of optimisation starting 
#     points is length(tInitials[[1]])
tInitials <- 
  expand.grid(
    alpha = seq(0.05, 1, by = 0.05),
    beta = seq(0.25, 15, by = 0.25)) %>%
  as.list()

mBasic <-
  getModelMice(tInitials) 
#it should take < 5min

mBasic = 
  merge(mBasic, iAnimals, all.x = TRUE) %>% 
  as_tibble()

mBasic$AIC <- 
  apply(mBasic, 1, function(x) {
  calculateAIC(
    n.parameters = length(tInitials), 
    NLL = as.numeric(x[(length(tInitials) + 2)]))
})
