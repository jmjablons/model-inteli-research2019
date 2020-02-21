# v "reprove
# 2019-11-27
# judyta

# REPROVAL ----------------------------------------------------------------
modelNLL <- 
  function(par, A) {
    A = A[with(A, order(StartDateTime)), ]
    nll = 0
    if (par[1] < 0 | par[1] > 1 |
        par[2] < 0 | par[2] > 50 |
        par[3] < 0 | par[3] > 100) {
      nll = Inf
    } else {
      temp = nrow(A)
      Q = c(0, 0)
      P <- vector()
      b0 = 0
      rewards = A$RewardDoor
      sides = ceiling(A$Corner / 2)
      previous = lag(sides)
      previous[1] = 1
      intervals = A$IntervalBefore
      intervals[1] = 0
      out <- list()
      for (i in seq_along(1:temp)) {
        r = rewards[i]
        s = sides[i]
        w = previous[i]
        t = intervals[i]
        if (is.finite(s) & is.finite(r)) { 
          pe = r - Q[s]   
          P[1] = exp(par[2] * Q[1]) / 
            (sum(exp(par[2] * Q[1]), 
                 exp(par[2] * Q[2])))
          P[2] = exp(par[2] * Q[2]) / 
            (sum(exp(par[2] * Q[1]), 
                 exp(par[2] * Q[2])))
          b0 = log(P[w] / (1 - P[w]))
          P[w] = exp(b0 + par[3] * t) / 
            (1 + exp(b0 + par[3] * t))
          if((1 - P[w]) < 0.001){
            P[-w] = .001
          } else {
            P[-w] = 1 - P[w]} 
          Q[s] = Q[s] + (par[1] * pe)
          nll = -log(P[s]) + nll          
          }}}
    nll}

mReproval <- 
  getModelMice(
    list.parameters = 
      expand.grid(
        alpha = seq(0.05, 1, by = 0.05),
        beta = seq(0.25, 15, by = 0.25),
        iota = seq(0.05, 1, by = 0.05)) %>% 
      as.list())

mReproval = merge(mReproval, iAnimals %>% mutate(tag = Tag), all.x = TRUE)

mReproval$AIC <-
  apply(mReproval, 1, function(x) {
    calculateAIC(
      n.parameters = 3,
      NLL = as.numeric(x[5]))})
