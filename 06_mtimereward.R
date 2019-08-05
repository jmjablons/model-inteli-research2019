
# TIME + GLM --------------------------------------------------------------

# include glm interval-related estimate

rGlm = 
  rGlm %>%
  mutate(Tag = as.character(Tag))
# type agreeing

modelNLL <- 
  function(par, A) {
    A = A[with(A, order(StartDateTime)),]
    nll = 0
    if (par[1] < 0 | par[1] > 1 |
        par[2] < 0 | par[2] > 50) {
      nll = Inf
    } else {
      Q = c(0, 0)
      P = vector()
      b0 = c(0, 0)
      rewards = A$RewardDoor
      sides = ceiling(A$Corner / 2)
      intervals = A$IntervalBefore
      #pretty obvious, isn't it!?
      intervals[1] = 0
      LOD = rGlm$Estimate[
        rGlm$Predictor == 'IntervalAfter' &
          rGlm$Tag == simple(A$Tag)]
      #TODO (get rid of simple())
      for (i in seq_along(sides)) {
        r = rewards[i]
        s = sides[i]
        t = intervals[i]
        if (is.finite(s) & is.finite(r)) {
          pe = r - Q[s]
          P[1] = exp(par[2] * Q[1]) / 
            (sum(exp(par[2] * Q[1]), 
                 exp(par[2] * Q[2])))
          P[2] = exp(par[2] * Q[2]) / 
            (sum(exp(par[2] * Q[1]), 
                 exp(par[2] * Q[2])))
          b0[1] = log(P[1] / (1 - P[1]))
          b0[2] = log(P[2] / (1 - P[2]))
          P[1] = exp(b0[1] + LOD * t) /
            (1 + exp(b0[1] + LOD * t))
          P[2] = exp(b0[2] + LOD * t) /
            (1 + exp(b0[2] + LOD * t))
          Q[s] = Q[s] + (par[1] * pe)
          nll = -log(P[s]) + nll
        } 
      }
    }
    nll
  }


tInitials <- expand.grid(
  alpha = seq(0.05, 1, by = 0.05),
  beta = seq(0.25, 15, by = 0.25)) %>%
  as.list()

mTime  <- 
  getModelMice(tInitials) #be carreful!

mTime  = 
  merge(mTime , iAnimals, all.x = TRUE) %>% 
  as_tibble()

mTime$AIC <- 
  apply(mTime , 1, function(x) {
    calculateAIC(
      n.parameters = length(tInitials), 
      NLL = as.numeric(x[(length(tInitials) + 2)]))
  })


# TIME WITHOUT GLM --------------------------------------------------------

# 3rd parameter optimised
#   in place of glm estimate

modelNLL <- 
  function(par, A) {
    A = A[with(A, order(StartDateTime)),]
    nll = 0
    if (par[1] < 0 | par[1] > 1  |
        par[2] < 0 | par[2] > 50 ) {
      nll = Inf
    } else {
      Q = c(0, 0)
      P = vector()
      b0 = c(0, 0)
      rewards = A$RewardDoor
      sides = ceiling(A$Corner / 2)
      intervals = A$IntervalBefore
      intervals[1] = 0
      #odd = par[3]
      for (i in seq_along(sides)) {
        r = rewards[i]
        s = sides[i]
        t = intervals[i]
        if (is.finite(s) & is.finite(r)) {
          pe = r - Q[s]
          P[1] = exp(par[2] * Q[1]) / 
            (sum(exp(par[2] * Q[1]), 
                 exp(par[2] * Q[2])))
          P[2] = exp(par[2] * Q[2]) / 
            (sum(exp(par[2] * Q[1]), 
                 exp(par[2] * Q[2])))
          b0[1] = log(P[1] / (1 - P[1]))
          b0[2] = log(P[2] / (1 - P[2]))
          P[1] = exp(b0[1] + par[3] * t) / 
            (1 + exp(b0[1] + par[3] * t))
          P[2] = exp(b0[2] + par[3] * t) / 
            (1 + exp(b0[2] + par[3] * t))
          Q[s] = Q[s] + (par[1] * pe)
          nll = -log(P[s]) + nll
        } 
      }
    }
    nll
  }

tInitials <- 
  expand.grid(
    alpha = seq(0.05, 1, by = 0.05),
    beta = seq(0.25, 15, by = 0.25),
    lod = seq(0, 0.1, by = 0.001)) %>% 
  as.list()
mTimeOptimised <- 
  getModelMice(tInitials) #be carreful!

mTimeOptimised = 
  merge(mTimeOptimised, iAnimals, all.x = TRUE) %>% 
  as_tibble()

mTimeOptimised$AIC <- 
  apply(mTimeOptimised, 1, function(x) {
    calculateAIC(
      n.parameters = length(tInitials),
      NLL = as.numeric(x[(length(tInitials) + 2)]))
  })

