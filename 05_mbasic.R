# 2019-11-28
# judyta
# model choices

# result ------------------------------------------------------------------
model <- list()

# basic -------------------------------------------------------------------
modelNLL <- 
  function(par, A) {
  A = A[with(A, order(StartDateTime)), ]
  nll = 0
  if (par[1] < 0 | par[1] > 1 |
      par[2] < 0 | par[2] > 50) {
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
          (sum(exp(par[2] * Q)))
        P[2] = exp(par[2] * Q[2]) / 
          (sum(exp(par[2] * Q)))
        if(P[1] < .001){P[1] = .001; P[2] = .999}
        if(P[2] < .001){P[2] = .001; P[1] = .999}
        nll = -log(P[s]) + nll
        pe = r - Q[s]
        Q[s] = Q[s] + (par[1] * pe)}}}
  nll}

{name = "basic"
  initial <- 
    expand.grid(
      alpha = seq(0.05, 1, by = 0.05),
      beta = seq(0.25, 15, by = 0.25)) %>%
    as.list()
  model[[name]] <-
    getModelMice(initial) %>%
    mutate(name = name,
           aic = calculateAIC(
             length(initial), as.numeric(value)))}

# dual --------------------------------------------------------------------
modelNLL <- 
  function(par, A) {
    A = A[with(A, order(StartDateTime)), ]
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
            (sum(exp(par[2] * Q)))
          P[2] = exp(par[2] * Q[2]) / 
            (sum(exp(par[2] * Q)))
          if(P[1] < .001){P[1] = .001; P[2] = .999}
          if(P[2] < .001){P[2] = .001; P[1] = .999}
          nll = -log(P[s]) + nll
          pe = r - Q[s]
          if (r == 1) { #if rewarded
            Q[s] = Q[s] + (par[1] * pe)
          } else {
            Q[s] = Q[s] + (par[3] * pe)}}}}
    nll}

{name = "dual"
  initial <-
    expand.grid(
      alpha.pos = seq(0.05, 1, by = 0.05),
      beta =  seq(0.25, 15, by = 0.25),
      alpha.neg = seq(0.05, 1, by = 0.05)) %>% 
    as.list()
  model[[name]] <-
    getModelMice(initial) %>%
    mutate(name = name,
           aic = calculateAIC(
             length(initial), as.numeric(value)))}

# attention ---------------------------------------------------------------
modelNLL <- 
  function(par, A) {
    A = A[with(A, order(StartDateTime)), ]
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
            (sum(exp(par[2] * Q)))
          P[2] = exp(par[2] * Q[2]) / 
            (sum(exp(par[2] * Q)))
          if(P[1] < .001){P[1] = .001; P[2] = .999}
          if(P[2] < .001){P[2] = .001; P[1] = .999}
          nll = -log(P[s]) + nll
          pe = r - Q[s]
          alpha = par[3] * abs(pe) + (1 - par[3]) * alpha
          Q[s] = Q[s] + (alpha * pe)}}}
    nll}

{name = "attention"
  initial <-
    expand.grid(
      alpha.zero = seq(0.05, 1, by = 0.05),
      beta = seq(0.25, 15, by = 0.25),
      ni= seq(0.05, 1, by = 0.05)) %>%
    as.list()
  model[[name]] <-
    getModelMice(initial) %>%
    mutate(name = name,
           aic = calculateAIC(
             length(initial), as.numeric(value)))}

# fictitious --------------------------------------------------------------
modelNLL <- 
  function(par, A) {
    A = A[with(A, order(StartDateTime)), ]
    nll = 0
    if ((par[1] < 0 | par[1] > 1) |
        (par[2] < 0 | par[2] > 50)) {
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
            (sum(exp(par[2] * Q)))
          P[2] = exp(par[2] * Q[2]) / 
            (sum(exp(par[2] * Q)))
          if(P[1] < .001){P[1] = .001; P[2] = .999}
          if(P[2] < .001){P[2] = .001; P[1] = .999}
          nll = -log(P[s]) + nll
          pe = r - Q[s]
          Q[s] = Q[s] + (par[1] * pe)
          Q[-s] = Q[-s] - (par[1] * pe)}}}
    nll}

{name = "fictitious"
  initial <-
    expand.grid(
      alpha = seq(0.05, 1, by = 0.05),
      beta = seq(0.25, 15, by = 0.25)) %>%
    as.list()
  model[[name]] <-
    getModelMice(initial) %>%
    mutate(name = name,
           aic = calculateAIC(
             length(initial), as.numeric(value)))}

# hybrid ------------------------------------------------------------------
modelNLL <- 
  function(par, A) {
    A = A[with(A, order(StartDateTime)), ]
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
            (sum(exp(par[2] * Q)))
          P[2] = exp(par[2] * Q[2]) / 
            (sum(exp(par[2] * Q)))
          if(P[1] < .001){P[1] = .001; P[2] = .999}
          if(P[2] < .001){P[2] = .001; P[1] = .999}
          nll = -log(P[s]) + nll
          pe = r - Q[s]
          if (r == 1) {
            Q[s] = Q[s] + (par[1] * pe)
            Q[-s] = Q[-s] - (par[1] * pe)
          } else {
            Q[s] = Q[s] + (par[3] * pe)
            Q[-s] = Q[-s] - (par[3] * pe)}}}}
    nll}

{name = "hybrid"
  initial <-
    expand.grid(
      alpha.pos = seq(0.05, 1, by = 0.05),
      beta =  seq(0.25, 15, by = 0.25),
      alpha.neg = seq(0.05, 1, by = 0.05)) %>% 
    as.list()
  model[[name]] <-
    getModelMice(initial) %>%
    mutate(name = name,
           aic = calculateAIC(
             length(initial), as.numeric(value)))}

# forgetful ---------------------------------------------------------------
modelNLL <- 
  function(par, A) {
    A = A[with(A, order(StartDateTime)), ]
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
            (sum(exp(par[2] * Q)))
          P[2] = exp(par[2] * Q[2]) / 
            (sum(exp(par[2] * Q)))
          if(P[1] < .001){P[1] = .001; P[2] = .999}
          if(P[2] < .001){P[2] = .001; P[1] = .999}
          nll = -log(P[s]) + nll
          pe = r - Q[s]
          Q[s] = Q[s] + par[1] * pe
          Q[s] = Q[s] + par[3] * (0.5 - Q[s])
          Q[-s] = Q[-s] + par[3] * (0.5 - Q[-s])}}}
    nll}

{name = "hybrid"
  initial <-
    expand.grid(
      alpha = seq(0.05, 1, by = 0.05),
      beta = seq(0.25, 15, by = 0.25),
      epsilon = seq(0.05, 1, by = 0.05)) %>% 
    as.list()
  model[[name]] <-
    getModelMice(initial) %>%
    mutate(name = name,
           aic = calculateAIC(
             length(initial), as.numeric(value)))}

# decay -------------------------------------------------------------------
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
          if(P[s] < .001){P[s] = .001}
          if(P[s] > .999){P[s] = .999}
          nll = -log(P[s]) + nll
          pe = r - Q[s]
          Q[s] = Q[s] + (par[1] * pe)
          Q[s] = exp( -(t[s]) * par[3] ) * Q[s]
          tempQ = exp( -(t[-s]) * par[3] ) * Q[-s]}}}
    nll}

{name = "decay"
  initial <-
    expand.grid(
      alpha = seq(0.05, 1, by = 0.05),
      beta = seq(0.25, 15, by = 0.25),
      storage = seq(0.05, 1, by = 0.05)) %>%
    as.list()
  model[[name]] <-
    getModelMice(initial) %>%
    mutate(name = name,
           aic = calculateAIC(
             length(initial), as.numeric(value)))}

# reproval ----------------------------------------------------------------
modelNLL <- 
  function(par, A) {
    A = A[with(A, order(StartDateTime)), ]
    nll = 0
    if (par[1] < 0 | par[1] > 1 |
        par[2] < 0 | par[2] > 50 |
        par[3] > 0 | par[3] < 1) {
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
        if (is.finite(s) & is.finite(r) & is.finite(w) & is.finite(t)) { 
          pe = r - Q[s]   
          P[1] = exp(par[2] * Q[1]) / 
            (sum(exp(par[2] * Q)))
          P[2] = exp(par[2] * Q[2]) / 
            (sum(exp(par[2] * Q)))
          b0 = log(P[w] / (1 - P[w]))
          print(P[w])
          P[w] = exp(b0 + par[3] * t) / 
            (1 + exp(b0 + par[3] * t))
          P[-w] = 1 - P[w]
          print(c(P[w], b0, t))
          # if(P[1] < .001){P = c(.001, .999)}
          # if(P[1] > .999){P = c(.999, .001)}
          # if(P[2] < .001){P = c(.999, .001)}
          # if(P[2] > .999){P = c(.001, .999)}
          if(P[s] < .001){P[s] = .001}
          if(P[s] > .999){P[s] = .999}
          Q[s] = Q[s] + (par[1] * pe)
          nll = -log(P[s]) + nll}}}
    nll}

{name = "reproval"
  initial <-
    expand.grid(
      alpha = seq(0.05, 1, by = 0.05),
      beta = seq(0.25, 15, by = 0.25),
      iota = seq(0.01, 1, by = 0.01)) %>% 
    as.list()
  model[[name]] <-
    getModelMice(initial) %>%
    mutate(name = name,
           aic = calculateAIC(
             length(initial), as.numeric(value)))}

# random ------------------------------------------------------------------
{name = "zero"
model[[name]] <-
    dModel %>%
      group_by(tag = Tag) %>%
      summarise(n = n()) %>%
  mutate(name = name,
         aic = calculateAIC(0, n * -log(0.5)))}


# analysis ----------------------------------------------------------------

aictidy <- 
  model %>%
  purrr::map(~select(., name, aic, tag)) %>%
  bind_rows() %>%
  mutate(delta = aic - .$aic[.$tag == tag & .$name == "basic"]) %>%
  merge(iAnimals, all.x = T) %>%
  as_tibble()

aictidy %>%
  ggplot(aes(x = name, y = delta)) +
  geom_hline(yintercept = 0)+
  geom_boxplot(
    outlier.colour = NA,
    fill = NA,
    size = 0.4) +
  geom_quasirandom(
    width = 0.2,
    method = "quasirandom",
    varwidth = TRUE,
    colour = 'black',
    size = 1) +
  theme_publication +
  labs(y = "dAIC", x = element_blank()) +
  theme(
    panel.spacing = unit(2, "lines"),
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.ticks.y = element_line(linetype = 'dashed'),
    axis.line.x = element_blank()) +
  facet_wrap( ~ substance, ncol = 2)
  