# 2019-11-28
# judyta
# model choices

# result ------------------------------------------------------------------
rmodel <- list()

# basic -------------------------------------------------------------------
model <- 
  function(par, a) {
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
      if (is.finite(s) & is.finite(r)) {
        P = exp(par[2] * Q) / sum(exp(par[2] * Q))
        if(any(P < .001)){
          index = which(P < .001)
          P[index] = .001; P[-(index)] = .999}
        nll = -log(P[s]) + nll
        pe = r - Q[s]
        Q[s] = Q[s] + (par[1] * pe)}}}
  nll}

{initial <- expand.grid(
      alpha = seq(0.05, 1, by = 0.05),
      beta = seq(0.25, 15, by = 0.25)) %>%
    as.list()
  rmodel[[(name = "basic")]] <- getModelMice(list.parameters = initial, A = dmodel) %>%
    mutate(name = name, aic = calculateAIC(length(initial), as.numeric(value)))}

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
        if (is.finite(s) & is.finite(r)) {
          P = exp(par[2] * Q) / sum(exp(par[2] * Q))
          if(any(P < .001)){
            index = which(P < .001)
            P[index] = .001; P[-(index)] = .999}
          nll = -log(P[s]) + nll
          pe = r - Q[s]
          if (r == 1) {Q[s] = Q[s] + (par[1] * pe)
          } else {Q[s] = Q[s] + (par[3] * pe)}}}}
    nll}

{name = "dual"
  initial <- expand.grid(
      alpha.pos = seq(0.05, 1, by = 0.05),
      beta =  seq(0.25, 15, by = 0.25),
      alpha.neg = seq(0.05, 1, by = 0.05)) %>% 
    as.list()
  rmodel[[name]] <- getModelMice(initial) %>%
    mutate(name = name,
           aic = calculateAIC(length(initial), as.numeric(value)))}

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
        if (is.finite(s) & is.finite(r)) {
          P = exp(par[2] * Q) / sum(exp(par[2] * Q))
          if(any(P < .001)){
            index = which(P < .001)
            P[index] = .001; P[-(index)] = .999}
          nll = -log(P[s]) + nll
          pe = r - Q[s]
          alpha = par[3] * abs(pe) + (1 - par[3]) * alpha.zero
          Q[s] = Q[s] + (alpha * pe)}}}
    nll}

{name = "attention"
  initial <- expand.grid(
      alpha.zero = seq(0.05, 1, by = 0.05),
      beta = seq(0.25, 15, by = 0.25),
      ni= seq(0.05, 1, by = 0.05)) %>%
    as.list()
  rmodel[[name]] <- getModelMice(initial) %>%
    mutate(name = name,
           aic = calculateAIC(length(initial), as.numeric(value)))}

# fictitious --------------------------------------------------------------
model <- 
  function(par, a) {
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
        if (is.finite(s) & is.finite(r)) {
          P = exp(par[2] * Q) / sum(exp(par[2] * Q))
          if(any(P < .001)){
            index = which(P < .001)
            P[index] = .001; P[-(index)] = .999}
          nll = -log(P[s]) + nll
          pe = r - Q[s]
          Q[s] = Q[s] + (par[1] * pe)
          Q[-s] = Q[-s] - (par[1] * pe)}}}
    nll}

{name = "fictitious"
  initial <- expand.grid(
      alpha = seq(0.05, 1, by = 0.05),
      beta = seq(0.25, 15, by = 0.25)) %>%
    as.list()
  rmodel[[name]] <- getModelMice(initial) %>%
    mutate(name = name,
           aic = calculateAIC(length(initial), as.numeric(value)))}

# hybrid ------------------------------------------------------------------
model <- 
  function(par, a) {
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
        if (is.finite(s) & is.finite(r)) {
          P = exp(par[2] * Q) / sum(exp(par[2] * Q))
          if(any(P < .001)){
            index = which(P < .001)
            P[index] = .001; P[-(index)] = .999}
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
  initial <- expand.grid(
      alpha.pos = seq(0.05, 1, by = 0.05),
      beta =  seq(0.25, 15, by = 0.25),
      alpha.neg = seq(0.05, 1, by = 0.05)) %>% 
    as.list()
  rmodel[[name]] <- getModelMice(initial) %>%
    mutate(name = name,
           aic = calculateAIC(
             length(initial), as.numeric(value)))}

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
        if (is.finite(s) & is.finite(r)) {
          P = exp(par[2] * Q) / sum(exp(par[2] * Q))
          if(any(P < .001)){
            index = which(P < .001)
            P[index] = .001; P[-(index)] = .999}
          nll = -log(P[s]) + nll
          pe = r - Q[s]
          Q[s] = Q[s] + par[1] * pe
          Q[s] = Q[s] + par[3] * (0.5 - Q[s])
          Q[-s] = Q[-s] + par[3] * (0.5 - Q[-s])}}}
    nll}

{name = "forgetful"
  initial <- expand.grid(
      alpha = seq(0.05, 1, by = 0.05),
      beta = seq(0.25, 15, by = 0.25),
      epsilon = seq(0.05, 1, by = 0.05)) %>% 
    as.list()
  rmodel[[name]] <- getModelMice(initial) %>%
    mutate(name = name,
           aic = calculateAIC(length(initial), as.numeric(value)))}

# forgetful+ --------------------------------------------------------------
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
    intervals = a$intervalb
    ep = 0
    for (i in seq_along(sides)) {
      r = rewards[i]
      s = sides[i]
      t = intervals[i]
      if(t > exp(1) & !is.na(t)) {ep = 1 - 1/log(t)}
      if (is.finite(s) & is.finite(r)) {
        P = exp(par[2] * Q) / sum(exp(par[2] * Q))
        if(any(P < .001)){
          index = which(P < .001)
          P[index] = .001; P[-(index)] = .999}
        nll = -log(P[s]) + nll
        pe = r - Q[s]
        Q[s] = Q[s] + par[1] * pe
        Q[s] = Q[s] + ep * (0.5 - Q[s])
        Q[-s] = Q[-s] + ep * (0.5 - Q[-s])}}}
  nll}

{name = "forgetful+"
  initial <- expand.grid(
    alpha = seq(0.05, 1, by = 0.05),
    beta = seq(0.25, 15, by = 0.25)) %>% 
    as.list()
  rmodel[[name]] <- getModelMice(initial) %>%
    mutate(name = name,
           aic = calculateAIC(length(initial), as.numeric(value)))}

# decay -------------------------------------------------------------------
model <- function(par, a) {
    a = a[with(a, order(start)), ]
    nll = 0
    if (par[1] < 0 | par[1] > 1 |
        par[2] < 0 | par[2] > 50 |
        par[3] < 0 | par[3] > 1) {
      nll = Inf
    } else {
      temp = nrow(a)
      tempQ = c()
      Q = c(0, 0)
      date = rep(a$start[1], 2)
      t = c(0, 0)
      P <- vector()
      for (i in seq_along(1:temp)) {
        r = a$dooropened[i]
        s = ceiling(a$corner[i] / 2)
        now = a$start[i]
        t = as.numeric(difftime(now, date, units = 'mins'))
        date[s] = now
        if (is.finite(s) & is.finite(r)) {
          Q[s] = exp( -(t[s]) * par[3] ) * Q[s]
          tempQ = exp( -(t[-s]) * par[3] ) * Q[-s]
          P[s] = exp(par[2] * Q[s]) / 
            (sum(exp(par[2] * Q[s]), exp(par[2] * tempQ)))
          if(P[s] < .001){P[s] = .001}
          nll = -log(P[s]) + nll
          pe = r - Q[s]
          Q[s] = Q[s] + (par[1] * pe)}}}
    nll}

{name = "decay"
  initial <- expand.grid(
      alpha = seq(0.05, 1, by = 0.05),
      beta = seq(0.25, 15, by = 0.25),
      storage = seq(0.05, 1, by = 0.05)) %>%
    as.list()
  rmodel[[name]] <- getModelMice(initial) %>%
    mutate(name = name,
           aic = calculateAIC(length(initial), as.numeric(value)))}


# decay hybrid ------------------------------------------------------------

model <- function(par, a) {
    a = a[with(a, order(start)), ]
    nll = 0
    if (par[1] < 0 | par[1] > 1 |
        par[2] < 0 | par[2] > 50 |
        par[3] < 0 | par[3] > 1 |
        par[4] < 0 | par[4] > 1 |
        par[5] < 0 | par[5] > 1) {
      nll = Inf
    } else {
      temp = nrow(a)
      tempQ = c()
      Q = c(0, 0)
      date = rep(a$start[1], 2)
      t = c(0, 0)
      P <- vector()
      for (i in seq_along(1:temp)) {
        r = a$dooropened[i]
        s = ceiling(a$corner[i] / 2)
        now = a$start[i]
        t = as.numeric(difftime(now, date, units = 'mins'))
        date[s] = now
        if (is.finite(s) & is.finite(r)) {
          if(r == 1){
            Q[s] = exp( -(t[s]) * par[3] ) * Q[s]  
            tempQ = exp( -(t[-s]) * par[5] ) * Q[-s]}
          if(r == 0){
            Q[s] = exp( -(t[s]) * par[4] ) * Q[s]  
            tempQ = exp( -(t[-s]) * par[5] ) * Q[-s]}
          P[s] = exp(par[2] * Q[s]) / 
            (sum(exp(par[2] * Q[s]), exp(par[2] * tempQ)))
          if(P[s] < .001){P[s] = .001}
          nll = -log(P[s]) + nll
          pe = r - Q[s]
          Q[s] = Q[s] + (par[1] * pe)}}}
    nll}

{name = "decay+"
  initial <- expand.grid(
      alpha = seq(0.05, 1, by = 0.05),
      beta = seq(0.25, 15, by = 0.25),
      storage.pos = seq(0.05, 1, by = 0.05),
      storage.neg = seq(0.05, 1, by = 0.05),
      storage.neutral = seq(0.05, 1, by = 0.05)) %>%
    as.list()
  rmodel[[name]] <- getModelMice(initial) %>%
    mutate(name = name,
           aic = calculateAIC(length(initial), as.numeric(value)))}

# old decay ---------------------------------------------------------------

model <- 
  function(par, a) {
    a = a[with(a, order(start)), ]
    nll = 0
    if (par[1] < 0 | par[1] > 1 |
        par[2] < 0 | par[2] > 50 |
        par[3] < 0 | par[3] > 1) {
      nll = Inf
    } else {
      temp = nrow(a)
      tempQ = c()
      Q = c(0, 0)
      date = rep(a$start[1], 2)
      t = c(0, 0)
      P <- vector()
      for (i in seq_along(1:temp)) {
        r = a$dooropened[i]
        s = ceiling(a$corner[i] / 2)
        now = a$start[i]
        t[1] = as.numeric(difftime(now, date[1], units = 'mins'))
        t[2] = as.numeric(difftime(now, date[2], units = 'mins'))
        date[s] = now
        if (is.finite(s) & is.finite(r)) {
          P[s] = exp(par[2] * Q[s]) /  
            (sum(exp(par[2] * Q[s]), exp(par[2] * tempQ)))
          if(P[s] < .001){P[s] = .001}
          if(P[s] > .999){P[s] = .999}
          nll = -log(P[s]) + nll
          pe = r - Q[s]
          Q[s] = Q[s] + (par[1] * pe)
          Q[s] = exp( -(t[s]) * par[3] ) * Q[s]
          tempQ = exp( -(t[-s]) * par[3] ) * Q[-s]}}}
    nll}

{name = "olddecay"
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
model <- function(par, a) {
    a = a[with(a, order(start)), ]
    nll = 0
    if (par[1] < 0 | par[1] > 1 |
        par[2] < 0 | par[2] > 50 |
        par[3] < 0 | par[3] > 1) {
      nll = Inf
    } else {
      temp = nrow(a)
      Q = c(0, 0)
      P <- vector()
      b0 = 0
      rewards = a$dooropened
      sides = ceiling(a$corner / 2)
      previous = lag(sides)
      previous[1] = 1
      intervals = a$intervalb
      intervals[1] = 0
      out <- list()
      for (i in seq_along(1:temp)) {
        r = rewards[i]
        s = sides[i]
        w = previous[i]
        t = intervals[i]
        if (is.finite(s) & is.finite(r)) { 
          pe = r - Q[s]   
          P = exp(par[2] * Q) / sum(exp(par[2] * Q))
          b0 = log(P[w] / (1 - P[w]))
          if(t > 120) t = 120
          P[w] = exp(b0 + par[3] * t) / 
            (1 + exp(b0 + par[3] * t))
          P[-w] = 1 - P[w]
          if(any(P < .001)){
            index = which(P < .001)
            P[index] = .001; P[-(index)] = .999}
          Q[s] = Q[s] + (par[1] * pe)
          nll = -log(P[s]) + nll}}}
    nll}

{name = "reproval"
  initial <- expand.grid(
      alpha = seq(0.05, 1, by = 0.05),
      beta = seq(0.25, 15, by = 0.25),
      iota = seq(0.05, 1, by = 0.05)) %>% 
    as.list()
  rmodel[[name]] <- getModelMice(initial) %>%
    mutate(name = name,
           aic = calculateAIC(length(initial), as.numeric(value)))}

# random ------------------------------------------------------------------
{name = "zero"
rmodel[[name]] <- dModel %>%
  group_by(tag = Tag) %>%
  summarise(n = n()) %>%
  mutate(name = name, aic = calculateAIC(0, n * -log(0.5)))}

# analysis ----------------------------------------------------------------
aictidy <- rmodel %>%
  purrr::map(~select(., name, aic, tag)) %>%
  bind_rows() %>%
  mutate(delta = aic - .$aic[.$tag == tag & .$name == "basic"]) %>%
  merge(manimal, all.x = T) %>%
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


# parameters --------------------------------------------------------------

rmodel$`decay+` %>%
  select(tag, par.alpha, par.beta, par.storage.pos, par.storage.neg, par.storage.neutral) %>%
  tidyr::gather(par, value, -tag) %>%
  left_join(manimal, by = "tag") %>%
  ggplot(aes(x = par, y = as.numeric(value), fill = substance)) +
  geom_boxplot()

rmodel$hybrid %>%
  select(tag, par.alpha.pos, par.beta, par.alpha.neg) %>%
  tidyr::gather(par, value, -tag) %>%
  left_join(manimal, by = "tag") %>%
  ggplot(aes(x = par, y = as.numeric(value), fill = substance)) +
  geom_boxplot()+
  facet_wrap(~par, scales = "free")
