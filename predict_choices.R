#correct prediction
# get the parameters
# simulate output for each choice
# verify with real outcome
# as a %

# variable ----------------------------------------------------------------
rprediction <- list()

# custom ------------------------------------------------------------------
wrap <- function(which.model = modelname, 
                 tag.list = manimal$tag, 
                 parameter.value = dmodel){
  lapply(tag.list, function(x){
      data.frame(
        prediction = predict_(
          par = rmodel[[which.model]] %>% 
            filter(name == which.model) %>%
            filter(tag == x) %>%
            .[grepl("par", names(.))] %>%
            as.numeric(), 
          a = filter(parameter.value, tag == x)),
        name = which.model,
        tag = x, stringsAsFactors = F)}) %>%
    bind_rows() %>% as_tibble()}

# basic -------------------------------------------------------------------
predict_ <- function(par, a) {
    a = a[with(a, order(start)), ]
      Q = c(0, 0)
      P <- c(0, 0)
      rewards = a$dooropened
      sides = ceiling(a$corner / 2)
      list.choice = list()
      correct.prediction = vector()
      for (i in seq_along(sides)) {
        r = rewards[i]
        s = sides[i]
        correct.prediction[i] = NA
        if (is.finite(s) & is.finite(r)) {
          P = exp(par[2] * Q) / sum(exp(par[2] * Q))
          if(any(P < .001)){
            index = which(P < .001)
            P[index] = .001; P[-(index)] = .999}
          pe = r - Q[s]
          Q[s] = Q[s] + (par[1] * pe)
          predicted.choice <- ifelse(length(P) > 1 & all(!is.na(P)), which(P > .5), NA)
          correct.prediction[i] <- as.numeric(predicted.choice == s)}}
    correct.prediction}

modelname = "basic"
rprediction[[modelname]] <- wrap()

# dual --------------------------------------------------------------------
predict_ <- function(par, a) {
    a = a[with(a, order(start)), ]
      Q = c(0, 0)
      P <- c(0, 0)
      rewards = a$dooropened
      sides = ceiling(a$corner / 2)
      list.choice = list()
      correct.prediction = vector()
      for (i in seq_along(sides)) {
        r = rewards[i]
        s = sides[i]
        correct.prediction[i] = NA
        if (is.finite(s) & is.finite(r)) {
          P = exp(par[2] * Q) / sum(exp(par[2] * Q))
          if(any(P < .001)){
            index = which(P < .001)
            P[index] = .001; P[-(index)] = .999}
          pe = r - Q[s]
          if (r == 1) {Q[s] = Q[s] + (par[1] * pe)
          } else {Q[s] = Q[s] + (par[3] * pe)}
          predicted.choice <- ifelse(length(P) > 1 & all(!is.na(P)), which(P > .5), NA)
          correct.prediction[i] <- as.numeric(predicted.choice == s)}}
    correct.prediction}

modelname = "dual"
rprediction[[modelname]] <- wrap()

# attention ---------------------------------------------------------------
predict_ <- function(par, a) {
  a = a[with(a, order(start)), ]
  Q = c(0, 0)
  P <- c(0, 0)
  rewards = a$dooropened
  sides = ceiling(a$corner / 2)
  correct.prediction = vector()
  for (i in seq_along(sides)) {
    Q = c(0, 0)
    P <- vector()
    rewards = a$dooropened
    sides = ceiling(a$corner / 2)
    alpha.zero = par[1]
    for (i in seq_along(sides)) {
      r = rewards[i]
      s = sides[i]
      correct.prediction[i] = NA
      if (is.finite(s) & is.finite(r)) {
        P = exp(par[2] * Q) / sum(exp(par[2] * Q))
        if(any(P < .001)){
          index = which(P < .001)
          P[index] = .001; P[-(index)] = .999}
        pe = r - Q[s]
        alpha = par[3] * abs(pe) + (1 - par[3]) * alpha.zero
        Q[s] = Q[s] + (alpha * pe)
        predicted.choice <- ifelse(length(P) > 1 & all(!is.na(P)), which(P > .5), NA)
        correct.prediction[i] <- as.numeric(predicted.choice == s)}}}
    correct.prediction}

modelname = "attention"
rprediction[[modelname]] <- wrap()


# fictitious --------------------------------------------------------------
predict_ <- function(par, a) {
  a = a[with(a, order(start)), ]
  Q = c(0, 0)
  P <- c(0, 0)
  rewards = a$dooropened
  sides = ceiling(a$corner / 2)
  correct.prediction = vector()
  for (i in seq_along(sides)) {
    r = rewards[i]
    s = sides[i]
    correct.prediction[i] = NA
    if (is.finite(s) & is.finite(r)) {
      P = exp(par[2] * Q) / sum(exp(par[2] * Q))
      if(any(P < .001)){
        index = which(P < .001)
        P[index] = .001; P[-(index)] = .999}
      pe = r - Q[s]
      Q[s] = Q[s] + (par[1] * pe)
      Q[-s] = Q[-s] - (par[1] * pe)
      predicted.choice <- ifelse(length(P) > 1 & all(!is.na(P)), which(P > .5), NA)
      correct.prediction[i] <- as.numeric(predicted.choice == s)}}
  correct.prediction}

modelname = "fictitious"
rprediction[[modelname]] <- wrap()

# hybird ------------------------------------------------------------------
predict_ <- function(par, a) {
  a = a[with(a, order(start)), ]
  Q = c(0, 0)
  P <- c(0, 0)
  rewards = a$dooropened
  sides = ceiling(a$corner / 2)
  correct.prediction = vector()
  for (i in seq_along(sides)) {
    r = rewards[i]
    s = sides[i]
    correct.prediction[i] = NA
    if (is.finite(s) & is.finite(r)) {
      P = exp(par[2] * Q) / sum(exp(par[2] * Q))
      if(any(P < .001)){
        index = which(P < .001)
        P[index] = .001; P[-(index)] = .999}
      pe = r - Q[s]
      if (r == 1) {
        Q[s] = Q[s] + (par[1] * pe)
        Q[-s] = Q[-s] - (par[1] * pe)
      } else {
        Q[s] = Q[s] + (par[3] * pe)
        Q[-s] = Q[-s] - (par[3] * pe)}
      predicted.choice <- ifelse(length(P) > 1 & all(!is.na(P)), which(P > .5), NA)
      correct.prediction[i] <- as.numeric(predicted.choice == s)}}
  correct.prediction}

modelname = "hybrid"
rprediction[[modelname]] <- wrap()

# forgetful ---------------------------------------------------------------
predict_ <- function(par, a) {
  a = a[with(a, order(start)), ]
  Q = c(0, 0)
  P <- c(0, 0)
  rewards = a$dooropened
  sides = ceiling(a$corner / 2)
  correct.prediction = vector()
  for (i in seq_along(sides)) {
    r = rewards[i]
    s = sides[i]
    correct.prediction[i] = NA
    if (is.finite(s) & is.finite(r)) {
      P = exp(par[2] * Q) / sum(exp(par[2] * Q))
      if(any(P < .001)){
        index = which(P < .001)
        P[index] = .001; P[-(index)] = .999}
      pe = r - Q[s]
      Q[s] = Q[s] + par[1] * pe
      delta = 0.5 - Q[s]
      Q[s] = Q[s] + par[3] * delta
      Q[-s] = Q[-s] + par[3] * delta
      predicted.choice <- ifelse(length(P) > 1 & all(!is.na(P)), which(P > .5), NA)
      correct.prediction[i] <- as.numeric(predicted.choice == s)}}
  correct.prediction}

modelname = "forgetful"
rprediction[[modelname]] <- wrap()

# decay -------------------------------------------------------------------
predict_ <- function(par, a) {
  a = a[with(a, order(start)), ]
  tempQ = c()
  Q = c(0, 0)
  date = rep(a$start[1], 2)
  t = c(0, 0)
  P <- vector()
  rewards = a$dooropened
  sides = ceiling(a$corner/2)
  nows = a$start
  correct.prediction = vector()
  for (i in seq_along(sides)) {
    r = rewards[i]
    s = sides[i]
    now = nows[i]
    t = as.numeric(difftime(now, date, units = 'mins'))
    date[s] = now
    correct.prediction[i] = NA
    if (is.finite(s) & is.finite(r)) {
      Q[s] = exp( -(t[s]) * par[3] ) * Q[s]
      tempQ = exp( -(t[-s]) * par[3] ) * Q[-s]
      P[s] = exp(par[2] * Q[s]) / 
        (sum(exp(par[2] * Q[s]), exp(par[2] * tempQ)))
      if(P[s] < .001){P[s] = .001}
      pe = r - Q[s]
      Q[s] = Q[s] + (par[1] * pe)
      predicted.choice <- ifelse(length(P) > 1 & all(!is.na(P)), which(P > .5), NA)
      correct.prediction[i] <- as.numeric(predicted.choice == s)}}
  correct.prediction}

modelname = "decay"
rprediction[[modelname]] <- wrap()


# decay split decay -------------------------------------------------------
predict_ <- function(par, a) {
  a = a[with(a, order(start)), ]
  tempQ = c()
  Q = c(0, 0)
  date = rep(a$start[1], 2)
  t = c(0, 0)
  P <- vector()
  rewards = a$dooropened
  sides = ceiling(a$corner/2)
  nows = a$start
  correct.prediction = vector()
  for (i in seq_along(sides)) {
    r = rewards[i]
    s = sides[i]
    now = nows[i]
    t = as.numeric(difftime(now, date, units = 'mins'))
    date[s] = now
    correct.prediction[i] = NA
    if (is.finite(s) & is.finite(r)) {
      if(r == 1){
        Q[s] = exp( -(t[s]) * par[3] ) * Q[s]  
        tempQ = exp( -(t[-s]) * par[5] ) * Q[-s]
      } else {
        Q[s] = exp( -(t[s]) * par[4] ) * Q[s]  
        tempQ = exp( -(t[-s]) * par[5] ) * Q[-s]}
      P[s] = exp(par[2] * Q[s]) / 
        (sum(exp(par[2] * Q[s]), exp(par[2] * tempQ)))
      if(P[s] < .001){P[s] = .001}
      pe = r - Q[s]
      Q[s] = Q[s] + (par[1] * pe)
      predicted.choice <- ifelse(length(P) > 1 & all(!is.na(P)), which(P > .5), NA)
      correct.prediction[i] <- as.numeric(predicted.choice == s)}}
  correct.prediction}

modelname = "decay+"
rprediction[[modelname]] <- wrap()

# reproval ----------------------------------------------------------------
predict_ <- function(par, a) {
  a = a[with(a, order(start)), ]
  Q = c(0, 0)
  P <- vector()
  b0 = 0
  rewards = a$dooropened
  sides = ceiling(a$corner / 2)
  previous = lag(sides)
  previous[1] = 1
  intervals = a$intervalb
  intervals[1] = 0
  correct.prediction = vector()
  for (i in seq_along(sides)) {
    r = rewards[i]
    s = sides[i]
    w = previous[i]
    t = intervals[i]
    correct.prediction[i] = NA
    if (is.finite(s) & is.finite(r)) {
      P = exp(par[2] * Q) / sum(exp(par[2] * Q))
      b0 = log(P[w] / (1 - P[w]))
      if(t > 660) t = 660
      P[w] = exp(b0 + par[3] * t) / 
        (1 + exp(b0 + par[3] * t))
      P[-w] = 1 - P[w]
      if(any(P < .001)){
        index = which(P < .001)
        P[index] = .001; P[-(index)] = .999}
      pe = r - Q[s] 
      Q[s] = Q[s] + (par[1] * pe)
      predicted.choice <- ifelse(length(P) > 1 & all(!is.na(P)), which(P > .5), NA)
      correct.prediction[i] <- as.numeric(predicted.choice == s)}}
  correct.prediction}

modelname = "reproval660"
rprediction[[modelname]] <- wrap()

# analyse -----------------------------------------------------------------
fig$prediction <- 
  rprediction %>%
  bind_rows() %>%
  group_by(tag, name) %>% 
  summarise(ratio = length(which((prediction == 1)))/n()) %>%
  ungroup() %>%
  left_join(manimal, by = "tag") %>%
  ggplot(aes(x = name, y = ratio, fill = substance)) +
  geom_boxplot()+
  geom_hline(yintercept = .5)+
  labs(y = "fraction of correct predictions")+
  facet_wrap(exp~substance)

fig$coursebasic <- rprediction$basic %>%
  left_join(manimal, by = "tag") %>%
  group_by(tag) %>%
  mutate(trial = row_number(), 
         cumsum = cumsum(tidyr::replace_na(prediction, 0)),
         maxcum = max(cumsum),
         ratio1 = cumsum/trial,
         ratio2 = cumsum/maxcum) %>%
  ungroup() %>%
  #filter(trial < 1000) %>%
  ggplot(aes(x = trial, y = ratio2, group = tag, colour = tag)) +
  geom_line()+
  #geom_smooth(method = "glm", formula = y ~ log(x))+
  theme_minimal()+
  theme(legend.position = "none")+
  labs(y = "fraction of correct predictions")+
  facet_wrap(~substance, ncol = 1)

predictiontidy <- 
  rprediction %>%
  bind_rows() %>%
  group_by(tag, name) %>% 
  summarise(ratio = length(which((prediction == 1)))/n()) %>%
  mutate(delta = ratio - ratio[name == "basic"]) %>% 
  merge(manimal, all.x = T) %>%
  as_tibble()
  
predictiontidy %>%
  ggplot(aes(x = name, y = delta, fill = substance)) +
  geom_boxplot()
