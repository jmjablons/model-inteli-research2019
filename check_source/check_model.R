# general -----------------------------------------------------------------
getoptimal2 <- function(input.data, list.parameters) {
  optim.results <- list()
  sample.size = nrow(list.parameters)
  npar = ncol(list.parameters)
  val = Inf
  mouse = as.numeric(unique(input.data$tag))
  for (i in 1:sample.size) {
    temp = optim(par = unlist(`[`(list.parameters, i, 1:npar)),
                 fn = model, a = input.data)
    if(temp$value < val){
      val = temp$value
      best.optim <- temp}}
  c(tag = mouse, unlist(best.optim))}

#temp <- microbenchmark::microbenchmark(getoptimal1, getoptimal2)
#microbenchmark::autoplot.microbenchmark(temp)

# all.equal(remodel[["basic"]] %>% select(-name), 
#           remodel[["basic2"]] %>% select(-name))

# many start points -------------------------------------------------------
repmodel <- readRDS(file.choose()) %>%
  append(readRDS(file.choose()))

pubmodel[["fictitious"]] %>% select(tag, one_shot = value) %>%
  left_join(repmodel[["fictitious"]] %>% select(tag, points = value)) %>%
  mutate(difference = one_shot - points) %>% View()

pubmodel[["basic"]] %>% select(tag, one_shot = value) %>%
  left_join(pubmodel[["dual"]] %>% select(tag, points = value)) %>%
  mutate(difference = one_shot - points) %>% 
  select(tag, pub_dif = difference) %>%
  left_join(
repmodel[["basic"]] %>% select(tag, one_shot = value) %>%
  left_join(repmodel[["dual"]] %>% select(tag, points = value)) %>%
  mutate(difference = one_shot - points) %>% 
  select(tag, rep_dif = difference)) %>% 
  tidyr::gather(measure, nll_dif, -tag) %>%
  ggplot(aes(x = measure, y = nll_dif, group = tag)) +
    geom_line(alpha = .1)+    
  geom_point(pch = 21, fill = "gray")+
  theme_publication

util$plotpar("hybrid", "par.alpha.pos", c(0,1), c(0, .5, 1), a = repmodel)  +
util$plotpar("hybrid", "par.alpha.neg", c(0,1), c(0, .5, 1), a = repmodel)   

repmodel[["hybrid"]] %>% 
  select(tag, one_shot = par.alpha.pos) %>%
  left_join(repmodel[["dual"]] %>% select(tag, points = par.alpha.pos)) %>%
  #mutate(difference = one_shot - points) %>% 
  #select(tag, rep_dif = difference) %>% 
  tidyr::gather(measure, nll_dif, -tag) %>%
  ggplot(aes(x = measure, y = nll_dif, group = tag)) +
  geom_line(alpha = .1)+    
  geom_point(pch = 21, fill = "gray")+
  theme_publication

repmodel[["dual"]] %>% 
  select(tag, par.alpha.pos, par.alpha.neg) %>%
  tidyr::gather(measure, nll_dif, -tag) %>%
  left_join(manimal) %>%
  ggplot(aes(x = measure, y = nll_dif, group = tag)) +
  geom_line(alpha = .5, size = .1)+    
  #geom_point(pch = 21, fill = "gray")+
  facet_wrap(~ substance, ncol = 1)+
  theme_publication+
  scale_x_discrete(expand = c(0,0.1))

p1 <- repmodel[["dual"]] %>% 
  select(tag, par.alpha.pos, par.alpha.neg) %>%
  tidyr::gather(measure, dif, -tag) %>%
  left_join(manimal) %>%
  filter(substance %in% "alcohol") %>%
  ggplot(aes(x = measure, y = dif, group = tag)) +
  geom_line(alpha = .5, size = .1)+    
  #geom_point(pch = 21, fill = "gray")+
  facet_wrap(~ substance, ncol = 1)+
  theme_publication+
  scale_x_discrete(expand = c(0,0.1))+
  scale_y_continuous(limits = c(0,0.1))

p2 <- repmodel[["dual"]] %>% 
  select(tag, par.alpha.pos, par.alpha.neg) %>%
  tidyr::gather(measure, dif, -tag) %>%
  left_join(manimal) %>%
  filter(substance %in% "saccharin") %>%
  ggplot(aes(x = measure, y = dif, group = tag)) +
  geom_line(alpha = .5, size = .1)+    
  #geom_point(pch = 21, fill = "gray")+
  facet_wrap(~ substance, ncol = 1)+
  theme_publication+
  scale_x_discrete(expand = c(0,0.1))+
  scale_y_continuous(limits = c(0,0.1))

repmodel[["dual"]] %>%
  left_join(manimal) %>%
  filter(substance %in% "alcohol") %>%
  summarise(mean(par.alpha.pos), mean(par.alpha.neg))

temp <- repmodel[["dual"]] %>% 
  select(tag, par.alpha.pos, par.alpha.neg) %>%
  tidyr::gather(measure, dif, -tag) %>%
  left_join(manimal) %>%
  filter(substance %in% "alcohol") %>%
  select(value = dif, exp = measure, tag) %>%
  mutate(exp = as.factor(exp))

with(temp, {wilcox.test(value ~ exp, conf.int = T)})

p1 + p2

# check split calculation -------------------------------------------------
modelcheck <- list()

model <- function(par, a) {
  a = a[with(a, order(start)), ]
  nll = 0
  if (par[1] < 0 | par[1] > 1 |
      par[2] < 0 | par[2] > 50 | 
      par[3] < 0 | par[3] > 1 |
      par[4] < 0 | par[4] > 1) {
    nll = Inf
  } else {
    Q = c(0, 0)
    t = c(0, 0)
    P <- vector()
    rewards = a$dooropened
    sides = ceiling(a$corner/2)
    nows.start = a$start
    nows.end = a$end
    intervals = a$intervalb
    intervals[1] = 0
    rew = c(0,0)
    for (i in seq_along(sides)) {
      r = rewards[i]
      s = sides[i]
      now.start = nows.start[i]
      now.end = nows.end[i]
      t = intervals[i]
      t = ifelse(t > 660, 660, t)
      decay = ifelse(rew == 1, par[3], par[4])
      rew[s] = r
      Q = exp( -(t) * decay ) * Q
      P = exp(par[2] * Q) / sum(exp(par[2] * Q))
      if(P[s] < .001){P[s] = .001}
      if(P[s] > .999){P[s] = .999}
      nll = -log(P[s]) + nll
      pe = r - Q[s]
      Q[s] = Q[s] + (par[1] * pe)}}
  nll}

modelcheck[["q-decay+"]] <- util_wrap("q-decay+",
                                      alpha = init$default,
                                      beta = init$beta,
                                      storage.pos = init$primitive,
                                      storage.neg = init$primitive)

model <- function(par, a) {
  a = a[with(a, order(start)), ]
  nll = 0
  if (par[1] < 0 | par[1] > 1 |
      par[2] < 0 | par[2] > 50 | 
      par[3] < 0 | par[3] > 1) {
    nll = Inf
  } else {
    par[4] = par[3]
    Q = c(0, 0)
    t = c(0, 0)
    P <- vector()
    rewards = a$dooropened
    sides = ceiling(a$corner/2)
    nows.start = a$start
    nows.end = a$end
    intervals = a$intervalb
    intervals[1] = 0
    rew = c(0,0)
    for (i in seq_along(sides)) {
      r = rewards[i]
      s = sides[i]
      now.start = nows.start[i]
      now.end = nows.end[i]
      t = intervals[i]
      t = ifelse(t > 660, 660, t)
      decay = ifelse(rew == 1, par[3], par[4])
      rew[s] = r
      Q = exp( -(t) * decay ) * Q
      P = exp(par[2] * Q) / sum(exp(par[2] * Q))
      if(P[s] < .001){P[s] = .001}
      if(P[s] > .999){P[s] = .999}
      nll = -log(P[s]) + nll
      pe = r - Q[s]
      Q[s] = Q[s] + (par[1] * pe)}}
  nll}

modelcheck[["q-decay+test"]] <- util_wrap("q-decay+",
                                          alpha = init$default,
                                          beta = init$beta,
                                          storage = init$primitive)

model <- function(par, a) {
  a = a[with(a, order(start)), ]
  nll = 0
  if (par[1] < 0 | par[1] > 1 |
      par[2] < 0 | par[2] > 50 | 
      par[3] < 0 | par[3] > 1) {
    nll = Inf
  } else {
    Q = c(0, 0)
    t = c(0, 0)
    P <- vector()
    rewards = a$dooropened
    sides = ceiling(a$corner/2)
    nows.start = a$start
    nows.end = a$end
    intervals = a$intervalb
    intervals[1] = 0
    for (i in seq_along(sides)) {
      r = rewards[i]
      s = sides[i]
      now.start = nows.start[i]
      now.end = nows.end[i]
      t = intervals[i]
      t = ifelse(t > 660, 660, t)
      Q = exp( -(t) * par[3] ) * Q
      P = exp(par[2] * Q) / sum(exp(par[2] * Q))
      if(P[s] < .001){P[s] = .001}
      if(P[s] > .999){P[s] = .999}
      nll = -log(P[s]) + nll
      pe = r - Q[s]
      Q[s] = Q[s] + (par[1] * pe)}}
  nll}

modelcheck[["q-decay"]] <- util_wrap("q-decay",
                                     alpha = init$default,
                                     beta = init$beta,
                                     storage = init$primitive)

model <- function(par, a) {
  a = a[with(a, order(start)), ]
  nll = 0
  if (par[1] < 0 | par[1] > 1 |
      par[2] < 0 | par[2] > 50 | 
      par[3] < 0 | par[3] > 1) {
    nll = Inf
  } else {
    Q = c(0, 0)
    t = c(0, 0)
    P <- vector()
    rewards = a$dooropened
    sides = ceiling(a$corner/2)
    nows.start = a$start
    nows.end = a$end
    intervals = a$intervalb
    intervals[1] = 0
    rew = c(0,0)
    for (i in seq_along(sides)) {
      r = rewards[i]
      s = sides[i]
      now.start = nows.start[i]
      now.end = nows.end[i]
      t = intervals[i]
      t = ifelse(t > 660, 660, t)
      decay = ifelse(rew == 1, par[3], par[3])
      rew[s] = r
      Q = exp( -(t) * decay ) * Q
      P = exp(par[2] * Q) / sum(exp(par[2] * Q))
      if(P[s] < .001){P[s] = .001}
      if(P[s] > .999){P[s] = .999}
      nll = -log(P[s]) + nll
      pe = r - Q[s]
      Q[s] = Q[s] + (par[1] * pe)}}
  nll}

modelcheck[["q-decay+test2"]] <- util_wrap("q-decay+",
                                           alpha = init$default,
                                           beta = init$beta,
                                           storage = init$primitive)

# final -------------------------------------------------------------------
all.equal(
  modelcheck[["q-decay+test2"]] %>% select(-name), 
  modelcheck[["q-decay"]]  %>% select(-name))

identical(modelcheck[["q-decay"]] %>% select(-name), 
          modelcheck[["q-decay+test"]]  %>% select(-name))

anti_join(modelcheck[["q-decay+test"]] %>% select(-name), 
          modelcheck[["q-decay+test2"]]  %>% select(-name))