## prediction ##
# basic
temp_basic <- (function(par, a) {
  a = a[with(a, order(start)), ]
  nll = 0
  Q = c(0, 0)
  P <- vector()
  rewards = a$dooropened
  sides = ceiling(a$corner / 2)
  nows = a$start
  probs2 = rep(NA, 79)
  probs = rep(NA, 79)
  for (i in seq_along(sides)) {
    r = rewards[i]
    s = sides[i]
    P = exp(par[2] * Q) / sum(exp(par[2] * Q))
    P = ifelse(P < .001, .001, P)
    P = ifelse(P > .999, .999, P)
    pe = r - Q[s]
    Q[s] = Q[s] + (par[1] * pe)
    probs2[i] <- P[2]
    probs[i] <- P[s]}
  tibble(choice = sides, 
         reward = rewards, 
         time = nows,
         prob = probs,
         prob2 = probs2)})(
           pubmodel[["basic"]] %>% filter(tag == hero) %>%  
             select(grep("par", colnames(.))) %>% unlist(), 
           dhero) %>%
  mutate(id = row_number())

# fictitious
temp_fictitious <- (function(par, a) {
  a = a[with(a, order(start)), ]
  nll = 0
  Q = c(0, 0)
  P <- vector()
  rewards = a$dooropened
  sides = ceiling(a$corner / 2)
  nows = a$start
  probs2 = rep(NA, 79)
  probs = rep(NA, 79)
  for (i in seq_along(sides)) {
    r = rewards[i]
    s = sides[i]
    P = exp(par[2] * Q) / sum(exp(par[2] * Q))
    P = ifelse(P < .001, .001, P)
    P = ifelse(P > .999, .999, P)
    nll = -log(P[s]) + nll
    pe = r - Q[s]
    Q[s] = Q[s] + (par[1] * pe)
    Q[-s] = Q[-s] - (par[1] * pe)
    probs2[i] <- P[2]
    probs[i] <- P[s]}
  tibble(choice = sides, 
         reward = rewards, 
         time = nows,
         prob = probs,
         prob2 = probs2)})(
           pubmodel[["fictitious"]] %>% filter(tag == hero) %>%  
             select(grep("par", colnames(.))) %>% unlist(), 
           dhero)%>%
  mutate(id = row_number())

p3 <- temp_basic %>% select(prob.basic = prob2, reward, 
                            choice, time, id) %>%
  left_join(temp_fictitious %>% select(prob.fictious = prob2, 
                                       choice, time, id)) %>%
  mutate(choice = choice -1) %>%
  ggplot(aes(x = time, y = choice))+
  geom_point(aes(size = as.factor(reward)), 
             colour = gg$point.colour, pch = 124)+
  geom_step(aes(y = prob.basic), 
            colour = "gray", direction = 'vh')+
  geom_step(aes(y = prob.fictious), 
            colour = "black", direction = 'vh')+
  geom_hline(yintercept = .5, color = "black", linetype = "dotted")+
  theme_publication+
  scale_y_continuous(limits = c(0, 1), breaks = c(0, .5, 1), 
                     expand = c(0,0))+
  scale_x_datetime(expand = c(0.01,0))+
  theme(axis.title.y.right = element_text(color = "black"),
        axis.line.y.right = element_line(color = "black"),
        axis.ticks.y.right = element_line(color = "black"),
        axis.text.y.right = element_text(color = "black"),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank())+
  labs(y = "probability of \nthe upper side choice")+
  scale_size_manual(values = c(3, 6))

fig[["sup3"]] <- (p1 | p2) / (p3) + plot_layout(heights = c(1,.75))+ 
  plot_annotation(tag_levels = "A")