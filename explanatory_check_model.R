remodel[[4]] %>%
  select(tag, par.alpha.pos = par.alpha.neg, name) %>%
  tidyr::spread(name, par.alpha.pos) %>%
  left_join(manimal) %>%
  ggplot(aes(x = cohort, y = hybrid, group = tag)) +
  geom_line(size = .1)+
  geom_jitter(width = .1, pch = 21, fill = "gray")+
  geom_hline(yintercept = 0, colour = "gray")+
  scale_y_continuous(limits = c(0, 1))+
  facet_wrap(~substance)+
  labs(y = "par.beta")

remodel[[4]] %>%
  select(tag, param = par.beta, name) %>%
  tidyr::spread(name, param) %>%
  left_join(manimal) %>%
  ggplot(aes(x = cohort, y = hybrid)) +
  box_default()+
  geom_jitter(width = .1, pch = 21, fill = "gray")+
  geom_hline(yintercept = 0, colour = "gray")+
  scale_y_continuous(limits = c(0, 10))+
  facet_wrap(~substance)+
  labs(y = "par.beta")

remodel[[12]] %>%
  select(tag, param = par.storage.neg, name) %>%
  left_join(manimal) %>%
  ggplot(aes(x = cohort, y = param)) +
  box_default()+
  geom_jitter(width = .1, pch = 21, fill = "gray")+
  geom_hline(yintercept = 0, colour = "gray")+
  scale_y_continuous(limits = c(0, 1))+
  facet_wrap(~substance)+
  labs(y = "par.storage", caption = "alk I - alk III p < 0.05")

(remodel[[12]] %>%
  select(tag, value = par.storage.neg) %>%
  left_join(manimal) %>%
  mutate(group = interaction(substance, cohort),
         group = as.factor(group)) %>%
  FSA::dunnTest(data = ., value ~ group))$res %>%
  filter(P.adj < 0.05)
  kruskal.test(data = ., value ~ substance)
  
  temp <- remodel[[12]] %>% 
    select(tag, value = par.storage.neg) %>%
    left_join(manimal) %>% 
    mutate(group = interaction(substance, cohort)) %>%
    group_by(substance) %>% #group
    summarise(val = median(value), min = quantile(value, .25), max = quantile(value, .75))
  
  plot(exp( -((1/60):60) ), xlim = c(0,60), ylim = c(0,1), type = "l")

  do.call(rbind, list(
    tibble(x = 0:120) %>%
      mutate(y = decay(x, temp[[1,2]]),
             upper = decay(x, temp[[1,4]]),
             lower = decay(x, temp[[1,3]]),
             substance = "alcohol"),
    tibble(x = 0:120) %>%
      mutate(y = decay(x, temp[[2,2]]),
             upper = decay(x, temp[[2,4]]),
             lower = decay(x, temp[[2,3]]),
             substance = "alcohol+saccharin"),
    tibble(x = 0:120) %>%
      mutate(y = decay(x, temp[[3,2]]),
             upper = decay(x, temp[[3,4]]),
             lower = decay(x, temp[[3,3]]),
             substance = "saccharin"),
    tibble(x = 0:120) %>%
      mutate(y = decay(x, temp[[4,2]]),
             upper = decay(x, temp[[4,4]]),
             lower = decay(x, temp[[4,3]]),
             substance = "water"))) %>%
    ggplot(aes(x = x, y = y, group = substance, colour = substance)) +
    geom_ribbon(aes(x = x, y = y, ymin = lower, ymax = upper, fill = substance), 
                colour = NA, alpha = .3)+
    geom_hline(yintercept = .5, linetype = "dotted")+
    geom_line(size = 1)+
    scale_x_continuous(limits = c(0, 120), breaks = c(0, 10, 20, 60, 120))+
    scale_y_continuous(limits = c(0,1), breaks = c(0, .5, 1))+
    theme_classic()+
    theme(legend.position = "bottom", legend.title = element_blank())+
    labs(y = "expected reward\n 0.25 - median - 0.75", 
         x = "interval [min]")
  
  
  temp_limit <- c(0,120)
  decay <- function(x, y) exp(-(x) * y)
  ggplot(data = data.frame(x = 0, y = 0), mapping = aes(x = x, y = y), xlim = temp_limit)+
    geom_hline(yintercept = .5, linetype = "dotted")+
    stat_function(fun = decay, args = list(y = temp[[1,2]]), colour = "lightblue", xlim = temp_limit)+
    stat_function(fun = decay, args = list(y = temp[[2,2]]), colour = "#CC0000", xlim = temp_limit)+
    stat_function(fun = decay, args = list(y = temp[[3,2]]), colour = "lightblue", xlim = temp_limit)+
    stat_function(fun = decay, args = list(y = temp[[4,2]]), colour = "purple", xlim = temp_limit)+
    stat_function(fun = decay, args = list(y = temp[[5,2]]), colour = "#CC0000", xlim = temp_limit)+
    stat_function(fun = decay, args = list(y = temp[[6,2]]), colour = "blue", xlim = temp_limit)+
    theme_publication+
    labs(x = "interval [min]", y = "median stored expected reward", 
         caption = "lightblue - alcohol,\nblue - water, \nred - saccharin, \npurple - saccharin + alkohol")
  