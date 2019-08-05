# model proposed by reviewer
#   reference in publication

# order just in case
dModel <- 
  dModel %>%
  arrange(Tag, StartDateTime)

modelNLL <- 
  # [!] object names different from other models
  function(par, A) {
  A = A[with(A, order(StartDateTime)), ]
  nll = 0
  if (par[1] < 0 | par[1] > 2) {
    nll = Inf
  } else {
    P <- vector()
    rewards = A$RewardDoor
    sides = ceiling(A$Corner / 2)
    stays = A$Stay
    for (i in seq_along(sides)) {
      r = rewards[i]
      c = sides[i]
      s = stays[i]
      if (is.finite(s) & 
          is.finite(r) & 
          is.finite(c)) {
        if(
          (s == 1 & r == 1) | 
          (s == 0 & r == 0)
        ){
          P[c] = 1 - par[1]/2
        } 
        else if(
          (s == 0 & r == 1) | 
          (s == 1 & r == 0)
        ){
          P[c] = par[1]/2
        }
        nll = -log(P[c]) + nll
      } 
    }
  }
  nll
  }

mNoiseWinStay<-
  do.call(
    args = list(
      list.parameters = seq(0, 2, 0.0001)), 
    what = 
      function(list.parameters,
               A = d.model,
               animal.tags = NA) {
        if (is.na(animal.tags))
          animal.tags = 
            as.numeric(levels(as.factor(A$Tag)))
        progressbar <-
          txtProgressBar(
            min = 0,
            max = length(animal.tags),
            char = '*',
            style = 3
          )
        model = list()
        for (m in seq_along(animal.tags)) {
          setTxtProgressBar(progressbar, m)
          d.mouse = A[A$Tag == animal.tags[m], ]
          model[[m]] = 
            optimalizeParameters_(
              d.mouse, 
              list.parameters)
        }
        close(progressbar)
        data.frame(t(
          sapply(model, function(x) {unlist(x)})))
      }
  )

mNoiseWinStay$AIC <- 
  apply(mNoiseWinStay, 1, function(x) {
    calculateAIC(n.parameters = 1,
                 NLL = as.numeric(x[3]))
  })

mNoiseWinStay = 
  merge(mNoiseWinStay, iAnimals, all.x = TRUE) %>% 
  as_tibble()


# RESPONSE TO REVIEWER ----------------------------------------------------

# statistic ---------------------------------------------------------------

do.call(
  what = 
    function(x){
      list(
        kruskal.test(
          value~substance, data = x),
        FSA::dunnTest(
          value~substance, data = x, method ="bh")
      )
    }, 
  args = 
    list(
      mNoiseWinStay %>%
        select(
          tag, par, substance) %>%
        tidyr::gather(
          parameter, value, -tag, - substance) %>%
        mutate(
          tag = as.factor(tag),
          substance = as.factor(substance))
    )
)

# parameter vs nll --------------------------------------------------------

rNllDueEpsilon <- 
  data.frame(par = seq(0,2,0.001))

# hero <- "900110000199546"
# as before
rNllDueEpsilon$nll <-
  apply(rNllDueEpsilon, 1, function(x){
    modelNLL(
      x[1], 
      A = d.model[d.model$Tag == hero,] %>% 
        arrange(StartDateTime))
  })

rNllDueEpsilon %>%
  #filter(par > 0.001, par < 1.99) %>%
  ggplot(aes(x = par, y = nll)) +
  geom_line()+
  theme_minimal()

# plots -------------------------------------------------------------------

mNoiseWinStay %>%
  select(tag, par, substance) %>%
  tidyr::gather(
    parameter, value, -tag, - substance) %>%
  ggplot(aes(x = substance, y = value)) +
  scale_y_continuous(
    limits = c(0, 2), 
    breaks = c(0, 0.5, 1, 2), 
    expand = c(0,0))+
  facet_wrap(~ parameter) +
  geom_boxplot(
    outlier.colour = NA, 
    colour = "grey") +
  geom_quasirandom(
    width = 0.2,
    method = "quasirandom",
    varwidth = TRUE,
    size = 0.4) +
  theme_publication +
  theme(
    panel.spacing = unit(2, "lines"),
    axis.title = element_blank(),
    axis.title.x = element_blank(),
    axis.text.x = element_text(
      angle = 45,
      hjust = 1),
    axis.ticks.x = element_blank(),
    axis.line.x = element_blank()) +
  
  rNllDueEpsilon %>%
  mutate(value = "nll") %>%
  ggplot(aes(x = par, y = nll)) +
  geom_line()+
  scale_y_continuous(
    limits = c(0, 8000),
    expand = c(0,0))+
  scale_x_continuous(
    limits = c(0, 2),
    expand = c(0,0))+
  theme_publication +
  labs(x = TeX('$\\epsilon$'))+
  theme(
    panel.spacing = unit(2, "lines"),
    axis.title = element_blank()
  )
