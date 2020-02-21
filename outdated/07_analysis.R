
# METADATA ----------------------------------------------------------------
# hero = "900110000199546"
dTheGreatestMouse <- dModel[dModel$Tag == hero,]

# AIC VALUES --------------------------------------------------------------

# get AIC values

rAll <- 
  Reduce(
    function(x, y) merge(x, y, all=TRUE), 
    list(
      select(mBasic, 
             tag, Substance = substance, `[1] basic` = AIC),
      # select(mDual,
      #        tag, substance, `[2] dual` = AIC),
      # select(mFictitious, 
      #        tag, substance, `[3] fictitious` = AIC),
      # select(mHybrid, 
      #        tag, substance, `[4] hybrid` = AIC),
      # select(mAttention, 
      #        tag, substance, `[5] attention` = AIC),
      # select(mTime, 
      #        tag, Substance, `[7] time glm` = AIC),
      # select(mTimeOptimised, 
      #        tag, substance, `[9] time free ` = AIC),
      select(mDecay,
             tag, Substance, `decay` = AIC),
      select(mReproval, 
             tag = Tag, Substance, `[7] reproval` = AIC),
      # select(mForgetful, 
      #        tag, substance, `[77] forgetful` = AIC),
      # select(mRandomTimeOptimisedLine, 
      #        tag, substance, `[99] random + time optLine` = AIC),
      select(mZero, 
             tag, Substance, `[999] random` = AIC)
      # select(mNoiseWinStay, 
      #        tag, substance, `noisewinstay` = AIC)
    )) 

newAll <-
  select(mBasic, 
         tag, Substance = substance, `basic` = AIC)%>%
  mutate(tag = as.character(tag)) %>%
  merge(
    select(mReproval, 
           tag, `reproval` = AIC)) %>%
  merge(
    select(mZero, 
           tag, `random` = AIC)) %>%
  merge(
  select(mDecay,
         tag, `decay` = AIC))

# DELTA AIC ---------------------------------------------------------------

rDelta <- newAll

rDelta[3:ncol(rDelta)] =  
  apply(
    rDelta[3:ncol(rDelta)],
    MARGIN = 1, 
    function(x){x - x[1]}) %>%
  # to the reference model [1]
  t()

r.wAIC <- 
  apply(
    newAll[3:(ncol(newAll))], 
    MARGIN = 1, 
    function(x){x = x - min(x)
    exp(-0.5*x) / sum(exp(-0.5*x))
    #makeRanking(x, threshold = 2)
    }) %>%
  t() %>%
  as.data.frame() %>%
  within({
    tag = as.factor(newAll$tag)
    substance = as.factor(newAll$substance)
  }) %>%
  tidyr::gather('model','value', -tag, -substance)


# plot --------------------------------------------------------------------

rDelta %>%
  mutate(substance = Substance, Substance = NULL) %>%
  tidyr::gather(
    'model', 'value',-tag,-substance) %>%
  ggplot(
    aes(x = model, y = value)) +
  geom_hline(yintercept = 0)+
  geom_boxplot(
    outlier.colour = NA,
    fill = NA,
    size = 0.4) +
  geom_quasirandom(
    width = 0.2,
    #method = "tukeyDense",
    method = "quasirandom",
    varwidth = TRUE,
    colour = 'black',
    size = 1
  ) +
  theme_publication +
  # scale_y_log10(
  #   breaks = 
  #     scales::trans_breaks(
  #       "log10", 
  #       function(x) 10 ^ x),
  #   labels = 
  #     scales::trans_format(
  #       "log10", 
  #       scales::math_format(10 ^ .x)),
  #   limits = c(10 ^ 0, 10 ^ 4),
  #   expand = c(0, 0)
  # ) +
  labs(y = "dAIC", x = element_blank()) +
  theme(
    panel.spacing = unit(2, "lines"),
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.ticks.y = element_line(linetype = 'dashed'),
    axis.line.x = element_blank()
  ) +
  facet_wrap( ~ substance, ncol = 2)

# OPTIMISATION ------------------------------------------------------------

temp <-
  expand.grid(
    alpha = seq(0.01, 0.99, by = 0.01),
    beta = seq(0.1, 49.9, by = 0.1))

oBasic <-temp
oBasic$nll <-
  apply(temp, 1, function(x) {
    x[3] = 
      modelNLL(
        A = dTheGreatestMouse, 
        par = c(x[1], x[2]))
  })

oBasic$min = F; oBasic$min[oBasic$nll == 
                             min(oBasic$nll)] = T

oTime <- temp
oTime$nll <-
  apply(temp, 1, function(x) {
    x[3] = modelNLL(A = dTheGreatestMouse, 
                    par = c(x[1], x[2]))
  })

oTime$min = F; oTime$min[oTime$nll == 
                           min(oTime$nll[is.finite(oTime$nll)])] = T

# PARAMETERS --------------------------------------------------------------

rParameters <- list()

iModels <- 
  list(
    basic = mBasic, 
    dual = mDual,
    fictitious = mFictitious,
    hybrid = mHybrid,
    attention = mAttention,
    `time constrained` = mTime,
    `time free` = mTimeOptimised,
    decay = mDecay,
    forgetful = mForgetful,
    `random + time opt` = mRandomTimeOptimisedLine
    #random = mZero
  )

for(i in seq_along(iModels)){
  temp <- names(iModels)[i]
  rParameters[[i]] <- 
    select(
      iModels[[i]], 
      tag, substance, names(iModels[[i]]) %>% 
        grep(pattern = "par")) %>%
    mutate(model = temp) %>%
    tidyr::gather(
      "parameter","value", -tag, -substance, -model) 
}

rParameters =
  bind_rows(rParameters)