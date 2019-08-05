
# METADATA ----------------------------------------------------------------

dTheGreatestMouse <- dModel[dModel$Tag == hero,]

# AIC VALUES --------------------------------------------------------------

# get AIC values

rAll <- 
  Reduce(
    function(x, y) merge(x, y, all=TRUE), 
    list(
      select(mBasic, 
             tag, substance, `[1] basic` = AIC),
      select(mDual, 
             tag, substance, `[2] dual` = AIC),
      select(mFictitious, 
             tag, substance, `[3] fictitious` = AIC),
      select(mHybrid, 
             tag, substance, `[4] hybrid` = AIC),
      select(mAttention, 
             tag, substance, `[5] attention` = AIC),
      select(mTime, 
             tag, substance, `[7] time glm` = AIC),
      select(mTimeOptimised, 
             tag, substance, `[9] time free ` = AIC),
      select(mDecay, 
             tag, substance, `[6] decay` = AIC),
      select(mForgetful, 
             tag, substance, `[66] forgetful` = AIC),
      select(mRandomTimeOptimisedLine, 
             tag, substance, `[99] random + time optLine` = AIC),
      select(mZero, 
             tag, substance, `[999] random` = AIC),
      select(mNoiseWinStay, 
             tag, substance, `noisewinstay` = AIC)
    )) 


# DELTA AIC ---------------------------------------------------------------

rDelta <- rAll

rDelta[3:ncol(rDelta)] =  
  apply(
    rDelta[3:ncol(rDelta)],
    MARGIN = 1, 
    function(x){ x - x[1]}) %>%
  # to the reference model [1]
  t()

r.wAIC <- 
  apply(
    rAll[3:(ncol(rAll))], 
    MARGIN = 1, 
    function(x){x = x - min(x)
    exp(-0.5*x) / sum(exp(-0.5*x))
    #makeRanking(x, threshold = 2)
    }) %>%
  t() %>%
  as.data.frame() %>%
  within({
    tag = as.factor(rAll$tag)
    substance = as.factor(rAll$substance)
  }) %>%
  tidyr::gather('model','value', -tag, -substance)

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