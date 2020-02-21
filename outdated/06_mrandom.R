# Assume no RL

# RANDOM + GLM  -----------------------------------------------------------

#include the glm interval-realted estimate

modelNLL <- 
  function(A) {
  nll = 0
  sides = ceiling(A$Corner / 2)
  intervals = A$IntervalBefore
  intervals[1] = 0
  LOD = rGlm$Estimate[
    rGlm$Predictor == 'IntervalAfter' &
      rGlm$Tag == simple(A$Tag)]
  b0 = c(0, 0)
  for (i in seq_along(sides)) {
    s = sides[i]
    t = intervals[i]
    P = c(0.5, 0.5)
    P[1] = exp(LOD * t) / (1 + exp(LOD * t))
    P[2] = exp(LOD * t) / (1 + exp(LOD * t))
    nll = -log(P[s]) + nll
  }
  nll
  }

mRandomTime <-
  data.frame(tag = iAnimals$tag)

mRandomTime$value <- 
  apply(mRandomTime, 1, function(x) {
    modelNLL(dModel[dModel$Tag == x[1],])
  })

mRandomTime =
  merge(mRandomTime, iAnimals, all.x = TRUE)

mRandomTime$AIC <- 
  apply(mRandomTime, 1, function(x) {
    calculateAIC(n.parameters = 0, NLL = as.numeric(x[2]))
  })


# RANDOM + TIME COEFFICIENT -----------------------------------------------

# one parameter included

mRandomTimeOptimisedLine<-
  do.call(
    args = list(list.parameters = seq(-10, 10, 0.0001)), 
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
          d.mouse = 
            A[A$Tag == animal.tags[m], ]
          model[[m]] = 
            optimalizeParameters_(
              d.mouse, 
              list.parameters)
        }
        close(progressbar)
        data.frame(t(
          sapply(model, 
                 function(x) {unlist(x) }))
        )
      }
  )

mRandomTimeOptimisedLine$AIC <- 
  apply(
    mRandomTimeOptimisedLine, 1, function(x) {
      calculateAIC(
        n.parameters = 1,
        NLL = as.numeric(x[3]))
    })

mRandomTimeOptimisedLine = 
  merge(
    mRandomTimeOptimisedLine, 
    iAnimals, 
    all.x = TRUE) %>% 
  as_tibble()