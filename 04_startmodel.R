
# DEPENDENCIES ------------------------------------------------------------

require(dplyr)
require(tidyr)

#for plots:
require(ggplot2)
require(patchwork)
require(latex2exp)
require(scales)

# METADATA ----------------------------------------------------------------

#inconvenience
#   iAnimals - declared in 01_initialize.R/METADATA
names(iAnimals) <- tolower(names(iAnimals))

# CUSTOM ------------------------------------------------------------------

calculateAIC <- 
  function(n.parameters, NLL) {
    2 * NLL + 2 * n.parameters
  }

simple <- #(unique()?)
  function(a, na.omit = TRUE) {
    if (na.omit == TRUE) {
      a = na.omit(a)
    }
    if (all(a[1] == a)) {
      return(a[1])
    }
  }

#for #parameters > 1
optimalizeParameters <- 
  function(A, list.parameters) {
    optim.results <- list()
    sample.size = 
      length(list.parameters[[1]])
    for (i in seq_along(sample.size)) {
      #TODO ( (function(x) {...})(x) )
      optim.results[[i]] = 
        optim(
          par = 
            unlist(
              lapply(
                list.parameters,
                function(x) {x[i]})),
          fn = modelNLL,
          A = A,
          hessian = FALSE)
    }
    index.best <-
      which.min(
        lapply(
          optim.results, 
          function(x) {unlist(x$value)})
        )
    best.optim <- optim.results[[index.best]]
    c(tag = simple(A$Tag), unlist(best.optim))
  }

getModelMice <-
  function(list.parameters,
           A = dModel,
           animal.tags = NA) {
    if (is.na(animal.tags))
      animal.tags = as.character(
        levels(as.factor(A$Tag)))
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
        optimalizeParameters(d.mouse, list.parameters)
    }
    close(progressbar)
    data.frame(t(
      sapply(model, function(x) {unlist(x)}))
      )
  }

# for one parameter
# no function overloading in R... :/
# note: no loop for animals inside
optimalizeParameters_ <-
  function(A, list.parameters){
    out = list(
      tag = A$Tag[1], 
      par = 0, 
      value = Inf, 
      maxPar = NA)
    for(i in list.parameters){
      value = 
        modelNLL(
          par = i, A = A)
      # takes the function of general env
      out$maxPar = i
      if(is.finite(value) & 
         value < out$value){
        out$par = i
        out$value = value
      }
    }
    out
  }