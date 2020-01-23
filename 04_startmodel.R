
calculateAIC <- function(n.parameters, NLL) {2 * NLL + 2 * n.parameters}

#for #parameters > 1
optimalizeParameters <- 
  function(A, list.parameters) {
    optim.results <- list()
    sample.size = length(list.parameters[[1]])
    for (i in seq_along(sample.size)) {
      optim.results[[i]] = 
        optim(par = unlist(lapply(list.parameters, function(x) {x[i]})),
          fn = model,
          a = A,
          hessian = FALSE)}
    index.best <- which.min(lapply(optim.results, function(x) {unlist(x$value)}))
    best.optim <- optim.results[[index.best]]
    c(tag = unique(A$tag), unlist(best.optim))}

getModelMice <-
  function(list.parameters,
           A = dmodel,
           animal.tags = NA) {
    if(is.na(animal.tags)){
      animal.tags = as.character(unique(A$tag))}
    progressbar <-
      txtProgressBar(
        min = 0,
        max = length(animal.tags),
        char = '*',
        style = 3)
    output = list()
    for(m in seq_along(animal.tags)) {
      setTxtProgressBar(progressbar, m)
      dmouse = A[A$tag == animal.tags[m], ]
      output[[m]] = optimalizeParameters(dmouse, list.parameters)}
    close(progressbar)
    data.frame(t(sapply(output, function(x) {unlist(x)})), stringsAsFactors = F)}

# for one parameter
# note: no loop for animals inside
optimalizeParameters_ <-
  function(A, list.parameters){
    out = list(
      tag = A$tag[1], 
      par = 0, 
      value = Inf, 
      maxPar = NA)
    for(i in list.parameters){
      value = model(par = i, A = A)
      # takes the function of general env
      out$maxPar = i
      if(is.finite(value) & 
         value < out$value){
        out$par = i
        out$value = value}}
    out}
