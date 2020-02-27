
getaic <- function(n.parameters, nll) {2 * nll + 2 * n.parameters}
listnth <- function(inp, n){sapply(inp, `[`, n)}

#for #parameters > 1
getoptimal <- function(input.data, list.parameters) {
    optim.results <- list()
    sample.size = length(list.parameters[[1]])
    val = Inf
    mouse = as.numeric(unique(input.data$tag))
    for (i in seq_along(sample.size)) {
      temp = optim(par = listnth(list.parameters, i), 
                   fn = model, a = input.data)
      if(temp$value < val){
        val = temp$value
        best.optim <- temp}}
    c(tag = mouse, unlist(best.optim))}

wrapmodel <- function(list.parameters, a = dmodel, animal.tags = NULL) {
    if(is.null(animal.tags)){animal.tags = as.character(unique(a$tag))}
    progressbar <- txtProgressBar(0, length(animal.tags), char = '*', style = 3)
    output = list()
    for(m in seq_along(animal.tags)) {
      setTxtProgressBar(progressbar, m)
      dmouse = a[a$tag == animal.tags[m], ]
      output[[m]] = getoptimal(dmouse, list.parameters)}
    close(progressbar)
    do.call(rbind, output)}

# for one parameter
# note: no loop for animals inside
getoptimal_ <-
  function(dataset, list.parameters){
    out = data.frame(tag = unique(dataset$tag), par = 0, value = Inf, maxPar = NA)
    for(i in list.parameters){
      value = model(par = i, a = dataset)
      out$maxPar = i
      if(value < out$value){
        out$par = i
        out$value = value}}
    out}
