# probability is always 0.5

mZero = 
  data.frame(tag = iAnimals$Tag)

mZero$NLL <- 
  apply(mZero, 1, function(x) {
  length(dModel$Corner[dModel$Tag == x[1]]) * -log(0.5)
})

mZero$AIC <- 
  apply(mZero, 1, function(x) {
    calculateAIC(
      n.parameters = 0, 
      NLL = as.numeric(x[2]))
  })

# optional
mZero$N <- 
  apply(mZero, 1, function(x) {
    length(dModel$Corner[dModel$Tag == x[1]])
  })

mZero = 
  merge(
    mZero, iAnimals %>% 
      ungroup() %>%
      mutate(tag = Tag, Tag = NULL), all.x = TRUE) %>% 
  as_tibble()
