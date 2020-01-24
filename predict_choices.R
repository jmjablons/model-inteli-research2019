#correct prediction
# get the parameters
# simulate output for each choice
# verify with real outcome
# as a %

# variable ----------------------------------------------------------------
rprediction <- list()

# prediction --------------------------------------------------------------

predict <- 
  function(par, a) {
    a = a[with(a, order(start)), ]
      Q = c(0, 0)
      P <- c(0, 0)
      rewards = a$dooropened
      sides = ceiling(a$corner / 2)
      predicted.choice = vector()
      for (i in seq_along(sides)) {
        r = rewards[i]
        s = sides[i]
        predicted.choice[i] = NA
        if (is.finite(s) & is.finite(r)) {
          P = exp(par[2] * Q) / sum(exp(par[2] * Q))
          if(any(P < .001)){
            index = which(P < .001)
            P[index] = .001; P[-(index)] = .999}
          pe = r - Q[s]
          Q[s] = Q[s] + (par[1] * pe)
          predicted.choice[i] <- ifelse(length(P) > 1 & all(!is.na(P)), which(P > .5), NA)}}
    predicted.choice}

{which.model = "basic"
  rprediction[[which.model]] = 
    lapply(manimal$tag, function(x, which.model = which.model){
    print(x)
    data.frame(
      prediction = predict(
        par = rmodel[[which.model]] %>% 
          filter(name == which.model) %>%
          filter(tag == x) %>%
          .[grepl("par", names(.))] %>%
          as.numeric(), 
        a = filter(dmodel, tag == x)),
      name = which.model,
      tag = x, stringsAsFactors = F)
  })}

#x = manimal$tag[2]
#x = "900110000199391"
for(i in seq_along(manimal$tag)){
  x = manimal$tag[i]
  rprediction[[i]] <-
  data.frame(
    prediction = predict(
      par = rmodel[[which.model]] %>% 
        filter(name == which.model) %>%
        filter(tag == x) %>%
        .[grepl("par", names(.))] %>%
        as.numeric(), 
      a = filter(dmodel, tag == x)),
    name = which.model,
    tag = x, stringsAsFactors = F)}

# temp <- rmodel[[which.model]] %>% 
#   filter(name == which.model, tag == x) %>% 
#   .[grepl("par", names(.))] %>% as.numeric()
# 
# which(temp > .5)
