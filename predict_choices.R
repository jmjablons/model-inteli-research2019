#correct prediction
# get the parameters
# simulate output for each choice
# verify with real outcome
# as a %

# variable ----------------------------------------------------------------
rprediction <- list()

# custom ------------------------------------------------------------------
wrap <- function(which.model = modelname, 
                 tag.list = manimal$tag, 
                 parameter.value = dmodel){
  lapply(tag.list, function(x){
      data.frame(
        prediction = predict_(
          par = rmodel[[which.model]] %>% 
            filter(name == which.model) %>%
            filter(tag == x) %>%
            .[grepl("par", names(.))] %>%
            as.numeric(), 
          a = filter(parameter.value, tag == x)),
        name = which.model,
        tag = x, stringsAsFactors = F)}) %>%
    bind_rows() %>% as_tibble()}

# basic -------------------------------------------------------------------
predict_ <- function(par, a) {
    a = a[with(a, order(start)), ]
      Q = c(0, 0)
      P <- c(0, 0)
      rewards = a$dooropened
      sides = ceiling(a$corner / 2)
      list.choice = list()
      correct.prediction = vector()
      for (i in seq_along(sides)) {
        r = rewards[i]
        s = sides[i]
        correct.prediction[i] = NA
        if (is.finite(s) & is.finite(r)) {
          P = exp(par[2] * Q) / sum(exp(par[2] * Q))
          if(any(P < .001)){
            index = which(P < .001)
            P[index] = .001; P[-(index)] = .999}
          pe = r - Q[s]
          Q[s] = Q[s] + (par[1] * pe)
          predicted.choice <- ifelse(length(P) > 1 & all(!is.na(P)), which(P > .5), NA)
          correct.prediction[i] <- as.numeric(predicted.choice == s)}}
    correct.prediction}

modelname = "basic"
rprediction[[modelname]] <- wrap()

# dual --------------------------------------------------------------------
predict_ <- function(par, a) {
    a = a[with(a, order(start)), ]
      Q = c(0, 0)
      P <- c(0, 0)
      rewards = a$dooropened
      sides = ceiling(a$corner / 2)
      list.choice = list()
      correct.prediction = vector()
      for (i in seq_along(sides)) {
        r = rewards[i]
        s = sides[i]
        correct.prediction[i] = NA
        if (is.finite(s) & is.finite(r)) {
          P = exp(par[2] * Q) / sum(exp(par[2] * Q))
          if(any(P < .001)){
            index = which(P < .001)
            P[index] = .001; P[-(index)] = .999}
          pe = r - Q[s]
          if (r == 1) {Q[s] = Q[s] + (par[1] * pe)
          } else {Q[s] = Q[s] + (par[3] * pe)}
          predicted.choice <- ifelse(length(P) > 1 & all(!is.na(P)), which(P > .5), NA)
          correct.prediction[i] <- as.numeric(predicted.choice == s)}}
    correct.prediction}

modelname = "dual"
rprediction[[modelname]] <- wrap()

# analyse -----------------------------------------------------------------
rprediction %>%
  bind_rows() %>%
  group_by(tag, name) %>% 
  summarise(ratio = length(which((prediction == 1)))/n()) %>%
  ungroup() %>%
  left_join(manimal, by = "tag") %>%
  ggplot(aes(x = name, y = ratio, fill = substance)) +
  geom_boxplot()

rprediction$dual %>%
  left_join(manimal, by = "tag") %>%
  group_by(tag) %>%
  mutate(trial = row_number(), 
         cumsum = cumsum(tidyr::replace_na(prediction, 0)),
         maxcum = max(cumsum),
         ratio1 = cumsum/trial,
         ratio2 = cumsum/maxcum) %>%
  ungroup() %>%
  #filter(trial < 1000) %>%
  ggplot(aes(x = trial, y = ratio2, group = tag, colour = tag)) +
  geom_line()+
  #geom_smooth(method = "glm", formula = y ~ log(x))+
  theme_minimal()+
  theme(legend.position = "none")+
  labs(y = "fraction of correct predictions")+
  facet_wrap(~substance, ncol = 1)
