outlier <- c("900110000199391", "900110000199541") #check glm
result <- list()

# glm win-stay behaviour --------------------------------------------------
## check for contrasts
# lapply(X = unique(dmodel$tag), function(a){dmodel[dmodel$tag == a,] %>%
#     group_by(tag, corner) %>% summarise(N = n())}) %>% bind_rows() %>%
#   tidyr::spread(corner, N) %>% View()
## gives:
# not enough trials for each corner:

get_errorrate <- function(real.data, predict.data) {
  real.data = as.vector(as.logical(real.data))
  predict.data = as.vector(predict.data > 0.5)
  1 - mean(predict.data == real.data, na.rm = T)}

model_stay <- function(mouse, a = dmodel, n.divisions = 10) {
  set.seed(123)
  dmouse <- a[a$tag == mouse,] %>% filter(!is.na(stay) & !is.na(intervala))
  dmouse <- mutate(dmouse, corner = as.factor(ceiling(corner/2)),
                   dooropened = as.factor(dooropened))
  dlenght <- seq_along(dmouse$stay)
  itest <- sample(max(dlenght), floor(max(dlenght) / n.divisions), FALSE)
  itrain <- setdiff(dlenght, sort(itest))
  result.glm <- glm(stay ~ dooropened + intervala + corner, 
                    data = dmouse[train,], family = binomial)
  prediction.data <- predict(result.glm, dmouse[itest,], type = "response")
  real.data <- dmouse$stay[itest]
  result.glm %>%
    summary() %>%
    .$coefficients %>%
    transform(oddsratio = exp(Estimate)) %>%
    rename(estimate = Estimate,
           stderror = Std..Error,
           probability = Pr...z..) %>%
    mutate(tag = mouse, 
           predictor = rownames(.),
           errorrate = get_errorrate(real.data, prediction.data),
           sig = ifelse(probability < 0.05, 1, 0))}

model_stay2 <- function(mouse, a = dmodel, k.fold = 10) {
  # k.fold = nrow(nodal) for LOOCV
  set.seed(123)
  dmouse <- a[a$tag == mouse,] %>% filter(!is.na(stay) & !is.na(intervala))
  dmouse <- mutate(dmouse, stay = as.factor(stay),
                   corner = as.factor(ceiling(corner/2)),
                   dooropened = as.factor(dooropened))
  dmouse = dmouse %>% select(stay, corner, dooropened, intervala)
  result.glm <- glm(stay ~ dooropened + intervala + corner, 
                    data = dmouse, family = binomial)
  errorcv <- boot::cv.glm(data = dmouse, glmfit = result.glm,
                          cost = function(r, pi = 0) {mean(abs(r-pi) > 0.5)},
                          K = k.fold)$delta
  result.glm %>%
    summary() %>%
    .$coefficients %>%
    transform(oddsratio = exp(Estimate)) %>%
    rename(estimate = Estimate,
           stderror = Std..Error,
           probability = Pr...z..) %>%
    mutate(tag = mouse, 
           predictor = rownames(.),
           deltafold = errorcv[1],
           deltafoldadj = errorcv[2],
           sig = ifelse(probability < 0.05, 1, 0))}

result$glm <- lapply(
  setdiff(unique(dmodel$tag), outlier), 
  #unique(dmodel$tag),
  function(x) {model_stay2(x)}) %>% bind_rows()

# explore -----------------------------------------------------------------
result$glm %>%
  left_join(manimal, by="tag") %>%
  filter(substance %in% substances[-4]) %>%
  filter(predictor %in% "intervala") %>%
  summarise(plus = length(which(sig > 0)),
            total = n())

result$glm %>%
  select(tag, predictor, sig) %>%
  tidyr::spread(predictor, sig) %>% 
  left_join(manimal %>% select(tag, substance)) %>% 
  mutate(measure = ifelse(dooropened1 > 0 & intervala >0, 1, 0)) %>%
  group_by(substance)%>%
  summarise(cross_rew_interval = sum(measure, na.rm = T),
            intercept = sum(`(Intercept)`),
            reward = sum(dooropened1),
            interval = sum(intervala),
            corner = sum(corner2),
            total = n())
