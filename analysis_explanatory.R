

# dependency --------------------------------------------------------------
library(boot)


# substance preference ----------------------------------------------------
getPreference <- function(n.hours = 96, period.info = "adaptation", a = dall){
  period <- a %>%
    filter(info == period.info) %>%
    group_by(exp, info) %>%
    arrange(start, .by_group = T) %>%
    summarise(finish = tail(start, 1),
              begin = finish - lubridate::hours(n.hours)) %>%
    ungroup()
  a %>%
    group_by(exp, info) %>%
    filter(start > period$begin[match(exp, period$exp)] & 
             end < period$finish[match(exp, period$exp)], 
           .preserve = TRUE) %>%
  group_by(tag) %>%
  summarise(
    nlickwater = sum(nlick[which(rp == 0)], na.rm = TRUE),
    nlickreward = sum(nlick[which(rp > 0)], na.rm = TRUE),
    value = nlickreward / (nlickreward + nlickwater))}

result$preference <- getPreference()

# better ratio ------------------------------------------------------------
result$br <- 
  dall %>%
  filter(label %in% c("0x0.3x0.9")) %>%
  group_by(tag) %>%
  summarise(n = n(),
            nbetter = length(which(rp > 0.5)),
            nworse = length(which(rp > 0.0 & rp < 0.5)),
            value = nbetter / (nbetter + nworse))

# stat --------------------------------------------------------------------
substances <- unique(temp$substance)

temp <- getPreference() %>%
  left_join(manimal) %>%
  mutate(substance = as.factor(substance))

temp <- result$br %>%
  left_join(manimal) %>%
  mutate(substance = as.factor(substance))

temp <- rmodel[["puzzlement*"]] %>%
  left_join(manimal) %>%
  mutate(substance = as.factor(substance),
         value = par.alpha)

## variance between groups
kruskal.test(data = temp, value ~ substance)$p.value %>%
  format(scientific = F, digits = 3)
FSA::dunnTest(data = temp, value ~ substance, method="bh")

## greater than random
for(i in seq_along(substances)){
  with(temp, {print(
    wilcox.test(value[substance == substances[i]], 
                mu = 0.5, alternative = 'greater')$p.value %>%
      format(scientific = F, digits = 3))})}

## size effect
{tsub = 2; tref = 1
with(temp, {effsize::cohen.d(
    lickpreference[substance == substances[tsub]], 
    lickpreference[substance == substances[tref]])})}

# glm win-stay behaviour --------------------------------------------------

getErrorRate <- function(real.data, predict.data) {
    real.data = as.vector(as.logical(real.data))
    predict.data = as.vector(predict.data > 0.5)
    1 - mean(predict.data == real.data, na.rm = T)}

modelStay <- function(mouse, a = dmodel, n.divisions = 10) {
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
           errorrate = getErrorRate(real.data, prediction.data),
           sig = ifelse(probability < 0.05, 1, 0))}

modelStay2 <- function(mouse, a = dmodel, k.fold = 10) {
  # k.fold = nrow(nodal) for LOOCV
  set.seed(123)
  dmouse <- a[a$tag == mouse,] %>% filter(!is.na(stay) & !is.na(intervala))
  dmouse <- mutate(dmouse, corner = ceiling(corner/2),
                   dooropened = dooropened)
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


result$glm2 <- lapply(manimal$tag, function(x) {modelStay2(x)}) %>% bind_rows()
