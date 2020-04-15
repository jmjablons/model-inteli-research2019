result <- list()

# dependency --------------------------------------------------------------
library(boot)

# summary activity --------------------------------------------------------
dmodel %>%
  group_by(tag) %>%
  summarise(value = n()) %>%
  left_join(manimal) %>%
  group_by(substance) %>%
  summarise(median = median(value, na.rm = T),
            iqr25 = quantile(value,na.rm = T)[[2]],
            iqr75 = quantile(value, na.rm = T)[[4]],
            min = min(value, na.rm = T),
            max = max(value, na.rm = T))

dall %>%
  filter(info %in% "reversal") %>%
  filter(as.numeric(visitduration) > 2) %>%
  group_by(tag) %>%
  summarise(value = n()) %>%
  left_join(manimal) %>%
  group_by(substance) %>%
  summarise(median = median(value, na.rm = T),
            iqr25 = quantile(value,na.rm = T)[[2]],
            iqr75 = quantile(value, na.rm = T)[[4]],
            min = min(value, na.rm = T),
            max = max(value, na.rm = T))

# substance preference ----------------------------------------------------
result$preference <- util$getpreference()

result$preference %>%
  left_join(manimal, by = "tag") %>%
  group_by(substance) %>%
  summarise(median = median(value, na.rm = T),
            iqr25 = quantile(value, na.rm = T)[[2]],
            iqr75 = quantile(value, na.rm = T)[[4]])

result$preference %>%
  left_join(select(manimal, tag, substance), by = "tag") %>%
  group_by(substance) %>%
  summarise(n = length(which(value < 0.5)),
            total = n()) %>% View()

# better ratio ------------------------------------------------------------
result$br <- dall %>%
  filter(info %in% "reversal") %>%
  filter(label %in% c("0x0.3x0.9")) %>%
  group_by(tag) %>%
  summarise(n = n(),
            nbetter = length(which(rp > 0.5)),
            nworse = length(which(rp > 0.0 & rp < 0.5)),
            value = nbetter / (nbetter + nworse))

result$br%>%
  left_join(manimal, by = "tag") %>%
  group_by(substance) %>%
  summarise(median = median(value, na.rm = T),
            iqr25 = quantile(value, na.rm = T)[[2]],
            iqr75 = quantile(value, na.rm = T)[[4]])

# stat --------------------------------------------------------------------
substances <- unique(manimal$substance)

#choices
temp <- dmodel %>%
  group_by(tag) %>%
  summarise(value = n()) %>%
  left_join(manimal) %>%
  mutate(substance = as.factor(substance))

temp <- dall %>%
  filter(info %in% "reversal") %>%
  filter(as.numeric(visitduration) > 2) %>%
  filter(rp > 0) %>%
  group_by(tag) %>%
  summarise(value = n()) %>%
  left_join(manimal) %>%
  mutate(substance = as.factor(substance))

# reward preference
temp <- util$getpreference() %>%
  left_join(manimal) %>%
  mutate(substance = as.factor(substance))

# better ratio
temp <- result$br %>%
  left_join(manimal) %>%
  mutate(substance = as.factor(substance))

# win-stay
wrap_winstay()

# model parameters
temp <- util$get("basic","par.beta", "saccharin")

## variance between groups
kruskal.test(data = temp, value ~ substance)
#$p.value %>% format(scientific = F, digits = 3)
FSA::dunnTest(data = temp, value ~ substance, method="bh")

## greater than random
for(i in seq_along(substances)){
  with(temp, {print(
    wilcox.test(value[substance == substances[i]], 
                mu = .5, alternative = 'greater')
    #$p.value %>%
    #  format(scientific = F, digits = 3)
      )})}

## between two groups
with(wrap_winstay("water", "lose-shift"), {
  wilcox.test(`[<2]`, `[>10]`, paired = T, conf.int = T)})

with(temp, {wilcox.test(value ~ exp, conf.int = T)})

## size effect
{tsub = 2; tref = 1
with(temp, {effsize::cohen.d(
    lickpreference[substance == substances[tsub]], 
    lickpreference[substance == substances[tref]])})}

# glm win-stay behaviour --------------------------------------------------

## check for contrasts
# lapply(X = unique(dmodel$tag), function(a){dmodel[dmodel$tag == a,] %>%
#     group_by(tag, corner) %>% summarise(N = n())}) %>% bind_rows() %>%
#   tidyr::spread(corner, N) %>% View()
## gives:
# not enough trials for each corner:
outlier <- c("900110000199391", "900110000199541")

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

result$glm <- lapply(setdiff(unique(dmodel$tag), outlier), 
                      function(x) {model_stay2(x)}) %>% bind_rows()

result$glm %>%
  left_join(manimal, by="tag") %>%
  filter(substance %in% "alcohol+saccharin") %>%
  filter(predictor %in% "intervala") %>%
  summarise(plus = length(which(sig > 0)),
            total = n())

result$glm %>%
  select(tag, predictor, sig) %>%
  tidyr::spread(predictor, sig) %>%
  left_join(manimal %>% select(tag, substance)) %>% 
  mutate(intercept = ifelse(dooropened1 > 0 & intervala >0, 1, 0)) %>%
  group_by(substance)%>%
  summarise(intercept = sum(intercept, na.rm = T),
            reward = sum(dooropened1),
            interval = sum(intervala),
            total = n())

# visits while dark -------------------------------------------------------
dall %>%
  filter(info %in% "reversal") %>%
  left_join(manimal) %>%
  arrange(start) %>%
  mutate(cohort = gg$cohort$label[match(exp, gg$cohort$exp)],
         gr = paste(substance, cohort, sep = " ")) %>%
  group_by(gr) %>%
  binal(allow.group = T) %>%
  ungroup() %>%
  filter(hour(start) %in% c(0:6, 20:24)) %>%
  group_by(gr, corner, bin) %>%
  summarise(sumvisit = sum(visitduration, na.rm = T),
            measure = (sumvisit / (24 * 60 * 60)) * 100) %>% View()
  summarise(min = min(measure), max = max(measure)) %>% View()


# pubmodel ----------------------------------------------------------------
  # explanatory short #
  pubmodel %>%
    purrr::map(~select(., tag, aic, name)) %>%
    dplyr::bind_rows() %>%
    group_by(tag) %>%
    summarise(top = name[aic == min(aic)],
              value = min(aic)) %>%
    left_join(manimal %>% select(tag, substance)) %>%
    group_by(substance, top) %>%
    summarise(n = n()) %>% 
    mutate(max = sum(n)) %>% View()
  
  pubmodel %>%
    purrr::map(~select(., tag, aic, name)) %>%
    dplyr::bind_rows() %>%
    group_by(tag, name) %>%
    arrange(aic, .by_group = T) %>%
    summarise(value = head(aic)[1]) %>%
    arrange(value, .by_group = T) %>%
    slice(1:2, .preserve = T) %>%
    summarise(
      what = paste0(unique(name), collapse = " - "),
      dif = value[1] - value[2]) %>%
    left_join(manimal %>% select(tag, substance)) %>%
    group_by(substance) %>%
    summarise(median(dif))  
  
#prepare table
  temp <- list()
  temp$name = c("basic", "random", "attention", "dual", "fictitious", "hybrid", 
                       "forgetful", "noisywinstay", "q-decay","q-decay*", "q-decay+", 
                       "b-decay","b-decay*", "b-decay+", "relational")
  temp$result <- pubmodel %>%
    purrr::map(~ select(., tag, name, aic, contains("par.")) %>%
                 tidyr::gather(param, value, -tag, -name) %>%
                 left_join(manimal %>% select(tag, substance), by = "tag") %>%
                 group_by(name, param, substance) %>%
                 summarise(med = median(value, na.rm = T),
                           low = quantile(value, na.rm = T)[2],
                           upp = quantile(value, na.rm = T)[4]) %>%
                 ungroup()) %>%
    bind_rows() %>%
    group_by(name, param, substance) %>%
    summarise(result = paste0(util$format_digit(med), 
                           " (", util$format_digit(low), ", ", 
                           util$format_digit(upp), ")", collapse = "")) %>%
    ungroup() %>%
    spread(substance, result) %>%
    mutate(name = factor(name, levels = temp$name, ordered = T)) %>%
    arrange(name)

  xlsx::write.xlsx2(temp$result, 'fig/table2.xls')
  