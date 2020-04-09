

# dependency --------------------------------------------------------------
library(boot)

util_outline <- function(x, na.omit = T){
  temp <- quantile(x, na.rm = na.omit)
  list(median = median(x, na.rm = na.omit), iqr25 = temp[[2]], 
       igr75 = temp[[4]])}


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

getPreference() %>%
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
substances <- unique(temp$substance)

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
temp <- getPreference() %>%
  left_join(manimal) %>%
  mutate(substance = as.factor(substance))

# better ratio
temp <- result$br %>%
  left_join(manimal) %>%
  mutate(substance = as.factor(substance))

# win-stay
wrap_winstay <- function(substance, what){
  util_winstay(substance) %>%
    filter(param == what) %>%
    tidyr::spread(short, value)}

# model parameters
temp <- pubmodel[["fictitious"]] %>%
  left_join(manimal) %>%
  mutate(substance = as.factor(substance),
         value = par.beta)

## variance between groups
kruskal.test(data = temp, value ~ substance)
$p.value %>%
  format(scientific = F, digits = 3)
FSA::dunnTest(data = temp, value ~ substance, method="bh")
format(5.546344e-01, scientific = F)

## greater than random
for(i in seq_along(substances)){
  with(temp, {print(
    wilcox.test(value[substance == substances[i]], 
                mu = .5, alternative = 'greater')$p.value %>%
      format(scientific = F, digits = 3))})}

## between two groups
with(wrap_winstay("water", "lose-shift"), {
  wilcox.test(`[<2]`, `[>10]`, paired = T, conf.int = T)})

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
                      function(x) {modelStay2(x)}) %>% bind_rows()

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
