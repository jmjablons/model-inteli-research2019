
# substance preference ----------------------------------------------------
getPreference <- function(n.hours = 96, period.info = "adaptation", a = dall){
  criteria <- a %>%
    filter(info == period.info) %>%
    group_by(exp, info) %>%
    arrange(start, .by_group = T) %>%
    summarise(finish = tail(start, 1),
              begin = finish - lubridate::hours(n.hours)) %>%
    ungroup()
  a %>%
    group_by(exp, info) %>%
    filter(start > criteria$begin[match(exp, criteria$exp)] & 
             end < criteria$finish[match(exp, criteria$exp)], 
           .preserve = TRUE) %>%
  group_by(tag) %>%
  summarise(
    nLickWater = sum(nlick[which(rp == 0)], na.rm = TRUE),
    nLickGood = sum(nlick[which(rp > 0)], na.rm = TRUE),
    lickpreference = nLickGood / (nLickGood + nLickWater))}

# substance preference stat -----------------------------------------------

substances <- unique(rPreference$substance)

## variance between groups
kruskal.test(data = rPreference, lickpreference ~ substance)$p.value %>%
  format(scientific = F, digits = 3)
FSA::dunnTest(data = rPreference, lickpreference ~ substance, method="bh")

## greater than random
for(substance in substances) {
  with(rPreference, {
    print(c(substance, 
            wilcox.test(lickpreference[substance == substance], 
                        mu = 0.5, alternative = 'greater')$p.value %>%
              format(scientific = F, digits = 3)))})}

## size effect
{tsub = 'saccharin'; tref = 'water'
with(rPreference, {
  effsize::cohen.d(
    lickpreference[substance == tsub], 
    lickpreference[substance == tref])})}

# for(substance in substances) {
#   for(s in substances){
#     cat(paste0('\n','\n','###','\t',substance,'\t',s,'\n'))
#     with(rPreference, {
#       print(effsize::cohen.d(lickpreference[substance == substance],
#                              lickpreference[substance == s]))})}}

# better ratio ------------------------------------------------------------
rBetterRatio <- 
  dall %>%
  filter(label %in% c('0.9x0.3', '0.3x0.9')) %>%
  group_by(tag) %>%
  summarise(n = n(),
    nBetter = length(which(rp > 0.5)),
    nWorse = length(which(rp > 0.0 & rp < 0.5)),
    br = nBetter / (nBetter + nWorse))

# better ratio stat -------------------------------------------------------

kruskal.test(br ~ substance, data = rBetterRatio)$p.value %>%
  format(scientific = TRUE, digits = 3)
FSA::dunnTest(br ~ substance, data = rBetterRatio, method="bh")
##Pairwise Mann–Whitney U-tests
# pairwise.wilcox.test(
#   r.BetterRatio$BR, 
#   r.BetterRatio$Substance,
#   p.adjust.method = "BH")

#Is the effect large enough?
tsub = 'saccharin'; tref = 'water'
with(rBetterRatio, {effsize::cohen.d(br[substance == tsub], br[substance == tref])})

#Is the value greater than random?
for(s in substances) {
  with(rBetterRatio, {
    print(c(substance, 
            wilcox.test(br[substance == s], mu = 0.5, alternative = 'greater')$p.value %>%
              format(scientific = F, digits = 3)))})}

# glm win-stay behaviour --------------------------------------------------

getErrorRate <- 
  function(real.data, predict.data) {
    real.data = as.vector(as.logical(real.data))
    predict.data = as.vector(prediction.data > 0.5)
    mean(predict.data != real.data)}

modelStay <- 
  function(mouse, a = dmodel, n.divisions = 10) {
    set.seed(123)
    dmouse <- a[a$tag == mouse,]
    dmouse <- mutate(dmouse, 
                     corner = as.factor(ceiling(corner/2)),
                     dooropened = as.factor(dooropened))
    dlenght <- seq_along(dmouse$stay)
    itest <- sample(max(dlenght), floor(max(dlenght) / n.divisions), replace = FALSE)
    itrain <- setdiff(dlenght, sort(itest))
    result.glm <- glm(stay ~ dooropened + iltervala + corner, 
                      data = dmouse[itrain,], family = binomial)
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
        # mcFaddenpR2 = pscl::pR2(object = model.greatest.mouse)['McFadden'],
        sig = ifelse(probability < 0.05, 1, 0))}

modelStay <- lapply(manimal$tag, function(x) {modelStay(x)}) %>% bind_rows()
