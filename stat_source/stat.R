substances <- unique(manimal$substance)

# input -------------------------------------------------------------------
#choices
temp <- dmodel %>%
  group_by(tag, exp) %>%
  summarise(value = n()) %>%
  ungroup() %>%
  left_join(manimal) %>%
  mutate(cohort = gg$cohort$label[match(exp, gg$cohort$exp)],
         gr = paste(substance, cohort, sep = " ")) %>%
  mutate(substance = as.factor(gr))

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

temp <- util$getpreference() %>%
  left_join(manimal, "tag") %>%
  mutate(cohort = gg$cohort$label[match(exp, gg$cohort$exp)],
         gr = paste(substance, cohort, sep = " ")) %>%
  mutate(substance = as.factor(gr))

# better ratio
temp <- util$betterratio(dall) %>%
  left_join(manimal) %>%
  mutate(substance = as.factor(substance))

temp <- util$betterratio(dall) %>%
  util$assign_cohort()

# model parameters
temp <- util$get("fictitious","par.beta", "saccharin")

temp <- pubmodel[["basic"]] %>%
  select(value = par.alpha, tag) %>%
  left_join(manimal, by = "tag") %>%
  mutate(substance = as.factor(substance))

temp <- pubmodel[["fictitious"]] %>%
  select(value = par.alpha, tag) %>%
  left_join(manimal, by = "tag") %>%
  mutate(substance = as.factor(substance))

# stat --------------------------------------------------------------------
#$p.value %>% format(scientific = F, digits = 3)

## variance between groups
kruskal.test(data = temp, value ~ substance)

FSA::dunnTest(data = temp, value ~ substance, method="bh")$res %>%
  filter(P.adj < 0.05)

## greater than random
for(i in seq_along(substances)){
  with(temp, {print(
    wilcox.test(value[substance == substances[i]], 
                mu = .5, alternative = 'greater'))})}

## between two groups
with(temp, {wilcox.test(value ~ exp, conf.int = T)})

with(util$wrap_winstay("water", "win-stay"), {
  wilcox.test(`<2`, `>10`, paired = T, conf.int = T)})

## size effect
{tsub = 2; tref = 1
  with(temp, {effsize::cohen.d(
    lickpreference[substance == substances[tsub]], 
    lickpreference[substance == substances[tref]])})}
