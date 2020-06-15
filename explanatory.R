# metadata ----------------------------------------------------------------
all_manimal; manimal 

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
util$getpreference() %>%
  left_join(manimal, by = "tag") %>%
  group_by(substance) %>%
  summarise(median = median(value, na.rm = T),
            iqr25 = quantile(value, na.rm = T)[[2]],
            iqr75 = quantile(value, na.rm = T)[[4]])

# util$getpreference() %>%
#   left_join(select(manimal, tag, substance), by = "tag") %>%
#   group_by(substance) %>%
#   summarise(n = length(which(value < 0.5)),
#             total = n()) %>% View()

# better ratio ------------------------------------------------------------
util$betterratio(dall) %>%
  left_join(manimal, by = "tag") %>%
  group_by(substance) %>%
  summarise(median = median(value, na.rm = T),
            iqr25 = quantile(value, na.rm = T)[[2]],
            iqr75 = quantile(value, na.rm = T)[[4]])

# visits while dark -------------------------------------------------------
dall %>%
  filter(info %in% "reversal") %>%
  left_join(manimal) %>%
  arrange(start) %>%
  mutate(cohort = gg$cohort$label[match(exp, gg$cohort$exp)],
         gr = paste(substance, cohort, sep = " ")) %>%
  group_by(gr) %>%
  util$binal(allow.group = T) %>%
  ungroup() %>%
  filter(lubridate::hour(start) %in% c(0:6, 20:24)) %>%
  group_by(gr, corner, bin) %>%
  summarise(sumvisit = sum(visitduration, na.rm = T),
            measure = (sumvisit / (24 * 60 * 60)) * 100) %>% View()
  summarise(min = min(measure), max = max(measure)) %>% View()
  
# parameters --------------------------------------------------------------
  util$plotpar("dual","par.alpha.pos",a = remodel) + 
    util$plotpar("dual","par.alpha.neg",a = remodel)
  
  remodel$dual %>%
    select(tag, par.alpha.pos, par.alpha.neg) %>%
    tidyr::gather(param, value, -tag) %>%
    left_join(manimal) %>%
    ggplot(aes(x =param, y = value, group = tag)) +
    geom_line()+
    geom_point()+
    facet_wrap(~substance)