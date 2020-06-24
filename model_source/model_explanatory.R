# explanatory short #
remodel %>%
  purrr::map(~select(., tag, aic, name)) %>%
  dplyr::bind_rows() %>%
  filter(name %in% c(temp_names$name, "basic")) %>%
  group_by(tag) %>%
  summarise(top = name[aic == min(aic)],
            value = min(aic)) %>%
  left_join(manimal %>% select(tag, substance)) %>%
  #group_by(substance, top) %>%
  group_by(top, substance) %>%
  summarise(n = n()) %>% 
  group_by(substance) %>%
  mutate(max = sum(n)) %>% 
  arrange(-n, .by_group = TRUE) %>%
  ungroup() %>% View()

remodel %>%
  purrr::map(~select(., tag, aic, name)) %>%
  dplyr::bind_rows() %>%
  filter(name %in% c(temp_names$name, "basic")) %>%
  group_by(tag) %>%
  summarise(top = name[aic == min(aic)],
            value = min(aic)) %>%
  left_join(manimal %>% select(tag, substance)) %>%
  #group_by(substance, top) %>%
  group_by(top) %>%
  summarise(n = n()) %>% 
  mutate(max = sum(n)) %>% 
  arrange(-n, .by_group = TRUE) %>%
  ungroup()

remodel %>%
  purrr::map(~select(., tag, aic, name)) %>%
  dplyr::bind_rows() %>%
  filter(name %in% c(temp_names$name, "basic")) %>%
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

# pubmodel %>%
#   util$waic() %>%
# left_join(manimal) %>%
#   filter(substance %in% substances) %>%
#   filter(name %in% "fictitious") %>%
#   left_join(dmodel %>% group_by(tag,exp) %>% summarise()) %>%
#   ungroup() %>%
#   mutate(exp = as.factor(exp),
#          substance = as.factor(substance)) %>%
#   select(tag, substance, exp, value = "waic") %>%
#   group_by(substance) %>%
#   summarise(median(value), quantile(value)[2], quantile(value)[4])
# 
# pubmodel %>%
#   util$waic() %>%
#   left_join(manimal) %>%
#   filter(substance %in% "water") %>%
#   left_join(dmodel %>% group_by(tag,exp) %>% summarise()) %>%
#   ungroup() %>%
#   mutate(exp = as.factor(exp),
#          substance = as.factor(substance)) %>%
#   select(tag, name, substance, exp, value = "waic") %>%
#   group_by(name, substance) %>%
#   summarise(median = round(median(value*100), 5), 
#             rst = round(quantile(value*100),5)[2],
#             rth = round(quantile(value*100),5)[4])