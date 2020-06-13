util_cornerrp <- tibble::enframe(c("0_0_0_0", "1_0_1_0", "0_1_0_1", 
                                   "0_0_1_1", "1_0_0_1", "0_1_1_0", "0.9_0_0.9_0", 
                                   "0.3_0_0.9_0", "0.9_0_0.3_0", "0.3_0_0.3_0", "0.9_0.9_0_0",
                                   "0_0.9_0_0.9", "0_0_0.9_0.9", "0.9_0_0_0.9", "0_0.9_0.9_0", 
                                   "0_0.3_0_0.9", "0_0.9_0_0.3", "0_0.3_0_0.3", 
                                   "1_1_0_0")) %>%
  select(-name) %>%
  arrange(value) %>%
  mutate(id = row_number())

temp <- list(
  util = function(.exp){
    icager::printscheme(dall %>% filter(exp %in% .exp)) %>%
      tidyr::unite("value", c(`1`,`2`,`3`,`4`)) %>%
      select(-start, -end, -label) %>%
      left_join(util_cornerrp) %>% 
      select(-value) %>%
      mutate(duration = as.numeric(duration)) %>% t()})

for(i in LETTERS[1:4]){
  xlsx::write.xlsx(temp$util(i),file = 'data/scheme.xls', append = T, 
                   col.names = F, sheetName = paste0("exp",i))}
xlsx::write.xlsx(util_cornerrp, file = 'data/scheme.xls', append = T, 
                 sheetName = "decode")

temp$util("A") %>% t() %>% as_tibble() %>%
  mutate(id = as.factor(id)) %>%
  ggplot(aes(x = contingency, y = 0)) +
  geom_point(aes(shape = id), size = gg$point.size)+
  geom_text(aes(label = duration), size = 4, nudge_x = 0, nudge_y = -.01) +
  scale_shape_manual(values=c(1:11)) +
  scale_y_continuous(limits = c(-0.03,0.01), expand = c(0,0))+
  theme_void()+
  theme(legend.position = "bottom")
