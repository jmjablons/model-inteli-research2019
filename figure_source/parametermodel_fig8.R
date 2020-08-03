# parameters
p1 <- util$plotpar("basic", "par.beta", c(0,12, 14),.gg.maxvalue = 50, .gg.break = c(0, 2, 12, 14), a = remodel)+
  util$signif(c(1,4), 11)+
  util$signif(c(3,4), 11.5)
p2 <- util$plotpar("fictitious", "par.beta", c(0,12, 14), .gg.maxvalue = 50, .gg.break = c(0, 2, 12, 14), a = remodel)+
  util$signif(c(1,2),11)+
  util$signif(c(1,4),11.4)+
  util$signif(c(3,4),11.9)
p3 <- util$plotpar("basic", "par.alpha", c(0,.6, .7),.gg.maxvalue = 1, c(0, .1, .6, .7), a = remodel)
p4 <- util$plotpar("fictitious", "par.alpha", c(0,.6, .7), .gg.maxvalue = 1, c(0, .1, .6, .7), a = remodel)+
  util$signif(c(1,2), .45)

tplot <- (p1 + p2) / (p3 + p4) + plot_annotation(tag_levels = "A")
ggsave("fig/czerwiec2020/fig8.pdf", tplot, device = cairo_pdf,  
       scale = 1.3, width = 80, height = 100, units = "mm")

# hybrid + q-decay+ revision ----------------------------------------------

tplot <- remodel[[4]] %>%
  select(tag, par.alpha.pos = par.beta, name) %>%
  rbind(
    remodel[[12]] %>%
      select(tag, par.alpha.pos = par.beta, name)) %>% 
  tidyr::spread(name, par.alpha.pos) %>%
  mutate(dif = hybrid - `q-decay+`) %>%
  tidyr::gather(param, value, -tag) %>%
  left_join(manimal) %>%
  filter(param %in% c("hybrid", "q-decay+")) %>%
  ggplot(aes(x = param, y = value, group = tag)) +
  geom_line(size = .1)+
  geom_jitter(width = .1, pch = 21, fill = "gray")+
  geom_hline(yintercept = 0, colour = "gray")+
  scale_y_continuous(limits = c(-15, 15), expand = c(0,0), breaks = c(-10, 0, 10))+
  facet_wrap(~substance)+
  labs(y = "par.beta")

tplot <- remodel[[4]] %>%
  select(tag, par.alpha.pos = par.alpha.neg, name) %>%
  rbind(
    remodel[[12]] %>%
      select(tag, par.alpha.pos = par.storage.pos, name)) %>% 
  tidyr::spread(name, par.alpha.pos) %>%
  mutate(dif = hybrid - `q-decay+`) %>%
  select(tag, value = dif) %>%
  left_join(manimal) %>%
  ggplot(aes(x = substance, y = value)) +
  box_default() + point_default()+
  scale_y_continuous(limits = c(-1, 1), expand = c(0,0), breaks = c(-1, 0 ,1))+
  labs(y = "diff par.alpha-")

tplot <- remodel[[4]] %>%
  select(tag, par.alpha.pos = par.beta, name) %>%
  rbind(
    remodel[[12]] %>%
      select(tag, par.alpha.pos = par.beta, name)) %>% 
  tidyr::spread(name, par.alpha.pos) %>%
  mutate(dif = hybrid - `q-decay+`) %>%
  select(tag, value = dif) %>%
  left_join(manimal) %>%
  ggplot(aes(x = substance, y = value)) +
  box_default() + point_default()+
  scale_y_continuous(limits = c(-15, 15), expand = c(0,0), breaks = c(-15, 0 ,15))+
  labs(y = "diff par.beta")

tplot <- remodel[[12]] %>%
  select(tag, value = par.storage.neg, name) %>% 
  left_join(manimal) %>%
  ggplot(aes(x = substance, y = value)) +
  box_default() + point_default()+
  scale_y_continuous(limits = c(0, 1), expand = c(.1,0), breaks = c(0, .1, .5, 1))+
  labs(y = "par.storage - q-decay+")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("./wyniki_20200803/param_storage.pdf", tplot, device = cairo_pdf,  
       scale = 1, width = 80, height = 80, units = "mm")
