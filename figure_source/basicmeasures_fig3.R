# preference + choices + better ratio
p1 <- util$getpreference() %>%
  left_join(manimal) %>%
  ggplot(aes(x = substance, y = value, group = substance)) +
  box_default() + median_default + point_default(point.width = .2) +
  util$signif(c(1,2), 1.01) +
  util$signif(c(1,3), 1.03) +
  util$signif(c(1,4), 1.05) +
  util$signif(c(2,4), 1.07) +
  util$signif(c(3,4), 1.09) +
  geom_hline(yintercept = 0.5, colour = 'black', linetype = "dashed") +
  scale_y_continuous(breaks = c(0, 0.5, 1), limits = c(0, 1.1), 
                     expand = c(0, 0)) +
  labs(y = 'Reward preference') +
  theme_publication +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.ticks.x = element_blank(),
        axis.line.x = element_blank())

p2 <- dall %>%
  filter(info == 'reversal') %>%
  group_by(tag, exp) %>% 
  summarise(measure = length(which(rp > 0 & visitduration > 2))) %>%
  left_join(manimal) %>%
  ggplot(aes(x = substance, y = measure, group = substance)) +
  box_default() + median_default + point_default() +
  util$signif(c(1,2), 3200) +
  util$signif(c(2,4), 3200) +
  scale_y_continuous(limits = c(0, 3300), expand = c(0, 0)) +
  labs(y = 'Total number of attempts\nduring reversals') +
  theme_publication +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.ticks.x = element_blank(),
        axis.line.x = element_blank())

p3 <- util$betterratio() %>%
  left_join(manimal) %>%
  ggplot(aes(x = substance, y = value, group = substance)) +
  box_default() + median_default + point_default(point.width = .2) + 
  util$signif(c(1,2), .76) +
  util$signif(c(1,3), .77) +
  util$signif(c(2,4), .78) +
  util$signif(c(3,4), .79) +
  geom_hline(yintercept = 0.5, colour = 'black', linetype = "dashed") +
  scale_y_continuous(breaks = c(0.25, .5, .75), limits = c(0.25, .8), expand = c(0, 0)) +
  labs(y = 'Preference of more \ncertain option') +
  theme_publication +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.ticks.x = element_blank(),
        axis.line.x = element_blank())

tplot <- (p1 | p2 | p3) + plot_layout(ncol = 3, widths = c(1, 1, 1)) + 
  plot_annotation(tag_levels = "A")

ggsave("fig/czerwiec2020/fig3.pdf", tplot, device = cairo_pdf,  
       scale = 2, width = 80, height = 40, units = "mm")
