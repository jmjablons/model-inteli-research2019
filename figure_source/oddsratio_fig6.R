# glm results
temp <- list(box_default(), median_default, point_default(),
             geom_hline(yintercept = 0, linetype = "dotted"),
             facet_wrap(~predictor, scales = "free_y"),
             theme_publication,
             theme(legend.position = 'none',
                   axis.title.x = element_blank(),
                   axis.text.x = element_text(angle = 45, hjust = 1),
                   axis.ticks.x = element_blank(),
                   axis.ticks.y = element_line(linetype = 'dashed'),
                   axis.line.x = element_blank()),
             labs(y = "log odds ratio of stay"))

p1 <- result$glm %>%
  filter(grepl("intercept", predictor, ignore.case = T)) %>%
  filter(sig == 1) %>%
  mutate(predictor = "Intercept") %>%
  left_join(manimal) %>%
  ggplot(aes(x = substance, y = estimate, 
             group = substance, colour = sig)) +
  scale_y_continuous(limits = c(-5, 5), expand = c(0, 0), 
                     breaks = c(-5, 0, 5)) + temp

p2 <- result$glm %>%
  filter(grepl("door", predictor, ignore.case = T)) %>%
  filter(sig == 1) %>%
  mutate(predictor = "Reward") %>%
  left_join(manimal) %>%
  ggplot(aes(x = substance, y = estimate, group = substance, 
             colour = sig)) +
  scale_y_continuous(limits = c(-1.05, 1.05), expand = c(0, 0), 
                     breaks = c(-1, 0, 1)) +
  temp

p3 <- result$glm %>%
  filter(grepl("intervala", predictor, ignore.case = T)) %>%
  filter(sig == 1) %>%
  mutate(predictor = "Per minute of interval") %>%
  left_join(manimal) %>%
  ggplot(aes(x = substance, y = estimate, group = substance, 
             colour = sig)) + 
  scale_y_continuous(limits = c(-.01, .01), 
                     expand = c(0, 0), breaks = c(-.01, 0, .01)) + 
  temp 

p4 <- result$glm %>%
  filter(grepl("corner", predictor, ignore.case = T)) %>%
  filter(sig == 1) %>%
  mutate(predictor = "Corner bias") %>%
  left_join(manimal) %>%
  ggplot(aes(x = substance, y = estimate, group = substance, 
             colour = sig)) +
  scale_y_continuous(limits = c(-8, 8), expand = c(0, 0),
                     breaks = c(-8, 0, 8))+
  temp

# p5 <- result$glm2 %>%
#   select(tag, estimate = deltafold) %>% distinct() %>% 
#   left_join(manimal) %>%
#   ggplot(aes(x = substance, y = estimate, group = substance)) +
#   geom_hline(yintercept = 0, linetype = "dotted")+
#   box_default +  point_default + 
#   scale_y_continuous(limits = c(0, 1), expand = c(0, 0), 
#                      breaks = c(0, 1)) + 
#   theme_publication + 
#   theme(legend.position = 'none',
#         axis.title.x = element_blank(),
#         axis.title.y = element_blank(),
#         axis.text.x = element_text(angle = 45, hjust = 1),
#         axis.ticks.x = element_blank(),
#         axis.ticks.y = element_line(linetype = 'dashed'),
#         axis.line.x = element_blank())

fig[[6]] <- ((p1 | p2) / (p3 | p4)) + plot_annotation(tag_levels = "A")