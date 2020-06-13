temp <- list()
temp$data <- dall %>% 
  filter(info %in% c("adaptation", "reversal")) %>%
  util$binal(hour = 13) %>%
  left_join(manimal, "tag") %>%
  group_by(bin, gr, tag) %>% 
  summarise(nvisit = n(),
            info = last(info)) %>%
  group_by(bin, gr, info) %>%
  summarise(measure = median(nvisit, na.rm = T),
            lower = quantile(nvisit, na.rm = T)[2],
            upper = quantile(nvisit, na.rm = T)[4]) %>%
  ungroup()

temp$plot <- list(
  geom_ribbon(aes(ymin=lower, ymax=upper, group = gr), 
              fill = gg$ribbon.fill, colour = gg$ribbon.colour),
  geom_point(size = gg$point.size, aes(fill = info), pch = 21, 
             colour = gg$point.colour),
  geom_hline(yintercept = 200, linetype = 'dotted'),
  scale_y_continuous(breaks=c(0, 200, 400), limits = c(0, 400),
                     expand = c(0,0)),
  scale_x_continuous(breaks=c(0, 15, 35), limits = c(0, 35), 
                     expand = c(0,0)),
  ylab('Number of visits in corners'),
  xlab('Bin (48h)'),
  scale_fill_manual(values = c('white','darkgray')),
  facet_wrap(~gr),
  theme_publication, 
  theme(legend.position = "bottom"))

p1 <- temp$data %>% filter(gr %in% "alcohol (I)") %>% 
  ggplot(aes(x=bin, y=measure)) + temp$plot + 
  theme(legend.position = c(0.0, 0.80), 
        legend.justification = c(0, 0), 
        axis.title.x = element_blank(), 
        axis.title.y = element_blank())

p2 <- temp$data %>% filter(gr %in% "alcohol (II)") %>% 
  ggplot(aes(x=bin, y=measure)) + temp$plot +
  theme(legend.position = "none", axis.title.x = element_blank(), 
        axis.title.y = element_blank())

p3 <- temp$data %>% filter(gr %in% "saccharin (I)") %>% 
  ggplot(aes(x=bin, y=measure)) + temp$plot +
  theme(legend.position = "none", axis.title.x = element_blank())

p4 <- temp$data %>% filter(gr %in% "saccharin (III)") %>% 
  ggplot(aes(x=bin, y=measure)) + temp$plot +
  theme(legend.position = "none", axis.title.x = element_blank(), 
        axis.title.y = element_blank())

p5 <- temp$data %>% filter(gr %in% "alcohol+saccharin (II)") %>% 
  ggplot(aes(x=bin, y=measure)) + temp$plot +
  theme(legend.position = "none", axis.title.y = element_blank())

p6 <- temp$data %>% filter(gr %in% "water (IV)") %>% 
  ggplot(aes(x=bin, y=measure)) + temp$plot + 
  theme(legend.position = "none", axis.title.y = element_blank())

fig[[2]] <- (p1 + p2) / (p3 + p4) / (p5 + p6) + 
  plot_annotation(tag_levels = "A")

fig$poprawiona2 <- (p1 + p2) / (p3 + p4) / (p5 + p6) + 
  plot_annotation(tag_levels = "A")

fig[[2]] | fig$poprawiona2