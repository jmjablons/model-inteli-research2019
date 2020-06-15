temp <- list(
  data = function(fn.name, fn.reward, a = dmodel){
    a %>% filter(tag %in% fn.name) %>% 
      filter(dooropened %in% fn.reward)},
  plot = list(
    geom_quasirandom(
      size = gg$point.size, groupOnX = FALSE, stroke = 0, shape = 16, 
      colour = gg$point.colour, alpha = 0.06, width = 0.07, 
      method = "tukeyDense"),
    geom_hline(yintercept = 0.5, linetype = 'dotted'),
    geom_smooth(method = "glm", method.args = list(family = "binomial"),
                se = TRUE, colour = 'darkgray'),
    scale_x_continuous(limits = c(-3.507, 7), breaks = gg$stay.value,
                       labels = util$format(exp(gg$stay.value)),
                       expand = c(0, 0)),
    scale_y_continuous(breaks = c(0, 0.5, 1),
                       labels = c(0, 0.5, 1),
                       expand = c(0.02, 0.02)),
    labs(y = 'probability of stay', 
         x = 'interval between choices [minutes]'),
    facet_wrap(~dooropened, labeller = labeller(dooropened = gg$stay.label)),
    theme_publication,
    theme(panel.spacing = unit(1, "lines"),
          axis.line.x = element_blank())))

p1 <- temp$data(hero, 1) %>% ggplot(aes(x = log(intervala), y = stay)) + 
  temp$plot + theme(axis.title.x = element_blank())

p2 <- temp$data(hero, 0) %>% ggplot(aes(x = log(intervala), y = stay)) + 
  temp$plot + theme(axis.title.y = element_blank(), 
                    axis.line.y = element_blank(),
                    axis.text.y = element_blank(),
                    axis.ticks.y = element_blank())

p3 <- temp$data(hero0, 1) %>% ggplot(aes(x = log(intervala), y = stay)) + 
  temp$plot + theme(axis.title.x = element_blank())

p4 <- temp$data(hero0, 0) %>% ggplot(aes(x = log(intervala), y = stay)) + 
  temp$plot + theme(axis.title.y = element_blank(), 
                    axis.line.y = element_blank(),
                    axis.text.y = element_blank(),
                    axis.ticks.y = element_blank())

tplot <- ((p1 | p2 | plot_spacer()) / (p3 | p4 | plot_spacer())) #+ plot_annotation(tag_levels = "A")
tplot <- ((p1 | p2) / (p3 | p4)) #+ plot_annotation(tag_levels = "A")

ggsave("fig/czerwiec2020/fig4.pdf", tplot, device = cairo_pdf,  
       scale = 1.1, width = 80, height = 80, units = "mm")
