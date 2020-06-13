temp <- list(geom_line(aes(group = interaction(tag, param)), 
                       size = .1, alpha = .7, 
                       colour = "black"),
             point_default(point.size = 2, point.width = .1), 
             scale_y_continuous(expand = c(0,0.05), limits = c(0,1.1), 
                                breaks = c(0, .5, 1)), 
             facet_wrap(~substance), theme_publication, 
             theme(axis.title.x = element_blank(),
                   axis.title.y = element_blank(),
                   axis.line.x = element_blank(),
                   axis.text.x = element_text(angle = 45, hjust = 1),
                   axis.ticks.x = element_blank()))

p1 <- util$winstay("alcohol") %>%
  ggplot(aes(x = interaction(short, param, sep = " ", lex.order = F), 
             y = value, group = tag)) + temp +
  util$signif(x.where = c(1,2), y.where = 1.06, y.space = 0.01, "black")+
  util$signif(x.where = c(3,4), y.where = 1.06, y.space = 0.01, "black")

p2 <- util$winstay("alcohol+saccharin") %>%
  ggplot(aes(x = interaction(short, param, sep = " ", lex.order = F), 
             y = value, group = tag)) + temp +
  util$signif(x.where = c(1,2), y.where = 1.06, y.space = 0.01, "black")+
  util$signif(x.where = c(3,4), y.where = 1.06, y.space = 0.01, "black")

p3 <- util$winstay("saccharin") %>%
  ggplot(aes(x = interaction(short, param, sep = " ", lex.order = F), 
             y = value)) + temp +
  util$signif(x.where = c(1,2), y.where = 1.06, y.space = 0.01, "black")+
  util$signif(x.where = c(3,4), y.where = 1.06, y.space = 0.01, "black")

p4 <- util$winstay("water") %>%
  ggplot(aes(x = interaction(short, param, sep = " ", lex.order = F), 
             y = value)) + temp +
  util$signif(x.where = c(3,4), y.where = 1.06, y.space = 0.01, "black")

fig[[5]] <- (p1 | p2) / (p3 | p4) + plot_annotation(tag_levels = "A")