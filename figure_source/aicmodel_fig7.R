temp <- list(
  data = util$aictidy(repmodel) %>% filter(name %in% temp_names$name & name %!in% c("random", "noisywinstay")),
  plot = list(box_default(), median_default, point_default(), 
              geom_hline(yintercept = 0, linetype = "dotted"),
              theme_publication, 
              labs(y = TeX("$\\Delta{AIC}$")),
              theme(panel.spacing = unit(2, "lines"),
                    axis.text.x = element_text(angle = 45, hjust = 1),
                    axis.title.x = element_blank(),
                    axis.ticks.y = element_line(linetype = 'dashed'),
                    axis.ticks.x = element_blank(),
                    axis.line.x = element_blank(),
                    legend.position = "none"),
              facet_wrap(~ substance, ncol = 1),
              scale_y_continuous(
                trans = 'asinh',
                expand = c(0,0),
                limits = c(-10^4, 10^4),
                breaks = c(-(10^(1:4)), 10^(1:4)),
                labels = TeX(c(paste("$-10^{",1:4, "}$", sep = ""),
                               paste("$10^{",1:4, "}$", sep = ""))))),
  util = function(sb, .set, .names = temp_names){
    temp$data %>% 
      filter(substance == sb) %>%
      mutate(name = factor(name, levels = .names$name, 
                           labels = .names$rename, ordered = T)) %>%
      filter(name %in% .names$rename[.names$set %in% .set]) %>%
      ggplot(aes(x = name, y = delta)) + temp$plot})

p1 <- temp$util("alcohol",1) + 
  theme(axis.text.x = element_blank())

p2 <- temp$util("saccharin",1)+ 
  theme(axis.text.x = element_blank())

p3 <- temp$util("alcohol+saccharin",1)+ 
  theme(axis.text.x = element_blank())

p4 <- temp$util("water",1)

p1a <- temp$util("alcohol",1) + 
  theme(axis.text.x = element_blank())

p2a <- temp$util("saccharin",1)+ 
  theme(axis.text.x = element_blank())

p3a <- temp$util("alcohol+saccharin",1)+ 
  theme(axis.text.x = element_blank())

p4a <- temp$util("water",1)

p5 <- temp$util("alcohol",2) + 
  theme(axis.text.x = element_blank(),
        axis.line.y = element_blank(),
        axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        axis.ticks.y = element_blank())

p6 <- temp$util("saccharin",2)+ 
  theme(axis.text.x = element_blank(),
        axis.line.y = element_blank(),
        axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        axis.ticks.y = element_blank())

p7 <- temp$util("alcohol+saccharin",2)+ 
  theme(axis.text.x = element_blank(),
        axis.line.y = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank())

p8 <- temp$util("water",2)+
  theme(axis.line.y = element_blank(),
        axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        axis.ticks.y = element_blank())

fig[[7]] <- ((p1 | p5) / (p2 | p6) / (p3 | p7) / (p4 | p8))

((p1 | p1a) / (p2 | p2a) / (p3 | p3a) / (p4 | p4a))