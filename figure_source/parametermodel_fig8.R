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
