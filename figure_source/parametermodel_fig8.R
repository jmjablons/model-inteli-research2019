# parameters

p1 <- util$plotpar("basic", "par.beta", c(0,12), gg$show.beta) +
  util$signif(c(1,4), 11) +
  util$signif(c(3,4), 11.5) 
p2 <- util$plotpar("fictitious", "par.beta", c(0,12), gg$show.beta) +
  util$signif(c(1,2),11)+
  util$signif(c(1,4),11.4)+
  util$signif(c(3,4), 11.9)
p3 <- util$plotpar("basic", "par.alpha", c(0,1), c(0, .5, 1))
p4 <- util$plotpar("fictitious", "par.alpha", c(0,.5), c(0, .5, 1)) +
  util$signif(c(1,2), .45)

fig[[8]] <- (p1 + p2) / (p3 + p4) + plot_annotation(tag_levels = "A")

p1a <- util$plotpar("basic", "par.beta", c(0,12), gg$show.beta, a = repmodel)
p2a <- util$plotpar("fictitious", "par.beta", c(0,12), gg$show.beta, a = repmodel)
p3a <- util$plotpar("basic", "par.alpha", c(0,1), c(0, .5, 1), a = repmodel)
p4a <- util$plotpar("fictitious", "par.alpha", c(0,.5), c(0, .5, 1), a = repmodel)

(p1 + p1a) / (p3 + p3a)
(p2 + p2a) / (p4 + p4a)