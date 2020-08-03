# 2020-08-03 judyta
stat_param <- function(.name, .par, a = remodel){
  temp = a[[.name]] %>%
    select(value = .par, tag) %>%
    left_join(manimal, by = "tag")
if((kruskal.test(data = temp, value ~ substance))$p.value <= 0.05){
  return((FSA::dunnTest(data = temp, value ~ substance, method="bh")$res %>%
    filter(P.adj < 0.05)))}}

stat_param("hybrid","par.alpha.neg")

p1 <- util$plotpar("hybrid", "par.alpha.pos", 
             c(0,.6, .7), .gg.maxvalue = 1, c(0, .1, .6, .7), a = remodel)+
  util$signif(c(2,4), .45, y.space = 0)+
  util$signif(c(3,4), .55, y.space = 0)

p2 <- util$plotpar("hybrid", "par.alpha.neg", 
                   c(0,.9, 1), .gg.maxvalue = 1, c(0, .1, .9, 1), a = remodel)

p3 <- util$plotpar("hybrid", "par.beta", 
             c(0, 8, 10), .gg.maxvalue = 50, c(0, 1, 5, 8), a = remodel)

tplot <- p1 + p2 + p3 + labs(caption = "model hybrid")

ggsave("./wyniki_20200803/hybrid.pdf", tplot, device = cairo_pdf,  
       scale = 1, width = 180, height = 100, units = "mm")

stat_param("q-decay+","par.alpha")
stat_param("q-decay+","par.beta")
stat_param("q-decay+","par.storage.pos")
stat_param("q-decay+","par.storage.neg")

p1 <- util$plotpar("q-decay+", "par.alpha", 
                   c(0,.6, .7), .gg.maxvalue = 1, c(0, .1, .6, .7), a = remodel)

p2 <- util$plotpar("q-decay+", "par.storage.pos", 
                   c(0,.9, 1), .gg.maxvalue = 1, c(0, .1, .9, 1), a = remodel)
p3 <- util$plotpar("q-decay+", "par.storage.neg", 
                   c(0,.9, 1), .gg.maxvalue = 1, c(0, .1, .9, 1), a = remodel)
p4 <- util$plotpar("q-decay+", "par.beta", 
                   c(0, 8, 10), .gg.maxvalue = 50, c(0, 1, 5, 8), a = remodel)

tplot <- p1 | p2 | p3 | p4 + labs(caption = "model hybrid \n all -ns-")

ggsave("./wyniki_20200803/q_decay_hybrid.pdf", tplot, device = cairo_pdf,  
       scale = 1, width = 180, height = 100, units = "mm")
