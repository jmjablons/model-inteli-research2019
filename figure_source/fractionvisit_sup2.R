# sup 2 fraction visits ---------------------------------------------------
temp <- list(
  data = dall %>%
    filter(info %in% c("adaptation","reversal")) %>%
    util$binal(hour = 13) %>%
    group_by(period = bin, tag, exp) %>% 
    summarise(value = length(which(rp>0))/n(), 
              info = last(info)) %>%
    left_join(manimal, "tag") %>%
    group_by(period, gr, info) %>%
    summarise(measure = median(value, na.rm = T),
              lower = quantile(value, na.rm = T)[[2]],
              upper = quantile(value, na.rm = T)[[4]]) %>% ungroup())
temp$util = function(x, a=temp$data){filter(a, gr %in% x)}
temp$plot = function(a){
  list(geom_ribbon(aes(ymin=lower, ymax=upper, 
                       group = gr),
                   colour = NA, fill = "gray"),
       geom_hline(yintercept = .5, linetype = "dotted"),
       geom_point(aes(fill = info), size = gg$point.size, pch = 21),
       scale_y_continuous(breaks=c(0, 0.5, 1), limits = c(0, 1), 
                          expand = c(0,0)),
       scale_x_continuous(breaks=c(0, 15, 35), limits = c(0, 35), 
                          expand = c(0,0)),
       scale_fill_manual(values = c("white", "darkgray")),
       facet_wrap(~gr),
       theme_publication,
       theme(axis.title.y = element_blank(),
             legend.position = "none",
             axis.title.x = element_blank()))}

p1 <- ggplot(temp$util("alcohol (I)"), aes(x=period, y=measure)) + 
  temp$plot() + theme(legend.position = c(0.0, 0.80), 
                      legend.justification = c(0, 0))
p2 <- ggplot(temp$util("alcohol (II)"), aes(x=period, y=measure)) + 
  temp$plot()
p3 <- ggplot(temp$util("saccharin (I)"), aes(x=period, y=measure)) + 
  temp$plot() + theme(axis.title.y = element_text(angle = 90)) + 
  labs(y = "Fraction of visits in reward corners")
p4 <- ggplot(temp$util("saccharin (III)"), aes(x=period, y=measure)) + 
  temp$plot()
p5 <- ggplot(temp$util("alcohol+saccharin (II)"), 
             aes(x=period, y=measure)) + 
  temp$plot() + theme(axis.title.x = element_text(hjust = 1)) + 
  labs(x = "Bin (48h)")
p6 <- ggplot(temp$util("water (IV)"), aes(x=period, y=measure)) + 
  temp$plot() + theme(axis.title.x = element_text(hjust = 1)) + 
  labs(x = "Bin (48h)")

fig[["sup2"]] <- (p1 + p2) / (p3 + p4) / (p5 + p6)