# waic --------------------------------------------------------------------
#"w_{i} / Akaike weights / weight of evidence"
temp <- list(
  name = c("random","noisywinstay","basic",  "dual", "fictitious", "hybrid", 
           "attention","forgetful",  "q-decay",
           "q-decay*", "q-decay+", "b-decay","b-decay*", "b-decay+"),
  data = util$waic(pubmodel),
  plot = list(geom_hline(yintercept = 0, linetype = "dotted"),
              box_default(), median_default, point_default(), 
              theme_publication, 
              labs(y = TeX("w_{i}")),
              theme(panel.spacing = unit(2, "lines"),
                    axis.text.x = element_text(angle = 45, hjust = 1),
                    axis.title.x = element_blank(),
                    axis.ticks.y = element_line(linetype = 'dashed'),
                    axis.ticks.x = element_blank(),
                    axis.line.x = element_blank(),
                    legend.position = "none"),
              facet_wrap(~ substance, ncol = 1),
              scale_y_continuous(
                trans = 'sqrt',
                labels = util$format(c(0.01,.1, .5, 1)),
                breaks = c(0.01,.1, .5, 1),
                limits = c(0,1))),
  util = function(sb, set){temp$data %>%
      filter(substance == sb) %>%
      mutate(name = factor(name, levels = temp$name, ordered = T)) %>%
      ggplot(aes(x = name, y = waic)) + temp$plot})

p1 <- temp$util("alcohol","general") + 
  theme(axis.text.x = element_blank())

p2 <- temp$util("saccharin","general")+ 
  theme(axis.text.x = element_blank())

p3 <- temp$util("alcohol+saccharin","general")+ 
  theme(axis.text.x = element_blank())

p4 <- temp$util("water","general")

(p1 / p2 / p3 / p4) + plot_annotation(tag_levels = "A")    

# sup 3 drinking episodes -------------------------------------------------
temp <- list(
  data = dall %>%
    filter(info %in% c("adaptation","reversal")) %>%
    util$binal(hour = 13) %>%
    mutate(reward = ifelse(rp > 0, "reward", "water"))%>%
    group_by(period = bin, tag, exp, reward) %>% 
    summarise(value = length(which(nlick>0)), 
              info = last(info)) %>%
    left_join(manimal, "tag") %>%
    mutate(cohort = gg$cohort$label[match(exp, gg$cohort$exp)],
           gr = paste(substance, cohort, sep = " ")) %>%
    group_by(period, gr, info, reward) %>%
    summarise(measure = median(value, na.rm = T),
              upper = quantile(value, na.rm = T)[[4]],
              lower = quantile(value, na.rm = T)[[2]]) %>%
    ungroup())
temp$util = function(x, a=temp$data){filter(a, gr %in% x)}
temp$plot = function(a){
  list(geom_ribbon(aes(ymin=lower, ymax=upper, 
                       group = interaction(gr, reward)),
                   colour = NA, fill = "gray"),
       geom_point(aes(fill = info, shape = reward), size = gg$point.size),
       scale_y_continuous(breaks=c(0, 200), limits = c(0, 200),
                          expand = c(0,0)),
       scale_x_continuous(breaks=c(0, 15, 35), limits = c(0, 35), 
                          expand = c(0,0)),
       scale_fill_manual(values = c("white", "darkgray")),
       scale_shape_manual(name = "drink type", values = c(24, 21)),
       facet_wrap(~gr),
       theme_publication,
       theme(axis.title.y = element_blank(),
             legend.position = "none",
             axis.title.x = element_blank()))}

p1 <- ggplot(temp$util("alcohol (I)"), aes(x=period, y=measure)) + 
  temp$plot() +   theme(legend.position = c(0.0, 0.80), 
                        legend.justification = c(0, 0))
p2 <- ggplot(temp$util("alcohol (II)"), aes(x=period, y=measure)) + 
  temp$plot()
p3 <- ggplot(temp$util("saccharin (I)"), aes(x=period, y=measure)) + 
  temp$plot() + theme(axis.title.y = element_text(angle = 90)) + 
  labs(y = "Number of visits with drinking detected [h]")
p4 <- ggplot(temp$util("saccharin (III)"), aes(x=period, y=measure)) + 
  temp$plot()
p5 <- ggplot(temp$util("alcohol+saccharin (II)"), 
             aes(x=period, y=measure)) + 
  temp$plot() + theme(axis.title.x = element_text(hjust = 1)) + 
  labs(x = "Bin (48h)")
p6 <- ggplot(temp$util("water (IV)"), aes(x=period, y=measure)) + 
  temp$plot() + theme(axis.title.x = element_text(hjust = 1)) + 
  labs(x = "Bin (48h)")

fig[["sup3"]] <- (p1 + p2) / (p3 + p4) / (p5 + p6) 

#(fig[["sup1"]] | fig[["sup2"]] | fig[["sup3"]])
#+ plot_annotation(tag_levels = "A")

# combo plot --------------------------------------------------------------
temp <- list()
temp[[1]] <- util$getpreference() %>% select(tag, exp, preference = value)

temp[[2]] <- dall %>%
  filter(info == 'reversal') %>%
  group_by(tag, exp) %>% 
  summarise(measure = length(which(rp > 0 & visitduration > 2))) %>%
  mutate(nattempt = measure) %>% select(-measure)

temp[[3]] <- result$br %>% select( tag, br = value)

temp[[4]] <- util$winstay(substances) %>%
  unite("measure", c(short, param), remove = FALSE, sep = "x") %>%
  select(tag, measure, value) %>% 
  tidyr::spread(measure, value)

temp[[5]] <- result$glm %>% select(oddsratio, tag, predictor, sig) %>%
  mutate(oddsratio = ifelse(sig == 0, 0, oddsratio)) %>%
  select(-sig) %>%
  tidyr::spread(predictor, oddsratio)

temp[[6]] <- util$waic() %>%
  filter(name %in% "fictitious") %>%
  select(tag, waic)
  
temp[[7]] <- pubmodel[["fictitious"]] %>% select(par.alpha, par.beta, tag)

temp_all <- left_join(temp[[1]], temp[[2]]) %>%
    left_join(temp[[3]]) %>%
    left_join(temp[[4]]) %>%
    left_join(temp[[5]]) %>%
    left_join(temp[[6]]) %>%
    left_join(temp[[7]]) %>%
  left_join(manimal %>% select(tag, substance)) %>%
  mutate(cohort = gg$cohort$label[match(exp, gg$cohort$exp)],
         gr = paste(substance, cohort, sep = " ")) %>%
  select(-cohort) %>%
  mutate(gr = as.factor(gr))
  
GGally::ggparcoord(temp_all, columns = 3:16, groupColumn = "gr",alphaLines = .3,
                         #scale = "globalminmax",
                         scale="uniminmax")+ 
  theme_publication +
  scale_x_discrete(expand = c(0,0))+
  scale_y_continuous(breaks = c(0,1), limits = c(0,1), expand = c(0,0))+
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.line.x = element_blank(),
        legend.position = "bottom",
        axis.ticks.x = element_blank())+
  labs(y = "normalized value")

