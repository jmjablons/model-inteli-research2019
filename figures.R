#new figures
#2020-02-24

# dataset -----------------------------------------------------------------
hero = "900110000199546" #saccharin
hero0 = "900110000351935" #water

dhero <- dmodel[dmodel$tag == hero,] %>% 
  filter(contingency == 17)

#dmodel <- readRDS(file.choose())

# custom ------------------------------------------------------------------
theme_publication <- theme(
    legend.title = element_blank(),
    legend.background = element_blank(),
    legend.direction = "horizontal",
    legend.box = "vertical",
    legend.position = c(0.0, 0.80),
    legend.justification = c(0, 0),
    legend.key = element_blank(),
    axis.ticks = element_line(linetype = 'dashed'),
    axis.line = element_line(
      colour = "black",
      linetype = "solid"),
    axis.title.x = element_text(hjust = 1),
    panel.border = element_blank(),
    panel.grid = element_blank(),
    panel.spacing = unit(1, "lines"),
    panel.background = element_rect(fill = "white", colour = NA),
    strip.background = element_blank(),
    strip.placement = "inside",
    strip.text = element_text(angle = 0, hjust = 0),
    complete = FALSE)

box_default <- function(...){
  stat_summary(geom = "crossbar", fill = gg$box.fill, 
               colour = gg$box.outline, ...,
               fun.data = function(x) {
                 data.frame(y = median(x), ymin = quantile(x)[2], 
                            ymax = quantile(x)[4])})}
  
median_default <- stat_summary(geom = "crossbar", fill = NA, 
                               color = "black",
      fun.data = function(x) {data.frame(y = median(x),
                                         ymin = median(x),
                                         ymax = median(x))})

point_default <- function(point.width = gg$point.width, 
                          point.size = gg$point.size, 
                          point.colour = "black", 
                          point.pch = 21, point.fill = "lightgray", ...){
  geom_quasirandom(size = point.size, width = point.width, 
                   colour = point.colour, method = "tukeyDense", 
                   varwidth = TRUE, pch = point.pch, 
                   fill = point.fill, ...)}

# visuals -----------------------------------------------------------------
#general visuals
gg <- list(
  point.width = 0.2,
  point.alpha = 1,
  point.size = 2,
  point.colour = "black",
  box.fill = "lightgray",
  box.outline = "black",
  ribbon.fill = "lightgray",
  ribbon.colour = NA,
  stay.label = c(`0` = 'after lose', `1` = 'after win'),
  stay.value = log(c(0.03, 1, 10, 60, 660)),
  show.beta <- c(0, 5, 10, 12),
  limit.nll <- c(50, 175),
  cohort = data.frame(exp = c(LETTERS[1:4]), 
                label = c(paste0("(",c("II","III","I","IV"),")"))))

temp_names <- tibble(
  name = c("random","noisywinstay","dual",
           "fictitious","hybrid","forgetful",
           "q-decay","q-decay*","q-decay+",
           "b-decay","b-decay*","b-decay+"),
  rename = c("random","noisywinstay","dual",
             "fictitious","hybrid","forgetful",
             "dQ","dQ+fictitious","dQ+split",
             "dB","dB+fictitious","dB+split"),
  set = c(rep(1,6),rep(2,6)))

# fig 2 -------------------------------------------------------------------
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

# fig 3 -------------------------------------------------------------------
p1 <- util$getpreference() %>%
  left_join(manimal) %>%
  ggplot(aes(x = substance, y = value, group = substance)) +
  box_default() + median_default + point_default(point.width = .2) +
  util$signif(c(1,2), 1.01) +
  util$signif(c(1,3), 1.03) +
  util$signif(c(1,4), 1.05) +
  util$signif(c(2,4), 1.07) +
  util$signif(c(3,4), 1.09) +
  geom_hline(yintercept = 0.5, colour = 'black', linetype = "dashed") +
  scale_y_continuous(breaks = c(0, 0.5, 1), limits = c(0, 1.1), 
                     expand = c(0, 0)) +
  labs(y = 'Reward preference') +
  theme_publication +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1),
    axis.ticks.x = element_blank(),
    axis.line.x = element_blank())

p2 <- dall %>%
  filter(info == 'reversal') %>%
  group_by(tag, exp) %>% 
  summarise(measure = length(which(rp > 0 & visitduration > 2))) %>%
  left_join(manimal) %>%
  ggplot(aes(x = substance, y = measure, group = substance)) +
  box_default() + median_default + point_default() +
  util$signif(c(1,2), 3200) +
  util$signif(c(2,4), 3200) +
  scale_y_continuous(limits = c(0, 3300), expand = c(0, 0)) +
  labs(y = 'Total number of attempts\nduring reversals') +
  theme_publication +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.ticks.x = element_blank(),
        axis.line.x = element_blank())

p3 <- result$br %>%
  left_join(manimal) %>%
  ggplot(aes(x = substance, y = value, group = substance)) +
  box_default() + median_default + point_default(point.width = .2) + 
  util$signif(c(1,2), .76) +
  util$signif(c(1,3), .77) +
  util$signif(c(2,4), .78) +
  util$signif(c(3,4), .79) +
  geom_hline(yintercept = 0.5, colour = 'black', linetype = "dashed") +
  scale_y_continuous(breaks = c(0.25, .5, .75), limits = c(0.25, .8), expand = c(0, 0)) +
  labs(y = 'Preference of more \ncertain option') +
  theme_publication +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1),
    axis.ticks.x = element_blank(),
    axis.line.x = element_blank())

fig[[3]] <- (p1 | p2 | p3) + plot_layout(ncol = 3, widths = c(1, 1, 1)) + 
  plot_annotation(tag_levels = "A")

# fig 4 -------------------------------------------------------------------
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

fig[[4]] <- ((p1 | p2) / (p3 | p4)) + plot_annotation(tag_levels = "A")

# fig 5 win stay ----------------------------------------------------------
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

# fig 6 -------------------------------------------------------------------
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

# fig 7 -------------------------------------------------------------------
temp <- list(
  data = util$aictidy(pubmodel) %>% filter(name %in% temp_names$name),
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

# fig 8 -------------------------------------------------------------------
# parameters

p1 <- util$plotpar("basic", "par.beta", c(0,12), gg$show.beta) +
  util$signif(c(1,4), 11) +
  util$signif(c(3,4), 11.5) 
p2 <- util$plotpar("fictitious", "par.beta", c(0,12), gg$show.beta) +
  util$signif(c(1,2),11)+
  util$signif(c(1,4),11.4)+
  util$signif(c(3,4), 11.9)
p3 <- util$plotpar("basic", "par.alpha", c(0,1), c(0, .5, 1))
p4 <- util$plotpar("fictitious", "par.alpha", c(0,1), c(0, .5, 1)) +
  util$signif(c(1,2), .95)

fig[[8]] <- (p1 + p2) / (p3 + p4) + plot_annotation(tag_levels = "A")

# sup 1 time spent in corners ---------------------------------------------
# temp <- list(
#   data = dall %>%
#     filter(info %in% c("adaptation","reversal")) %>%
#     util$binal(hour = 13) %>%
#     group_by(period = bin, tag, exp) %>% 
#     summarise(value = sum(as.numeric(visitduration)/3600), 
#               info = last(info)) %>%
#     left_join(manimal, "tag") %>%
#     mutate(cohort = gg$cohort$label[match(exp, gg$cohort$exp)],
#            gr = paste(substance, cohort, sep = " ")) %>%
#     group_by(period, gr, info) %>%
#     summarise(measure = median(value, na.rm = T),
#               lower = quantile(value, na.rm = T)[2],
#               upper = quantile(value, na.rm = T)[4]) %>%
#     ungroup())
# temp$util = function(x, a=temp$data){filter(a, gr %in% x)}
# temp$plot = function(a){
#   list(geom_ribbon(aes(ymin=lower, ymax=upper, group = gr),
#                    colour = NA, fill = "gray"),
#     geom_hline(yintercept = 1, linetype = "dotted"),
#       geom_point(aes(fill = info), size = gg$point.size, pch = 21),
#       scale_y_continuous(breaks=c(0, 1, 5), limits = c(0, 5),
#                          expand = c(0,0)),
#       scale_x_continuous(breaks=c(0, 15, 35), limits = c(0, 35), 
#                          expand = c(0,0)),
#       scale_fill_manual(values = c('white','darkgray')),
#       facet_wrap(~gr),
#       theme_publication,
#       theme(axis.title.y = element_blank(),
#             legend.position = "none",
#             axis.title.x = element_blank()))}
# 
# p1 <- ggplot(temp$util("alcohol (I)"), aes(x=period, y=measure)) + 
#   temp$plot() + theme(legend.position = c(0.0, 0.80), 
#                         legend.justification = c(0, 0))
# p2 <- ggplot(temp$util("alcohol (II)"), aes(x=period, y=measure)) + 
#   temp$plot()
# p3 <- ggplot(temp$util("saccharin (I)"), aes(x=period, y=measure)) + 
#   temp$plot() + theme(axis.title.y = element_text(angle = 90)) + 
#   labs(y = "Total time spent in cage corners [h]")
# p4 <- ggplot(temp$util("saccharin (III)"), aes(x=period, y=measure)) + 
#   temp$plot()
# p5 <- ggplot(temp$util("alcohol+saccharin (II)"), 
#              aes(x=period, y=measure)) + temp$plot() + 
#   theme(axis.title.x = element_text(hjust = 1)) + labs(x = "Bin (48h)")
# p6 <- ggplot(temp$util("water (IV)"), aes(x=period, y=measure)) + 
#   temp$plot() + theme(axis.title.x = element_text(hjust = 1)) + 
#   labs(x = "Bin (48h)")
# 
# fig[["sup1"]] <- (p1 + p2) / (p3 + p4) / (p5 + p6)

# sup 1 corner occupancy --------------------------------------------------
temp <- list(
  data = dall %>%
    util$binal(hour = 13) %>%
    filter(info %in% c("reversal")) %>%
    filter(rp > 0) %>%
    left_join(manimal, "tag") %>%
    mutate(corner = as.factor(ceiling(corner/2))) %>%
    group_by(period = bin, corner, gr, deviceid) %>%
    summarise(value = as.numeric(sum(visitduration, na.rm = T), 
                                 units = "hours"),
              info = last(info)) %>%
    ungroup())
temp$plot <- function(a){
  a %>% ggplot(aes(x=period, y=value, group = corner)) +
    geom_line(linetype = "dashed") +
    geom_point(aes(fill = corner), size = gg$point.size, pch = 21)+
    scale_y_continuous(breaks=c(0, 12,13), limits = c(0,13),
                       expand = c(0,0))+
    scale_x_continuous(breaks=c(15, 35), limits = c(15, 35),
                       expand = c(0,0))+
    scale_fill_manual(values = c('darkgray',"white"))+
    facet_wrap(~gr, ncol = 2)+
    theme_publication}
temp$util <- function(.gr, a = temp$data){
  a %>% filter(gr %in% .gr)}

p1 <- temp$util(util_stat$cohorts[1]) %>% 
  temp$plot() +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.position = c(0.0, 0.80),
        legend.justification = c(0, 0))

p2 <- temp$util(util_stat$cohorts[2]) %>% 
  mutate(originalvalue = value) %>%
  mutate(value = ifelse(value > 12.1, 12.1, value)) %>%
  temp$plot() +
  geom_text(aes(label = ifelse(value < 12, NA, round(originalvalue, 2))), 
             hjust = 0, nudge_x = 0.4, size = 2)+
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.position = "none")

p3 <- temp$util(util_stat$cohorts[3]) %>% 
  temp$plot() +
  theme(axis.title.x = element_blank(),
        legend.position = "none")+
  labs(y = "Corner occupancy [hours]")

p4 <- temp$util(util_stat$cohorts[4]) %>% 
  temp$plot() +
  theme(axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        legend.position = "none")

p5 <- temp$util(util_stat$cohorts[5]) %>% 
  temp$plot() +
  theme(axis.title.y = element_blank(),
        legend.position = "none")+
  labs(x = "Bin (48h)")

p6 <- temp$util(util_stat$cohorts[6]) %>% 
  temp$plot() +
  theme(axis.title.y = element_blank(),
        legend.position = "none")+
  labs(x = "Bin (48h)")

fig[["sup1"]] <- (p1 + p2) / (p3 + p4) / (p5+ p6)

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

# sup 3 surface -----------------------------------------------------------
## surface ##
name <- "basic"
p1 <- surface[[name]] %>%
  ggplot(aes(alpha, beta, z = nll)) +
  geom_raster(aes(fill = nll), interpolate = TRUE) +
  geom_contour(colour = "black", binwidth = 5) +
  metR::geom_text_contour(aes(z = nll), rotate = F, min.size = 10)+
  theme_publication +
  theme(legend.position = "bottom") +
  scale_x_continuous(breaks = c(0, 0.5, 1), limits = c(0, 1), expand = c(0, 0))+
  scale_y_continuous(breaks = c(0, 1, 5, 10), limits = c(0, NA), expand = c(0, 0)) +
  scale_fill_gradientn(colours = c("white", "black"),
                       limits = gg$limit.nll) +
  coord_flip()+
  theme(legend.position = "none")+
  geom_point(data = (pubmodel[[name]] %>% filter(tag == hero)), 
             aes(x = par.alpha, y = par.beta, z = NA), colour = "black")

name <- "fictitious"
p2 <- surface[[name]] %>%
  ggplot(aes(alpha, beta, z = nll)) +
  geom_raster(aes(fill = nll), interpolate = TRUE) +
  geom_contour(colour = "black", binwidth = 5) +
  metR::geom_text_contour(aes(z = nll), rotate = F, min.size = 10)+
  theme_publication +
  theme(legend.position = "bottom") +
  scale_x_continuous(breaks = c(0, 0.5, 1), limits = c(0, 1), expand = c(0, 0))+
  scale_y_continuous(breaks = c(0, 1, 5, 10), limits = c(0, NA), expand = c(0, 0)) +
  scale_fill_gradientn(colours = c("white", "black"), limits = gg$limit.nll) +
  coord_flip()+
  theme(legend.position = "none")+
  geom_point(data = (pubmodel[[name]] %>% filter(tag == hero)), 
             aes(x = par.alpha, y = par.beta, z = NA), colour = "black")

# lattice::wireframe(nll ~ alpha*beta, data = surface$basic,
#           xlab = "alpha", ylab = "beta",
#           #col.groups=c("grey"),
#           alpha = .5,
#           par.settings = list(axis.line = list(col = "transparent"), 
#                               alpha = 0.7),
#           col.groups=c(rgb(red=255,green=153,blue=102,
#                            alpha=200,maxColorValue=255)),  # Orange
#           #drape = TRUE,
#           scales = list(arrows=FALSE, col="black"),
#           screen = list(z = -45, x = -45))
# 
# persp(temp,xlab = "alpha", ylab = "beta", theta = c(-45, -45), phi = 30, 
# col = c("gray"), axes = TRUE, ticktype = "detailed", border = NA, 
# shade = .7, ltheta = c(.5,.5))
# plot3D::scatter3D(x = surface$basic$alpha, y = surface$basic$beta, z = surface$basic$nll,
#                   pch = 18, col = ramp.col(col = c("lightgrey", "black")))
# 
# plotly::plot_ly() %>% add_surface(z~temp, x = as.numeric(colnames(temp)[-1]),
#                                   y = temp[,1])
# 
# plot_ly() %>% 
#   add_contour(z~temp, coloraxis = 'coloraxis', 
#               contours = list(showlabels = TRUE))

## prediction ##
# basic
temp_basic <- (function(par, a) {
  a = a[with(a, order(start)), ]
  nll = 0
  Q = c(0, 0)
  P <- vector()
  rewards = a$dooropened
  sides = ceiling(a$corner / 2)
  nows = a$start
  probs2 = rep(NA, 79)
  probs = rep(NA, 79)
  for (i in seq_along(sides)) {
    r = rewards[i]
    s = sides[i]
    P = exp(par[2] * Q) / sum(exp(par[2] * Q))
    P = ifelse(P < .001, .001, P)
    P = ifelse(P > .999, .999, P)
    pe = r - Q[s]
    Q[s] = Q[s] + (par[1] * pe)
    probs2[i] <- P[2]
    probs[i] <- P[s]}
  tibble(choice = sides, 
         reward = rewards, 
         time = nows,
         prob = probs,
         prob2 = probs2)})(
           pubmodel[["basic"]] %>% filter(tag == hero) %>%  
             select(grep("par", colnames(.))) %>% unlist(), 
           dhero) %>%
  mutate(id = row_number())

# fictitious
temp_fictitious <- (function(par, a) {
  a = a[with(a, order(start)), ]
  nll = 0
  Q = c(0, 0)
  P <- vector()
  rewards = a$dooropened
  sides = ceiling(a$corner / 2)
  nows = a$start
  probs2 = rep(NA, 79)
  probs = rep(NA, 79)
  for (i in seq_along(sides)) {
    r = rewards[i]
    s = sides[i]
    P = exp(par[2] * Q) / sum(exp(par[2] * Q))
    P = ifelse(P < .001, .001, P)
    P = ifelse(P > .999, .999, P)
    nll = -log(P[s]) + nll
    pe = r - Q[s]
    Q[s] = Q[s] + (par[1] * pe)
    Q[-s] = Q[-s] - (par[1] * pe)
    probs2[i] <- P[2]
    probs[i] <- P[s]}
  tibble(choice = sides, 
         reward = rewards, 
         time = nows,
         prob = probs,
         prob2 = probs2)})(
           pubmodel[["fictitious"]] %>% filter(tag == hero) %>%  
             select(grep("par", colnames(.))) %>% unlist(), 
           dhero)%>%
  mutate(id = row_number())

p3 <- temp_basic %>% select(prob.basic = prob2, reward, 
                                  choice, time, id) %>%
  left_join(temp_fictitious %>% select(prob.fictious = prob2, 
                                       choice, time, id)) %>%
  mutate(choice = choice -1) %>%
  ggplot(aes(x = time, y = choice))+
  geom_point(aes(size = as.factor(reward)), 
             colour = gg$point.colour, pch = 124)+
  geom_step(aes(y = prob.basic), 
            colour = "gray", direction = 'vh')+
  geom_step(aes(y = prob.fictious), 
            colour = "black", direction = 'vh')+
  geom_hline(yintercept = .5, color = "black", linetype = "dotted")+
  theme_publication+
  scale_y_continuous(limits = c(0, 1), breaks = c(0, .5, 1), 
                     expand = c(0,0))+
  scale_x_datetime(expand = c(0.01,0))+
  theme(axis.title.y.right = element_text(color = "black"),
        axis.line.y.right = element_line(color = "black"),
        axis.ticks.y.right = element_line(color = "black"),
        axis.text.y.right = element_text(color = "black"),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank())+
  labs(y = "probability of \nthe upper side choice")+
  scale_size_manual(values = c(3, 6))

fig[["sup3"]] <- (p1 | p2) / (p3) + plot_layout(heights = c(1,.75))+ 
  plot_annotation(tag_levels = "A")
