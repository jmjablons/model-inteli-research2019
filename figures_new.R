#new figures
#2020-02-24

# dependency --------------------------------------------------------------
library(ggplot2)
library(dplyr)
library(tidyr)
library(lubridate)
library(patchwork) 
library(ggbeeswarm)
library(scales)
library(latex2exp)

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

# util --------------------------------------------------------------------

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


sem <- function(x, na.rm = T) {
  stopifnot(is.numeric(x))
  if (na.rm) x = na.omit(x)
  sd(x) / sqrt(length(x))}

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
  cohort = data.frame(exp = c(LETTERS[1:4]), 
                label = c(paste0("(",c("II","III","I","IV"),")"))))

# fig 2 -------------------------------------------------------------------
temp <- list()
temp$data <- dall %>% 
  filter(info %in% c("adaptation", "reversal")) %>%
  left_join(manimal, "tag") %>%
  mutate(cohort = gg$cohort$label[match(exp, gg$cohort$exp)],
         gr = paste(substance, cohort, sep = " ")) %>%
  util$binal(hour = 13) %>%
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

## check
dall %>% 
  filter(info %in% c("adaptation", "reversal")) %>%
  left_join(manimal, "tag") %>%
  mutate(cohort = gg$cohort$label[match(exp, gg$cohort$exp)],
         gr = paste(substance, cohort, sep = " ")) %>%
  util$binal(hour = 13) %>%
  filter(gr == "alcohol (II)") %>% 
  group_by(info, bin) %>% 
  summarise(beg = first(start), n=n()) %>% View()

## check schemes
# dall %>% filter(info == "finish") %>% 
#   left_join(manimal) %>% icager::printscheme() %>% View()
#
# dall %>% filter(info == "finish") %>% 
#   left_join(manimal) %>% group_by(exp, substance, contingency) %>%
#   summarise(n())
#
# dall %>%
#   left_join(manimal) %>%
#   mutate(cohort = temp$label[match(exp, temp$exp)],
#          gr = paste(substance, cohort, sep = " ")) %>%
#   group_by(gr, substance, cohort) %>%
#   summarise(beg = first(start), fin = last(end))
#
# dall %>% filter(info == "welcome" & exp == "D") %>% 
#   left_join(manimal) %>% icager::printscheme() %>% View()

# fig 3 -------------------------------------------------------------------
p1 <- util$getpreference() %>%
  left_join(manimal, "tag") %>%
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
  #util$assign_cohort()
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

hero = "900110000199546" #saccharin
hero0 = "900110000351935" #water

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

# {dmodel %>% 
#     filter(tag %in% hero) %>% 
#     ggplot(aes(x=intervala, group = interaction(stay, dooropened), 
#                colour = interaction(stay, dooropened)))+
#     geom_density()}%>%
#   plotly::ggplotly()

# fig 5 win stay ----------------------------------------------------------
#win-stay lose-shift
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
        #axis.title.y = element_blank(),
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
  scale_y_continuous(limits = c(-1, 1), expand = c(0, 0), 
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

fig[[6]] <- ( (p1 | p2) / (p3 | p4)) + plot_annotation(tag_levels = "A")

(fig[[4]] | fig[[5]] | fig[[6]]) + plot_layout() + 
  plot_annotation(tag_levels = "A")

# fig 7 -------------------------------------------------------------------
temp <- list(
  name = c("random","noisywinstay","basic",  "dual", "fictitious", "hybrid", 
           "attention","forgetful",  "q-decay",
           "q-decay*", "q-decay+", "b-decay","b-decay*", "b-decay+"),
  data = util$aictidy(allmodels),
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
  set = list(general = c("noisywinstay", "basic", "dual", "fictitious", "hybrid", 
                           "forgetful",  "attention",
                         "q-decay", "q-decay*", "q-decay+", 
                      "b-decay","b-decay*", 
                      "b-decay+","random")),
  util = function(sb, set){temp$data %>% 
    filter(substance == sb) %>%
    #mutate(name = factor(name, levels = temp$name, ordered = T)) %>%
    #filter(name %in% temp$set[[set]]) %>%
    ggplot(aes(x = name, y = delta)) + temp$plot})

# general
p1 <- temp$util("alcohol","general") + 
  theme(axis.text.x = element_blank())

p2 <- temp$util("saccharin","general")+ 
  theme(axis.text.x = element_blank())

p3 <- temp$util("alcohol+saccharin","general")+ 
  theme(axis.text.x = element_blank())

p4 <- temp$util("water","general")

fig[[7]] <- (p1 / p2 / p3 / p4) + plot_annotation(tag_levels = "A")    

# fig 8 -------------------------------------------------------------------
# parameters
gg$show.beta <- c(0, 5, 10, 12) 

p1 <- util$plotpar("basic", "beta", gg.limits = c(0,12), gg$show.beta) +
  util$signif(c(1,4), 11) +
  util$signif(c(3,4), 11.5) 
p2 <- util$plotpar("fictitious", "beta", c(0,12), gg$show.beta) +
  util$signif(c(1,2),11)+
  util$signif(c(1,4),11.4)+
  util$signif(c(3,4), 11.9)
p3 <- util$plotpar("basic", "alpha", c(0,1), c(0, .5, 1))
p4 <- util$plotpar("fictitious", "alpha", c(0,1), c(0, .5, 1)) +
  util$signif(c(1,2), .95)

gg$limit.nll <- c(50, 175)

name <- "basic"
p5 <- surface[[name]] %>%
  ggplot(aes(alpha, beta, z = nll)) +
  geom_raster(aes(fill = nll), interpolate = TRUE) +
  geom_contour(colour = "black", binwidth = 5) +
  metR::geom_text_contour(aes(z = nll), rotate = F, min.size = 10)+
  theme_publication +
  theme(legend.position = "bottom") +
  # geom_point(size = gg$point.size, fill = "gray",
  #            data = filter(util_getoptimpoint(name = name), dot == 1), 
  #            colour = "black", pch = 21)+
  scale_x_continuous(breaks = c(0, 0.5, 1), limits = c(0, 1), expand = c(0, 0))+
  scale_y_continuous(breaks = c(0, 1, 5, 10), limits = c(0, NA), expand = c(0, 0)) +
  scale_fill_gradientn(colours = c("white", "black"),
                       limits = gg$limit.nll) +
  coord_flip()+
  theme(legend.position = "none")+
  geom_point(data = (pubmodel[[name]] %>% filter(tag == hero)), 
             aes(x = par.alpha, y = par.beta, z = NA), colour = "black")

name <- "fictitious"
p6 <- surface[[name]] %>%
  ggplot(aes(alpha, beta, z = nll)) +
  geom_raster(aes(fill = nll), interpolate = TRUE) +
  geom_contour(colour = "black", binwidth = 5) +
  metR::geom_text_contour(aes(z = nll), rotate = F, min.size = 10)+
  theme_publication +
  theme(legend.position = "bottom") +
  # geom_point(size = gg$point.size, fill = "gray",
  #            data = filter(util_getoptimpoint(name = name), dot == 1), 
  #            colour = "black", pch = 21)+
  scale_x_continuous(breaks = c(0, 0.5, 1), limits = c(0, 1), expand = c(0, 0))+
  scale_y_continuous(breaks = c(0, 1, 5, 10), limits = c(0, NA), expand = c(0, 0)) +
  scale_fill_gradientn(colours = c("white", "black"), limits = gg$limit.nll) +
  coord_flip()+
  theme(legend.position = "none")+
  geom_point(data = (pubmodel[[name]] %>% filter(tag == hero)), 
             aes(x = par.alpha, y = par.beta, z = NA), colour = "black")

fig[[8]] <- (p1 + p2) / (p3 + p4) / (p5 + p6) + plot_annotation(tag_levels = "A")

# fig 9 -------------------------------------------------------------------

dhero <- dmodel[dmodel$tag == hero,] %>% 
  filter(contingency == 17)

# dmodel %>%
#   #filter(visitduration > 600) %>%
#   mutate(visitduration = visitduration) %>%
#   ggplot(aes(x = visitduration))+
#   geom_density() %>%
#   plotly::ggplotly()

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
    if(P[s] < .001){P[s] = .001}
    if(P[s] > .999){P[s] = .999}
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
dhero)

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
    if(P[s] < .001){P[s] = .001}
    if(P[s] > .999){P[s] = .999}
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
  dhero)

temp_basic %>%
  mutate(choice = ifelse(choice == 2, 0, 1)) %>%
  ggplot(aes(x = time, y = choice))+
  geom_point(aes(size = as.factor(reward)), 
             colour = gg$point.colour, pch = 124)+
  geom_line(data = temp_basic, aes(x = time, y = 1 - prob2), 
            colour = "gray")+
  geom_line(data = temp_fictitious, aes(x = time, y = 1 - prob2), 
            colour = "black")+
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

# sup 1 time spent in corners ---------------------------------------------
temp <- list(
  data = dall %>%
    filter(info %in% c("adaptation","reversal")) %>%
    util$binal(hour = 13) %>%
    group_by(period = bin, tag, exp) %>% 
    summarise(value = sum(as.numeric(visitduration)/3600), 
              info = last(info)) %>%
    left_join(manimal, "tag") %>%
    mutate(cohort = gg$cohort$label[match(exp, gg$cohort$exp)],
           gr = paste(substance, cohort, sep = " ")) %>%
    group_by(period, gr, info) %>%
    summarise(measure = median(value, na.rm = T),
              lower = quantile(value, na.rm = T)[2],
              upper = quantile(value, na.rm = T)[4]) %>%
    ungroup())
temp$util = function(x, a=temp$data){filter(a, gr %in% x)}
temp$plot = function(a){
  list(geom_ribbon(aes(ymin=lower, ymax=upper, group = gr),
                   colour = NA, fill = "gray"),
    geom_hline(yintercept = 1, linetype = "dotted"),
      geom_point(aes(fill = info), size = gg$point.size, pch = 21),
      scale_y_continuous(breaks=c(0, 1, 5), limits = c(0, 5),
                         expand = c(0,0)),
      scale_x_continuous(breaks=c(0, 15, 35), limits = c(0, 35), 
                         expand = c(0,0)),
      scale_fill_manual(values = c('white','darkgray')),
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
  labs(y = "Total time spent in cage corners [h]")
p4 <- ggplot(temp$util("saccharin (III)"), aes(x=period, y=measure)) + 
  temp$plot()
p5 <- ggplot(temp$util("alcohol+saccharin (II)"), 
             aes(x=period, y=measure)) + temp$plot() + 
  theme(axis.title.x = element_text(hjust = 1)) + labs(x = "Bin (48h)")
p6 <- ggplot(temp$util("water (IV)"), aes(x=period, y=measure)) + 
  temp$plot() + theme(axis.title.x = element_text(hjust = 1)) + 
  labs(x = "Bin (48h)")

fig[["sup1"]] <- (p1 + p2) / (p3 + p4) / (p5 + p6)

#fig[[2]] | fig[["sup1"]]

# sup 2 fraction visits ---------------------------------------------------
temp <- list(
  data = dall %>%
    filter(info %in% c("adaptation","reversal")) %>%
    util$binal(hour = 13) %>%
    group_by(period = bin, tag, exp) %>% 
    summarise(value = length(which(rp>0))/n(), 
              info = last(info)) %>%
    left_join(manimal, "tag") %>%
    mutate(cohort = gg$cohort$label[match(exp, gg$cohort$exp)],
           gr = paste(substance, cohort, sep = " ")) %>%
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
gg$limit.nll <- c(50, 175)

name <- "basic"
p1 <- surface[[name]] %>%
  ggplot(aes(alpha, beta, z = nll)) +
  geom_raster(aes(fill = nll), interpolate = TRUE) +
  geom_contour(colour = "black", binwidth = 5) +
  metR::geom_text_contour(aes(z = nll), rotate = F, min.size = 10)+
  theme_publication +
  theme(legend.position = "bottom") +
  # geom_point(size = gg$point.size, fill = "gray",
  #            data = filter(util_getoptimpoint(name = name), dot == 1), 
  #            colour = "black", pch = 21)+
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
  # geom_point(size = gg$point.size, fill = "gray",
  #            data = filter(util_getoptimpoint(name = name), dot == 1), 
  #            colour = "black", pch = 21)+
  scale_x_continuous(breaks = c(0, 0.5, 1), limits = c(0, 1), expand = c(0, 0))+
  scale_y_continuous(breaks = c(0, 1, 5, 10), limits = c(0, NA), expand = c(0, 0)) +
  scale_fill_gradientn(colours = c("white", "black"), limits = gg$limit.nll) +
  coord_flip()+
  theme(legend.position = "none")+
  geom_point(data = (pubmodel[[name]] %>% filter(tag == hero)), 
             aes(x = par.alpha, y = par.beta, z = NA), colour = "black")

# temp <- surface$`bdecay*` %>%
#   mutate(id = row_number()) %>%
#   tidyr::gather(param, value, -id) %>%
# ggplot(aes(x = param, y = value, group = id))+
#   geom_line(alpha = .1)+
#   theme_publication +
#   scale_x_discrete(expand = c(0,0))+
#   theme(axis.title.x = element_blank(),
#         axis.line.x = element_blank(),
#         axis.ticks.x = element_blank())

# library(lattice)
# wireframe(nll ~ alpha*beta, data = surface$basic,
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
# library(plot3D)
# plot3D::scatter3D(x = surface$basic$alpha, y = surface$basic$beta, z = surface$basic$nll,
#                   pch = 18, col = ramp.col(col = c("lightgrey", "black")))
# 
# library(plotly)
# plotly::plot_ly() %>% add_surface(z~temp, x = as.numeric(colnames(temp)[-1]),
#                                   y = temp[,1])
# 
# plot_ly() %>% add_contour(z~temp, coloraxis = 'coloraxis', contours = list(showlabels = TRUE))
# 
# temp <- surface$basic %>%
#   tidyr::spread(beta, nll) %>%
# data.matrix(., rownames.force = NA) 

fig[["sup3"]] <- (p1 + p2) + plot_annotation(tag_levels = "A")

# scale_fill_gradientn( trans = "log10",
#TODO limits = gg$limit.nll

# notebook ----------------------------------------------------------------
#dmodel <- readRDS(file.choose())
#stat_summary(geom = "crossbar", fun.y = "median")+
#geom_violin(fill = "gray", colour = NA)+
#geom_boxplot(alpha = 0.3, outlier.colour = NA) +
#   annotation=c('','*',''), 
#   textsize = 2 * ggplot2:::.pt, 
#   tip_length=0.02, 