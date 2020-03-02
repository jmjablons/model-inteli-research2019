#new figures
#2020-02-24

# dependency --------------------------------------------------------------
library(ggplot2)
library(dplyr)
library(tidyr)
library(lubridate)
library(patchwork) 
library(ggbeeswarm)

# custom ------------------------------------------------------------------

binal <- function(input, how.long = "2 days", hour = 1, allow.group = T) {
    input = dplyr::arrange(input, start, .by_group = allow.group)
    input$bin = NA
    for(exp in unique(input$exp)){
      input$bin[input$exp == exp] <- 
        cut(input$start[input$exp == exp],
            breaks = as.POSIXct(seq(from = as.POSIXct(paste0(
              strftime(input$start[input$exp == exp][1], 
                       format = '%Y-%m-%d'), hour, ':00:00')),
              to = input$end[input$exp == exp][nrow(input[input$exp == exp,])], 
              by = how.long)),labels = FALSE)} 
    input}

plotpar <- function(modelname, arg = 'par', 
                    gg.limits = c(0,1), gg.breaks = c(0,1), 
                    include = TRUE,
                    dat = rmodel, metadata = manimal, 
                    gg.further = theme_publication){
  if(include){fn = function(pat, x) (grepl(pat, x))
  } else {fn = function(pat, x) !grepl(pat, x)}
  dat[[modelname]] %>%
    tidyr::gather(par, value, -tag) %>%
    filter(grepl('par', par)) %>%
    filter(fn(arg, par)) %>%
    left_join(metadata, by = "tag") %>%
    ggplot(aes(x = substance, y = as.numeric(value))) +
    box_default + median_default + point_default +
    facet_wrap(~par, scales = "free_y") + 
    scale_y_continuous(limits = gg.limits, expand = c(0,0), 
                       breaks = gg.breaks) + gg.further +
    theme(axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          axis.line.x = element_blank(),
          axis.text.x = element_text(angle = 45, hjust = 1),
          axis.ticks.x = element_blank())}


# util --------------------------------------------------------------------


box_default <- stat_summary(geom = "crossbar", fill = gg$fill.box, colour = gg$outline.box,
    fun.data = function(x) {data.frame(y = median(x), ymin = quantile(x)[2], 
                 ymax = quantile(x)[4])})
  
median_default <- stat_summary(geom = "crossbar", colour = gg$colour.median, fill = NA,
      fun.data = function(x) {data.frame(y = median(x),ymin = median(x),ymax = median(x))})

point_default <- geom_quasirandom(size = gg$point.size, 
                                  width = gg$point.width, 
                                  colour = gg$point.colour, 
                   alpha = gg$point.alpha, stroke = 0, shape=16, 
                   method = "tukeyDense", varwidth = TRUE)

# visuals -----------------------------------------------------------------
#general visuals
gg <- list()
gg$point.width = 0.1
gg$point.alpha = 1
gg$point.size = 2
gg$point.colour = "black"
gg$fill.box = "lightgray"
gg$colour.median = "black"
gg$outline.box = NA
gg$ribbon.fill = "lightgray"
gg$ribbon.colour = NA

gg$stay.label = c(`0` = 'after lose', `1` = 'after win')
gg$stay.value = log(c(0.03, 1, 10, 60, 660))


# fig 2 -------------------------------------------------------------------

#number of visits
fig[[2]] <- dall %>% 
  binal() %>%
  filter(info != "finish") %>%
  mutate(info = ifelse(info %in% "welcome", "adaptation", info)) %>%
  group_by(period = bin, tag, info) %>% 
  summarise(nvisit = n()) %>%
  left_join(manimal) %>%
  group_by(period, info, exp, substance) %>%
  summarise(measure = mean(nvisit, na.rm = T),
    sem = sem(nvisit, na.rm = T)) %>%
  ungroup()%>%
  mutate(gr = paste(substance, exp, sep = " ")) %>%
  # plot #
  ggplot(aes(x=period, y=measure)) + 
  geom_ribbon(aes(ymin=measure-sem, ymax=measure+sem, group = gr), 
              fill = gg$ribbon.fill, colour = gg$ribbon.colour)+ 
  geom_point(size = gg$point.size, aes(fill = info), pch = 21, colour = gg$point.colour)+
  geom_hline(yintercept = 200, linetype = 'dotted')+
  scale_y_continuous(breaks=c(0, 200, 400), limits = c(0, 400), expand = c(0,0))+
  scale_x_continuous(breaks=c(0, 15, 35), limits = c(0, 35), expand = c(0,0))+
  ylab('Number of visits')+
  xlab('Period of 48h')+
  scale_fill_manual(values = c('white','darkgray'))+
  facet_wrap(~gr, ncol = 2)+ 
  theme_publication+
  theme(legend.position = "bottom")


# fig 3 -------------------------------------------------------------------

p1 <- dall %>%
  filter(info == 'reversal') %>%
  group_by(tag) %>% 
  summarise(measure = length(which(rp > 0 & visitduration > 2))) %>%
  left_join(manimal) %>%
  ggplot(aes(x = substance, y = measure, group = substance)) +
  box_default + median_default + point_default +
  scale_y_continuous(limits = c(0, 3000), expand = c(0, 0)) +
  labs(y = 'Number of attempts') +
  theme_publication +
  theme(axis.title.x = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.ticks.x = element_blank(),
    axis.line.x = element_blank())

p2 <- getPreference() %>%
  left_join(manimal) %>%
  ggplot(aes(x = substance, y = value, group = substance)) +
  box_default + median_default + point_default +
  geom_hline(yintercept = 0.5, colour = 'black', linetype = "dashed") +
  scale_y_continuous(breaks = c(0, 0.5, 1), limits = c(0, 1), expand = c(0, 0)) +
  labs(y = 'Reward preference') +
  theme_publication +
  theme(axis.title.x = element_blank(), 
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.ticks.x = element_blank(),
    axis.line.x = element_blank())

p3 <- result$br %>%
  left_join(manimal) %>%
  ggplot(aes(x = substance, y = value, group = substance)) +
  box_default + median_default + point_default +
  geom_hline(yintercept = 0.5, colour = 'black', linetype = "dashed") +
  scale_y_continuous(breaks = c(0.2, .4, .6, .8), limits = c(0.2, .8), expand = c(0, 0)) +
  labs(y = 'Preference of more certain option') +
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

temp <- {list(
  geom_quasirandom(size = gg$point.size,
                   groupOnX = FALSE,
                   stroke = 0,
                   shape = 16,
                   colour = gg$point.colour,
                   alpha = 0.2,
                   width = 0.05,
                   method = "tukeyDense"),
  geom_hline(yintercept = 0.5, linetype = 'dotted'),
  geom_smooth(method = "glm",
              method.args = list(family = "binomial"),
              se = TRUE,
              colour = 'darkgray'),
  scale_x_continuous(limits = c(-3.507, 7),
                     breaks = gg$stay.value,
                     labels = format(exp(gg$stay.value),
                                     digits = 2, trim = T),
                     expand = c(0, 0)),
  scale_y_continuous(breaks = c(0, 0.5, 1),
                     labels = c(0, 0.5, 1),
                     expand = c(0.02, 0.02)),
  facet_wrap( ~ dooropened, labeller = 
                labeller(dooropened = gg$stay.label)),
  labs(y = 'probability of stay', x = 'log(interval) [min]'),
  theme_publication,
  theme(panel.spacing = unit(1, "lines"),
        axis.line.x = element_blank(),
        axis.text.y = element_text(face = 'bold')))}

p1 <- dmodel %>% filter(tag == hero) %>% filter(dooropened == 1) %>%
  ggplot(aes(x = log(intervala), y = stay)) + temp

p2 <- dmodel %>% filter(tag == hero) %>% filter(dooropened == 0) %>%
  ggplot(aes(x = log(intervala), y = stay)) + temp

p3 <- dmodel %>% filter(tag == hero0) %>% filter(dooropened == 1) %>%
  ggplot(aes(x = log(intervala), y = stay)) + temp

p4 <- dmodel %>% filter(tag == hero0) %>% filter(dooropened == 0) %>%
  ggplot(aes(x = log(intervala), y = stay)) + temp

fig[[4]] <- (p1 | p2) / (p3 | p4) + plot_annotation(tag_levels = "A")


# fig 5 win stay ----------------------------------------------------------
#win-stay lose-shift

temp <- list(point_default, 
             geom_line(aes(group = interaction(tag, param)), alpha = .4, 
                       colour = "darkgray"),
             scale_y_continuous(expand = c(0,0), limits = c(0,1), 
                                breaks = c(0, .5, 1)), 
             facet_wrap(~substance), theme_publication, 
             theme(axis.title.x = element_blank(),
                   axis.title.y = element_blank(),
                   axis.line.x = element_blank(),
                   axis.text.x = element_text(angle = 45, hjust = 1),
                   axis.ticks.x = element_blank()))

util_winstay <- function(sb){
  dmodel %>% left_join(manimal, by = "tag") %>%
    filter(substance %in% sb) %>% 
    filter(intervalb <= 2 | intervalb >= 10) %>%
    mutate(short = ifelse(intervalb <= 2, "[<2]", "[>10]")) %>%
    group_by(tag, short) %>%
    summarise(winstay = length(which(dooropened == 1 & stay == 1))/ 
                length(which(dooropened == 1)),
              loseshift = length(which(dooropened == 0 & stay == 0))/
                length(which(dooropened == 0))) %>%
    tidyr::gather(param, value, -tag, -short) %>%
    mutate(param = factor(param, levels = c("winstay", "loseshift"), 
                          ordered = T)) %>%
    left_join(manimal)}

p1 <- util_winstay("alcohol") %>%
  ggplot(aes(x = interaction(short, param, sep = " ", lex.order = F), 
             y = value)) + temp

p2 <- util_winstay("alcoholsaccharin") %>%
  ggplot(aes(x = interaction(short, param, sep = " ", lex.order = F), 
             y = value)) + temp

p3 <- util_winstay("saccharin") %>%
  ggplot(aes(x = interaction(short, param, sep = " ", lex.order = F), 
             y = value)) + temp

p4 <- util_winstay("water") %>%
  ggplot(aes(x = interaction(short, param, sep = " ", lex.order = F), 
             y = value)) + temp
  

fig[[5]] <- (p1 | p2) / (p3 | p4) + plot_annotation(tag_levels = "A")

# fig 5 -------------------------------------------------------------------
# glm results
temp <- {list(box_default, median_default, point_default,
  geom_hline(yintercept = 0, linetype = "dotted"),
  facet_wrap(~predictor, scales = "free_y"),
  theme_publication,
  theme(legend.position = 'none',
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_line(linetype = 'dashed'),
        axis.line.x = element_blank()))}

p1 <- result$glm2 %>%
  filter(grepl("intercept", predictor, ignore.case = T)) %>%
  filter(sig == 1) %>%
  left_join(manimal) %>%
  ggplot(aes(x = substance, y = estimate, group = substance, colour = sig)) +
  scale_y_continuous(limits = c(-4, 4), expand = c(0, 0), 
                     breaks = c(-4, -2, 0, 2, 4)) + temp

p2 <- result$glm2 %>%
  filter(grepl("door", predictor, ignore.case = T)) %>%
  filter(sig == 1) %>%
  left_join(manimal) %>%
  ggplot(aes(x = substance, y = estimate, group = substance, colour = sig)) +
  scale_y_continuous(limits = c(-1, 1), expand = c(0, 0), breaks = c(-1, 0, 1)) +
  temp

p3 <- result$glm2 %>%
  filter(grepl("intervala", predictor, ignore.case = T)) %>%
  filter(sig == 1) %>%
  left_join(manimal) %>%
  ggplot(aes(x = substance, y = estimate, group = substance, 
             colour = sig)) + box_default + median_default + 
  scale_y_continuous(limits = c(-.01, .01), expand = c(0, 0), 
                     breaks = c(-.01, 0, .01)) + temp 

p4 <- result$glm2 %>%
  filter(grepl("corner", predictor, ignore.case = T)) %>%
  filter(sig == 1) %>%
  left_join(manimal) %>%
  ggplot(aes(x = substance, y = estimate, group = substance, 
             colour = sig)) + box_default + median_default + 
  scale_y_continuous(limits = c(-7, 7), expand = c(0, 0), 
                     breaks = c(-7, -3, 0, 3, 7))+ temp

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

fig[[6]] <- ( (p1 | p2) / (p3 | p4)) + plot_layout() + 
  plot_annotation(tag_levels = "A")


# fig 6 -------------------------------------------------------------------

util_getoptimpoint <- function(name, limit.beta = 10, 
                               substances = "saccharin", 
                               metadata = manimal){
  rmodel[[name]] %>% left_join(metadata) %>% 
    filter(substance %in% substances) %>% 
    filter(par.beta < limit.beta) %>% 
    rename(alpha = par.alpha, beta = par.beta) %>% 
    mutate(dot = 1, average = as.double(NA))}

util_getavsurface <- function(obj){
  obj = bind_rows(obj) %>% as_tibble()
  obj2 = obj %>% tidyr::spread(tag, nll)
  obj2$average <- apply(obj2, 1, function(x){mean(x[3:length(x)])})
  obj2 = obj2 %>% select(alpha, beta, average) 
  where.dot <- obj %>% group_by(tag) %>% 
    mutate(dot = ifelse(nll == min(nll), 1L, 0L)) %>% ungroup() %>% 
    unique() %>% filter(dot > 0)
  left_join(obj2, select(where.dot, alpha, beta, dot), 
            by = c("alpha","beta"))}

# temp <- surfacebasic %>% bind_rows()
# temp <- temp[temp$beta <= 10,]
# temp$nll %>% range()
#gg$optimpoint <- util_getoptimpoint("fictitious")
gg$limit.nll <- c(10, 3500)

name <- "basic"
p1 <- util_getavsurface(surfacebasic) %>% 
  ggplot(aes(alpha, beta, z = average)) +
  geom_raster(aes(fill = average)) +
  geom_contour(colour = 'white', binwidth = 100) +
  theme_publication +
  theme(legend.position = "bottom") +
  #geom_point(size = gg$point.size, data = function(x) {filter(x, dot == 1)}, colour = "black", shape = 21)+
  geom_point(size = gg$point.size, data = filter(util_getoptimpoint(name = name), dot == 1), 
             colour = "black", shape = 4)+
  scale_x_continuous(breaks = c(0, 0.5, 1), limits = c(0, 1), expand = c(0, 0))+
  scale_y_continuous(breaks = c(0, 1, 5, 10), limits = c(0, NA), expand = c(0, 0)) +
  scale_fill_gradientn(colours = c("white", "black"),
                       limits = gg$limit.nll) +
  coord_flip()

name <- "fictitious"
p2 <- util_getavsurface(surfacefictitious) %>% 
  ggplot(aes(alpha, beta, z = average)) +
  geom_raster(aes(fill = average)) +
  geom_contour(colour = 'white', binwidth = 100) +
  theme_publication +
  theme(legend.position = "bottom") +
  #geom_point(size = gg$point.size, data = function(x) {filter(x, dot == 1)}, colour = "black", shape = 21)+
  geom_point(size = gg$point.size, data = filter(util_getoptimpoint(name = name), dot == 1), 
             colour = "black", shape = 4)+
  scale_x_continuous(breaks = c(0, 0.5, 1), limits = c(0, 1), expand = c(0, 0))+
  scale_y_continuous(breaks = c(0, 1, 5, 10), limits = c(0, NA), expand = c(0, 0)) +
  scale_fill_gradientn(colours = c("white", "black"), 
                       limits = gg$limit.nll) +
  coord_flip()

fig[[7]] <- p1 + p2 + plot_annotation(tag_levels = "A")

# scale_fill_gradientn( trans = "log10",
#TODO limits = gg$limit.nll

# fig 7 -------------------------------------------------------------------

temp <- c("basic", "basic4arm", "zero", "dual", "fictitious", "hybrid", 
          "forgetful", "noisywinstay", "noisywinstay+", "attention", 
          "attention+", "decay", "decay*", "decay+", "decay++",
          "puzzlement", "puzzlement*", "puzzlement+", "puzzlement+*", 
          "reproval", "betadown", "betadown-", "betadown_")

temp2 <- list(
  box_default, median_default, point_default, 
  geom_hline(yintercept = 0, linetype = "dotted"),
  theme_publication, labs(x = element_blank()),
  theme(panel.spacing = unit(2, "lines"),
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.ticks.y = element_line(linetype = 'dashed'),
        axis.ticks.x = element_blank(),
        axis.line.x = element_blank(),
        legend.position = "none"),
  facet_wrap(~ substance, ncol = 1),
  scale_y_continuous(trans = 'asinh', expand = c(0,0), 
                     limits = c(-12^3, 12^3),
                     breaks = c(-(12^(1:9)), 12^(1:9)),
                     labels = as.character(c(-(1:9), (1:9)))))

# general
p1 <- aictidy %>%
  filter(substance == "alcohol") %>%
  mutate(name = factor(name, levels = temp, ordered = T)) %>%
  filter(name %in% c("zero", "dual", "fictitious", "hybrid", 
                     "noisywinstay", "forgetful",  "attention")) %>%
  ggplot(aes(x = name, y = delta, fill = name)) + temp2

p2 <- aictidy %>%
  filter(substance == "saccharin") %>%
  mutate(name = factor(name, levels = temp, ordered = T)) %>%
  filter(name %in% c("zero", "dual", "fictitious", "hybrid", 
                     "noisywinstay", "forgetful",  "attention")) %>%
  ggplot(aes(x = name, y = delta, fill = name)) + temp2

p3 <- aictidy %>%
  filter(substance == "alcoholsaccharin") %>%
  mutate(name = factor(name, levels = temp, ordered = T)) %>%
  filter(name %in% c("zero", "dual", "fictitious", "hybrid", 
                     "noisywinstay", "forgetful",  "attention")) %>%
  ggplot(aes(x = name, y = delta, fill = name)) + temp2

p4 <- aictidy %>%
  filter(substance == "water") %>%
  mutate(name = factor(name, levels = temp, ordered = T)) %>%
  filter(name %in% c("zero", "dual", "fictitious", "hybrid", 
                     "noisywinstay", "forgetful",  "attention")) %>%
  ggplot(aes(x = name, y = delta, fill = name)) + temp2

#time-dependent
p5 <- aictidy %>%
  filter(substance == "alcohol") %>%
  mutate(name = factor(name, levels = temp, ordered = T)) %>%
  filter(name %in% c("decay", "reproval", "decay*","decay+", 
                     "puzzlement", "puzzlement*", "puzzlement+")) %>%
  ggplot(aes(x = name, y = delta, fill = name)) + temp2

p6 <- aictidy %>%
  filter(substance == "saccharin") %>%
  mutate(name = factor(name, levels = temp, ordered = T)) %>%
  filter(name %in% c("decay", "reproval", "decay*","decay+", 
                     "puzzlement", "puzzlement*", "puzzlement+")) %>%
  ggplot(aes(x = name, y = delta, fill = name)) + temp2

p7 <- aictidy %>%
  filter(substance == "alcoholsaccharin") %>%
  mutate(name = factor(name, levels = temp, ordered = T)) %>%
  filter(name %in% c("decay", "reproval", "decay*","decay+", 
                     "puzzlement", "puzzlement*", "puzzlement+")) %>%
  ggplot(aes(x = name, y = delta, fill = name)) + temp2

p8 <- aictidy %>%
  filter(substance == "water") %>%
  mutate(name = factor(name, levels = temp, ordered = T)) %>%
  filter(name %in% c("decay", "reproval", "decay*","decay+", 
                     "puzzlement", "puzzlement*", "puzzlement+")) %>%
  ggplot(aes(x = name, y = delta, fill = name)) + temp2

fig[[8]] <- ((p1 | p5) / (p2 | p6) / (p3 | p7) / (p4 | p8)) +
  plot_annotation(tag_levels = "A")    

# fig 8 -------------------------------------------------------------------
# parameters
gg$show.beta <- c(0, 5, 10, 25, 50) 

util_signif <- function(x.where, y.where, y.space = 0.1, 
                        colour = "darkgray"){
  list(annotate(geom = "line", x = x.where, y = y.where, colour = colour),
    annotate(geom = "text", label = "*", x = mean(x.where), 
             y = y.where + y.space, colour = colour))}

fig[[9]] <- (
  (
    plotpar("basic", "beta", c(0,50), gg$show.beta) +
      util_signif(c(1,4), 49)+
      util_signif(c(3,4), 45) | 
      plotpar("basic", "alpha", c(0,1), c(0, .5, 1)) |
      patchwork::plot_spacer() |
      plotpar("puzzlement", "beta", c(0,50), gg$show.beta)+
      util_signif(c(1,4), 49)+
      util_signif(c(3,4), 48) |
      plotpar("puzzlement", "bdecay", c(0,.2), c(0, .2))+
      util_signif(c(1,2), .17, colour = "green")+
      util_signif(c(1,3), .18) |
      plotpar("puzzlement", "alpha", c(0,1), c(0, .5, 1))
  )
  / 
    (
      plotpar("fictitious", "beta", c(0,50), gg$show.beta) +
        util_signif(c(1,2),47)+
        util_signif(c(1,4), 49)+
        util_signif(c(2,4), 48) | 
        plotpar("fictitious", "alpha", c(0,1), c(0, .5, 1)) + 
        util_signif(c(1,2), .95)+
        util_signif(c(1,3), .9, colour = "green") |
        patchwork::plot_spacer() |
        plotpar("puzzlement*", "beta", c(0,50), gg$show.beta)+
        util_signif(c(1,4), 47) |
        plotpar("puzzlement*", "bdecay", c(0,.2), c(0, .2))+
        util_signif(c(1,4), .18)+
        util_signif(c(3,4), .17)+
        util_signif(c(2,3), .16, colour = "green") |
        plotpar("puzzlement*", "alpha", c(0,1), c(0, .5, 1))+
        util_signif(c(1,4), .9)+
        util_signif(c(3,4), .8)
    )
) + plot_annotation(tag_levels = "A")

# fig 9 -------------------------------------------------------------------

dhero <- dmodel[dmodel$tag == hero,] %>% 
  filter(contingency == 17)

temp <- (function(par, a) {
  nll = 0
  a = a[with(a, order(start)), ]
  Q = c(0, 0)
  date = rep(a$start[1], 2)
  t = c(0, 0)
  P <- vector()
  rewards = a$dooropened
  sides = ceiling(a$corner/2)
  nows = a$start
  beta.zero = par[2]
  output <- list()
  prob = c()
  nlls = c()
  for (i in seq_along(sides)) {
    r = rewards[i]
    s = sides[i]
    now = nows[i]
    t = as.numeric(difftime(now, date, units = 'mins'))
    date[s] = now
    beta = exp( -(t[s]) * par[3] ) * beta.zero
    P = exp(beta * Q) / sum(exp(beta * Q))
    if(P[s] < .001){P[s] = .001}
    if(P[s] > .999){P[s] = .999}
    nll = -log(P[s]) + nll
    pe = r - Q[s]
    Q[s] = Q[s] + (par[1] * pe)
    prob[i] <- P[2]
    nlls[i] <- nll}
  tibble(choice = lead(sides), reward = lead(rewards), 
         time = lead(nows), probability = prob, like = nlls)})(
           rmodel[["puzzlement"]] %>% filter(tag == hero) %>%
             select(grep("par", names(.))) %>% unlist(),
           dhero)

dummy <- (function(par, a) {
  nll = 0
  a = a[with(a, order(start)), ]
  Q = c(0, 0)
  date = rep(a$start[1], 2)
  t = c(0, 0)
  P <- vector()
  rewards = a$dooropened
  sides = ceiling(a$corner/2)
  nows = a$start
  beta.zero = par[2]
  output <- list()
  beta = 1
  x <- 1
  for (i in seq_along(sides[-(1)])) {
    r = rewards[i]
    s = sides[i]
    now = nows[i]
    #dates <- seq(nows[(i-1)], nows[i], 30)
    dif = date
    while(any(dif < nows[(i+1)])){
      P = exp(beta * Q) / sum(exp(beta * Q))
      if(P[s] < .001){P[s] = .001}
      if(P[s] > .999){P[s] = .999}
      dif = dif + lubridate::seconds(30)
      t = as.numeric(difftime(now, date + dif, units = 'mins'))
      beta = exp( -(t[s]) * par[3] ) * beta.zero
      output[[x]] <- tibble(time = dif, prob1 = P[1], prob2 = P[2], 
                            probside = P[s], probdiv = ifelse(s > 1, P[s], -P[s]))
      x = x + 1}
    date[s] = now
    pe = r - Q[s]
    Q[s] = Q[s] + (par[1] * pe)}
  output})(rmodel[["puzzlement"]] %>% filter(tag == hero) %>%
             select(grep("par", names(.))) %>% unlist(),
           dhero)
dummy = bind_rows(dummy) %>% mutate(time = lubridate::as_datetime(time))

temp %>%
  mutate(choice = ifelse(choice == 2, 1, 0)) %>%
  ggplot(aes(x = time, y = choice))+
  geom_point(size = gg$point.size, aes(shape = as.factor(reward)), 
             colour = "gray")+
  geom_line(data = dummy, aes(x = time, y = prob2), colour = "black")+
  theme_publication+
  scale_y_continuous(limits = c(0,1), breaks = c(0, 1),
    sec.axis = sec_axis(~., breaks = c(0, .5, 1), 
                        name = "probability of correct predction"))+
  scale_x_datetime(expand = c(0,0))+
  theme(axis.title.y.right = element_text(color = "black"),
        axis.line.y.right = element_line(color = "black"),
        axis.ticks.y.right = element_line(color = "black"),
        axis.text.y.right = element_text(color = "black"),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank())

fig[[10]]

# notebook ----------------------------------------------------------------
#dmodel <- readRDS(file.choose())
#stat_summary(geom = "crossbar", fun.y = "median")+
#geom_violin(fill = "gray", colour = NA)+
#geom_boxplot(alpha = 0.3, outlier.colour = NA) +
#   annotation=c('','*',''), 
#   textsize = 2 * ggplot2:::.pt, 
#   tip_length=0.02, 