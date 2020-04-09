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
                    dat = pubmodel, metadata = manimal, 
                    gg.further = theme_publication){
  if(include){fn = function(pat, x) (grepl(pat, x))
  } else {fn = function(pat, x) !grepl(pat, x)}
  dat[[modelname]] %>%
    tidyr::gather(par, value, -tag) %>%
    filter(grepl('par', par)) %>%
    filter(fn(arg, par)) %>%
    left_join(metadata, by = "tag") %>%
    ggplot(aes(x = substance, y = as.numeric(value))) +
    box_default() + median_default + point_default() +
    facet_wrap(~par, scales = "free_y") + 
    scale_y_continuous(limits = gg.limits, expand = c(0.03,0), 
                       breaks = gg.breaks) + gg.further +
    theme(axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          axis.line.x = element_blank(),
          axis.text.x = element_text(angle = 45, hjust = 1),
          axis.ticks.x = element_blank())}

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


util_signif <- function(x.where, y.where, y.space = 0.1, 
                        colour = "darkgray"){
  list(annotate(geom = "line", x = x.where, y = y.where, colour = colour),
       annotate(geom = "text", label = "*", x = mean(x.where), 
                y = y.where + y.space, colour = colour))}

sem <- function(x, na.rm = T) {
  stopifnot(is.numeric(x))
  if (na.rm) x = na.omit(x)
  sd(x) / sqrt(length(x))}

# visuals -----------------------------------------------------------------
#general visuals
gg <- list()
gg$point.width = 0.2
gg$point.alpha = 1
gg$point.size = 2
gg$point.colour = "black"
gg$box.fill = "lightgray"
gg$box.outline = "black"
gg$ribbon.fill = "lightgray"
gg$ribbon.colour = NA
gg$stay.label = c(`0` = 'after lose', `1` = 'after win')
gg$stay.value = log(c(0.03, 1, 10, 60, 660))
gg$cohort = data.frame(exp = c(LETTERS[1:4]), 
                       label = c(paste0("(",c("II","III","I","IV"),")")))

# fig 2 -------------------------------------------------------------------

tempdata <- dall %>% 
  filter(info %in% c("adaptation", "reversal")) %>%
  left_join(manimal, "tag") %>%
  mutate(cohort = gg$cohort$label[match(exp, gg$cohort$exp)],
         gr = paste(substance, cohort, sep = " ")) %>%
  binal(hour = 13) %>%
  with(., {.$info[
    (gr %in% "alcohol (I)" & bin == 16 & info == "adaptation") |
      (gr %in% "alcohol (II)" & bin == 15 & info == "adaptation") |
      (gr %in% "saccharin (I)" & bin == 16 & info == "adaptation") |
      (gr %in% "saccharin (III)" & bin == 15 & info == "adaptation") |
      (gr %in% "water (IV)" & bin == 15 & info == "adaptation") |
      (gr %in% "alcohol+saccharin (II)" & bin == 15 &
         info == "adaptation")] = "reversal"
    return(.)}) %>%
  group_by(bin, info, gr, tag) %>% 
  summarise(nvisit = n()) %>%
  summarise(measure = median(nvisit, na.rm = T),
            lower = quantile(nvisit, na.rm = T)[2],
          upper = quantile(nvisit, na.rm = T)[4],
            sem = sem(nvisit, na.rm = T)) %>%
  ungroup()

## check
dall %>% 
  filter(info %in% c("adaptation", "reversal")) %>%
  left_join(manimal, "tag") %>%
  mutate(cohort = gg$cohort$label[match(exp, gg$cohort$exp)],
         gr = paste(substance, cohort, sep = " ")) %>%
  binal(hour = 13) %>%
  filter(gr == "alcohol (II)") %>% 
  group_by(info, bin) %>% 
  summarise(beg = first(start), n=n()) %>% View()

temp <- list(
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

p1 <- tempdata %>% filter(gr %in% "alcohol (I)") %>% 
  ggplot(aes(x=bin, y=measure)) + temp + 
  theme(theme(legend.position = c(0.0, 0.80), 
              legend.justification = c(0, 0)), 
        axis.title.x = element_blank(), 
        axis.title.y = element_blank())

p2 <- tempdata %>% filter(gr %in% "alcohol (II)") %>% 
  ggplot(aes(x=bin, y=measure)) + temp +
  theme(legend.position = "none", axis.title.x = element_blank(), 
        axis.title.y = element_blank())

p3 <- tempdata %>% filter(gr %in% "saccharin (I)") %>% 
  ggplot(aes(x=bin, y=measure)) + temp +
  theme(legend.position = "none", axis.title.x = element_blank())

p4 <- tempdata %>% filter(gr %in% "saccharin (III)") %>% 
  ggplot(aes(x=bin, y=measure)) + temp +
  theme(legend.position = "none", axis.title.x = element_blank(), 
        axis.title.y = element_blank())

p5 <- tempdata %>% filter(gr %in% "alcohol+saccharin (II)") %>% 
  ggplot(aes(x=bin, y=measure)) + temp +
  theme(legend.position = "none", axis.title.y = element_blank())

p6 <- tempdata %>% filter(gr %in% "water (IV)") %>% 
  ggplot(aes(x=bin, y=measure)) + temp + 
  theme(legend.position = "none", axis.title.y = element_blank())

fig[[2]] <- (p1 + p2) / (p3 + p4) / (p5 + p6) + 
  plot_annotation(tag_levels = "A")

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

p1 <- getPreference() %>%
  left_join(manimal) %>%
  ggplot(aes(x = substance, y = value, group = substance)) +
  box_default() + median_default + point_default(point.width = .2) +
  util_signif(c(1,2), .91) +
  util_signif(c(1,3), .93) +
  util_signif(c(1,4), .95) +
  util_signif(c(2,4), .97) +
  util_signif(c(3,4), .99) +
  geom_hline(yintercept = 0.5, colour = 'black', linetype = "dashed") +
  scale_y_continuous(breaks = c(0, 0.5, 1), limits = c(0, 1), expand = c(0, 0)) +
  labs(y = 'Reward preference') +
  theme_publication +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1),
    axis.ticks.x = element_blank(),
    axis.line.x = element_blank())

p2 <- dall %>%
  filter(info == 'reversal') %>%
  group_by(tag) %>% 
  summarise(measure = length(which(rp > 0 & visitduration > 2))) %>%
  ungroup() %>%
  left_join(manimal) %>%
  ggplot(aes(x = substance, y = measure, group = substance)) +
  box_default() + median_default + point_default() +
  util_signif(c(1,2), 2900) +
  util_signif(c(2,4), 2800) +
  scale_y_continuous(limits = c(0, 3000), expand = c(0, 0)) +
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
  util_signif(c(1,2), .73) +
  util_signif(c(1,3), .75) +
  util_signif(c(2,4), .77) +
  util_signif(c(3,4), .79) +
  geom_hline(yintercept = 0.5, colour = 'black', linetype = "dashed") +
  scale_y_continuous(breaks = c(0.2, .5, .8), limits = c(0.2, .8), expand = c(0, 0)) +
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

util_format <- function(x){ifelse(x%%1 > 0, 
                as.character(x), as.character(as.integer(x)))}
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
                     labels = util_format(exp(gg$stay.value)),
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
  temp$plot + theme(axis.title.y = element_blank())

p3 <- temp$data(hero0, 1) %>% ggplot(aes(x = log(intervala), y = stay)) + 
  temp$plot + theme(axis.title.x = element_blank())

p4 <- temp$data(hero0, 0) %>% ggplot(aes(x = log(intervala), y = stay)) + 
  temp$plot + theme(axis.title.y = element_blank())

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

util_winstay <- function(sb){
  dmodel %>% left_join(manimal, by = "tag") %>%
    filter(substance %in% sb) %>% 
    filter(intervala <= 2 | intervala >= 10) %>%
    mutate(short = ifelse(intervala <= 2, "<2", ">10")) %>%
    group_by(tag, short) %>%
    summarise(`win-stay` = length(which(dooropened == 1 & stay == 1))/ 
                length(which(dooropened == 1)),
              `lose-shift` = length(which(dooropened == 0 & stay == 0))/
                length(which(dooropened == 0))) %>%
    tidyr::gather(param, value, -tag, -short) %>%
    mutate(param = factor(param, levels = c("win-stay", "lose-shift"), 
                          ordered = T)) %>%
    left_join(manimal)}

p1 <- util_winstay("alcohol") %>%
  ggplot(aes(x = interaction(short, param, sep = " ", lex.order = F), 
             y = value, group = tag)) + temp +
  util_signif(x.where = c(1,2), y.where = 1.06, y.space = 0.01, "black")+
  util_signif(x.where = c(3,4), y.where = 1.06, y.space = 0.01, "black")

p2 <- util_winstay("alcohol+saccharin") %>%
  ggplot(aes(x = interaction(short, param, sep = " ", lex.order = F), 
             y = value, group = tag)) + temp +
  util_signif(x.where = c(1,2), y.where = 1.06, y.space = 0.01, "black")+
  util_signif(x.where = c(3,4), y.where = 1.06, y.space = 0.01, "black")

p3 <- util_winstay("saccharin") %>%
  ggplot(aes(x = interaction(short, param, sep = " ", lex.order = F), 
             y = value)) + temp +
  util_signif(x.where = c(1,2), y.where = 1.06, y.space = 0.01, "black")+
  util_signif(x.where = c(3,4), y.where = 1.06, y.space = 0.01, "black")

p4 <- util_winstay("water") %>%
  ggplot(aes(x = interaction(short, param, sep = " ", lex.order = F), 
             y = value)) + temp +
  util_signif(x.where = c(3,4), y.where = 1.06, y.space = 0.01, "black")

fig[[5]] <- (p1 | p2) / (p3 | p4) + plot_annotation(tag_levels = "A")

# fig stat ----------------------------------------------------------------

anova_result <- aov(formula = value ~ short * substance + Error(tag/(short)), 
    data = util_winstay(c("alcohol", "alcoholsaccharin", "saccharin", "water")) %>%
      filter(param == "win-stay"))

summary(anova_result)


# fig 5 -------------------------------------------------------------------
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

p3 <- result$glm %>%
  filter(grepl("door", predictor, ignore.case = T)) %>%
  filter(sig == 1) %>%
  mutate(predictor = "Reward") %>%
  left_join(manimal) %>%
  ggplot(aes(x = substance, y = estimate, group = substance, 
             colour = sig)) +
  scale_y_continuous(limits = c(-1, 1), expand = c(0, 0), 
                     breaks = c(-1, 0, 1)) +
  temp

p4 <- result$glm %>%
  filter(grepl("intervala", predictor, ignore.case = T)) %>%
  filter(sig == 1) %>%
  mutate(predictor = "Per minute of interval") %>%
  left_join(manimal) %>%
  ggplot(aes(x = substance, y = estimate, group = substance, 
             colour = sig)) + 
   scale_y_continuous(limits = c(-.01, .01), 
                      expand = c(0, 0), breaks = c(-.01, 0, .01)) + 
  temp 

p2 <- result$glm %>%
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
  geom_point(size = gg$point.size, fill = "gray",
             data = filter(util_getoptimpoint(name = name), dot == 1), 
             colour = "black", pch = 21)+
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
  geom_point(size = gg$point.size, fill = "gray",
             data = filter(util_getoptimpoint(name = name), dot == 1), 
             colour = "black", pch = 21)+
  scale_x_continuous(breaks = c(0, 0.5, 1), limits = c(0, 1), expand = c(0, 0))+
  scale_y_continuous(breaks = c(0, 1, 5, 10), limits = c(0, NA), expand = c(0, 0)) +
  scale_fill_gradientn(colours = c("white", "black"), 
                       limits = gg$limit.nll) +
  coord_flip()

fig[[7]] <- p1 + p2 + plot_annotation(tag_levels = "A")

# scale_fill_gradientn( trans = "log10",
#TODO limits = gg$limit.nll

# fig 7 -------------------------------------------------------------------
temp <- list(
  name = c("basic", "random", "attention", "dual", "fictitious", "hybrid", 
           "forgetful", "noisywinstay", "q-decay","q-decay*", "q-decay+", 
           "b-decay","b-decay*", "b-decay+", "relational"),
  data = util_aictidy(pubmodel),
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
                #labels = scientific_format(),
                labels = TeX(c(paste("$-10^{",1:4, "}$", sep = ""), 
                               paste("$10^{",1:4, "}$", sep = ""))))),
  set = list(general = c("random", "dual", "fictitious", "hybrid", 
                         "noisywinstay", "forgetful",  "attention"),
             time = c("q-decay", "q-decay*", "q-decay+", 
                      "b-decay","b-decay*", 
                      "b-decay+", "relational")),
  util = function(sb, set){temp$data %>%
    filter(substance == sb) %>%
    mutate(name = factor(name, levels = temp$name, ordered = T)) %>%
    filter(name %in% temp$set[[set]]) %>%
    ggplot(aes(x = name, y = delta)) + temp$plot})

# general
p1 <- temp$util("alcohol","general") + 
  theme(axis.text.x = element_blank())

p2 <- temp$util("saccharin","general")+ 
  theme(axis.text.x = element_blank())

p3 <- temp$util("alcohol+saccharin","general")+ 
  theme(axis.text.x = element_blank())

p4 <- temp$util("water","general")

#time-dependent
p5 <- temp$util("alcohol","time")+ 
  theme(axis.text.x = element_blank())

p6 <- temp$util("saccharin","time")+ 
  theme(axis.text.x = element_blank())

p7 <- temp$util("alcohol+saccharin","time")+ 
  theme(axis.text.x = element_blank())

p8 <- temp$util("water","time")

fig[[8]] <- ((p1 | p5) / (p2 | p6) / (p3 | p7) / (p4 | p8)) +
  plot_annotation(tag_levels = "A")    

#fig[["8.previous"]] <- fig[[8]]

# fig 8 -------------------------------------------------------------------
# parameters
gg$show.beta <- c(0, 5, 10, 12) 

p1 <- plotpar("basic", "alpha", c(0,1), c(0, .5, 1))
p2 <- plotpar("fictitious", "alpha", c(0,1), c(0, .5, 1)) +
  util_signif(c(1,2), .95) 
p3 <- plotpar("b-decay*", "alpha", c(0,1), c(0, .5, 1))+
  util_signif(c(1,2), .95)
p4 <- plotpar("basic", "beta", c(0,12), gg$show.beta) +
  util_signif(c(1,4), 11) +
  util_signif(c(3,4), 11.5) 
p5 <- plotpar("fictitious", "beta", c(0,12), gg$show.beta) +
  util_signif(c(1,2),11)+
  util_signif(c(1,4),11.4)+
  util_signif(c(3,4), 11.9) 
p6 <- plotpar("b-decay*", "beta", c(0,12), gg$show.beta)+
  util_signif(c(3,4), 11)
p7 <- plotpar("b-decay*", "bdecay", c(0,.2), c(0, .2))+
  util_signif(c(3,4), .17, y.space = .01) 

fig[[9]] <- (p1 + p4) / (p2 + p5) / 
  (p3 + p6) / (p7 + p7) + plot_annotation(tag_levels = "A")

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

dummy <- (function(par, a) {
  a = a[with(a, order(start)), ]
  Q = c(0, 0)
  t = 0
  P <- vector()
  rewards = a$dooropened
  sides = ceiling(a$corner/2)
  nows.start = a$start
  nows.end = a$end
  beta.zero = par[2]
  dif = nows.start[1]
  output <- list()
  x = 1
  for (i in seq_along(sides)) {
    now.start = nows.start[i]
    now.end = nows.end[i]
    s = sides[i]
    times = seq(from = dif, to = now.start, by = "10 sec")
    for(d in seq_along(times)){
      t = as.numeric(difftime(now.start, times[d], units = 'mins'))
      t = ifelse(t > 660, 660, t)
      beta = exp( -t * par[3] ) * beta.zero
      P = exp(beta * Q) / sum(exp(beta * Q))
      if(P[s] < .001){P[s] = .001}
      if(P[s] > .999){P[s] = .999}
      output[[x]] = tibble(time = times[d], pside = P[s],
                           pfirst = P[1], psecond = P[2])
      x = x + 1}
    r = rewards[i]
    pe = r - Q[s]
    Q[s] = Q[s] + (par[1] * pe)
    Q[-s] = Q[-s] - (par[1] * pe)
    dif = now.end}
  return(output)})(
    #c(.5, 2, 0.5),
    pubmodel[["b-decay*"]] %>% filter(tag == hero) %>%
      select(grep("par", colnames(.))) %>% unlist(),
    dhero) %>% bind_rows()

p1 <- temp_basic %>%
  mutate(choice = ifelse(choice == 2, 0, 1)) %>%
  ggplot(aes(x = time, y = choice))+
  geom_point(aes(size = as.factor(reward)), 
             colour = gg$point.colour, pch = 124)+
  geom_line(data = temp_basic, aes(x = time, y = 1 - prob2), 
            colour = "gray", linetype = "dashed")+
  geom_line(data = temp_fictitious, aes(x = time, y = 1 - prob2), 
            colour = "black", linetype = "dashed")+
  geom_line(data = dummy, aes(x = time, y = 1 - psecond), 
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
  labs(y = "probability of choice 1")+
  scale_size_manual(values = c(3, 6))

# sup 1 time spent in corners ---------------------------------------------
temp <- list(
  data = dall %>%
    filter(info %in% c("adaptation","reversal")) %>%
    binal(hour = 13) %>%
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
    binal(hour = 13) %>%
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

# sup 3 drinking episodes -------------------------------------------------
temp <- list(
    data = dall %>%
      filter(info %in% c("adaptation","reversal")) %>%
      binal(hour = 13) %>%
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

# notebook ----------------------------------------------------------------
#dmodel <- readRDS(file.choose())
#stat_summary(geom = "crossbar", fun.y = "median")+
#geom_violin(fill = "gray", colour = NA)+
#geom_boxplot(alpha = 0.3, outlier.colour = NA) +
#   annotation=c('','*',''), 
#   textsize = 2 * ggplot2:::.pt, 
#   tip_length=0.02, 