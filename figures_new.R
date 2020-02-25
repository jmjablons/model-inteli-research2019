#new figures
#2020-02-24

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

plotpar <- function(modelname, arg = 'par', include = TRUE,
                      dat = rmodel, metadata = manimal, gg.further = NULL){
  if(include){fn = function(pat, x) (grepl(pat, x))
  } else {fn = function(pat, x) !grepl(pat, x)}
  dat[[modelname]] %>%
    tidyr::gather(par, value, -tag) %>%
    filter(grepl('par', par)) %>%
    filter(fn(arg, par)) %>%
    left_join(metadata, by = "tag") %>%
    ggplot(aes(x = substance, y = as.numeric(value))) +
    geom_boxplot() + facet_wrap(~par, scales = "free_y") + gg.further}


# util --------------------------------------------------------------------


box_default <- stat_summary(geom = "crossbar", fill = gg$fill.box, colour = gg$outline.box,
    fun.data = function(x) {data.frame(y = median(x), ymin = quantile(x)[2], 
                 ymax = quantile(x)[4])})
  
median_default <- stat_summary(geom = "crossbar", colour = gg$colour.median, fill = NA,
      fun.data = function(x) {data.frame(y = median(x),ymin = median(x),ymax = median(x))})

point_default <- geom_quasirandom(width = gg$point.width, colour = gg$point.colour, 
                   alpha = gg$point.alpha, stroke = 0, shape=16, 
                   method = "tukeyDense", varwidth = TRUE)

# visuals -----------------------------------------------------------------
#general visuals
gg <- list()
gg$point.width = 0.1
gg$point.alpha = 1
gg$point.colour = "darkgray"
gg$fill.box = "lightgray"
gg$colour.median = "gray"
gg$outline.box = "gray"
gg$ribbon.fill = "lightgray"
gg$ribbon.colour = NA

gg$stay.label = c(`0` = 'after lose', `1` = 'after win')
gg$stay.value = log(c(0.03, 1, 10, 60, 660))


# fig 2 -------------------------------------------------------------------

#number of visits
dall %>% 
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
  geom_point(aes(fill = info), pch = 21, colour = gg$point.colour)+
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
  scale_y_continuous(breaks = c(0.4, .5, .6, .7, .8), limits = c(0.4, .8), expand = c(0, 0)) +
  labs(y = 'Preference of more certain option') +
  theme_publication +
  theme(axis.title.x = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.ticks.x = element_blank(),
    axis.line.x = element_blank())

(p1 | p2 | p3) + plot_layout(ncol = 3, widths = c(1, 1, 1)) + plot_annotation(tag_levels = "A")


# fig 4 -------------------------------------------------------------------

hero = "900110000199546" #saccharin
hero0 = "900110000351935" #water

p1 <- dmodel %>%
  filter(tag == hero) %>%
  ggplot(aes(x = log(intervala), y = stay)) +
  geom_quasirandom(groupOnX=FALSE, stroke = 0, shape=16, colour = gg$point.colour,
    alpha = 0.2, width = 0.05, method = "tukeyDense") +
  geom_hline(yintercept = 0.5, linetype = 'dotted') +
  geom_smooth(method="glm", method.args=list(family="binomial"), se=TRUE, colour = 'darkgray')+
  scale_x_continuous(limits = c(-3.507, 7), breaks= gg$stay.value, 
                     labels = format(exp(gg$stay.value), digits = 2, trim = T), 
                     expand = c(0,0)) +
  scale_y_continuous(breaks= c(0, 0.5, 1), labels = c(0,0.5,1), expand = c(0.02,0.02)) +
  facet_wrap(~dooropened, labeller = labeller(dooropened = gg$stay.label)) +
  labs(y = 'probability of stay', x = 'log(interval) [min]')+
  theme_publication+
  theme(panel.spacing = unit(1, "lines"),
    axis.line.x = element_blank(),
    axis.ticks.y = element_blank(),
    axis.line.y = element_blank(),
    axis.text.y = element_text(face = 'bold'))

p2 <- dmodel %>%
  filter(tag == hero0) %>%
  ggplot(aes(x = log(intervala), y = stay)) +
  geom_quasirandom(groupOnX=FALSE, stroke = 0, shape=16, colour = gg$point.colour,
                   alpha = 0.2, width = 0.05, method = "tukeyDense") +
  geom_hline(yintercept = 0.5, linetype = 'dotted') +
  geom_smooth(method="glm", method.args=list(family="binomial"), se=TRUE, colour = 'darkgray')+
  scale_x_continuous(limits = c(-3.507, 7), breaks= gg$stay.value, 
                     labels = format(exp(gg$stay.value), digits = 2, trim = T), 
                     expand = c(0,0)) +
  scale_y_continuous(breaks= c(0, 0.5, 1), labels = c(0,0.5,1), expand = c(0.02,0.02)) +
  facet_wrap(~dooropened, labeller = labeller(dooropened = gg$stay.label)) +
  labs(y = 'probability of stay', x = 'log(interval) [min]')+
  theme_publication+
  theme(panel.spacing = unit(1, "lines"),
        axis.line.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.line.y = element_blank(),
        axis.text.y = element_text(face = 'bold'))

(p1 | p2) + plot_annotation(tag_levels = "A")


# fig 5 -------------------------------------------------------------------
# glm results
temp <- {theme_publication +
    theme(legend.position = 'none',
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          axis.text.x = element_text(angle = 45, hjust = 1),
          axis.ticks.x = element_blank(),
          axis.ticks.y = element_line(linetype = 'dashed'),
          axis.line.x = element_blank())}

p1 <- result$glm %>%
  filter(grepl("intercept", predictor, ignore.case = T)) %>%
  filter(sig == 1) %>%
  left_join(manimal) %>%
  ggplot(aes(x = substance, y = estimate, group = substance, colour = sig)) +
  box_default + median_default + point_default +
  geom_hline(yintercept = 0, linetype = "dotted")+
  scale_y_continuous(limits = c(-4, 2), expand = c(0, 0), breaks = c(-4, -2, 0, 2)) +
  facet_wrap(~predictor, scales = "free_y") + temp

p2 <- result$glm %>%
  filter(grepl("door", predictor, ignore.case = T)) %>%
  filter(sig == 1) %>%
  left_join(manimal) %>%
  ggplot(aes(x = substance, y = estimate, group = substance, colour = sig)) +
  box_default + median_default + point_default +
  geom_hline(yintercept = 0, linetype = "dotted")+
  scale_y_continuous(limits = c(-1, 1), expand = c(0, 0), breaks = c(-1, 0, 1)) +
  facet_wrap(~predictor, scales = "free_y") + temp

p3 <- result$glm %>%
  filter(grepl("iltervala", predictor, ignore.case = T)) %>%
  filter(sig == 1) %>%
  left_join(manimal) %>%
  ggplot(aes(x = substance, y = estimate, group = substance, colour = sig)) +
  box_default + median_default + point_default +
  geom_hline(yintercept = 0, linetype = "dotted")+
  scale_y_continuous(limits = c(0, .01), expand = c(0, 0), breaks = c(.001, .005, .01)) +
  facet_wrap(~predictor, scales = "free_y") + temp 

p4 <- result$glm %>%
  filter(grepl("corner", predictor, ignore.case = T)) %>%
  filter(sig == 1) %>%
  left_join(manimal) %>%
  ggplot(aes(x = substance, y = estimate, group = substance, colour = sig)) +
  box_default + median_default + point_default +
  geom_hline(yintercept = 0, linetype = "dotted")+
  scale_y_continuous(limits = c(-7, 7), expand = c(0, 0), breaks = c(-7, -3, 0, 3, 7)) +
  facet_wrap(~predictor, scales = "free_y") + temp

( p1 | p2 | p3 | p4 ) + plot_layout() + plot_annotation(tag_levels = "A")


# fig 5 -------------------------------------------------------------------

tLimitBeta <- 10

pOptimBasic <- oBasic %>%
  filter(beta <= tLimitBeta) %>%
  ggplot(aes(alpha, beta, z = nll)) +
  geom_raster(aes(fill = nll)) +
  geom_contour(colour = 'black', binwidth = 200) +
  theme_publication +
  theme(legend.position = "bottom") +
  scale_x_continuous(breaks = c(0, 0.5, 1), limits = c(0, 1), expand = c(0, 0)) +
  scale_y_continuous(breaks = c(0, 2, 10, 20), limits = c(0, NA), expand = c(0, 0)) +
  geom_point(data = filter(oBasic, min == T), colour = "red", shape = 8) +
  scale_fill_gradient(low = "white", high = "black", limits = c(
    oTime %>% filter(beta <= tLimitBeta) %>% summarise(min(nll)) %>% unlist(),
    oBasic %>% filter(beta <= tLimitBeta) %>% summarise(max(nll)) %>% unlist())) + 
  coord_flip()


# fig 6 -------------------------------------------------------------------



# notebook ----------------------------------------------------------------

  #stat_summary(geom = "crossbar", fun.y = "median")+
  #geom_violin(fill = "gray", colour = NA)+
  #geom_boxplot(alpha = 0.3, outlier.colour = NA) +
  
geom_signif(
  y_position=c(0.9,0.9,0.9), 
  xmin=c(1,2,4), 
  xmax=c(2,2,2),
  annotation=c('','*',''), 
  textsize = 2 * ggplot2:::.pt, 
  tip_length=0.02, 
  vjust = 0.6)