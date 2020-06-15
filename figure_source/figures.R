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
             "Qd","Qd+fictitious","Qd+split",
             "Bd","Bd+fictitious","Bd+split"),
  set = c(rep(1,6),rep(2,6)))