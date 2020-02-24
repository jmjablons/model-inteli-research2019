
# dependency --------------------------------------------------------------
require(ggplot2)
require(dplyr)
require(tidyr)
require(lubridate)
require(patchwork) 
require(ggbeeswarm)
library(scales)

# utils -------------------------------------------------------------------
asinh_trans <- function() {
  trans_new("asinh",
            transform = asinh,
            inverse   = sinh)}

# analysis ----------------------------------------------------------------
aictidy <- rmodel %>%
  purrr::map(~select(., name, aic, tag)) %>%
  bind_rows() %>%
  mutate(delta = aic - .$aic[.$tag == tag & .$name == "basic"]) %>%
  merge(manimal, all.x = T) %>%
  as_tibble() %>%
  left_join((dmodel %>%
               filter(is.finite(dooropened) & is.finite(corner)) %>%
               group_by(tag) %>%
               summarise(ntrial = n())), 
            by = "tag")

# plot --------------------------------------------------------------------
fig$deltaaic <- 
  aictidy %>%
  ggplot(aes(x = name, y = delta, fill = name, colour = name)) +
  geom_hline(yintercept = 0)+
  geom_boxplot(
    outlier.colour = NA,
    alpha = .5,
    size = 0.4) +
  geom_quasirandom(
    width = 0.2,
    method = "quasirandom",
    varwidth = TRUE,
    size = 1) +
  theme_publication +
  labs(x = element_blank()) +
  theme(
    panel.spacing = unit(2, "lines"),
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.ticks.y = element_line(linetype = 'dashed'),
    axis.line.x = element_blank(),
    legend.position = "top") +
  facet_wrap(~ substance, ncol = 1)+
  scale_y_continuous(trans = 'asinh', 
                     breaks = c(-12^(1:10), 12^(1:10)))

fig$aic <- 
  rmodel %>%
  purrr::map(~select(., name, aic, tag)) %>%
  bind_rows() %>%
  merge(manimal, all.x = T) %>%
  filter(name != "olddecay") %>%
  filter(name != "basic4arm") %>%
  ggplot(aes(x = name, y = aic)) +
  geom_hline(yintercept = 0)+
  geom_boxplot(
    outlier.colour = NA,
    fill = NA,
    size = 0.4) +
  geom_quasirandom(
    width = 0.2,
    method = "quasirandom",
    varwidth = TRUE,
    colour = 'black',
    size = 1) +
  theme_publication +
  labs(x = element_blank()) +
  theme(
    panel.spacing = unit(2, "lines"),
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.ticks.y = element_line(linetype = 'dashed'),
    axis.line.x = element_blank()) +
  facet_wrap(~substance, ncol = 1)

# parameters --------------------------------------------------------------
# rmodel[["basic4arm"]] %>%
#   select(tag, contains('par'), -contains('beta')) %>%
#   tidyr::gather(par, value, -tag) %>%
#   left_join(manimal, by = "tag") %>%
#   ggplot(aes(x = par, y = as.numeric(value), fill = substance)) +
#   geom_boxplot()


# parameter surface -------------------------------------------------------
#subject = "900110000199545" #saccharin group
# 
# decayfunction <- expand.grid(
#     alpha = c(0.01, 0.1, .99),
#     beta = seq(0.1, 10, by = 0.1),
#     storage = seq(0.01, 0.99, by = 0.01))%>% 
#   as_tibble()
# 
# decayfunction$nll <-
#   apply(decayfunction, 1, function(x) {
#     model(a = filter(dmodel, tag %in% x[1]), par = x[2:4])})
#
#decayfunction$min = F; decayfunction$min[decayfunction$nll == 
#                             min(decayfunction$nll)] = T

decay_optim <- list()
for(m in manimal$tag){
  tempdata <- filter(dmodel, tag %in% m)
  decayfunction <- expand.grid(
    tag = m,
    alpha = c(0.01, 0.1, .99),
    beta = seq(0.1, 10, by = 0.1),
    storage = seq(0.01, 0.99, by = 0.01))%>% 
    as_tibble()
  decay_optim[[m]] <-
    apply(decayfunction, 1, function(x) {
      x[4] = model(tempdata, par = as.numeric(x[2:4])); x})}

# plot it
persp(decayfunction$beta, decayfunction$storage, decayfunction$nll,
      zlab = "Height",
      theta = 30, phi = 15,
      col = "blue", shade = 0.5)

decayfunction %>%
  filter(beta < 15) %>%
  ggplot(aes(beta, storage, z = nll)) +
  geom_raster(aes(fill = nll)) +
  geom_contour(colour = 'gray',
               binwidth = 4) +
  theme_publication +
  theme(legend.position = "bottom")+
scale_y_continuous(
  breaks = c(0, 0.5, 1),
  limits = c(0, 1),
  expand = c(0, 0)) +
  scale_x_continuous(
    breaks = c(0, 2, 10, 20),
    limits = c(0, NA),
    expand = c(0, 0)) +
  geom_point(data = filter(decayfunction, min == T),
             colour = "red",
             shape = 8) +
  scale_fill_gradient(
    low = "white",
    high = "black",
    limits = c(
      decayfunction %>%
        filter(beta <= 15) %>%
        summarise(min(nll)) %>%
        unlist(),
      decayfunction %>%
        filter(beta <= 15) %>%
        summarise(max(nll)) %>%
        unlist()))

# plot it better
library(plotly)
decayfunction_matrix <- as.matrix(decayfunction) #hue hue
plot_ly(z = decayfunction_matrix, type = "surface")


# check for correlation ---------------------------------------------------
with(subset(dmodel, !is.na(nlick)), {
  cor(nlick, as.numeric(visitduration), method = "pearson")})

dmodel %>%
  mutate(visitduration = as.numeric(visitduration)) %>%
  filter(visitduration < 120) %>%
  filter(!is.na(nlick)) %>%
  ggplot(aes(x = nlick, y = visitduration, group = tag, fill = tag, colour = tag))+
  geom_point(alpha = .3)+
  #geom_smooth(alpha = .1)+
  theme_classic()+
  theme(legend.position = "none")+
  scale_y_continuous(limits = c(0,120), expand = c(0,0))+
  scale_x_continuous(limits = c(0, 200), expand = c(0,0))


# check for outliers ------------------------------------------------------

with(dmodel, {table(tag, contingency)})

