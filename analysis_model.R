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
            by = "tag") %>%
  mutate(`aic/trial` = aic / ntrial)

# plot --------------------------------------------------------------------
fig$deltaaic <- 
  aictidy %>%
  filter(name != "basic4arm") %>%
  filter(name != "olddecay") %>%
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
  facet_wrap(~ substance, ncol = 1)

fig$averageaic <- 
  aictidy %>%
  filter(name != "basic4arm") %>%
  filter(name != "olddecay") %>%
  ggplot(aes(x = name, y = `aic/trial`, fill = name, colour = name)) +
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
  facet_wrap(~ substance, ncol = 1)

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
subject = "900110000199545" #saccharin group
temp <- expand.grid(
    alpha = 0.1,
    beta = seq(0.1, 20, by = 0.1),
    storage = seq(0.01, 0.99, by = 0.01))
tempdata <- filter(dmodel, tag == subject)

decayfunction <- temp
decayfunction$nll <-
  apply(temp, 1, function(x) {x[4] = model(a = tempdata, par = x[1:3])})

# plot it
persp(decayfunction$beta, decayfunction$storage, decayfunction$nll,
      zlab = "Height",
      theta = 30, phi = 15,
      col = "blue", shade = 0.5)

decayfunction %>%
  ggplot(aes(beta, storage, z = nll)) +
  geom_raster(aes(fill = nll)) +
  geom_contour() +
  theme_publication +
  theme(legend.position = "bottom")

# plot it better
library(plotly)
decayfunction_matrix <- as.matrix(decayfunction) #hue hue
plot_ly(z = decayfunction_matrix, type = "surface")