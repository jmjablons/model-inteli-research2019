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

#fig[["sup1"]] <- (p1 + p2) / (p3 + p4) / (p5+ p6)
fig[["sup1poprawiona"]] <- (p1 + p2) / (p3 + p4) / (p5+ p6)

fig[["sup1"]] | fig[["sup1poprawiona"]]
