# other -------------------------------------------------------------------
dall_withduplicates%>%
  group_by(start, deviceid, corner) %>%
  summarise(number = n()) %>%
  filter(number > 1)

dmodel %>%
  group_by(start, deviceid, corner) %>%
  summarise(number = n()) %>%
  filter(number > 1)

# figure 2 ----------------------------------------------------------------
# Activity of animals
temp <- list()
temp$up <- dall %>% 
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

temp$nah <- dall_withduplicates %>% 
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

temp$dif <- temp$up %>% select(bin, gr, info, 
                               measureup = measure,
                               lowerup = lower,
                               upperup = upper) %>%
  left_join(temp$nah) %>% 
  mutate(measure = measureup - measure,
         lower = lowerup - lower,
         upper = upperup - upper) %>%
  select(bin, gr, info, measure, lower, upper)

temp$plot <- list(
  geom_ribbon(aes(ymin=lower, ymax=upper, group = gr), 
              fill = gg$ribbon.fill, colour = gg$ribbon.colour),
  geom_point(size = gg$point.size, aes(fill = info), pch = 21, 
             colour = gg$point.colour),
  geom_hline(yintercept = 200, linetype = 'dotted'),
  scale_y_continuous(breaks=c(0, 40), limits = c(0, 40),
                     expand = c(0,0)),
  scale_x_continuous(breaks=c(0, 15, 35), limits = c(0, 35), 
                     expand = c(0,0)),
  ylab('Number of visits in corners'),
  xlab('Bin (48h)'),
  scale_fill_manual(values = c('white','darkgray')),
  facet_wrap(~gr),
  theme_publication, 
  theme(legend.position = "bottom"))

temp$dif %>% 
  ggplot(aes(x=bin, y=-measure)) + temp$plot + 
  theme(legend.position = c(0.0, 0.80), 
        legend.justification = c(0, 0), 
        axis.title.x = element_blank(), 
        axis.title.y = element_blank())

# sup1 --------------------------------------------------------------------
temp$dif <- dall %>%
  util$binal(hour = 13) %>%
  filter(info %in% c("reversal")) %>%
  filter(rp > 0) %>%
  left_join(manimal, "tag") %>%
  mutate(corner = as.factor(ceiling(corner/2))) %>%
  group_by(period = bin, corner, gr, deviceid) %>%
  summarise(value = as.numeric(sum(visitduration, na.rm = T), 
                               units = "hours"),
            info = last(info)) %>%
  ungroup() %>% left_join(
    dall_withduplicates %>%
      util$binal(hour = 13) %>%
      filter(info %in% c("reversal")) %>%
      filter(rp > 0) %>%
      left_join(manimal, "tag") %>%
      mutate(corner = as.factor(ceiling(corner/2))) %>%
      group_by(period = bin, corner, gr, deviceid) %>%
      summarise(value = as.numeric(sum(visitduration, na.rm = T), 
                                   units = "hours"),
                info = last(info)) %>%
      ungroup() %>% rename(valuedown = value)) %>%
  mutate(value = abs(valuedown - value))

temp$dif %>% summary()

# fig 3 preference --------------------------------------------------------
temp$dif =
  util$getpreference() %>% left_join(
    util$getpreference(a = dall_withduplicates) %>% rename(dnlickwater = nlickwater,
                                                           dnlickreward = nlickreward,
                                                           down = value)) %>%
  mutate(value = abs(value - down),
         nlickreward = abs(nlickreward - dnlickreward),
         nlickwater = abs(nlickwater - dnlickwater))
temp$dif %>% View()

# sup 2 fraction visit
temp$dif = dall %>%
  filter(info %in% c("adaptation","reversal")) %>%
  util$binal(hour = 13) %>%
  group_by(period = bin, tag, exp) %>% 
  summarise(value = length(which(rp>0))/n(), 
            info = last(info)) %>%
  left_join(manimal, "tag") %>%
  group_by(period, gr, info) %>%
  summarise(measure = median(value, na.rm = T),
            lower = quantile(value, na.rm = T)[[2]],
            upper = quantile(value, na.rm = T)[[4]]) %>% ungroup() %>%
  left_join(
    dall_withduplicates %>%
      filter(info %in% c("adaptation","reversal")) %>%
      util$binal(hour = 13) %>%
      group_by(period = bin, tag, exp) %>% 
      summarise(value = length(which(rp>0))/n(), 
                info = last(info)) %>%
      left_join(manimal, "tag") %>%
      group_by(period, gr, info) %>%
      summarise(dmeasure = median(value, na.rm = T),
                dlower = quantile(value, na.rm = T)[[2]],
                dupper = quantile(value, na.rm = T)[[4]]) %>% ungroup()) %>%
  mutate(measure = abs(dmeasure - measure),
         lower = abs(dlower - lower),
         upper = abs(dupper - upper)) 
temp$dif %>% View()

# fig 3 choices -----------------------------------------------------------
dall %>%
  filter(info == 'reversal') %>%
  group_by(tag, exp) %>% 
  summarise(measure = length(which(rp > 0 & visitduration > 2))) %>%
  left_join(manimal) %>% View()

dall %>%
  filter(info %in% "reversal") %>%
  filter(as.numeric(visitduration) > 2) %>%
  filter(rp > 0) %>%
  group_by(tag) %>%
  summarise(value = n()) %>%
  left_join(
    dall_withduplicates %>%
      filter(info %in% "reversal") %>%
      filter(as.numeric(visitduration) > 2) %>%
      filter(rp > 0) %>%
      group_by(tag) %>%
      summarise(dvalue = n())
  ) %>% mutate(value = abs(value - dvalue)) %>%
  filter(value > 0) %>% View()

# fig3 better ratio -------------------------------------------------------
util$betterratio(dall_withduplicates) %>% select(tag, ndup = value) %>%
  left_join(util$betterratio(dall) %>% select(tag, nup = value)) %>%
  mutate(diff = `-`(ndup, nup)) %>%
  filter(abs(diff) > 0) %>% View()

# win-stay ----------------------------------------------------------------
temp$dif =
  util$betterratio() %>% left_join(
    util$betterratio(a = dmodel_withduplicates) %>% 
      rename(dnbetter = nbetter,
             dn = n,
             dnworse = nworse,
             down = value)) %>%
  mutate(value = abs(value - down),
         nworse = abs(nworse - dnworse),
         nbetter = abs(nbetter - dnbetter))
temp$dif %>% View()


# check hero --------------------------------------------------------------
temp1 <- dall_withduplicates %>% 
  filter(info %in% "reversal", visitduration >2 ,rp > 0, tag %in% hero) %>%
  select(-temperature)

temp2 <- dall %>% 
  filter(info %in% "reversal", visitduration >2 ,rp > 0, tag %in% hero)

temp1 <- dmodel %>% filter(tag %in% hero)
temp2 <- dmodel_withduplicates %>% filter(tag %in% hero) %>% select(-temperature)

temp <- list()
temp <- init
temp$nll <- apply(init, 1, function(x){model(par = x, a = dhero)})

temp$nll <- lapply(X = init, FUN = function(x){model(par = x, a = dhero)})

all.equal(temp1, dhero)

model(unlist(init[1]), dhero)