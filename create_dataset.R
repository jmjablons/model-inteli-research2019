# 2020-01-23  judyta
# dependency --------------------------------------------------------------
library(dplyr)

# create dataset ----------------------------------------------------------
dall <- 
  do.call(rbind, list(d1,d2,d3,d4)) %>%
  mutate(visitduration = difftime(end, start, units = "sec"),
         dooropened = as.integer(dooropened),
         corner = as.integer(corner)) %>%
  filter(tag %in% manimal$tag)

dmodel <-
  dall %>%
  filter(info == 'reversal', rp > 0, visitduration > 2) %>%
  group_by(tag) %>%
  arrange(tag, start, .by_group = T) %>%
  mutate(
    stay = ifelse(corner == lead(corner), 1, 0),
    intervalb = as.numeric(start - lag(end), units = 'mins'),
    intervala = -as.numeric(end - lead(start), units = 'mins')) %>%
  ungroup() %>%
  arrange(tag, start)

# metadata ----------------------------------------------------------------
manimal <- readxl::read_xlsx("animals.xlsx", col_names = T)

# not expected event ------------------------------------------------------
#
# mouse presence noted for 18 hours
dall[dall$visitduration == max(dall$visitduration),] %>% View()
#
# presence longer than 1h: 15
#   one per animal + 3 per one animal
dall %>%
  filter(visitduration > 3600) %>% 
  mutate(vis = as.numeric(visitduration)/3600) %>%
  select(vis, tag, contingency, start, end, exp)

# import rds --------------------------------------------------------------
#dall <- readRDS(file.choose())

# export data -------------------------------------------------------------
#saveRDS(manimal, "data/manimal.rds")
