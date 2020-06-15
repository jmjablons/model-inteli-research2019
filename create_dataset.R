# 2020-01-23  judyta
# dependency --------------------------------------------------------------
library(dplyr)

# create dataset ----------------------------------------------------------
dall <- 
  do.call(rbind, list(d1,d2,d3,d4)) %>%
  filter(tag %in% with(manimal, {tag[strain == "C57BL6N"| 
                                       is.na(strain)]})) %>%
  filter(tag != "900110000324267") %>% #lost transponder 
  mutate(visitduration = difftime(end, start, units = "sec"),
         dooropened = as.integer(dooropened),
         corner = as.integer(corner)) 
dall_withduplicates #<- dall
dall = dall %>% select(-temperature) %>% distinct()

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

dmodel_withduplicates

# metadata ----------------------------------------------------------------
manimal <- readxl::read_xlsx("animals.xlsx", col_names = T)

# import rds --------------------------------------------------------------
#dall <- readRDS(file.choose())

# export data -------------------------------------------------------------
#saveRDS(manimal, "data/manimal.rds")
