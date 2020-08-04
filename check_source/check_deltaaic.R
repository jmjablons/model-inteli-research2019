util$aictidy2 <- function(a = rmodel){
  a %>% purrr::map(~select(., name, aic, tag)) %>%
    bind_rows() %>%
    group_by(tag) %>%
    dplyr::group_split(keep = TRUE) %>%
    purrr::map(~ mutate(., delta = .$aic - .$aic[.$name %in% "basic"]))%>%
    dplyr::bind_rows() %>%
    ungroup() %>%
    #mutate(delta = aic - .$aic[.$tag == tag & .$name == "basic"]) %>%
    left_join(manimal) %>%
    left_join(
      (dmodel %>% group_by(tag) %>% summarise(ntrial = n())), 
      by = "tag")}

all.equal(util$aictidy(remodel), util$aictidy2(remodel))

temp <- util$aictidy(remodel)
temp2 <- util$aictidy2(remodel)
all.equal(temp, temp2)
identical(temp, temp2)

temp2 = temp2 %>% arrange(tag, name)

temp %>% View()

setdiff(temp$delta, temp2$delta)

temp$delta - temp2$delta

temp <- remodel %>% 
  purrr::map(~select(., name, aic, tag)) %>%
  bind_rows() %>%
  group_by(tag) %>%
  dplyr::group_split(keep = TRUE) %>%
  purrr::map(~ mutate(., delta = .$aic - .$aic[.$name %in% "basic"]))%>%
  dplyr::bind_rows() %>%
  ungroup() %>%
  arrange(tag, name)

temp2 <- remodel %>% 
  purrr::map(~select(., name, aic, tag)) %>%
  bind_rows() %>%
  mutate(delta = aic - .$aic[.$tag == tag & .$name == "basic"]) %>%
  ungroup() %>%
  arrange(tag, name)

temp[30,]
temp2[30,]
