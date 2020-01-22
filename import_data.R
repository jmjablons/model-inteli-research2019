library(icager)
library(dplyr)

# d1, d3 - version 2 plus; d2, d4 - version 1 star
# variable ----------------------------------------------------------------

scheme <- list()

# d A ---------------------------------------------------------------------
#mistake: one experiment conducted in two cages

d1 <- 
  icager::import(
    list.files(
      path = choose.dir(),
      full.names = T), 
    version = 2) %>%
  purrr::keep(~ !is.null(.)) %>%
  dplyr::bind_rows()

d1 = d1 %>% 
  icager::standardise() %>% 
  icager::contingencise()

scheme$A <- d1 %>% printscheme()
#icager::schematize(d1) %>% with(., {table(contingency, info)})
temp <- tibble(
  contingency = 0:29,
  info = c("welcome", rep("adaptation", 9), rep("reversal", 20))) %>%
  merge(select(scheme$A, contingency, label), all.x = T)

d1 = d1 %>% 
  dplyr::left_join(temp, all.x = T) %>% 
  dplyr::mutate(exp = "A", soft = "plus") %>%
  dplyr::as_tibble()


# d B ---------------------------------------------------------------------
# green - 100% rp
# red - 90%
# yellow - 30%
# white - 0% (water)
# mistake: water corners during welcome denoted as "Green"

d2 <- 
  icager::import(
    list.files(
      path = choose.dir(),
      full.names = T), 
    version = 2) %>%
  purrr::keep(~ !is.null(.)) %>%
  dplyr::bind_rows()

d2 = d2 %>% 
  icager::standardise() %>% 
  icager::contingencise(
    metadata = c(Green = 1.0, Red = 0.9, Yellow = 0.3, White = 0.0))

scheme$B <- d2 %>% printscheme()

# handle mistake
d2$rp[d2$contingency == 0] = 0 

#icager::schematize(d2) %>% with(., {table(contingency, info)})
temp <- tibble(
  contingency = 0:27,
  info = c("welcome", rep("adaptation", 9), rep("reversal", 18))) %>%
  merge(select(scheme$B, contingency, label), all.x = T)

d2 = d2 %>% 
  dplyr::left_join(temp, all.x = T) %>% 
  dplyr::mutate(exp = "B", soft = "star") %>%
  dplyr::as_tibble()


# d C ---------------------------------------------------------------------
d3 <- 
  icager::import(
    list.files(
      path = choose.dir(),
      full.names = T), 
    version = 1) %>%
  purrr::keep(~ !is.null(.)) %>%
  dplyr::bind_rows()

d3 = d3 %>% 
  icager::standardise() %>% 
  icager::contingencise()

scheme$C <- d3 %>% printscheme()
View(scheme$C)

#icager::schematize(d3) %>% with(., {table(contingency, info)})
temp <- tibble(
  contingency = 0:31,
  info = c(rep("adaptation", 14), rep("reversal", 18))) %>%
  merge(select(scheme$C, contingency, label), all.x = T)

d3 = d3 %>% 
  dplyr::left_join(temp, all.x = T) %>% 
  dplyr::mutate(exp = "C", soft = "plus") %>%
  dplyr::as_tibble()


# d D ---------------------------------------------------------------------
# Green - adaptation 100%
# Green - 100% rp
# Red - 90% rp
# Yellow - 30% rp
# White - 0% rp
# Mistake: Not all data availale in raw form
# Mistake: water corners during welcome denoted as "Green"

d4 <- 
  icager::import(
    list.files(
      path = choose.dir(),
      full.names = T), 
    version = 1) %>%
  purrr::keep(~ !is.null(.)) %>%
  dplyr::bind_rows()

d4 <- (function(A, pattern = "cornercondition", replacement = "condition"){
  names(A) = tolower(names(A))
  names(A)[grep(x = names(A), pattern = pattern)] <- replacement
  A})(d4) %>% 
  dplyr::select(
    deviceid, start, end, corner, condition, 
    tag, nlick, durationlick, 
    nnosepoke, dooropened) %>% 
  dplyr::mutate(
    deviceid = as.character(deviceid), 
    deviceid = ifelse(!grepl(x = deviceid, "Cage "), 
                      paste0("Cage ", 
                             deviceid), deviceid), 
    corner = as.character(corner), 
    condition = as.character(condition), tag = as.character(tag), 
    dooropened = as.character(dooropened)) %>%
  icager::contingencise(
    metadata = c(Green = 1.0, Red = 0.9, Yellow = 0.3, White = 0.0))

# handle mistake
d4$rp[d4$contingency == 0] = 0 

scheme$D <- d4 %>% printscheme()

#icager::schematize(d4) %>% with(., {table(contingency, info)})
temp <- tibble(
  contingency = 0:27,
  info = c("welcome", rep("adaptation", 10), rep("reversal", 17))) %>%
  merge(select(scheme$D, contingency, label), all.x = T)

d4 = d4 %>% 
  dplyr::left_join(temp, all.x = T) %>% 
  dplyr::mutate(exp = "D", soft = "star") %>%
  dplyr::as_tibble()
