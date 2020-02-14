dToSave = 
  dModel %>% 
  group_by(Substance, Exp) %>% 
  group_split() %>%
  lapply(function(a){
    dplyr::mutate(a, corner = ceiling(Corner/2))}) %>%
  bind_rows() %>%
  ungroup() %>%
  select(Tag, StartDateTime, corner, DoorOpened, Substance, Exp) %>%
  rename(tag = Tag, 
         choice = corner, 
         outcome = DoorOpened, 
         substance = Substance,
         cohort = Exp) %>%
  mutate(outcome = ifelse(outcome == 0, -1, outcome),
         subjID = as.numeric(as.factor(tag)))%>%
  arrange(StartDateTime) %>%
  select(-StartDateTime)

write.table(dToSave, "dataset_model_bayesian.txt", 
            sep = '\t', 
            row.names = F, 
            quote = FALSE)

# divide into groups ------------------------------------------------------

dToSaveGroup = 
  dModel %>% 
  group_by(Substance, Exp) %>% 
  group_split() %>%
  lapply(function(a){
    dplyr::mutate(a, corner = ceiling(Corner/2))}) %>%
  bind_rows() %>%
  ungroup() %>%
  select(Tag, StartDateTime, corner, DoorOpened, Substance, Exp) %>%
  rename(tag = Tag, 
         choice = corner, 
         outcome = DoorOpened, 
         substance = Substance,
         cohort = Exp) %>%
  mutate(outcome = ifelse(outcome == 0, -1, outcome),
         subjID = as.numeric(as.factor(tag)))%>%
  group_by(substance) %>%
  arrange(StartDateTime, .by_group = T) %>%
  select(-StartDateTime) %>%
  group_split()

write.table(dToSaveGroup[4], "dataset_model_bayesian_water.txt", 
            sep = '\t', 
            row.names = F, 
            quote = FALSE)
