# wczytanie obiektow hbayes -----------------------------------------------
resulthbayes <- list()
for(i in c("bandit2arm","prl_ewa","prl_rp","prl_fictitious","prl_fictitious_rp")){
cat("Wybierz pliki dla modelu: ",i,"\n")
  for(j in c("alcohol","alcoholsaccharin","saccharin","water")){
    cat("Substancja: ", j, "\n")
    resulthbayes[[i]][[j]] <- readRDS(file.choose())}}

paramhbayes <- list()

for(i in c("bandit2arm","prl_ewa","prl_rp","prl_fictitious","prl_fictitious_rp")){
  paramhbayes[[i]] <- do.call(rbind, list(
    alcohol = resulthbayes[[i]]$alcohol$allIndPars %>%
      mutate(substance = "alcohol"),
    alcoholsaccharin = resulthbayes[[i]]$alcoholsaccharin$allIndPars %>%
      mutate(substance = "alcoholsaccharin"),
    saccharin = resulthbayes[[i]]$saccharin$allIndPars %>%
      mutate(substance = "saccharin"),
    water = resulthbayes[[i]]$water$allIndPars %>%
      mutate(substance = "water")))}

plotparamhbayes <- function(name, choo){
  paramhbayes[[name]] %>%
    mutate(tag = subjID,
           subjID = NULL) %>%
    tidyr::gather(par, value, -tag, -substance) %>%
    filter(par %in% choo) %>%
    ggplot(aes(x = substance, y = as.numeric(value), fill = substance)) +
    geom_boxplot()+
    facet_wrap(~par, scales = "free_y")}

# custom ------------------------------------------------------------------
plotparamA <- function(modelname){
  rmodel[[modelname]] %>%
    select(tag, contains('par'), -contains("beta")) %>%
    tidyr::gather(par, value, -tag) %>%
    left_join(manimal, by = "tag") %>%
    ggplot(aes(x = substance, y = as.numeric(value), fill = substance)) +
    geom_boxplot()+
    facet_wrap(~par, scales = "free_y")}

plotparamB <- function(modelname){
  rmodel[[modelname]] %>%
    select(tag, contains("beta")) %>%
    tidyr::gather(par, value, -tag) %>%
    left_join(manimal, by = "tag") %>%
    ggplot(aes(x = substance, y = as.numeric(value), fill = substance)) +
    geom_boxplot()+
    facet_wrap(~par, scales = "free_y")}

# figury ------------------------------------------------------------------
#omowienie wynikow
# aic surowki
fig$aic
# aic tylko potrzebne
fig$aic_
# delta aic
fig$deltaaic
# aic/trial
fig$averageaic
# poprawnych predykcji
fig$prediction + theme_publication + theme(legend.position = "none")
#zmiany predykcji wraz z próbami
fig$coursebasic
fig$`coursedecay+`
fig$coursefictitious
fig$coursebasic4arm
#parametry basic
(plotparamA("basic4arm")+
  plotparamA("basic")+
  plotparamhbayes("bandit2arm", "A") + 
  scale_y_continuous(limits = c(0,1))) * theme(legend.position = "none")
#parametry dual
(plotparamA("dual") + 
  plotparamhbayes("prl_rp", c("Apun", "Arew"))  + 
  scale_y_continuous(limits = c(0,1)))  * theme(legend.position = "none")
#parametry fictitious
(plotparamA("fictitious") + scale_y_continuous(limits = c(0,1)) +
  paramhbayes[["prl_fictitious"]] %>% 
  mutate(tag = subjID,
         subjID = NULL) %>%
  tidyr::gather(par, value, -tag, -substance) %>%
  filter(par %in% c("alpha","eta")) %>%
  ggplot(aes(x = substance, y = as.numeric(value), fill = substance)) +
  geom_boxplot()+
  facet_wrap(~par, scales = "free_y") +
  scale_y_continuous(limits = c(0,1))) * theme(legend.position = "none")
#parametry hybrid
(plotparamA("hybrid") + 
  plotparamhbayes("prl_fictitious_rp", c("eta_pos","eta_neg","alpha")) + 
  scale_y_continuous(limits = c(0,1))) * theme(legend.position = "none")

plotparamB("infirmitive")

# siatka do optymalizacji
#   stara
expand.grid(
  alpha = seq(0.05, 1, by = 0.05),
  beta = seq(0.25, 15, by = 0.25)) %>%
  ggplot(aes(x = alpha, y = beta))+
  geom_point()+
  scale_y_continuous(limits = c(0,50))
#   nowa
expand.grid(
  alpha = initials.default,
  beta = initials.beta) %>%
  ggplot(aes(x = alpha, y = beta))+
  geom_point()+
  scale_y_continuous(limits = c(0,50))

# paramhbayes[["prl_fictitious"]] %>% 
#   mutate(tag = subjID,
#          subjID = NULL) %>%
#   tidyr::gather(par, value, -tag, -substance) %>%
#   filter(par %in% c("alpha","eta")) %>%
#   ggplot(aes(x = substance, y = as.numeric(value), fill = substance)) +
#   geom_boxplot()+
#   facet_wrap(~par, scales = "free_y")


# cross validation --------------------------------------------------------

#saveRDS(dall, "~/phDComputations/model-inteli-research2019/data/dall.rds")
#saveRDS(dmodel, "~/phDComputations/model-inteli-research2019/data/dmodel.rds")
