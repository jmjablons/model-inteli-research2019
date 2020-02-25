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

# cross validation --------------------------------------------------------

#saveRDS(dall, "~/phDComputations/model-inteli-research2019/data/dall.rds")
#saveRDS(dmodel, "~/phDComputations/model-inteli-research2019/data/dmodel.rds")
