# judyta
check_figure <- list()
stat_result <- list()

# COHORTS DIFFERENCES -----------------------------------------------------
# check cohort effect -----------------------------------------------------
util_stat <- list(
  cohorts = c("alcohol (I)","alcohol (II)","alcohol+saccharin (II)",
              "saccharin (I)","saccharin (III)", "water (IV)"),
  kruskal = function(a = temp){kruskal.test(data = a, value ~ substance)},
  dunn = function(a = temp){ FSA::dunnTest(data = a, value ~ substance, method="bh")$res %>%
      filter(P.adj < 0.05) %>% select(Comparison, P.adj)},
  wilcox_greater = function(a = temp, .mu = .5, .cohort = util_stat$cohorts){
    .temp = data.frame(cohort = rep(NA,6), pvalue = rep(NA,6))
    for(i in seq_along(.cohort)){
      .temp$pvalue[i] = with(a, {wilcox.test(value[substance == .cohort[i]], 
                                             mu = .5, alternative = 'greater')$p.value})
      .temp$cohort[i] = .cohort[i]}
    return(.temp %>% filter(pvalue < .05))})

# reward preference -------------------------------------------------------
temp <- util$getpreference() %>%
               left_join(manimal) %>%
  mutate(substance = as.factor(gr))

check_figure[[1]] <- temp %>%  ggplot(aes(x = gr, y = value, group = gr)) +
  box_default() + median_default + point_default(point.width = .2) +
  geom_hline(yintercept = 0.5, colour = 'black', linetype = "dashed") +
  scale_y_continuous(breaks = c(0, 0.5, 1), limits = c(0, 1.1), 
                     expand = c(0, 0)) +
  labs(y = 'Reward preference') +
  theme_publication +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.ticks.x = element_blank(),
        axis.line.x = element_blank())

stat_result[["rewardpreference"]] <- list(
util_stat$kruskal()$p.value %>% format(scientific = F, digits = 3),

util_stat$dunn(),

util_stat$wilcox_greater())

# number of attempts ------------------------------------------------------
temp <- dall %>%
  filter(info == 'reversal') %>%
  group_by(tag, exp) %>% 
  summarise(value = length(which(rp > 0 & visitduration > 2))) %>%
  ungroup() %>%
  left_join(manimal) %>%
  mutate(substance = as.factor(gr))

check_figure[[2]] <- temp %>% ggplot(aes(x = substance, y = value, group = substance)) +
  box_default() + median_default + point_default() +
  scale_y_continuous(limits = c(0, 3300), expand = c(0, 0)) +
  labs(y = 'Total number of attempts\nduring reversals') +
  theme_publication +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.ticks.x = element_blank(),
        axis.line.x = element_blank())

stat_result[["attempts"]]<- list(util_stat$kruskal(), util_stat$dunn())

# better ratio ------------------------------------------------------------
temp <- result$br %>%
  left_join(manimal) %>%
  mutate(substance = as.factor(gr))

check_figure[[3]] <- temp %>%  ggplot(aes(x = substance, y = value, 
                                          group = substance)) +
  box_default() + median_default + point_default(point.width = .2) + 
  geom_hline(yintercept = 0.5, colour = 'black', linetype = "dashed") +
  scale_y_continuous(breaks = c(0.25, .5, .75), limits = c(0.25, .8), 
                     expand = c(0, 0)) +
  labs(y = 'Preference of more \ncertain option') +
  theme_publication +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.ticks.x = element_blank(),
        axis.line.x = element_blank())

stat_result[["betterratio"]] <- list(util_stat$kruskal(), 
                                     util_stat$dunn(), 
                                     util_stat$wilcox_greater())

# win-stay ----------------------------------------------------------------
temp <- util$winstay(substances)

check_figure[[4]] <- ggplot(temp, aes(x = interaction(
  short, param, sep = " ", lex.order = F), 
             y = value, group = tag))+ 
  geom_line(aes(group = interaction(tag, param)), 
            size = .1, alpha = .7, 
            colour = "black") +
  point_default(point.size = 2, point.width = .1) +
  scale_y_continuous(expand = c(0,0.05), limits = c(0,1.1), 
                     breaks = c(0, .5, 1)) +
  facet_wrap(~gr) + theme_publication + 
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.line.x = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.ticks.x = element_blank())

temp <- dmodel %>%
  filter(intervala <= 2 | intervala >= 10) %>%
  mutate(short = ifelse(intervala <= 2, "<2", ">10")) %>%
  group_by(tag, short) %>%
  summarise(`win-stay` = length(which(dooropened == 1 & stay == 1))/ 
              length(which(dooropened == 1)),
            `lose-shift` = length(which(dooropened == 0 & stay == 0))/
              length(which(dooropened == 0))) %>%
  tidyr::gather(param, value, -tag, -short) %>%
  mutate(param = factor(param, levels = c("win-stay", "lose-shift"), 
                        ordered = T)) %>%
  tidyr::spread(short, value) %>%
  left_join(manimal)

temp_grid <- expand.grid(substance = util_stat$cohorts, 
                         param = unique(temp$param)) %>% 
  as_tibble() %>%
  mutate(param = as.character(param), substance = as.character(substance))
  
for(i in 1:nrow(temp_grid)){
  temp_grid$pvalue[i] = 
    (function(.sub, .par, a = temp){
      with((a %>% 
              filter(gr %in% .sub) %>% 
              filter(param %in% .par)), {
                wilcox.test(`<2`, `>10`, paired = T, conf.int = T)$p.value})})(
                  temp_grid$substance[i], temp_grid$param[i])}

stat_result[["winstay"]] <- temp_grid %>% filter(pvalue < .05)

# glm predictors ----------------------------------------------------------
temp <- list(box_default(), median_default, point_default(),
             geom_hline(yintercept = 0, linetype = "dotted"),
             facet_wrap(~predictor, scales = "free_y"),
             theme_publication,
             theme(legend.position = 'none',
                   axis.title.x = element_blank(),
                   #axis.title.y = element_blank(),
                   axis.text.x = element_text(angle = 45, hjust = 1),
                   axis.ticks.x = element_blank(),
                   axis.ticks.y = element_line(linetype = 'dashed'),
                   axis.line.x = element_blank()),
             labs(y = "log odds ratio of stay"))

p5 <- result$glm %>%
  filter(grepl("intercept", predictor, ignore.case = T)) %>%
  filter(sig == 1) %>%
  mutate(predictor = "Intercept") %>%
  left_join(manimal) %>%
  ggplot(aes(x = gr, y = estimate, 
             group = substance, colour = sig)) +
  scale_y_continuous(limits = c(-5, 5), expand = c(0, 0), 
                     breaks = c(-5, 0, 5)) + temp

p6 <- result$glm %>%
  filter(grepl("door", predictor, ignore.case = T)) %>%
  filter(sig == 1) %>%
  mutate(predictor = "Reward") %>%
  left_join(manimal) %>%
  ggplot(aes(x = gr, y = estimate, group = gr, 
             colour = sig)) +
  scale_y_continuous(limits = c(-1, 1), expand = c(0, 0), 
                     breaks = c(-1, 0, 1)) +
  temp

p7 <- result$glm %>%
  filter(grepl("intervala", predictor, ignore.case = T)) %>%
  filter(sig == 1) %>%
  mutate(predictor = "Per minute of interval") %>%
  left_join(manimal) %>%
  ggplot(aes(x = gr, y = estimate, group = gr, 
             colour = sig)) + 
  scale_y_continuous(limits = c(-.01, .01), 
                     expand = c(0, 0), breaks = c(-.01, 0, .01)) + 
  temp 

p8 <- result$glm %>%
  filter(grepl("corner", predictor, ignore.case = T)) %>%
  filter(sig == 1) %>%
  mutate(predictor = "Corner bias") %>%
  left_join(manimal) %>%
  ggplot(aes(x = gr, y = estimate, group = gr, 
             colour = sig)) +
  scale_y_continuous(limits = c(-8, 8), expand = c(0, 0),
                     breaks = c(-8, 0, 8))+ temp

check_figure[[5]] <- (p5 | p6 )/ (p7 | p8)

# aic ---------------------------------------------------------------------
temp <- list(
  data = util$aictidy(pubmodel) %>% filter(name %in% temp_names$name),
  plot = list(box_default(), median_default, point_default(), 
              geom_hline(yintercept = 0, linetype = "dotted"),
              theme_publication, 
              labs(y = TeX("$\\Delta{AIC}$")),
              theme(panel.spacing = unit(2, "lines"),
                    axis.text.x = element_text(angle = 45, hjust = 1),
                    axis.title.x = element_blank(),
                    axis.ticks.y = element_line(linetype = 'dashed'),
                    axis.ticks.x = element_blank(),
                    axis.line.x = element_blank(),
                    legend.position = "none"),
              facet_wrap(~ gr, ncol = 1),
              scale_y_continuous(
                trans = 'asinh',
                expand = c(0,0),
                limits = c(-10^4, 10^4),
                breaks = c(-(10^(1:4)), 10^(1:4)),
                labels = TeX(c(paste("$-10^{",1:4, "}$", sep = ""),
                               paste("$10^{",1:4, "}$", sep = ""))))),
  util = function(sb, .set, .names = temp_names){temp$data %>% 
      #filter(gr %in% sb) %>%
      mutate(name = factor(name, levels = .names$name, 
                           labels = .names$rename, ordered = T)) %>%
      filter(name %in% .names$rename[.names$set %in% .set]) %>%
      ggplot(aes(x = name, y = delta)) + temp$plot})

# general
p9 <- temp$util("alcohol (I)",1) + 
  theme(axis.text.x = element_blank())

p10 <- temp$util("alcohol (I)",2) + 
  theme(axis.text.x = element_blank(),
        axis.line.y = element_blank(),
        axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        axis.ticks.y = element_blank())

check_figure[[6]] <- p9 | p10

# parameters --------------------------------------------------------------
util_templot <- function(modelname, arg = 'par', 
         gg.limits = c(0,1), gg.breaks = c(0,1), 
         include = TRUE,
         dat = pubmodel, metadata = manimal, 
         gg.further = theme_publication){
  if(include){fn = function(pat, x) (grepl(pat, x))
  } else {fn = function(pat, x) !grepl(pat, x)}
  dat[[modelname]] %>%
    tidyr::gather(par, value, -tag) %>%
    filter(grepl('par', par)) %>%
    filter(fn(arg, par)) %>%
    left_join(metadata, by = "tag") %>%
    ggplot(aes(x = gr, y = (function(x){
      ifelse(x <= gg.limits[2], x, gg.limits[2])})(
        as.numeric(value)))) +
    box_default() + median_default + point_default() +
    facet_wrap(~par, scales = "free_y") + 
    scale_y_continuous(limits = gg.limits, expand = c(0.03,0), 
                       breaks = gg.breaks) + gg.further +
    theme(axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          axis.line.x = element_blank(),
          axis.text.x = element_text(angle = 45, hjust = 1),
          axis.ticks.x = element_blank())}

p1 <- util_templot("basic", "beta", gg.limits = c(0,12), gg$show.beta)
p2 <- util_templot("fictitious", "beta", gg.limits = c(0,12), gg$show.beta)
p3 <- util_templot("basic", "alpha", gg.limits = c(0,1))
p4 <- util_templot("fictitious", "alpha", gg.limits = c(0,1))

check_figure[[7]] <- (p1 | p2) / (p3 | p4)

temp <- pubmodel[["basic"]] %>%
  select(tag, value = par.alpha) %>%
  left_join(manimal, by = "tag") %>%
  mutate(substance = as.factor(gr))

temp <- pubmodel[["basic"]] %>%
  select(tag, value = par.beta) %>%
  left_join(manimal, by = "tag") %>%
  mutate(substance = as.factor(gr))

temp <- pubmodel[["fictitious"]] %>%
  select(tag, value = par.alpha) %>%
  left_join(manimal, by = "tag") %>%
  mutate(substance = as.factor(gr))

temp <- pubmodel[["fictitious"]] %>%
  select(tag, value = par.beta) %>%
  left_join(manimal, by = "tag") %>%
  mutate(substance = as.factor(gr))

util_stat$kruskal(); util_stat$dunn()

stat_result[["fictitious"]]$"beta" <- list(util_stat$kruskal(), 
                                           util_stat$dunn())
                                        
# activity ----------------------------------------------------------------

# ## check
# dall %>% 
#   filter(info %in% c("adaptation", "reversal")) %>%
#   left_join(manimal, "tag") %>%
#   mutate(cohort = gg$cohort$label[match(exp, gg$cohort$exp)],
#          gr = paste(substance, cohort, sep = " ")) %>%
#   util$binal(hour = 13) %>%
#   filter(gr == "alcohol (II)") %>% 
#   group_by(info, bin) %>% 
#   summarise(beg = first(start), n=n()) %>% View()

## check schemes
# dall %>% filter(info == "finish") %>% 
#   left_join(manimal) %>% icager::printscheme() %>% View()
#
# dall %>% filter(info == "finish") %>% 
#   left_join(manimal) %>% group_by(exp, substance, contingency) %>%
#   summarise(n())
#
# dall %>%
#   left_join(manimal) %>%
#   mutate(cohort = temp$label[match(exp, temp$exp)],
#          gr = paste(substance, cohort, sep = " ")) %>%
#   group_by(gr, substance, cohort) %>%
#   summarise(beg = first(start), fin = last(end))
#
# dall %>% filter(info == "welcome" & exp == "D") %>% 
#   left_join(manimal) %>% icager::printscheme() %>% View()

# visit duration > 660 ----------------------------------------------------
# dmodel %>%
#   #filter(visitduration > 600) %>%
#   mutate(visitduration = visitduration) %>%
#   ggplot(aes(x = visitduration))+
#   geom_density() %>%
#   plotly::ggplotly()

dmodel %>%
  group_by(tag, contingency) %>%
  summarise(as.numeric(max(visitduration, na.rm = T), units = "hours")) %>%
  left_join(manimal) %>% View()

# hero info ---------------------------------------------------------------
dmodel %>%
  filter(tag%in%hero)%>%
  group_by(dooropened) %>%
  summarise(n(), value = length(which(intervala > exp(-3.507) & 
                                        intervala < exp(7))))

# lick vs visit -----------------------------------------------------------
with(subset(dmodel, !is.na(nlick)), {
  cor(nlick, as.numeric(visitduration), method = "pearson")})

# dmodel %>%
#   mutate(visitduration = as.numeric(visitduration)) %>%
#   filter(visitduration < 120) %>%
#   filter(!is.na(nlick)) %>%
#   # plot #
#   ggplot(aes(x = nlick, y = visitduration, group = tag, fill = tag, colour = tag))+
#   geom_point(alpha = .3)+
#   theme_classic()+
#   theme(legend.position = "none")+
#   scale_y_continuous(limits = c(0,120), expand = c(0,0))+
#   scale_x_continuous(limits = c(0, 200), expand = c(0,0))

# outlier activity --------------------------------------------------------
with(dmodel, {table(tag, contingency)})

# check optim -------------------------------------------------------------
# optimx::optimx(par = c(.1, 1, .01), fn = model, 
#                a = filter(dmodel, tag == manimal$tag[4]), 
#                method = c("Nelder-Mead", "BFGS","CG", "SANN", "ucminf", 
#                           "bobyqa", "nmkb"))

# check dataset -----------------------------------------------------------
dall2$tag %>% unique() %>% length()
match("900110000324267", (dall2$tag %>% unique()))
"900110000324267" %in% (dall2$tag %>% unique())
is.element("900110000324267", (dall2$tag %>% unique()))
dplyr::anti_join(dall, dall2)
class(dall2$illumination)
dplyr::setdiff(dall2, dall)
identical(dall2,dall)
all.equal(dall2,dall)

# duplicated rows ---------------------------------------------------------
dall %>% arrange(start) %>% 
  filter(duplicated(start) | duplicated(start, fromLast = TRUE)) %>% View()

dmodel %>%
  select(-intervala, -intervalb) %>%
  group_by(tag) %>%
  distinct()

temp <- dall %>%
  distinct() %>%
  group_by(exp, info) %>%
  summarise(distn = n()) %>%
  left_join(dall %>%
              group_by(exp, info) %>%
              summarise(origin = n())) %>%
  mutate(duplicate = origin - distn)

temp$duplicate %>% sum()

dall %>% filter(duplicated(start) | duplicated(start, fromLast = TRUE))

library(dplyr)
icager::specify(dmodel)

#util <- list()
util$dtag <- function(.tag, a = dmodel){
  filter(a, tag %in% .tag)}

util$test <- function(.par){
  for(m in unique(dmodel$tag)){
    print(m)
    print(paste(m, model(.par, a = util$dtag(m)), sep = " | "))}}

util$test(c(0.1,5,1))

util$dtag("900110000340502") %>% filter(intervalb < 1/600) %>% 
  filter(!duplicated(start))

model(c(1,2,1), util$dtag("900110000340502") %>% filter(!duplicated(start)))

util$dtag("900110000340502") %>%
  group_by(dooropened, corner) %>%
  summarise(n = n())

dmodel %>%
  group_by(tag) %>%
  filter(duplicated(start) | duplicated(start, fromLast = TRUE)) %>% View()

dmodel %>%
  group_by(tag) %>%
  filter(duplicated(start) | duplicated(start, fromLast = TRUE)) %>% 
  summarise(number = n())

dmodel %>%
  group_by(tag) %>%
  filter(!duplicated(start) & !duplicated(start, fromLast = TRUE)) %>% 
  summarise(number = n())

temp %>%
  group_by(tag) %>%
  filter(!duplicated(start, fromLast = TRUE)) %>% 
  summarise(number = n())

temp %>% group_by(tag, start) %>% filter(n() > 1)

temp %>% distinct()

dmodel %>%
  select(-intervala, -intervalb) %>%
  group_by(tag) %>%
  distinct()

temp <- tibble(
  corner = runif(20L, 1L, 3.5) %>% round(digits = 0),
  start = sample(c(Sys.time(), Sys.time() + lubridate::minutes(240)), 20, replace = T),
  end = start + lubridate::minutes(2),
  dooropened = sample(c(0,1), size = 20, replace = T),
  tag = runif(20L, 10L, 13L) %>% round(digits = 0),
  group = 1) %>%
  arrange(tag, start) %>%
  mutate(id = 1:20)

# check how it works
temp %>% unique()
temp %>% distinct(tag, corner, start, .keep_all = T)
temp %>% distinct(.keep_all = T)
temp %>% duplicated()

temp <- dall %>%
  group_by(start,tag)%>%
  arrange(tag, start) %>%
  mutate(count = 1:n()) %>%
  filter((count > 1 & lag(count) > 0) | lead(count) > 1)

temp2 <- dall %>%
  distinct() %>%
  group_by(start,tag)%>%
  arrange(tag, start) %>%
  mutate(count = 1:n()) %>%
  filter((count > 1 & lag(count) > 0) | lead(count) > 1)

temp3 <- dall %>%
  select(-temperature) %>%
  distinct() %>%
  group_by(start,tag)%>%
  arrange(tag, start) %>%
  mutate(count = 1:n()) %>%
  filter((count > 1 & lag(count) > 0) | lead(count) > 1)

temp %>%
  #filter(count == 1)%>%
  group_by(start) %>%
  summarise(N = n(),
            tag = length(unique(tag)),
            corner = length(unique(corner)),
            door = length(unique(dooropened)),
            end = length(unique(end)),
            deviceid = length(unique(deviceid)),
            start1 = length(unique(start)),
            condition = length(unique(condition)),
            temperature = length(unique(temperature)),
            nlick = length(unique(nlick)),
            durationlick = length(unique(durationlick)),
            nnosepoke = length(unique(nnosepoke)),
            rp = length(unique(rp)),
            contingency = length(unique(contingency)),
            info = length(unique(info)),
            label = length(unique(label)),
            exp = length(unique(exp)),
            soft = length(unique(soft)),
            visitduration = length(unique(visitduration)),
            illumination = length(unique(illumination)),
            count = length(unique(count))) %>% summary()

identical(dall %>% unique(), dall %>% distinct())