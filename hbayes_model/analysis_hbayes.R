# analysis ----------------------------------------------------------------
library(hBayesDM)

result_hbayes <- list(
  alcohol = readRDS(file.choose()),
  alcoholsaccharin = readRDS(file.choose()),
  saccharin = readRDS(file.choose()),
  water = readRDS(file.choose()),
  all = readRDS(file.choose()))

info_animal <- df %>%
  group_by(tag, subjID, substance, cohort) %>%
  summarise(n = n())

plotInd(result_hbayes$water, "tau")

plot(result_hbayes$alcohol)

result_hbayes$allIndPars

# (
#   (
#     result_hbayes2$allIndPars %>% 
#       left_join(info_animal) %>%
#       ggplot(aes(y = A, x = substance))+
#       geom_boxplot(alpha = .3, outlier.colour = NA)+
#       geom_quasirandom(
#         width = 0.2,
#         method = "quasirandom",
#         varwidth = TRUE) +
#       theme_classic()+
#       scale_y_continuous(limits = c(0,1), expand = c(0,0))
#   ) +
#     (
#       result_hbayes$allIndPars %>% 
#         left_join(info_animal) %>%
#         ggplot(aes(y = A, x = substance))+
#         geom_boxplot(alpha = .3, outlier.colour = NA)+
#         geom_quasirandom(
#           width = 0.2,
#           method = "quasirandom",
#           varwidth = TRUE) +
#         theme_classic()+
#         scale_y_continuous(limits = c(0,1), expand = c(0,0))
#     ) + 
#     (alpha)
# )/
#   (
#     (
#       result_hbayes2$allIndPars %>% 
#         left_join(info_animal) %>%
#         ggplot(aes(y = tau, x = substance))+
#         geom_boxplot(alpha = .3, outlier.colour = NA)+
#         geom_quasirandom(
#           width = 0.2,
#           method = "quasirandom",
#           varwidth = TRUE) +
#         theme_classic()+
#         scale_y_continuous(limits = c(0,20), expand = c(0,0))
#     ) +
#       (
#         result_hbayes$allIndPars %>% 
#           left_join(info_animal) %>%
#           ggplot(aes(y = tau, x = substance))+
#           geom_boxplot(alpha = .3, outlier.colour = NA)+
#           geom_quasirandom(
#             width = 0.2,
#             method = "quasirandom",
#             varwidth = TRUE) +
#           theme_classic()+
#           scale_y_continuous(limits = c(0,20), expand = c(0,0))
#       ) +
#       (beta)
#   )
# 
# printFit(result_hbayes, result_hbayes2)

# write model -------------------------------------------------------------

pathTo_gng_m1 = system.file("stan/gng_m1.stan", package="hBayesDM")