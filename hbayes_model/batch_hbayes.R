
# run ---------------------------------------------------------------------
library(hBayesDM)

setwd("~/phDComputations/model-inteli-research2019/hbayes_model/OUTPUT/")

data_alcohol = "~/phDComputations/model-inteli-research2019/hbayes_model/DATA/dataset_model_bayesian_alcohol.txt"
data_alcoholsaccharin = "~/phDComputations/model-inteli-research2019/hbayes_model/DATA/dataset_model_bayesian_alcoholsaccharin.txt"
data_saccharin = "~/phDComputations/model-inteli-research2019/hbayes_model/DATA/dataset_model_bayesian_saccharin.txt"
data_water = "~/phDComputations/model-inteli-research2019/hbayes_model/DATA/dataset_model_bayesian_water.txt"

start <- Sys.time()

# # model: prl_ewa
#output_prl_ewa_alcohol<- prl_ewa(
# data = data_alcohol, niter = 2000, nwarmup = 1000, nchain = 4, ncore = 4)
# finish.ewa.one <- Sys.time()
# saveRDS(output_prl_ewa_alcohol, "output_hbayes_prl_ewa_alcohol.rds")
#
# output_prl_ewa_alcoholsaccharin<- prl_ewa(
#   data = data_alcoholsaccharin, niter = 2000, nwarmup = 1000, nchain = 4, ncore = 4)
# saveRDS(output_prl_ewa_alcoholsaccharin, "output_hbayes_prl_ewa_alcoholsaccharin.rds")
#
# output_prl_ewa_saccharin<- prl_ewa(
#   data = data_saccharin, niter = 2000, nwarmup = 1000, nchain = 4, ncore = 4)
# saveRDS(output_prl_ewa_saccharin, "output_hbayes_prl_ewa_saccharin.rds")
#
# output_prl_ewa_water<- prl_ewa(
#   data = data_water, niter = 2000, nwarmup = 1000, nchain = 4, ncore = 4)
# saveRDS(output_prl_ewa_water, "output_hbayes_prl_ewa_water.rds")
# finish.ewa.all <- Sys.time()
#
# # model: fictitious
# output_prl_fictitious_alcohol<- prl_fictitious(
#   data = data_alcohol, niter = 2000, nwarmup = 1000, nchain = 4, ncore = 4)
# saveRDS(output_prl_fictitious_alcohol, "output_hbayes_prl_fictitious_alcohol.rds")
#
# output_prl_fictitious_alcoholsaccharin<- prl_fictitious(
#   data = data_alcoholsaccharin, niter = 2000, nwarmup = 1000, nchain = 4, ncore = 4)
# saveRDS(output_prl_fictitious_alcoholsaccharin, "output_hbayes_prl_fictitious_alcoholsaccharin.rds")
#
# output_prl_fictitious_saccharin<- prl_fictitious(
#   data = data_saccharin, niter = 2000, nwarmup = 1000, nchain = 4, ncore = 4)
# saveRDS(output_prl_fictitious_saccharin, "output_hbayes_prl_fictitious_saccharin.rds")
#
# output_prl_fictitious_water<- prl_fictitious(
#   data = data_water, niter = 2000, nwarmup = 1000, nchain = 4, ncore = 4)
# saveRDS(output_prl_fictitious_water, "output_hbayes_prl_fictitious_water.rds")
# finish.fictitious.all <- Sys.time()
#
# # model: hybrid
# output_prl_fictitious_rp_alcohol<- prl_fictitious_rp(
#   data = data_alcohol, niter = 2000, nwarmup = 1000, nchain = 4, ncore = 4)
# saveRDS(output_prl_fictitious_rp_alcohol, "output_hbayes_prl_fictitious_rp_alcohol.rds")
#
# output_prl_fictitious_rp_alcoholsaccharin<- prl_fictitious_rp(
#   data = data_alcoholsaccharin, niter = 2000, nwarmup = 1000, nchain = 4, ncore = 4)
# saveRDS(output_prl_fictitious_rp_alcoholsaccharin, "output_hbayes_prl_fictitious_rp_alcoholsaccharin.rds")
#
# output_prl_fictitious_rp_saccharin<- prl_fictitious_rp(
#   data = data_saccharin, niter = 2000, nwarmup = 1000, nchain = 4, ncore = 4)
# saveRDS(output_prl_fictitious_rp_saccharin, "output_hbayes_prl_fictitious_rp_saccharin.rds")
#
# output_prl_fictitious_rp_water<- prl_fictitious_rp(
#   data = data_water, niter = 2000, nwarmup = 1000, nchain = 4, ncore = 4)
# saveRDS(output_prl_fictitious_rp_water, "output_hbayes_prl_fictitious_rp_water.rds")
# finish.fictitious.all <- Sys.time()

# model: dual
output_prl_rp_alcohol<- prl_rp(
  data = data_alcohol, niter = 2000, nwarmup = 1000, nchain = 4, ncore = 4)
saveRDS(output_prl_rp_alcohol, "output_hbayes_prl_rp_alcohol.rds")

output_prl_rp_alcoholsaccharin<- prl_rp(
  data = data_alcoholsaccharin, niter = 2000, nwarmup = 1000, nchain = 4, ncore = 4)
saveRDS(output_prl_rp_alcoholsaccharin, "output_hbayes_prl_rp_alcoholsaccharin.rds")

output_prl_rp_saccharin<- prl_rp(
  data = data_saccharin, niter = 2000, nwarmup = 1000, nchain = 4, ncore = 4)
saveRDS(output_prl_rp_saccharin, "output_hbayes_prl_rp_saccharin.rds")

output_prl_rp_water<- prl_rp(
  data = data_water, niter = 2000, nwarmup = 1000, nchain = 4, ncore = 4)
saveRDS(output_prl_rp_water, "output_hbayes_prl_rp_water.rds")

finish <- Sys.time()
print(c(start, finish))
cat("")

# done before -------------------------------------------------------------
# library(dplyr)
# library(ggplot2)
# df <- read.table(
#  file.choose(),
#  header = T,
#  colClasses = "character")
#
# df = within(df, {
#   subjID = as.numeric(subjID)
#   choice = as.integer(choice)
#   outcome = as.integer(outcome)
#   })
# output_alcohol <- bandit2arm_delta(
#   data = data_alcohol, niter = 2000, nwarmup = 1000, nchain = 4, ncore = 4)
# output_alcoholsaccharin <- bandit2arm_delta(
#   data = data_alcoholsaccharin, niter = 2000, nwarmup = 1000, nchain = 4, ncore = 4)
# output_saccharin <- bandit2arm_delta(
#   data = data_saccharin, niter = 2000, nwarmup = 1000, nchain = 4, ncore = 4)
# output_water <- bandit2arm_delta(
#   data = data_water, niter = 2000, nwarmup = 1000, nchain = 4, ncore = 4)
# notes -------------------------------------------------------------------
# bandit2arm_preprocess_func <- function(raw_data, general_info) {
#   # Currently class(raw_data) == "data.table"
#
#   # Use general_info of raw_data
#   subjs   <- general_info$subjs
#   n_subj  <- general_info$n_subj
#   t_subjs <- general_info$t_subjs
#   t_max   <- general_info$t_max
#
#   # Initialize (model-specific) data arrays
#   choice  <- array(-1, c(n_subj, t_max))
#   outcome <- array( 0, c(n_subj, t_max))
#
#   # Write from raw_data to the data arrays
#   for (i in 1:n_subj) {
#     subj <- subjs[i]
#     t <- t_subjs[i]
#     DT_subj <- raw_data[raw_data$subjid == subj]
#
#     choice[i, 1:t]  <- DT_subj$choice
#     outcome[i, 1:t] <- DT_subj$outcome
#   }
#
#   # Wrap into a list for Stan
#   data_list <- list(
#     N       = n_subj,
#     T       = t_max,
#     Tsubj   = t_subjs,
#     choice  = choice,
#     outcome = outcome
#   )
#
#   # Returned data_list will directly be passed to Stan
#   return(data_list)
# }
#
# output_FN <- hBayesDM_model(
#   task_name       = "bandit2arm",
#   model_name      = "delta",
#   model_type      = "",
#   data_columns    = c("subjID", "choice", "outcome"),
#   parameters      = list(
#     "A" = c(0, 0.5, 1),
#     "tau" = c(0, 1, 9)
#   ),
#   regressors      = NULL,
#   postpreds       = c("y_pred"),
#   preprocess_func = bandit2arm_preprocess_func)
#
# output <- output_FN(
#   data = "choose",
#   niter = 2000,
#   nwarmup = 1000,
#   nchain = 4,
#   ncore = 4)
