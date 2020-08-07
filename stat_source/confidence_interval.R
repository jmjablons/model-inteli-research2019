summary_model <- function(.name, .vars, .group = substance){
remodel[[.name]] %>%
  left_join(manimal) %>%
  group_by({{.group}}) %>%
  summarise(across({{ .vars }}, ~ median(., na.rm = TRUE)))}

summary_model(12, c(par.alpha, par.storage.pos, 
                    par.beta, par.storage.neg))

getci <- function(.name, .substance, .var, a = remodel, .rvalue = 10000){
  (a[[.name]] %>%
     left_join(manimal) %>%
     filter(substance %in% .substance) %>%
     select(name, tag, param = {{.var}}))$param %>%
    boot::boot(., function(x,i) {median(x[i])},
               R = .rvalue) %>%
    boot::boot.ci(boot.out = ., 
                  conf = .95, 
                  type = c("norm", "basic" ,"perc", "bca"))}

(getci("hybrid", "alcohol", par.beta))$bca

# JR 
# analiza <- rcompanion::groupwiseMedian(
#   measure ~ name * param + substance,
#                            data       = temp,
#                            conf       = 0.95,
#                            R          = 5000,
#                            percentile = TRUE,
#                            bca        = FALSE,
#                            digits     = 3)
