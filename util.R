util <- list()

util$binal <- function(input, how.long = "2 days", hour = 1, allow.group = T) {
  input = dplyr::arrange(input, start, .by_group = allow.group)
  input$bin = NA
  for(exp in unique(input$exp)){
    input$bin[input$exp == exp] <- 
      cut(input$start[input$exp == exp],
          breaks = as.POSIXct(seq(from = as.POSIXct(paste0(
            strftime(input$start[input$exp == exp][1], 
                     format = '%Y-%m-%d'), hour, ':00:00')),
            to = input$end[input$exp == exp][nrow(input[input$exp == exp,])], 
            by = how.long)),labels = FALSE)} 
  input}

util$plotpar <- function(.name = "basic", .par = "par.alpha", .gg.limit = c(0,1),
                         .gg.break = c(0,1), .gg.space = 2, a = pubmodel){
  a[[.name]] %>%
    select(value = .par, tag) %>%
    left_join(manimal, by = "tag") %>%
    mutate(par = .par,
           originalvalue = value,
           value = ifelse(value <= .gg.limit[2], value, .gg.limit[2])) %>%
    ggplot(aes(x = substance, y = value)) +
    box_default() + median_default + point_default() +
    geom_text(aes(label = ifelse(originalvalue < .gg.limit[2], 
                                 NA, round(originalvalue, 2))), 
              hjust = 0, nudge_x = 0, size = 2)+
    scale_y_continuous(limits = .gg.limit, expand = c(0.1,0),
                       breaks = .gg.break) +
    facet_wrap(~par, scales = "free_y") + 
    theme_publication +
    theme(axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          axis.line.x = element_blank(),
          axis.text.x = element_text(angle = 45, hjust = 1),
          axis.ticks.x = element_blank())}

util$signif <- function(x.where, y.where, y.space = 0.1, 
                        colour = "darkgray"){
  list(annotate(geom = "line", x = x.where, y = y.where, colour = colour),
       annotate(geom = "text", label = "*", x = mean(x.where), 
                y = y.where + y.space, colour = colour))}

util$getpreference <- function(n.hours = 96, period.info = "adaptation", a = dall){
  period <- a %>%
    filter(info == period.info) %>%
    group_by(exp, info) %>%
    arrange(start, .by_group = T) %>%
    summarise(finish = tail(start, 1),
              begin = finish - lubridate::hours(n.hours)) %>%
    ungroup()
  a %>%
    group_by(exp, info) %>%
    filter(start > period$begin[match(exp, period$exp)] & 
             end < period$finish[match(exp, period$exp)], 
           .preserve = TRUE) %>%
    group_by(tag, exp) %>%
    summarise(
      nlickwater = sum(nlick[which(rp == 0)], na.rm = TRUE),
      nlickreward = sum(nlick[which(rp > 0)], na.rm = TRUE),
      value = nlickreward / (nlickreward + nlickwater)) %>%
    ungroup()}

util$get <- function(.name = "basic", .column = "par.beta",
                     .substance = substances, a = pubmodel){
  a[[.name]] %>%
    left_join(manimal) %>%
    filter(substance %in% .substance) %>%
    left_join(dmodel %>% group_by(tag,exp) %>% summarise()) %>%
    ungroup() %>%
    mutate(exp = as.factor(exp),
           substance = as.factor(substance)) %>%
    select(tag, substance, exp, value = .column)}

util$format <- function(x){ifelse(x%%1 > 0, 
    as.character(x), as.character(as.integer(x)))}

util$winstay <- function(sb, .short = 2, .long = 10){
  dmodel %>%
    filter(intervala <= .short | intervala >= .long) %>%
    mutate(short = ifelse(intervala <= .short, 
                          paste0("<",.short, collapse = ""), 
                          paste0(">",.long, collapse = ""))) %>%
    group_by(tag, short) %>%
    summarise(`win-stay` = length(which(dooropened == 1 & stay == 1))/ 
                length(which(dooropened == 1)),
              `lose-shift` = length(which(dooropened == 0 & stay == 0))/
                length(which(dooropened == 0))) %>%
    tidyr::gather(param, value, -tag, -short) %>%
    mutate(param = factor(param, levels = c("win-stay", "lose-shift"), 
                          ordered = T)) %>%
    left_join(manimal) %>% 
    filter(substance %in% sb)}

util$wrap_winstay <- function(.substance, what){
  util$winstay(sb = .substance) %>%
    filter(param %in% what) %>%
    tidyr::spread(short, value)}

util$aictidy <- function(a = rmodel){
  a %>% purrr::map(~select(., name, aic, tag)) %>%
    bind_rows() %>%
    mutate(delta = aic - .$aic[.$tag == tag & .$name == "basic"]) %>%
    left_join(manimal) %>%
    left_join(
      (dmodel %>% group_by(tag) %>% summarise(ntrial = n())), 
      by = "tag")}

util$waic <- function(a = pubmodel){
  a %>% purrr::map(~select(., name, aic, tag)) %>%
    bind_rows() %>%
    group_by(tag) %>%
    arrange(aic, .by_group = T) %>%
    mutate(minaic = .$aic[.$tag == tag][1])%>%
    group_by(tag, name) %>%
    mutate(delta = aic - minaic) %>%
    ungroup() %>%
    group_by(tag) %>%
    mutate(sumall = sum(exp(-delta/2))) %>% 
    ungroup() %>%
    group_by(tag, name) %>%
    mutate(waic = exp(-(delta/2)) / sumall) %>%
    ungroup() %>%
    left_join(manimal) %>%
    left_join(
      (dmodel %>% group_by(tag) %>% summarise(ntrial = n())), 
      by = "tag")}

util$format_digit <- function(x, .digits = 2, .replacement = "x10^", na.omit = T){
  ifelse(x <= 1, (format(x, digits = .digits, scientific = T) %>%
                    gsub(pattern = "e", replacement = .replacement)), round(x, digits = .digits))}

util$assign_cohort <- function(a){
  a %>% left_join(manimal) %>%
    mutate(cohort = gg$cohort$label[match(exp, gg$cohort$exp)],
           gr = paste(substance, cohort, sep = " ")) %>%
    mutate(substance = as.factor(gr))}

util$surface <- function(tag.vec = NULL, a = subset(dmodel, tag %in% hero)) {
  if(is.null(tag.vec)){tag.vec = unique(a$tag)} 
  lapply(tag.vec, function(m){
    dmouse = a[a$tag %in% m,]
    do.call(rbind, lapply(init, function(x){
      c(tag = as.numeric(m), x, nll = model(par = x, a = dmouse))}))})}

sem <- function(x, na.rm = T) {
  stopifnot(is.numeric(x))
  if (na.rm) x = na.omit(x)
  sd(x) / sqrt(length(x))}

asinh_trans <- function() {
  #as posted on stackoverflow
  #   https://kutt.it/huJptV
  trans_new("asinh",
            transform = asinh,
            inverse   = sinh)}