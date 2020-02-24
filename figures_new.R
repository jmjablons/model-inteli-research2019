#new figures
#2020-02-24

# custom ------------------------------------------------------------------

binal <- function(input, how.long = "2 days", hour = 1) {
    input = dplyr::arrange(input, start, .by_group = TRUE) %>%
      mutate(bin = NA)
    for(exp in unique(input$exp)){
      input$bin[input$exp == exp] <- 
        cut(input$start[input$exp == exp],
            breaks = as.POSIXct(seq(from = as.POSIXct(paste0(
              strftime(input$start[input$exp == exp][1], 
                       format = '%Y-%m-%d'), hour, ':00:00')),
              to = input$end[input$exp == exp][nrow(input[input$exp == exp,])], 
              by = how.long)),labels = FALSE)} 
    input}

# fig 2 -------------------------------------------------------------------
#number of visits

dall %>% 
  binal() %>%
  filter(info != "finish") %>%
  mutate(info = ifelse(info %in% "welcome", "adaptation", info)) %>%
  group_by(period = bin, tag, info) %>% 
  summarise(nvisit = n()) %>%
  left_join(manimal) %>%
  group_by(period, info, exp, substance) %>%
  summarise(measure = mean(nvisit, na.rm = T),
    sem = sem(nvisit, na.rm = T)) %>%
  #plot
  ggplot(aes(x=period, y=measure)) + 
  geom_hline(yintercept = 200, linetype = 'dotted')+
  geom_ribbon(aes(ymin=measure-sem, ymax=measure+sem, 
      group = interaction(substance, exp)), 
    colour = NA, alpha = 0.2)+ 
  geom_point(aes(fill = info), pch = 21)+
  scale_y_continuous(
    breaks=c(0, 100, 200, 400), limits = c(0, 400), expand = c(0,0))+
  scale_x_continuous(
    breaks=c(0, 15, 35), limits = c(0, 35), expand = c(0,0))+
  ylab('Number of visits')+
  xlab('Bin [48h]')+
  scale_color_manual(values = c('darkgrey','black'))+
  scale_fill_manual(values = c('white','black'))+
  facet_wrap(exp~substance, ncol = 2)+ 
  theme_publication
