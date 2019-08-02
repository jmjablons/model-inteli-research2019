
# DEPENDENCIES ------------------------------------------------------------

require(ggplot2)
require(dplyr)
require(tidyr)
require(lubridate)
require(patchwork) 
require(ggbeeswarm)
#devtools::install_github("thomasp85/patchwork")
#devtools::install_github("eclarke/ggbeeswarm")

# CHOSEN ONES -------------------------------------------------------------

# fig 5
#   to make the code shorter
#   mainly to avoid repetition
#   just choose an animal as below
hero = "900110000199546" # champion from saccharin group
hero = "900110000351935" # champion from water/control group


# CUSTOM ------------------------------------------------------------------

sem <- 
  function(a, na.rm = T) {
    stopifnot(is.numeric(a))
    if (na.rm) {
      a = na.omit(a)
    }
    sd(a) / sqrt(length(a))
  }

# expand a ggplot2 theme
theme_publication <- 
  theme(
    legend.title = element_blank(),
    legend.background = element_blank(),
    legend.direction = "horizontal",
    legend.box = "vertical",
    legend.position = c(0.0, 0.80),
    legend.justification = c(0, 0),
    legend.key = element_blank(),
    axis.ticks = element_line(linetype = 'dashed'),
    axis.line = element_line(
      colour = "black",
      linetype = "solid"),
    axis.title.x = element_text(hjust = 1),
    panel.border = element_blank(),
    panel.grid = element_blank(),
    panel.spacing = unit(1, "lines"),
    panel.background = element_blank(),
    strip.background = element_blank(),
    strip.placement = "inside",
    strip.text = element_text(
      angle = 0,
      hjust = 0
    ),
    complete = FALSE
  )


# FIGURE 2 ----------------------------------------------------------------

#pVisits <- 
  dData %>%
  mutate(
    Info = ifelse(
      Info == 'preadaptation', 
      'adaptation', 
      Info)
    ) %>%
  group_by(
    Period = Bin, Tag, Strain, Genotype, Substance, Exp
    ) %>% 
  summarise(
    nVisits = n(),
    Info = Info[1]) %>%
  group_by(
    Period, Substance, Strain, Genotype, Info, Exp
    ) %>%
  summarise(
    measure = mean(nVisits, na.rm = T),
    sem = sem(nVisits, na.rm = T)) %>%
  ggplot(
    aes(x=Period, y=measure)) + 
  geom_hline(
    yintercept = 200, 
    linetype = 'dotted')+
  geom_ribbon(
    aes(
      ymin=measure-sem, 
      ymax=measure+sem, 
      group = interaction(Substance, Exp)), 
    colour = NA, 
    alpha = 0.2)+ 
  geom_point(
    aes(fill = Info), 
    pch = 21)+
  scale_y_continuous(
    breaks=c(0, 100, 200, 400), 
    limits = c(0, 400), 
    expand = c(0,0))+
  scale_x_continuous(
    breaks=c(0, 15, 35), 
    limits = c(0, 35), 
    expand = c(0,0))+
  ylab('Number of visits')+
  xlab('Bin [48h]')+
  scale_color_manual(
    values = c('darkgrey','black'))+
  scale_fill_manual(
    values = c('white','black'))+
  facet_wrap(Substance~Exp, ncol = 2)+ 
  theme_publication


# FIGURE 3 ----------------------------------------------------------------

(pChoices | pPreferenceLick | pBetterRatio) + 
  plot_layout(ncol = 3, widths = c(1, 1, 1)) + 
  plot_annotation(tag_levels = "A")

#pChoices <- 
  dData %>%
  mutate(
    Info = 
      ifelse(
        Info == 'preadaptation', 
        'adaptation', Info)
    ) %>%
  filter(Info == 'reversals') %>%
  group_by(
    Tag, Strain, Genotype, Substance
    ) %>% 
  summarise(
    measure = 
      length(which(RewardProbability > 0   & 
                     VisitDuration > 2))
    ) %>%
  ggplot(
    aes(
      x = Substance,
      y = measure,
      group = Substance)) +
  geom_boxplot(
    alpha = 0.3, 
    outlier.colour = NA) +
  geom_quasirandom(
    width = 0.12,
    method = "tukeyDense",
    varwidth = TRUE) +
  scale_y_continuous(
    limits = c(0, 3000),
    expand = c(0, 0)
  ) +
  labs(y = 'Number of choices [reversals]') +
  theme_publication +
  theme(
    axis.title.x = element_blank(),
    axis.text.x = 
      element_text(angle = 45, hjust = 1),
    axis.ticks.y = 
      element_line(linetype = 'dashed'),
    axis.line.x = element_blank()
  )

  
#pPreferenceLick <- 
  rPreference %>%
  ggplot(
    aes(
      x = Substance,
      y = LickPreference,
      group = Substance)) +
  geom_hline(
    yintercept = 0.5,
    colour = 'black',
    linetype = "dashed") +
  geom_boxplot(
    alpha = 0.3, 
    outlier.colour = NA) +
  geom_quasirandom(
    width = 0.2,
    method = "tukeyDense",
    varwidth = TRUE) +
  scale_y_continuous(
    breaks = c(0, 0.5, 1),
    limits = c(0, 1),
    expand = c(0, 0)
  ) +
  labs(y = 'Reward preference [licks]') +
  theme_publication +
  theme(
    axis.title.x = element_blank(),
    axis.text.x = 
      element_text(angle = 45, hjust = 1),
    axis.ticks.y = 
      element_line(linetype = 'dashed'),
    axis.line.x = element_blank()
  )
  
#pBetterRatio <- 
  rBetterRatio %>%
  ggplot(
    aes(
      x = Substance,
      y = BR,
      group = Substance)) +
  geom_hline(
    yintercept = 0.5,
    colour = 'black',
    linetype = "dashed") +
  geom_boxplot(
    alpha = 0.3, 
    outlier.colour = NA) +
  geom_quasirandom(
    width = 0.2,
    method = "tukeyDense",
    varwidth = TRUE) +
  scale_y_continuous(
    breaks = c(0, 0.5, 1),
    limits = c(0, 1),
    expand = c(0, 0)) +
  labs(
    y = 'Preference of more certain probability') +
  theme_publication +
  theme(
    axis.title.x = element_blank(),
    axis.text.x = 
      element_text(angle = 45, hjust = 1),
    axis.ticks.y = 
      element_line(linetype = 'dashed'),
    axis.line.x = element_blank())+
  geom_signif(
    y_position=c(0.9,0.9,0.9), 
    xmin=c(1,2,4), 
    xmax=c(2,2,2),
    annotation=c('','*',''), 
    textsize = 2 * ggplot2:::.pt, 
    tip_length=0.02, 
    vjust = 0.6)+
  geom_signif(
    y_position=c(0.95,0.95,0.95), 
    xmin=c(1,3,4), 
    xmax=c(3,3,3),
    annotation=c('','*',''), 
    textsize = 2 * ggplot2:::.pt, 
    tip_length=0.02, 
    vjust = 0.6)

(pChoices | pPreferenceLick | pBetterRatio) + 
    plot_layout(ncol = 3, widths = c(1, 1, 1)) + 
    plot_annotation(tag_levels = "A")
  

# FIGURE 4 ----------------------------------------------------------------

tLabel <- c(
  `0` = 'after lose', 
  `1` = 'after win')
tValues <- log(c(0.03, 1, 5, 60, 600))

#pStayHero <-
  dChoices %>%
  filter(
    Tag == hero
  ) %>%
  ggplot(
    aes(
      x = log(IntervalAfter), 
      y = Stay)) +
  geom_quasirandom(
    groupOnX=FALSE,
    stroke = 0,
    shape=16,
    alpha = 0.4,
    width = 0.05, 
    method = "tukeyDense"
  ) +
  geom_hline(
    yintercept = 0.5, 
    linetype = 'dotted') +
  geom_smooth(
    method="glm", 
    method.args=list(family="binomial"), 
    se=TRUE, 
    colour = 'black')+
  scale_x_continuous(
    limits = c(-6, 8),
    breaks= tValues, 
    labels = exp(tValues), 
    expand = c(0,0)
  ) +
  scale_y_continuous(
    breaks= c(0, 0.5, 1), 
    labels = c(0,0.5,1), 
    expand = c(0.02,0.02)
  ) +
  facet_wrap(
    ~RewardDoor, 
    labeller = labeller(RewardDoor = tLabel)
  ) +
  labs( 
    y = 'probability of stay', 
    x = 'interval [min]'
  )+
  theme_publication+
  theme(
    panel.spacing = unit(1, "lines"),
    axis.line.x = element_blank(),
    axis.text.y = element_text(face = 'bold')
  )

#pIntervalsHero <-
  dChoices %>%
  filter(Tag == hero,
         !is.na(Stay)) %>%
  mutate(
    Stay = as.character(Stay)) %>%
  ggplot(
    aes(
      y = log(IntervalAfter), 
      x = Stay, 
      group = Stay)) +
  geom_boxplot(
    outlier.colour = NA, 
    fill = NA)+
  geom_quasirandom(
    width = 0.2,
    method = "tukeyDense",
    varwidth = TRUE, 
    alpha = 0.1, 
    stroke = 0,
    shape=16) +
  scale_x_discrete(
    labels = c(
      `0` = 'shift', 
      `1` = 'stay')) +
  scale_y_continuous(
    limits = c(-6, 8),
    breaks= tValues, 
    labels = exp(tValues), 
    expand = c(0,0)) +
  theme_publication+
  theme(axis.line.y = element_blank(),
        axis.line.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_text(face = 'bold'),
        axis.ticks.y = element_blank())+
  labs(y = 'interval [min]')+
  coord_flip()
  
(pIntervalsHero | pIntervalsControl) /
  (pStayHero | pStayControl) + 
#for *Control  
#    do the same, but change the value of 'hero' variable
    plot_layout(ncol = 1, heights = c(4, 6)) + 
    plot_annotation(tag_levels = "A")


# FIGURE 5 ----------------------------------------------------------------

# [list] of plots

pGlmOdds <-
  list(
    intercept = 
      rGlm2 %>%
      filter(
        Predictor == 
          (.$Predictor %>% 
             as.factor() %>% 
             levels())[1]
      ) %>% 
      filter(Signif == 1) %>%
      ggplot(
        aes(
          x = Substance, 
          y = Estimate, 
          group = Substance, 
          colour = Predictor)) +
      geom_boxplot(
        fill = NA,
        outlier.colour = NA
      ) +
      geom_quasirandom(
        fill = "black",
        shape = 21, 
        width = 0.12,
        method = "tukeyDense",
        varwidth = TRUE
      ) +
      scale_y_continuous(
        limits = c(-4, 4),
        expand = c(0, 0),
        breaks = c(-4, -2, 0, 2, 4)
      ) +
      facet_wrap(
        ~Predictor, 
        scales = "free_y"
      )+
      theme_publication +
      theme(
        legend.position = 'none',
        axis.title.x = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.ticks.y = element_line(linetype = 'dashed'),
        axis.line.x = element_blank()
      )+
      scale_colour_manual(
        values = rep("black",3)
      ),
    corner = 
      rGlm2 %>%
      filter(
        Predictor == 
          (.$Predictor %>% 
             as.factor() %>% 
             levels())[2]
      ) %>% 
      filter(Signif == 1) %>% 
      ggplot(
        aes(
          x = Substance, 
          y = Estimate, 
          group = Substance, 
          colour = Predictor)) +
      geom_boxplot(
        fill = NA,
        outlier.colour = NA
      ) +
      geom_quasirandom(
        fill = "black",
        shape = 21, 
        width = 0.12,
        method = "tukeyDense",
        varwidth = TRUE
      ) +
      scale_y_continuous(
        limits = c(-7, 7),
        expand = c(0, 0),
        breaks = c(-7, -3, 0, 3, 7)
      ) +
      facet_wrap(
        ~Predictor, 
        scales = "free_y"
      )+
      theme_publication +
      theme(
        legend.position = 'none',
        axis.title.x = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.ticks.y = element_line(linetype = 'dashed'),
        axis.line.x = element_blank()
      )+
      scale_colour_manual(
        values = rep("black",3)
      ),
    interval = 
      rGlm2 %>%
      filter(
        Predictor == 
          (.$Predictor %>% 
             as.factor() %>% 
             levels())[3]
      ) %>% 
      filter(Signif == 1) %>% 
      #summarise(min(Estimate), max(Estimate))
      ggplot(
        aes(
          x = Substance, 
          y = Estimate, 
          group = Substance, 
          colour = Predictor)) +
      geom_boxplot(
        fill = NA,
        outlier.colour = NA
      ) +
      geom_quasirandom(
        fill = "black",
        shape = 21, 
        width = 0.12,
        method = "tukeyDense",
        varwidth = TRUE
      ) +
      scale_y_continuous(
        limits = c(-0.01, 0.01),
        expand = c(0, 0)
        #breaks = c(-7, -3, 0, 3, 7)
      ) +
      facet_wrap(
        ~Predictor, 
        scales = "free_y"
      )+
      theme_publication +
      theme(
        legend.position = 'none',
        axis.title.x = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.ticks.y = element_line(linetype = 'dashed'),
        axis.line.x = element_blank()
      )+
      scale_colour_manual(
        values = rep("black",3)
      ),
    reward = 
      rGlm2 %>%
      filter(
        Predictor == 
          (.$Predictor %>% 
             as.factor() %>% 
             levels())[4]
      ) %>% 
      filter(Signif == 1) %>%
      ggplot(
        aes(
          x = Substance, 
          y = Estimate, 
          group = Substance, 
          colour = Predictor)) +
      geom_boxplot(
        fill = NA,
        outlier.colour = NA
      ) +
      geom_quasirandom(
        fill = "black",
        shape = 21, 
        width = 0.12,
        method = "tukeyDense",
        varwidth = TRUE
      ) +
      scale_y_continuous(
        limits = c(-1, 1),
        expand = c(0, 0)
        #breaks = c(-7, -3, 0, 3, 7)
      ) +
      facet_wrap(
        ~Predictor, 
        scales = "free_y"
      )+
      theme_publication +
      theme(
        legend.position = 'none',
        axis.title.x = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.ticks.y = element_line(linetype = 'dashed'),
        axis.line.x = element_blank()
      )+
      scale_colour_manual(
        values = rep("black",3)
      )
  )
  
((pGlmOdds$intercept | pGlmOdds$reward) / 
    (pGlmOdds$interval | pGlmOdds$corner)) + 
  plot_layout() + 
  plot_annotation(tag_levels = "A")