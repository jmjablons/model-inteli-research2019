
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

# if only given animals wanted
#   then `{r} dplyr::filter(tag %in% manimal$tag)`

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
  
#for *Control  
#    do the same, but change the value of 'hero' variable
  
(pIntervalsHero | pIntervalsControl) /
  (pStayHero | pStayControl) + 
    plot_layout(ncol = 1, heights = c(4, 6)) + 
    plot_annotation(tag_levels = "A")


# FIGURE 5 ----------------------------------------------------------------

# [list] of plots

pGlmOdds <-
  list(
    intercept = 
      rGlm %>%
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
      rGlm %>%
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
      rGlm %>%
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
      rGlm %>%
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
  

# FIGURE 6 ----------------------------------------------------------------

  tLimitBeta <- 10
  
  pOptimBasic <-
    oBasic %>%
    filter(beta <= tLimitBeta) %>%
    ggplot(aes(alpha, beta, z = nll)) +
    geom_raster(aes(fill = nll)) +
    geom_contour(colour = 'black',
                 binwidth = 200) +
    theme_publication +
    theme(legend.position = "bottom") +
    scale_x_continuous(
      breaks = c(0, 0.5, 1),
      limits = c(0, 1),
      expand = c(0, 0)
    ) +
    scale_y_continuous(
      breaks = c(0, 2, 10, 20),
      limits = c(0, NA),
      expand = c(0, 0)
    ) +
    geom_point(data = filter(oBasic, min == T),
               colour = "red",
               shape = 8) +
    scale_fill_gradient(
      low = "white",
      high = "black",
      limits = c(
        oTime %>%
          filter(beta <= tLimitBeta) %>%
          summarise(min(nll)) %>%
          unlist(),
        oBasic %>%
          filter(beta <= tLimitBeta) %>%
          summarise(max(nll)) %>%
          unlist()
      )
    ) +
    coord_flip()
  
  pOptimTime <-
    oTime %>%
    filter(
      beta <= tLimitBeta) %>%
    ggplot(
      aes(alpha, beta, z = nll)) +
    geom_raster(
      aes(fill = nll)) +
    geom_contour(
      colour = 'black', 
      binwidth = 200) +
    theme_publication +
    theme(legend.position = "bottom") +
    scale_x_continuous(
      breaks = c(0, 0.5, 1),
      limits = c(0, 1),
      expand = c(0, 0)
    ) +
    scale_y_continuous(
      breaks = c(0, 2, 10, 20),
      limits = c(0, NA),
      expand = c(0, 0)
    ) +
    geom_point(
      data = filter(oTime, min == T),
      colour = "red",
      shape = 8) +
    scale_fill_gradient(
      low = "white",
      high = "black",
      limits = c(
        oTime %>% 
          filter(beta <= tLimitBeta) %>% 
          summarise(min(nll)) %>% 
          unlist(),
        oBasic %>% 
          filter(beta <= tLimitBeta) %>% 
          summarise(max(nll)) %>% 
          unlist()
      )
    ) +
    coord_flip()
  
  (pOptimBasic + pOptimTime) + 
    plot_layout() + 
    plot_annotation(
      tag_levels = "A", 
      caption = list("(A) basic (B) time "))
  

# FIGURE 7 ----------------------------------------------------------------

  #pAIC <-
    newAll %>%
    mutate(substance = Substance, Substance = NULL) %>%
    tidyr::gather(
      'model', 'value',-tag,-substance) %>%
    ggplot(
      aes(x = model, y = value)) +
    geom_boxplot(
      outlier.colour = NA,
      fill = NA,
      size = 0.4) +
    geom_quasirandom(
      width = 0.2,
      #method = "tukeyDense",
      method = "quasirandom",
      varwidth = TRUE,
      colour = 'black',
      size = 1
    ) +
    theme_publication +
    scale_y_log10(
      breaks = 
        scales::trans_breaks(
          "log10", 
          function(x) 10 ^ x),
      labels = 
        scales::trans_format(
          "log10", 
          scales::math_format(10 ^ .x)),
      limits = c(10 ^ 0, 10 ^ 4),
      expand = c(0, 0)
    ) +
    labs(y = "AIC", x = element_blank()) +
    theme(
      panel.spacing = unit(2, "lines"),
      axis.text.x = element_text(angle = 45, hjust = 1),
      axis.ticks.y = element_line(linetype = 'dashed'),
      axis.line.x = element_blank()
    ) +
    facet_wrap( ~ substance, ncol = 2)

# FIGURE 8 ----------------------------------------------------------------

  pTimeF <-
    list(
      lod = mTimeOptimised %>%
        select(tag, par.lod, substance) %>%
        filter(tag %in% tAnimals) %>%
        tidyr::gather(parameter, value,-tag,-substance) %>%
        ggplot(aes(x = substance, y = value)) +
        scale_y_continuous(limits = c(0, 2), expand = c(0, 0)) +
        facet_wrap( ~ parameter) +
        geom_boxplot(outlier.colour = NA) +
        geom_quasirandom(
          width = 0.2,
          method = "quasirandom",
          varwidth = TRUE
        ) +
        theme_publication +
        theme(
          panel.spacing = unit(2, "lines"),
          axis.text.x = element_text(angle = 45, hjust = 1),
          axis.ticks.x = element_blank(),
          axis.line.x = element_blank()
        ),
      #
      alpha = mTimeOptimised %>%
        select(tag, par.alpha, substance) %>%
        filter(tag %in% tAnimals) %>%
        tidyr::gather(parameter, value,-tag,-substance) %>%
        ggplot(aes(x = substance, y = value)) +
        scale_y_continuous(limits = c(0, 1), expand = c(0, 0)) +
        facet_wrap( ~ parameter) +
        geom_boxplot(outlier.colour = NA) +
        geom_quasirandom(
          width = 0.2,
          method = "quasirandom",
          varwidth = TRUE
        ) +
        theme_publication +
        theme(
          panel.spacing = unit(2, "lines"),
          axis.text.x = element_text(angle = 45, hjust = 1),
          axis.ticks.x = element_blank(),
          axis.line.x = element_blank()
        ),
      #
      beta = mTimeOptimised %>%
        select(tag, par.beta, substance) %>%
        filter(tag %in% tAnimals) %>%
        tidyr::gather(parameter, value,-tag,-substance) %>%
        ggplot(aes(x = substance, y = value)) +
        scale_y_continuous(
          limits = c(0, 50),
          breaks = c(0, 5, 15, 50),
          expand = c(0, 0)
        ) +
        facet_wrap( ~ parameter) +
        geom_boxplot(outlier.colour = NA) +
        geom_quasirandom(
          width = 0.2,
          method = "quasirandom",
          varwidth = TRUE
        ) +
        theme_publication +
        theme(
          panel.spacing = unit(2, "lines"),
          axis.text.x = element_text(angle = 45, hjust = 1),
          axis.ticks.x = element_blank(),
          axis.line.x = element_blank()
        )
    )
  
  pTimeC <-
    list(
      lod = rGlm %>%
        filter(Predictor == 'IntervalAfter') %>%
        mutate(
          tag = Tag,
          estimate = Estimate,
          substance = Substance
        ) %>%
        filter(tag %in% tAnimals) %>%
        select(tag, estimate, substance) %>%
        tidyr::gather(parameter, value,-tag,-substance) %>%
        ggplot(aes(x = substance, y = value)) +
        scale_y_continuous(
          limits = c(-.003, .009),
          breaks = c(-0.003, 0, .003, .009),
          expand = c(0, 0)
        ) +
        facet_wrap( ~ parameter) +
        geom_boxplot(outlier.colour = NA) +
        geom_quasirandom(
          width = 0.2,
          method = "quasirandom",
          varwidth = TRUE
        ) +
        theme_publication +
        theme(
          panel.spacing = unit(2, "lines"),
          axis.text.x = element_text(angle = 45, hjust = 1),
          axis.ticks.x = element_blank(),
          axis.line.x = element_blank()
        ),
      alpha = mTime %>%
        select(tag, par.alpha, substance) %>%
        filter(tag %in% tAnimals) %>%
        tidyr::gather(parameter, value,-tag,-substance) %>%
        ggplot(aes(x = substance, y = value)) +
        scale_y_continuous(limits = c(0, 1), expand = c(0, 0)) +
        facet_wrap( ~ parameter) +
        geom_boxplot(outlier.colour = NA) +
        geom_quasirandom(
          width = 0.2,
          method = "quasirandom",
          varwidth = TRUE
        ) +
        theme_publication +
        theme(
          panel.spacing = unit(2, "lines"),
          axis.text.x = element_text(angle = 45, hjust = 1),
          axis.ticks.x = element_blank(),
          axis.line.x = element_blank()
        ),
      beta = mTime %>%
        select(tag, par.beta, substance) %>%
        filter(tag %in% tAnimals) %>%
        tidyr::gather(parameter, value,-tag,-substance) %>%
        ggplot(aes(x = substance, y = value)) +
        scale_y_continuous(
          limits = c(0, 50),
          breaks = c(0, 5, 15, 50),
          expand = c(0, 0)
        ) +
        facet_wrap( ~ parameter) +
        geom_boxplot(outlier.colour = NA) +
        geom_quasirandom(
          width = 0.2,
          method = "quasirandom",
          varwidth = TRUE
        ) +
        theme_publication +
        theme(
          panel.spacing = unit(2, "lines"),
          axis.text.x = element_text(angle = 45, hjust = 1),
          axis.ticks.x = element_blank(),
          axis.line.x = element_blank()
        )
    )
  
  pDecay <-
    list(
      alpha = mDecay %>%
        select(tag, par.alpha, substance) %>%
        filter(tag %in% tAnimals) %>%
        tidyr::gather(parameter, value, -tag, -substance) %>%
        ggplot(aes(x = substance, y = value)) +
        facet_wrap(~ parameter) +
        geom_boxplot(outlier.colour = NA) +
        geom_quasirandom(
          width = 0.2,
          method = "quasirandom",
          varwidth = TRUE
        ) +
        scale_y_continuous(limits = c(0, 1), expand = c(0, 0)) +
        theme_publication +
        theme(
          panel.spacing = unit(2, "lines"),
          axis.text.x = element_text(angle = 45, hjust = 1),
          axis.ticks.x = element_blank(),
          axis.line.x = element_blank()
        ),
      beta = mDecay %>%
        select(tag, par.beta, substance) %>%
        filter(tag %in% tAnimals) %>%
        tidyr::gather(parameter, value, -tag, -substance) %>%
        ggplot(aes(x = substance, y = value)) +
        facet_wrap(~ parameter) +
        geom_boxplot(outlier.colour = NA) +
        geom_quasirandom(
          width = 0.2,
          method = "quasirandom",
          varwidth = TRUE
        ) +
        scale_y_continuous(
          limits = c(0, 50),
          breaks = c(0, 5, 15, 50),
          expand = c(0, 0)
        ) +
        theme_publication +
        theme(
          panel.spacing = unit(2, "lines"),
          axis.text.x = element_text(angle = 45, hjust = 1),
          axis.ticks.x = element_blank(),
          axis.line.x = element_blank()
        ),
      storage = mDecay %>%
        select(tag, par.storage, substance) %>%
        filter(tag %in% tAnimals) %>%
        tidyr::gather(parameter, value, -tag, -substance) %>%
        ggplot(aes(x = substance, y = value)) +
        facet_wrap(~ parameter) +
        geom_boxplot(outlier.colour = NA) +
        geom_quasirandom(
          width = 0.3,
          method = "quasirandom",
          varwidth = TRUE
        ) +
        scale_y_continuous(
          limits = c(0, 0.3),
          breaks = c(0, .1, .3),
          expand = c(0, 0)
        ) +
        theme_publication +
        theme(
          panel.spacing = unit(2, "lines"),
          axis.text.x = element_text(angle = 45, hjust = 1),
          axis.ticks.x = element_blank(),
          axis.line.x = element_blank()
        )
    )
  
  pBasic <- list(
    alpha = mBasic %>%
      select(tag, par.alpha, substance) %>%
      filter(tag %in% tAnimals) %>%
      tidyr::gather(parameter, value, -tag, -substance) %>%
      ggplot(aes(x = substance, y = value)) +
      facet_wrap(~ parameter) +
      geom_boxplot(outlier.colour = NA) +
      geom_quasirandom(
        width = 0.2,
        method = "quasirandom",
        varwidth = TRUE
      ) +
      scale_y_continuous(limits = c(0, 1), expand = c(0, 0)) +
      theme_publication +
      theme(
        panel.spacing = unit(2, "lines"),
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.ticks.x = element_blank(),
        axis.line.x = element_blank()
      ),
    beta = mBasic %>%
      select(tag, par.beta, substance) %>%
      filter(tag %in% tAnimals) %>%
      tidyr::gather(parameter, value, -tag, -substance) %>%
      ggplot(aes(x = substance, y = value)) +
      facet_wrap(~ parameter) +
      geom_boxplot(outlier.colour = NA) +
      geom_quasirandom(
        width = 0.2,
        method = "quasirandom",
        varwidth = TRUE
      ) +
      scale_y_continuous(
        limits = c(0, 50),
        breaks = c(0, 5, 15, 50),
        expand = c(0, 0)
      ) +
      theme_publication +
      theme(
        panel.spacing = unit(2, "lines"),
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.ticks.x = element_blank(),
        axis.line.x = element_blank()
      )
  )
  
  pTimeFadd <-
    mTimeOptimised %>%
    select(tag, par.beta, substance) %>%
    filter(tag %in% tAnimals) %>%
    tidyr::gather(parameter, value,-tag,-substance) %>%
    ggplot(aes(x = substance, y = value)) +
    scale_y_continuous(limits = c(0, 1.5),
                       #breaks = c(0, 5, 15, 50),
                       expand = c(0, 0)) +
    facet_wrap( ~ parameter) +
    geom_boxplot(outlier.colour = NA) +
    geom_quasirandom(
      width = 0.2,
      method = "quasirandom",
      varwidth = TRUE,
      size = .4
    ) +
    theme_publication +
    theme(
      panel.spacing = unit(2, "lines"),
      #axis.text = element_text(angle = 45, hjust = 1),
      axis.ticks.x = element_blank(),
      axis.line.x = element_blank(),
      axis.text.x = element_blank(),
      axis.title = element_blank(),
      axis.title.x = element_blank()
    )
  
  pRandomTimeOptimised <-
    list(
      lod = mRandomTimeOptimisedLine %>%
        select(tag, par, substance) %>%
        filter(tag %in% tAnimals) %>%
        tidyr::gather(parameter, value, -tag, -substance) %>%
        ggplot(aes(x = substance, y = value)) +
        facet_wrap(~ parameter) +
        geom_boxplot(outlier.colour = NA) +
        geom_quasirandom(
          width = 0.2,
          method = "quasirandom",
          varwidth = TRUE
        ) +
        scale_y_continuous(limits = c(0, 2), expand = c(0, 0)) +
        theme_publication +
        theme(
          panel.spacing = unit(2, "lines"),
          axis.text.x = element_text(angle = 45, hjust = 1),
          axis.ticks.x = element_blank(),
          axis.line.x = element_blank()
        )
    )
  
  pRandomTimeOptimisedLine <-
    list(
      lod = mRandomTimeOptimisedLine %>%
        select(tag, par, substance) %>%
        filter(tag %in% tAnimals) %>%
        tidyr::gather(parameter, value, -tag, -substance) %>%
        ggplot(aes(x = substance, y = value)) +
        facet_wrap(~ parameter) +
        geom_boxplot(outlier.colour = NA) +
        geom_quasirandom(
          width = 0.2,
          method = "quasirandom",
          varwidth = TRUE
        ) +
        scale_y_continuous(limits = c(0, 2), expand = c(0, 0)) +
        theme_publication +
        theme(
          panel.spacing = unit(2, "lines"),
          axis.text.x = element_text(angle = 45, hjust = 1),
          axis.ticks.x = element_blank(),
          axis.line.x = element_blank()
        )
    )
  
# plot all the graphs  
  ((pBasic$alpha | pBasic$beta | pRandomTimeOptimisedLine$lod) /
      (pTimeC$alpha | pTimeC$beta | pTimeC$lod) /
      (pTimeF$alpha | pTimeF$beta | pTimeF$lod) /
      (pDecay$alpha | pDecay$beta | pDecay$storage)) +
    plot_layout(nrow = 4) +
    plot_annotation(
      tag_levels = "A",
      caption = "by rows: (1) model basic (C) random + time optimised\n
      (2) time with LOD from regression \n
      (3) time without constrain on lod \n
      (4) decay
      \n ANIMALS CHOSEN IF PREDICTOR REWARD (GLM) SIGNIFICANT"
    ) &
    theme(axis.title.x = element_blank(), 
          axis.ticks.x = element_blank())
  
pTimeFadd
  
# ADDS --------------------------------------------------------------------

#as posted on stackoverflow
#   https://kutt.it/huJptV
# asinh_trans <- function(){
#   trans_new(
#     name = 'asinh', 
#     transform = function(x) asinh(x), 
#     inverse = function(x) sinh(x))
# }
# 
  # pDelta <- 
  #   rDelta %>%
  #   merge(
  #     iAnimals %>% select(tag, exp)) %>%
  #   tidyr::gather(
  #     'model','value', -tag, -substance, -exp)%>%
  #   ggplot(
  #     aes(x = model, y = value))+
  #   geom_line(
  #     aes(group = tag), alpha = 0.5)+
  #   geom_hline( 
  #     yintercept = 0, colour = "red", linetype = "dashed")+
  #   geom_hline( 
  #     yintercept = -50, linetype = "dashed")+
  #   scale_y_continuous(
  #     trans = 'asinh', 
  #     #limits = c(-200, 2000), 
  #     breaks = c(-2000, -50, -2, 2, 50, 2000), 
  #     expand = c(0,1))+
  #   theme_publication+
  #   facet_wrap(substance ~ exp, ncol = 1)+
  #   theme(
  #     panel.spacing = unit(1, "lines"),
  #     axis.text.x = element_text(angle = 45, hjust = 1),
  #     axis.ticks.y = element_line(linetype = 'dashed'),
  #     axis.line.x = element_blank())+
  #   labs(y = 'delta AIC')  
  