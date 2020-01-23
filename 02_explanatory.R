
# DATA DESCRIPTION --------------------------------------------------------

# The whole dataset consists of 4 experiments as indicated
# its coded as letters in the Exp column
# A - alcohol inteliStar
# B - alcohol inteliPlus
# C - mixture of saccharin and alcohol
# D - water as a control


# NOTES -------------------------------------------------------------------

#TODO (get rid of all the loop - KISS)

# TODO --------------------------------------------------------------------

# if only given animals wanted
#   then `{r} dplyr::filter(tag %in% manimal$tag)`

# SUBSTANCE PREFERENCE ----------------------------------------------------

# Take data from
# last 96h of adaptation
# for each experiment

temp <- 
  dData %>%
  filter(
    Info == 'adaptation'
    ) %>%
  group_by(Exp, Info) %>%
  summarise(
    end = tail(StartDateTime, 1),
    start = end - hours(96)) 

rPreference <- 
  dData %>%
  filter( 
    #TODO(dplyr | do.call | apply?) 
    (Exp == temp$Exp[1] &
       StartDateTime > temp$start[1] &
       StartDateTime < temp$end[1]) |
      (Exp == temp$Exp[2] & 
         StartDateTime > temp$start[2] & 
         StartDateTime < temp$end[2]) |
      (Exp == temp$Exp[3] &
         StartDateTime > temp$start[3] &
         StartDateTime < temp$end[3]) |
      (Exp == temp$Exp[4] &
         StartDateTime > temp$start[4] &
         StartDateTime < temp$end[4])
    ) %>%
  group_by(
    Tag, Strain, Genotype, Substance
    ) %>%
  summarise(
    nLicksWater = 
      sum(
        LicksNumber[which(RewardProbability == 0)], 
          na.rm = TRUE),
    nLicksGood = 
      sum(
        LicksNumber[which(RewardProbability > 0)], 
          na.rm = TRUE),
    nPleasure = 
      length(
        which(LicksNumber > 0 & 
                RewardProbability > 0)
        ),
    nRewarded = 
      length(
        which(RewardProbability > 0 & 
                DoorOpened == 1)
        ),
    nGood = 
      length(
        which(RewardProbability > 0)
        ),
    nRight = 
      length(
        which(
        (RewardProbability > 0 & 
           DoorOpened == 0) |
          (RewardProbability > 0 & 
             DoorOpened == 1 & 
             LicksNumber > 0))
        ),
    nWater = 
      length(
        which(
          LicksNumber > 0 & 
            RewardProbability == 0)
        ),
    rLickRewarded = 
      nPleasure / nRewarded,
    rLickWater = 
      nLicksWater / nWater,
    rReward = 
      rLickRewarded / (rLickRewarded + rLickWater),
    bias = 
      ((nLicksGood / nRight) / 
         ((nLicksGood / nRight) + 
            (nLicksWater / nWater)) - 0.5),
    AdaptationPreference   =  
      (nRight + (nRight * bias)) / 
      ((nRight + (nRight * bias)) + nWater),
    LickPreference = 
      nLicksGood / (nLicksGood + nLicksWater),
    VisitPreference = 
      nRight / (nRight + nWater)
    ) %>%
  within({
    Substance = as.factor(Substance)
    Tag = as.factor(Tag)
    Strain = as.factor(Strain)
    Genotype = as.factor(Genotype)
  })

#rm(temp)

# PREFERENCE STATISTICS ---------------------------------------------------

#variance between groups

kruskal.test(
  data = rPreference, 
  VisitPreference ~ Substance)$p.value %>%
  format(scientific = F, digits = 3)

FSA::dunnTest(
  data = rPreference, 
  VisitPreference ~ Substance, 
  method="bh")

#greater than random
#TODO (get rid of the loop)

for(temp in levels(rPreference$Substance)) {
  with(rPreference, {
    print(c(
      temp,
      wilcox.test(
        AdaptationPreference[Substance == temp],
        mu = 0.5,
        alternative = 'greater'
      )$p.value %>%
        format(scientific = F, digits = 3)
    ))
  })
}

#size effect
#TODO
tSubstance = 'saccharin'
tReference = 'water'
with(rPreference, {
  effsize::cohen.d(
    AdaptationPreference[Substance == tSubstance],
    AdaptationPreference[Substance == tReference])
})

#TODO
for(temp in levels(rPreference$Substance)) {
  for(temp2 in levels(rPreference$Substance)){
    cat(paste0('\n','\n','###','\t',temp, '\t',temp2,'\n'))
    with(rPreference, {
      print(
        effsize::cohen.d(
          LickPreference[Substance == temp],
          LickPreference[Substance == temp2])
      )
    }
    )
  }
}

# BETTER RATIO ------------------------------------------------------------

# called fancier as -preference of the option with the greater
#   probability of obtaining a reward-
# calculated for data from reversals period

rBetterRatio <- 
  dChoices %>%
  filter(
    PhaseLabel %in% c('0.9x0.3', '0.3x0.9')
    ) %>%
  group_by(
    Tag, Strain, Genotype, Substance) %>%
  summarise(
    n = n(),
    nBetter = 
      length(which(RewardProbability > 0.5)),
    nWorse =
      length(which(RewardProbability > 0.0 & 
                     RewardProbability < 0.5)),
    BR =
      nBetter / (nBetter + nWorse)) %>%
  within({
    Substance = as.factor(Substance)
    Tag = as.factor(Tag)
    Genotype = as.factor(Genotype)
    Substance = as.factor(Substance)
  })


# BETTER RATIO STATISTICS -------------------------------------------------

kruskal.test(
  BR ~ Substance, 
  data = rBetterRatio)$p.value %>%
  format(scientific = TRUE, digits = 3)

FSA::dunnTest(
  BR ~ Substance, 
  data = rBetterRatio,
  method="bh")

##Pairwise Mann–Whitney U-tests
# pairwise.wilcox.test(
#   r.BetterRatio$BR, 
#   r.BetterRatio$Substance,
#   p.adjust.method = "BH")

#Is the effect large enough?

tSubstance = 'saccharin'
tReference = 'water'
with(
  rBetterRatio, {
  effsize::cohen.d(
    BR[Substance == tSubstance],
    BR[Substance == tReference])
})

#Is the value greater than random?

for(temp in levels(rBetterRatio$Substance)) {
  with(rBetterRatio, {
    print(c(
      temp,
      wilcox.test(
        BR[Substance == temp],
        mu = 0.5,
        alternative = 'greater'
      )$p.value %>%
        format(scientific = F, digits = 3)
    ))
  })
}


# CORRELATIONS ------------------------------------------------------------

##Correlations between better ratio and number of attempts
# 
# cor(
#   rBetterRatio$BR[rBetterRatio$Substance == tSubstance],
#   rBetterRatio$n[rBetterRatio$Substance == tsSbstance], 
#   method = "spearman") 
# #sperman is bad method since ranking 
# #   loses the distances between measures... 
# 
# ggplot(rBetterRatio, aes(x = n, y = BR))+
#   geom_point()+
#   geom_smooth(method = "lm")+
#   facet_wrap(~Substance)

# WIN-STAY LOSE-SHIFT -----------------------------------------------------

# omitted, not necessary
