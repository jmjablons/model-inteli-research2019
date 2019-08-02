# general linear model
#   of 'stay' behaviour probability 
#   depending on predictors
#
# version after revision 1st
#   + added third predictor - corner bias

# CUSTOM FUNCTIONS --------------------------------------------------------

calculateErrorRate_ <- 
  function(real.data, predict.data) {
    if (!is.logical(real.data)) {
      real.data = as.vector(as.logical(real.data))
      stopifnot(length(real.data) > 1)
    }
    if (!is.logical(predict.data)) {
      predict.data = as.vector(predict.data > 0.5)
      stopifnot(length(predict.data) > 1)
    }
    mean(predict.data != real.data) 
  }

modelStay <- 
  function(A,
           mouse,
           module = 'reversals',
           divide.set = T,
           n.divisions = 3) {
    set.seed(123)
    mouse = as.numeric(mouse) #soft spot
    greatest.mouse = 
      A[A$Tag == mouse,] %>%
      filter(!is.na(Stay) & Info == module) %>%
      mutate(Corner = as.factor(ceiling(Corner/2)))
    if (divide.set) {
      index.test <- 
        sample(
          seq_along(greatest.mouse$Stay),
          floor(nrow(greatest.mouse) / n.divisions),
          replace = FALSE)
      index.learn <- 
        setdiff(
          seq_along(greatest.mouse$Stay),
          sort(index.test))
      model.greatest.mouse <- 
        glm(
          Stay ~ RewardDoor + IntervalAfter + Corner,
          data = greatest.mouse[index.learn,],
          family = binomial)
      predictions.test <- 
        predict(
          model.greatest.mouse,
          greatest.mouse[index.test,],
          type = "response")
      real.data <- 
        greatest.mouse$Stay[index.test]
      tModel <- 
        model.greatest.mouse %>%
        summary() %>%
        .$coefficients %>%
        transform(., OddsRatio = exp(Estimate)) %>%
        mutate(
          Tag = mouse,
          Predictor = rownames(.),
          ErrorRate = 
            calculateErrorRate_(
              real.data = real.data,
              predict.data = predictions.test),
          # McFaddenpR2 = 
          #   pscl::pR2(object = model.greatest.mouse)['McFadden'],
          Probability = Pr...z..,
          Signif = ifelse(.$Pr...z.. < 0.05, 1, 0)
        ) %>%
        select(-Pr...z.., -Std..Error, -z.value)
      tModel
    } else {
      print(
        '\nYou became protected from making horibble mistake.\n
        Hint: set `divide.set = TRUE`'
      )
    }
  }


# RUN ---------------------------------------------------------------------

#TODO (loop inside)
#TODO (lapply)

rGlm <- data.frame()
for(temp in iAnimals$Tag){
  print(temp)
  rGlm = 
    rbind(
      rGlm,
      modelStay(
        A = (dChoices %>% 
               mutate(RewardDoor = as.factor(RewardDoor))
             ), mouse = temp)
      )
}

# add animal info
rGlm = 
  merge(rGlm, iAnimals, 
        by = 'Tag', all.x = TRUE) %>%
  within({
    Signif = as.factor(Signif)
  })

# # summary
# iGlmRoundup <-
#   rGlm %>%
#   arrange(Predictor) %>%
#   group_by(
#     Substance, Tag, Signif
#     ) %>%
#   summarise(
#     n = n(),
#     predictors = list(Predictor)) %>%
#   mutate(
#     predictors =
#       ifelse(
#         Signif == 0 & n == 4, 
#         list("dummy"), 
#         predictors)
#     )

# # answer for reviewer
# rGlm %>%
#   filter(
#     Predictor == "RewardDoor1" & 
#       Signif ==1) %>%
#   group_by(Tag) %>%
#   summarise() %>%
#   mutate(Tag = as.character(Tag)) %>%
#   saveRDS("animals_vurnerable_reward.RDS")