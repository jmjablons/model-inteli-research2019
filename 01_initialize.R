
# ACTIONS -----------------------------------------------------------------

# import dataset
# declare custom functions
# explanatory analysis
# modelling

# TODO
# list important objects
# describe functions
# list dependencies

# GLOSSARY ----------------------------------------------------------------

# object name include
#   d - data
#   r - results
#   i - metadata 
#   p - plot
#   t - temporary
#
# connected with functions
#   out - function output
#   A - data frame to be processed
#   a - vector to be processed
#   x - one element to be processed

# DEPENDENCIES ------------------------------------------------------------

require(dplyr)
#require(readr)
#require(tidyr)

# IMPORT PARSE DATA -------------------------------------------------------

# if WIN the path may be replaced with file.choose()
# then the widget window with dir tree emerges

dData <- 
  readr::read_delim(
    "~/phd_data/
    publikacja_materialy_surowki/
    analiza/dData.csv", # <- here: file.choose()
    ";", 
    escape_double = FALSE, 
    col_types = cols(
      Corner = col_integer(),
      DoorOpened = col_integer(), 
      EndDateTime = 
        col_datetime(
          format = "%Y-%m-%d %H:%M:%S"), 
      StartDateTime = 
        col_datetime(
          format = "%Y-%m-%d %H:%M:%S"), 
      Tag = col_character(), 
      VisitId = col_character(),
      X1 = col_skip()
    ), 
    locale = locale(decimal_mark = ","), 
    trim_ws = TRUE)

# CUSTOM FUNCTIONS --------------------------------------------------------

is.homogenous <-
  function(a, na.rm = FALSE) {
    if(na.rm){
      a = na.omit(a)
    }
    stopifnot(length(a) > 1)
    out = T
    for (i in seq_along(a)) {
      if (a[i] != a[1]) {
        out = F
        break()
      }
    }
    out
  }

assignPhaseLabel_ <- 
  function(a, sep = 'x', sorted = T){
    label = character()
    rp = as.double(a[5:8])
    if(is.homogenous(rp)){ 
      label = paste0(rp[1])
    } else {
      uni = unique(rp)
      if(sorted){
        label = paste(sort(uni), collapse = sep) 
      } else {
        label = paste(uni, collapse = sep) 
      }
    }
  }

informPhases <-
  function(A) {
    phases <- 
      A %>%
      group_by(Phase, Corner, RewardProbability) %>%
      summarise(
        N = n(),
        start = min(StartDateTime),
        end = max(EndDateTime)
      ) %>%
      group_by(Phase) %>%
      mutate(
        N = sum(N),
        start = min(start),
        end = max(end)
        ) %>%
      tidyr::spread(
        key = Corner, 
        value = RewardProbability
        )
    phases$PhaseLabel <-
      apply(X = phases, 1, function(x) {
        assignPhaseLabel_(x)
      })
    phases$Duration <-
      difftime(
        phases$end, 
        phases$start, 
        units = 'hours') %>%
      round(digits = 0)
    phases
  }

getSpecifications <- 
  function(A, include.phase = T){
    listCage = levels(as.factor(A$DeviceId))
    howMany = length(listCage)
    out <- list(rep(NA, howMany))
    for(i in seq_along(1:howMany)){
      A0 = A[A$DeviceId == listCage[i],]
      temp = list(
        cage = listCage[i],
        listAnimals = levels(as.factor(A0$Tag)),
        nAnimals = levels(as.factor(A0$Tag)) %>% length(),
        listStrain = levels(as.factor(A0$Strain)),
        listGenotype = levels(as.factor(A0$Genotype)),
        listSubstance = levels(as.factor(A0$Substance)),
        rangeTime = (function(A){
            A = arrange(A, StartDateTime)
            c(A$StartDateTime[1], A$EndDateTime[nrow(A)])
          })(A0)
      )
      if (include.phase) {
        temp = append(
          temp, 
          list(
            expScheme = table(A0$Info, A0$Phase),
            minPhase = min(A0$Phase),
            maxPhase = max(A0$Phase)
          )
        )
      }
      out[[i]] = temp
    }
    out
  }

# INFORM EXP SCHEME -------------------------------------------------------

# check for experimental scheme 
#
# data-driven so... truth will be revealed
# returns a tibble [~df]

informPhases(
  dData %>% filter(Exp == "A" & Substance == "saccharin")
  )

# get to know all the neccessary details 
#   about an experiment
#
# returns a list [list]
#   split by device cage

#iExp <- 
  getSpecifications(dData %>% filter(Exp == "A"))

# then check whatever, as pleased
#
#iExp[[1]]$listSubstance
