# model-inteli-research2019

[![DOI](https://zenodo.org/badge/200035373.svg)](https://zenodo.org/badge/latestdoi/200035373)

R script used for research paper 

## dataset

https://doi.org/10.6084/m9.figshare.9250175.v1

## research article pre-print

https://www.biorxiv.org/content/10.1101/643965v1

# usage

prepare & preprocess data

```{r}
source("dependency.R")     #load needed packages
source("util.R")           #load custom util functions
source("import_data.R")    #read from raw files
source("create_dataset.R") #preprocess the datasets
```

further analyses 

* brief `explanatory.R`
* figures `dir: figure_source`
* statistics `dir: stat_source`
* models `dir: model_source: batch_pub`