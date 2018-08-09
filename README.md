# Enhancing climate change research with open science

This repository contains code for the data queries, R scripts and Figures accompanying Tai & Robinson commentary article on adopting open science for climate change research.

The following R packages were used to analyse data and create figures.

```
install.packages(c("tidyverse", "lme4", "here", "plotrix", "scales"))
```

* `Scopus_models.R` contains model code for analysing Scopus citation data
* `Altmetric_models.R` contains model code for analysing Altmetric mentions data
* `Figure1.R` and `Figure2.R` contain code for recreating figures

The raw citation metrics that we analyzed are privately owned, but may be available through direct requests to Scopus and Altmetric. We provide analyzed data and search queries:

* `query.md` contains the search terms used in data requests
* `scopus_lmer_fit.Rdata` contains the Figure 2a analysis, including mean citations per journal, model structure, and model predictions
* `altmetric_lmer_fit.Rdata` contains the Figure 2b-d analysis, including mean citations per journal, model structure, and model predictions
* The 225 journals included in the data analysed are listed at `climate_journals.csv`

