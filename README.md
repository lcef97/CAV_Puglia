## Territorial analysis of accesses to support centers for gender-based violence in Apulia

Ongoing analysis. This is a working paper studying the accesses to support centers for gender-based violence in the Apulia region.

#### Main contents 
  - Markdown reports, namely:
    - [2022 data analysis](https://lcef97.github.io/CAV_Puglia/), currently the one compiled both in `.html` and `.pdf`
    - [2021-23 data analysis with time-varying coefficients](https://github.com/lcef97/CAV_Puglia/blob/main/CAV_3years.pdf), on which I have been working until April 2025. 
      I am not currently working on it and can be considered superseded.
    - [2021-23 data analysis with constant coefficients](https://github.com/lcef97/CAV_Puglia/blob/main/CAV_panel.pdf) and spatial effects modelled in the M-models framework. 
      Ongoing, I still have to implement the BYM and some Poisson - Logistic regression. 
  - R script `CAV_2022`: used for dataset construction and data exploration, 
  limited to 2022 data. Most of the code is commented [(link)](https://github.com/lcef97/CAV_Puglia/blob/main/CAV_2022.R)
  - R script `CAV_full`: analysis performed over three years, which is the full information currently available [(link)](https://github.com/lcef97/CAV_Puglia/blob/main/CAV_full.R).
    It is quite long and needs to be reordered.
  - inputs and metadata: all data this analysis is based on. Some datasets are derived by aggregation of sensible data, which we should not make publicly available
