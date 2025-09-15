## Territorial analysis of accesses to support centers for gender-based violence in Apulia

Ongoing analysis. This is a working paper studying the accesses to support centers for gender-based violence in the Apulia region.
Due to the extremely sensitive nature of data, only municipality-level aggregated data are provided.
Moreover, this merely statistical analysis addresses an issue too wide and too severe to be
solved by means of aseptic data analysis, no matter how deep and complex models are defined. 
Thus my hope is that this humble contribution helps at least to spread a piece of awareness on the
omnipresent tragedy of the systemic oppression of women by the current society. 
Maybe we do not speak enough of it, or if we do the question is limited to a handful of 
specific environments. As a budding statistician the best I can do is to aseptically work with numbers, 
so here are some.

## Contents of the present GitHub repository 

  - [Slides](https://lcef97.github.io/CAV_Puglia/#1). Move on and try completing this by Sunday. 
  This clearly serves no more than as a presentation. Just a few selected models are included here, 
  as they are multivariate ones with 10 -- 14 hyperparameters, so they take quite a long time
  to compile.
  
  - R script [`CAV_full`](https://github.com/lcef97/CAV_Puglia/blob/main/CAV_full.R): complete analysis performed over four years, 
  which is the full information currently available [(link)].
  
  - R script [`Functions`](https://github.com/lcef97/CAV_Puglia/blob/main/Auxiliary/Functions.R): 
  R code, compatible with R-INLA, with all the handmade INLA models [(here)]. As this is quite experimental, 
  if for any reason you use this code and you trust it enough (!) 
  it would be nice if you cited the source :)
  However, it is getting a bit too long and clumsy so maybe I'll move the functions to an R package.
  
  - [Markdown report](https://github.com/lcef97/CAV_Puglia/blob/main/CAV_panel.pdf) 
  of 2021-24 data analysis with constant coefficients and spatial effects
  modelled as multivariate autoregressive models.  I will try providing also an HTML version
  but don't know to which extent it can be trusted.  
  More complex models with time - varying regression coefficients are tested in the `R` script 
  but due to higher complexity yielding no fitting improvement they have been superseded
  and are not referred to in the report.
  
  - [inputs](https://github.com/lcef97/CAV_Puglia/tree/main/input): all data this analysis is based on. 
  As said before, some datasets are derived by aggregation of sensible data, 
  which we should not make publicly available
 