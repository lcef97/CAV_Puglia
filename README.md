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

  - R script [`CAV_output`](https://github.com/lcef97/CAV_Puglia/blob/main/CAV_output.R): complete analysis performed over four years, 
  which is the full information currently available. This is the complete source code of the paper.

  - [Slides](https://lcef97.github.io/CAV_Puglia/#1). Old presentation, a previous and more complex version of the
  analysis carried out.
  Few, selected models are included here, 
  as they are multivariate ones with 10 -- 14 hyperparameters. This was the presentation at the PRIN workshop
  held in Lecce, Salento, Apulia, Italy, on September 19--20th, 2025. However we moved away from the 
  general multivariate model towards a spatiotemporal one.
  
  
  - R script [`Functions`](https://github.com/lcef97/CAV_Puglia/blob/main/Auxiliary/Functions.R): 
  R code, compatible with R-INLA, with all the handmade INLA models, including spatio-temporal, 
  block-factorisable multivariate models, and M-models. 

  
  - [inputs](https://github.com/lcef97/CAV_Puglia/tree/main/input): all data this analysis is based on. 
  As said before, some datasets are derived by aggregation of sensible data, 
  which we should not make publicly available
  
## Manuscript

 The paper has been submitted to a journal and is now under revision. While all we can do is hope 
 at this stage, here is the [arXiv draft](https://arxiv.org/abs/2511.20481).
 