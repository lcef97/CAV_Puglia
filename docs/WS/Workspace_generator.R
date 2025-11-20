#' ---------  Analisi CAV Puglia 2021-24 --------------------------------------#
#' 
#' This is only a resume 
#' of \url{https://github.com/lcef97/CAV_Puglia/blob/main/CAV_panel.Rmd}
#' employed to generate workspaces, which speeds up the HTML compiler.
#' It is memory-intensive but time-saving -------------------------------------#
#' The good news is that INLA::inla.hyperpar.sample needs not the configurations
##' ------------------  INPUT  -------------------------------------------------
#'
#' NEVER forget calling magrittr and sf. Never --------------------------------#
#' 
library(magrittr)
library(sf)
#'
#' Set WD to parent directory. One step back ----------------------------------#
wd <- getwd()

if(substr(wd, nchar(wd)-1, nchar(wd)) == "WS"){
  setwd("../")
} else if(substr(wd, nchar(wd)-9, nchar(wd)) == "CAV_Puglia"){
  setwd(paste0(wd, "/docs"))
}

library(knitr)
#' This is a great function to extract R code from a remote script ------------#
knitr::purl("index.Rmd", output = "extracted_chunks.R", documentation = 0)
code_lines <- readLines("extracted_chunks.R")

start_source <- grep("StartRemoteSource", code_lines)
end_source <- grep("EndRemoteSource", code_lines)
remote_source <- code_lines[start_source:end_source]

#' Why not copy-pasting code from index.Rmd? 
#' Because maybe I have changed it in the meanwhile. Don't 
#' trust me, just trust computers. --------------------------------------------#
eval(parse(text = remote_source), envir = globalenv())

#' Before spatial models, let us write the nonspatial one,
#' which however is so easy it could even be run
#' on the main script.
cav_nosp_inla <- inla(
  N_ACC ~0 + Y_2021 + Y_2022 +Y_2023 +Y_2024 +
    TEP_th + ELI + PGR + UIS + ELL + PDI + ER  ,
  offset = log(nn),
  family = "poisson", data =dd,
  num.threads = 1, control.compute = list(
    internal.opt = F, cpo = F, dic = T, waic = T, config = F), 
  verbose = T)

save(cav_nosp_inla, file = "WS/cav_nosp_inla.RData")


#' Now, the burdensome part :)
#' 
##' ------------------  ICAR model ---------------------------------------------
#'

cav_IMCAR_inla <- inla(
  N_ACC ~ 0 + Y_2021 + Y_2022 + Y_2023 + Y_2024 +
    TEP_th + ELI + PGR + UIS + ELL + PDI + ER+ 
    f(ID, model = inla.IMCAR.Bartlett(k = 4, W = W_con, df = 8), 
      extraconstr = list(A = A_constr, e = c(0,0,0,0))),
  offset = log(nn), family = "poisson", data =dd,
  num.threads = 1, control.compute = list(internal.opt = F, cpo = F,
                                          dic = T, waic = T, config = F), 
  verbose = T)

save(cav_IMCAR_inla, file = "WS/cav_IMCAR_inla.RData")

##' ------------------  PCAR model ----------------------------------------------


cav_PMMCAR_inla_unif <- inla(
  N_ACC ~  0+ Y_2021 + Y_2022 + Y_2023 + Y_2024 +
    TEP_th + ELI + PGR + UIS + ELL + PDI + ER+ 
    f(ID, model = inla.PMMCAR.model(k = 4, W = W_con, df = 8, PC = F)),
  offset = log(nn), control.inla = list(stupid.search = F),
  family = "poisson", data =dd, safe = F,
  num.threads = 1, control.compute = list(
    internal.opt = F, cpo = F, dic = T, waic = T, config = F), 
  verbose = T)


# this is better 
cav_PMMCAR_inla_pc_default  <- inla(
  N_ACC ~  0+ Y_2021 + Y_2022 + Y_2023 + Y_2024 +
    TEP_th + ELI + PGR + UIS + ELL + PDI + ER+ 
    f(ID, model = inla.PMMCAR.model(k = 4, W = W_con, df = 8, PC = T )),
  offset = log(nn),
  family = "poisson", data =dd,
  num.threads = 1, control.compute = list(
    internal.opt = F, cpo = T, waic = T, dic =T, config = F), 
  verbose = T)


cav_PMMCAR_inla_pc_strict  <- inla(
  N_ACC ~  0+ Y_2021 + Y_2022 + Y_2023 + Y_2024 +
    TEP_th + ELI + PGR + UIS + ELL + PDI + ER+ 
    f(ID, model = inla.PMMCAR.model(k = 4, W = W_con,
                                    df = 8, PC = T, alpha.rho=0.9, U.rho = 0.6)),
  offset = log(nn),
  family = "poisson", data =dd,
  num.threads = 1, control.compute = list(
    internal.opt = F, cpo = T, waic = T, dic = T, config = F), 
  verbose = T)

if(cav_PMMCAR_inla_pc_strict$waic$waic < cav_PMMCAR_inla_pc_default$waic$waic){
  cav_PMMCAR_inla_pc <- cav_PMMCAR_inla_pc_strict
} else{
  cav_PMMCAR_inla_pc <- cav_PMMCAR_inla_pc_default
}

save(cav_PMMCAR_inla_unif, file = "WS/cav_PMMCAR_inla_unif.RData")
save(cav_PMMCAR_inla_pc, file = "WS/cav_PMMCAR_inla_pc.RData")


##' ------------------  LCAR model ----------------------------------------------


cav_LMMCAR_inla_unif <- inla(
  N_ACC ~  0+ Y_2021 + Y_2022 + Y_2023 + Y_2024 +
    TEP_th + ELI + PGR + UIS + ELL + PDI + ER+ 
    f(ID, model = inla.LMMCAR.model(k = 4, W = W_con, df = 8, PC = F)),
  offset = log(nn), control.inla = list(stupid.search = F),
  family = "poisson", data =dd, safe = F,
  num.threads = 1, control.compute = list(
    internal.opt = F, cpo = F, dic = T, waic = T, config = F), 
  verbose = T)


cav_LMMCAR_inla_pc_default <- inla(
  N_ACC ~  0+ Y_2021 + Y_2022 + Y_2023 + Y_2024 +
    TEP_th + ELI + PGR + UIS + ELL + PDI + ER+ 
    f(ID, model = inla.LMMCAR.model(k = 4, W = W_con, df = 8, PC = T   )),
  offset = log(nn),
  family = "poisson", data =dd,
  num.threads = 1, control.compute = list(
    internal.opt = F, cpo = F, dic = T, waic = T, config = F), 
  verbose = T)

#' This is better
cav_LMMCAR_inla_pc_strict <- inla(
  N_ACC ~  0+ Y_2021 + Y_2022 + Y_2023 + Y_2024 +
    TEP_th + ELI + PGR + UIS + ELL + PDI + ER+ 
    f(ID, model = inla.LMMCAR.model(k = 4, W = W_con, df = 8, PC = T,
                                    alpha.lambda=9/10, U.lambda = 3/5)),
  offset = log(nn),
  family = "poisson", data =dd,
  num.threads = 1, control.compute = list(
    internal.opt = F, cpo = T, waic = T, config = F), 
  verbose = T)


if(cav_LMMCAR_inla_pc_strict$waic$waic < cav_LMMCAR_inla_pc_default$waic$waic){
  cav_LMMCAR_inla_pc <- cav_LMMCAR_inla_pc_strict
} else{
  cav_LMMCAR_inla_pc <- cav_LMMCAR_inla_pc_default
}


save(cav_LMMCAR_inla_unif, file = "WS/cav_LMMCAR_inla_unif.RData")
save(cav_LMMCAR_inla_pc, file = "WS/cav_LMMCAR_inla_pc.RData")







##' ------------------  BYM  model ---------------------------------------------



cav_MMBYM_inla_pc_default <- inla(
  N_ACC ~ 0 + Y_2021 + Y_2022 + Y_2023 + Y_2024 +
    TEP_th + ELI + PGR + UIS + ELL + PDI + ER+ 
    f(ID, model = inla.MMBYM.model(k = 4, W = W_con,  PC= T, df = 8,scale.model = T) , 
      extraconstr = constr.BYM),
  offset = log(nn), 
  family = "poisson", data =dd,
  num.threads = 1, control.compute = list(internal.opt = F, cpo = T, waic = T, config = F), 
  verbose = T)


cav_MMBYM_inla_pc_strict <- inla(
  N_ACC ~ 0 + Y_2021 + Y_2022 + Y_2023 + Y_2024 +
    TEP_th + ELI + PGR + UIS + ELL + PDI + ER+ 
    f(ID, model = inla.MMBYM.model(k = 4, W = W_con,  PC= T, df = 8,
                                   scale.model=T, alpha.phi =0.9, U.phi=0.6) , 
      extraconstr = constr.BYM),
  offset = log(nn), 
  family = "poisson", data =dd,
  num.threads = 1, control.compute = list(internal.opt = F, cpo = T, waic = T, config = F), 
  verbose = T)



cav_MMBYM_inla_unif <- inla(
  N_ACC ~ 0 + Y_2021 + Y_2022 + Y_2023 + Y_2024 +
    TEP_th + ELI + PGR + UIS + ELL + PDI + ER+ 
    f(ID, model = inla.MMBYM.model(k = 4, W = W_con,  PC= F, df = 8,scale.model = T) , 
      extraconstr = constr.BYM),
  offset = log(nn), 
  family = "poisson", data =dd,
  num.threads = 1, control.compute = list(internal.opt = F, cpo = T, waic = T, config = F), 
  verbose = T)






if(cav_MMBYM_inla_pc_strict$waic$waic < cav_MMBYM_inla_pc_default$waic$waic){
  cav_MMBYM_inla_pc <- cav_MMBYM_inla_pc_strict
} else{
  cav_MMBYM_inla_pc <- cav_MMBYM_inla_pc_default
}
save(cav_MMBYM_inla_unif, file = "WS/cav_MMBYM_inla_unif.RData")
save(cav_MMBYM_inla_pc, file = "WS/cav_MMBYM_inla_pc.RData")







