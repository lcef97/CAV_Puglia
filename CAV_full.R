#' ---------  Analisi CAV Puglia 2021-23 --------------------------------------#
## Input -----------------------------------------------------------------------

#' NEVER forget calling magrittr and sf. Never --------------------------------#
#' 
library(magrittr)
library(sf)
#'  ---------------------------------------------------------------------------#
#' 
#'  Do not delete!! Code used to build input data from excel files
#'  Excel inputs include sensible information which we prefer not to share;
#'  we only leave track of how we aggregate data from individual level
#'  to municipality level -----------------------------------------------------#
#'  ---------------------------------------------------------------------------#
#'
#'  CAV_report_2022 <- readxl::read_excel("input/cav_2022.xlsx", 
#'                                       sheet = "Sintesi")
#'
#'  CAV_mun_22 <- CAV_report_2022 %>%
#'    dplyr::filter(.data$ESITO_ACCESSO == "presa in carico") %>% 
#'    dplyr::rename(PRO_COM = .data$PRO_COM_Residenza_Domicilio) %>% 
#'    dplyr::group_by(.data$PRO_COM) %>% 
#'    dplyr::summarise(N_ACC = dplyr::n(),
#'                     comune = stringr::str_to_title(
#'                       .data$Comune_Residenza_Domicilio[1L]))%>% 
#'    dplyr::ungroup()
#'    
#'    
#'  Same thing for 2021 and 2023 data:  ---------------------------------------#
#'  
#'  
#'  CAV_mun_21 <- cav_2021_dati %>%
#'      dplyr::filter(.data$ESITO_ACCESSO == "presa in carico") %>% 
#'      dplyr::filter(!is.na(.data$PRO_COM_Residenza_Domicilio)) %>% 
#'      dplyr::rename(PRO_COM = .data$PRO_COM_Residenza_Domicilio) %>% 
#'      dplyr::group_by(.data$PRO_COM) %>% 
#'      dplyr::summarise(N_ACC = dplyr::n(),
#'      comune = stringr::str_to_title(
#'      .data$Comune_Residenza_Domicilio[1L]))%>%   dplyr::ungroup()
#'     
#'  
#'  CAV_mun_23 <- cav_2023_dati %>%
#'      dplyr::filter(.data$ESITO_ACCESSO == "presa in carico") %>% 
#'      dplyr::filter(!is.na(.data$PRO_COM_Residenza_Domicilio)) %>% 
#'      dplyr::rename(PRO_COM = .data$PRO_COM_Residenza_Domicilio) %>% 
#'      dplyr::group_by(.data$PRO_COM) %>% 
#'      dplyr::summarise(N_ACC = dplyr::n(),
#'      comune = stringr::str_to_title(
#'      .data$Comune_Residenza_Domicilio[1L]))%>%   dplyr::ungroup()
#'      
#'     
#'  ---------------------------------------------------------------------------#   
#'  
#'  Then we download only the municipality-level data
#'  on accesses to support centers,
#'  aggregated from individual-level ones:

load("input/CAV_input_mun_2021.RData")
load("input/CAV_input_mun_2022.RData")
load("input/CAV_input_mun_2023.RData")

CAV_mun_21 <- CAV_mun_21 %>% dplyr::rename(N_ACC_21 = .data$N_ACC)
CAV_mun_22 <- CAV_mun_22 %>% dplyr::rename(N_ACC_22 = .data$N_ACC)
CAV_mun_23 <- CAV_mun_23 %>% dplyr::rename(N_ACC_23 = .data$N_ACC)

#'  2022 municipalities shapefiles; easily obtainable by scraping with the following 
#'  commented code:
#'  Mun22_shp <- SchoolDataIT::Get_Shapefile(2022)
#'  Shp <- Mun22_shp %>% dplyr::filter(.data$COD_REG == 16) %>% 
#'   dplyr::select(.data$COD_PROV, .data$PRO_COM, .data$COMUNE)
#'  
#'  In theory, a shapefile should be used for each year.
#'  Still, administrative units boundaries are unchanged at least
#'  from 2021 to 2023
#'
#'  Still, we leave the static shapefile in order NOT to need internet connection:
load("input/Shp.RData")

#'  Function to extract numeric digits from a strings vector (needed to filter age):
nn_extract <- function(string){
  nn <- gregexpr("([0-9])", string)
  ls.out <- regmatches(as.list(string), nn)
  res <- unlist(lapply(ls.out, function(x) as.numeric(paste(x, collapse = ""))))  
  return(res)
}

# Female population aged >= 15 years.
# Source data: http://dati.istat.it/Index.aspx?DataSetCode=DCIS_POPRES1#
Popolazione_Puglia_2021 <- readr::read_csv("input/Popolazione_Puglia_2021.csv") %>% 
  dplyr::select(.data$ITTER107, .data$Territorio, .data$SEXISTAT1, .data$ETA1, .data$Value) %>% 
  dplyr::filter(.data$ETA1 != "TOTAL") %>% 
  dplyr::mutate(ETA1 = nn_extract(ETA1)) %>% 
  dplyr::mutate(ITTER107 = as.numeric(ITTER107))

Popolazione_Puglia_2022 <- readr::read_csv("input/Popolazione_Puglia_2022.csv") %>% 
  dplyr::select(.data$ITTER107, .data$Territorio, .data$SEXISTAT1, .data$ETA1, .data$Value) %>% 
  dplyr::filter(.data$ETA1 != "TOTAL") %>% 
  dplyr::mutate(ETA1 = nn_extract(ETA1)) %>% 
  dplyr::mutate(ITTER107 = as.numeric(ITTER107))

Popolazione_Puglia_2023 <- readr::read_csv("input/Popolazione_Puglia_2023.csv") %>% 
  dplyr::select(.data$ITTER107, .data$Territorio, .data$SEXISTAT1, .data$ETA1, .data$Value) %>% 
  dplyr::filter(.data$ETA1 != "TOTAL") %>% 
  dplyr::mutate(ETA1 = nn_extract(ETA1)) %>% 
  dplyr::mutate(ITTER107 = as.numeric(ITTER107))




# Filter and aggregate:
Pop_f_15 <- Popolazione_Puglia_2021 %>% 
  dplyr::left_join(Popolazione_Puglia_2022,
                   by = c("ITTER107", "Territorio", "SEXISTAT1", "ETA1")) %>% 
  dplyr::left_join(Popolazione_Puglia_2023,
                   by = c("ITTER107", "Territorio", "SEXISTAT1", "ETA1")) 

names(Pop_f_15) <-  c("PRO_COM", "Comune", "Sesso", "Eta", 
                      "Popolazione_21", "Popolazione_22", "Popolazione_23")


Pop_f_15 <- Pop_f_15 %>% dplyr::filter(.data$Sesso == 2) %>% 
  dplyr::filter(.data$Eta > 14) %>% 
  dplyr::group_by(.data$PRO_COM, .data$Comune) %>% 
  dplyr::summarise(nn21 = sum(.data$Popolazione_21),
                   nn22 = sum(.data$Popolazione_22),
                   nn23 = sum(.data$Popolazione_23)) %>%
  dplyr::ungroup()




# Complete dataset:
dd <- Shp %>% dplyr::left_join(Pop_f_15[, -2],
                               by = "PRO_COM") %>% 
  dplyr::left_join(dplyr::select(CAV_mun_21, -.data$comune),by = "PRO_COM") %>% 
  dplyr::left_join(dplyr::select(CAV_mun_22, -.data$comune),by = "PRO_COM") %>% 
  dplyr::left_join(dplyr::select(CAV_mun_23, -.data$comune),by = "PRO_COM") %>% 
  dplyr::left_join(dplyr::select(Indicators, -.data$Comune), by = "PRO_COM")

# Municipalities from which no woman reported violence --> count == 0
dd$N_ACC_21[is.na(dd$N_ACC_21)] <- 0
dd$N_ACC_22[is.na(dd$N_ACC_22)] <- 0 
dd$N_ACC_23[is.na(dd$N_ACC_23)] <- 0 

# "access ratio"
dd$LN_ACC_21 <- log(dd$N_ACC_21/dd$nn21)
dd$LN_ACC_22 <- log(dd$N_ACC_22/dd$nn22)
dd$LN_ACC_23 <- log(dd$N_ACC_23/dd$nn23)


ggy21 <- ggplot2::ggplot() +
  ggplot2::geom_sf(data = dd, 
                   ggplot2::aes(fill = .data$LN_ACC_21))+
  ggplot2::scale_fill_viridis_c(na.value = "white",
                                limits = c(-9.5, -4.5))+
  ggplot2::theme_classic()

ggy22 <- ggplot2::ggplot() +
  ggplot2::geom_sf(data = dd, 
                   ggplot2::aes(fill = .data$LN_ACC_22))+
  ggplot2::scale_fill_viridis_c(na.value = "white",
                                limits = c(-9.5, -4.5))+
  ggplot2::theme_classic()

ggy23 <- ggplot2::ggplot() +
  ggplot2::geom_sf(data = dd, 
                   ggplot2::aes(fill = .data$LN_ACC_23))+
  ggplot2::scale_fill_viridis_c(na.value = "white",
                                limits = c(-9.5, -4.5))+
  ggplot2::theme_classic()


gridExtra::grid.arrange(ggy21, ggy22, ggy23, nrow = 3, ncol = 1)




#'  ## Mapping municipalities from support centers ----------------------------#
#'  Loop to compute miniman distances
#'
#'
#' Municipalities hosting a support center:
#' 
#'  munWcav_22 <- c(71020, 71024, 71051, 
#'                  72004, 72006, 72011, 72014, 72019, 72021, 72029, 72031, 72033, 72035, 
#'                  73013, 73027, 
#'                  74001, 74009, 
#'                  75018, 75029, 75035, 75059,
#'                  110001, 110002, 110009)
#'  
#'  munWcav_23 <- c(71020, 71024, 71029, 71051, 
#'                  72004, 72006, 72011, 72014, 72019, 72021, 72029, 72031, 72033, 72035,
#'                  73013, 73027, 
#'                  74001, 74009, 
#'                  75029, 75035, 75059,
#'                  110001, 110002, 110009)
#'  
#'
#'  dists_th_22 <- NULL
#'  for (i in c(1:nrow(dd[-singletons, ]))){
#'    X <- dd[-singletons, ]$PRO_COM[i]
#'    dists <- numeric(length(munWcav_22))
#'      IDs <- numeric(length(munWcav_22))
#'    for(j in c(1:length(munWcav_22))){
#'      c.out <- munWcav_22[j]
#'      id <- paste0(min(X, c.out),
#'                       " - ", max(X, c.out))
#'     nn <- which(dist_short$OR_DEST == id)
#'      dists[j] <- dist_short$TEP_TOT[nn]
#'      IDs[j] <- dist_short$OR_DEST[nn]
#'  }
#'    m <- which.min(dists)
#'    ret <- c(X, dists[m])
#'    dists_th_22 <- data.frame(rbind(dists_th_22, ret))
#'  }
#'  names(dists_th_22) <- c("PRO_COM", "TEP_th_22")
#'  dists_th_22$TEP_th <- as.numeric(dists_th_22$TEP_th)
#'  
#'  
#'  
#'  dists_th_23 <- NULL
#'  for (i in c(1:nrow(dd[-singletons, ]))){
#'    X <- dd[-singletons, ]$PRO_COM[i]
#'    dists <- numeric(length(munWcav_23))
#'    IDs <- numeric(length(munWcav_23))
#'    for(j in c(1:length(munWcav_23))){
#'      c.out <- munWcav_23[j]
#'      id <- paste0(min(X, c.out),
#'                   " - ", max(X, c.out))
#'      nn <- which(dist_short$OR_DEST == id)
#'      dists[j] <- dist_short$TEP_TOT[nn]
#'      IDs[j] <- dist_short$OR_DEST[nn]
#'    }
#'    m <- which.min(dists)
#'    ret <- c(X, dists[m])
#'    dists_th_23 <- data.frame(rbind(dists_th_23, ret))
#'  }
#'  names(dists_th_23) <- c("PRO_COM", "TEP_th_23")
#'  dists_th_23$TEP_th <- as.numeric(dists_th_23$TEP_th)

load("input/dists_th_22.RData")
load("input/dists_th_23.RData")

# Tremiti Islands are a singleton --> need to remove them to perform spatial analysis
suppressWarnings({
  singletons <- which(unlist(lapply(spdep::poly2nb(dd), function(x) x[1L] == 0)))
})

# This is the dataset we will concretely work on.
# Covariates are all scaled to zero mean and unit variance
dd_con <- dd[-singletons, ] %>% 
  dplyr::left_join(dists_th_22, by = "PRO_COM") %>% 
  dplyr::left_join(dists_th_23, by = "PRO_COM") %>% 
  dplyr::mutate(TEP_th_22 = as.vector(scale(.data$TEP_th_22))) %>% 
  dplyr::mutate(TEP_th_23 = as.vector(scale(.data$TEP_th_23))) %>% 
  dplyr::mutate(AES = as.vector(scale(.data$AES))) %>% 
  dplyr::mutate(MFI = as.vector(scale(.data$MFI)))  %>% 
  dplyr::mutate(PDI = as.vector(scale(.data$PDI)))  %>% 
  dplyr::mutate(ELL = as.vector(scale(.data$ELL)))  %>% 
  dplyr::mutate(ER = as.vector(scale(.data$ER)))  %>% 
  dplyr::mutate(PGR = as.vector(scale(.data$PGR)))  %>% 
  dplyr::mutate(UIS = as.vector(scale(.data$UIS)))  %>% 
  dplyr::mutate(ELI = as.vector(scale(.data$ELI))) 

# sd of travel time: almost 16 minutes
attr(scale(dists_th_22$TEP_th_22), "scaled:scale")

# neighbours list
nb_con <- spdep::poly2nb(dd_con)
# neighbouring/adjacency matrix
W_con <- spdep::nb2mat(nb_con, style = "B")
rownames(W_con) <- colnames(W_con) <- dd_con$PRO_COM

# Laplacian matrix:
Lapl_con <- diag(rowSums(W_con)) - W_con
V_con <- eigen(Lapl_con)$vectors
L_block <- kronecker(diag(1, 3), Lapl_con)
A_constr <- t(pracma::nullspace(L_block))

# row ID - needed for spatial models
dd_con$ID <- c(1:nrow(dd_con))

# Full GLM --> for model matrix
glm_all_X <- glm(N_ACC_21 ~ 1 + TEP_th_22 + TEP_th_23 + MFI + AES + PDI + ELL + ER +
                   PGR + UIS + ELI + offset(log(nn21)),
                 data = dd_con, family = "poisson")
# model matrix
X <- model.matrix(glm_all_X)


n <- nrow(dd_con)
dd_list <- list (
  Intercept = matrix(c(rep(1, n), rep(NA, 3*n), 
                       rep(1,n), rep(NA, 3*n), 
                       rep(1,n)), nrow = 3*n, 
                     ncol = 3, byrow = FALSE),
  N_ACC = matrix(c(dd_con$N_ACC_21, rep(NA, 3*n), 
                   dd_con$N_ACC_22, rep(NA, 3*n), 
                   dd_con$N_ACC_23), nrow = 3*n, 
                 ncol = 3, byrow = FALSE),
  TEP_th = matrix(c(dd_con$TEP_th_22, rep(NA, 3*n), 
                    dd_con$TEP_th_22, rep(NA, 3*n), 
                    dd_con$TEP_th_23), nrow = 3*n,
                  ncol = 3, byrow = FALSE),
  ELI = matrix(c(dd_con$ELI, rep(NA, 3*n), 
                 dd_con$ELI, rep(NA, 3*n), 
                 dd_con$ELI), nrow = 3*n,
               ncol = 3, byrow = FALSE),
  PGR = matrix(c(dd_con$PGR, rep(NA, 3*n), 
                 dd_con$PGR, rep(NA, 3*n), 
                 dd_con$PGR), nrow = 3*n,
               ncol = 3, byrow = FALSE),
  UIS = matrix(c(dd_con$UIS, rep(NA, 3*n), 
                 dd_con$UIS, rep(NA, 3*n), 
                 dd_con$UIS), nrow = 3*n,
               ncol = 3, byrow = FALSE),
  ELL = matrix(c(dd_con$ELL, rep(NA, 3*n), 
                 dd_con$ELL, rep(NA, 3*n), 
                 dd_con$ELL), nrow = 3*n,
               ncol = 3, byrow = FALSE),
  PDI = matrix(c(dd_con$PDI, rep(NA, 3*n), 
                 dd_con$PDI, rep(NA, 3*n), 
                 dd_con$PDI), nrow = 3*n,
               ncol = 3, byrow = FALSE),
  ER = matrix(c(dd_con$ER, rep(NA, 3*n), 
                dd_con$ER, rep(NA, 3*n), 
                dd_con$ER), nrow = 3*n,
              ncol = 3, byrow = FALSE),
  ID = c(1:n, (n + c(1:n)), (2*n + c(1:n))),
  ID2 = c(1:n, (n + c(1:n)), (2*n + c(1:n)),
          (3*n)+c(1:n, (n + c(1:n)), (2*n + c(1:n)))),
  nn = c(dd_con$nn21, dd_con$nn22, dd_con$nn23)) 

#'  May be useful?
#'  
#'  dataframe input obejct ==> same regression coefficients
#'  for all years (not so trustable indeed)
#'  ID_m is the municipality ID; ID_ym is the unique ID

dd_long <- do.call(rbind, rep(list(
  dplyr::select(sf::st_drop_geometry(dd_con), 
                colnames(X)[-c(1:3)])),3) )  %>% 
  dplyr::mutate(N_ACC = c(dd_con$N_ACC_21, dd_con$N_ACC_22, dd_con$N_ACC_23),
                TEP_th = c(dd_con$TEP_th_22, dd_con$TEP_th_22, dd_con$TEP_th_23),
                nn = c(dd_con$nn21, dd_con$nn22, dd_con$nn23),
                Intercept = rep(1, nrow(.)),
                ID_ym = rep(dd_con$ID, 3) + rep(c(0, n, 2*n), each = n),
                ID_m = rep(dd_con$ID, 3),
                Year = rep(c(1,2,3), each = nrow(dd_con)))

#' Here some internal function that may or may not be useful
zhat_plot <- function(model, main = NULL){
  
  zbound <- range(model$summary.random$ID$mean)
  
  if(is.null(main)) main <- deparse(substitute(model))
  
  ggzhat21 <- dd_con %>% 
    dplyr::mutate(zhat_21 = model$summary.random$ID$mean[c(1:n)]) %>% 
    ggplot2::ggplot() +
    ggplot2::geom_sf(ggplot2::aes(fill = .data$zhat_21))+
    ggplot2::scale_fill_viridis_c(na.value = "white", direction = -1, limits = zbound) +
    ggplot2::theme_classic()
  
  ggzhat22 <- dd_con %>% 
    dplyr::mutate(zhat_22 = model$summary.random$ID$mean[c(n+(1:n))]) %>% 
    ggplot2::ggplot() +
    ggplot2::geom_sf(ggplot2::aes(fill = .data$zhat_22))+
    ggplot2::scale_fill_viridis_c(na.value = "white", direction = -1, limits = zbound) +
    ggplot2::theme_classic()
  
  ggzhat23 <- dd_con %>% 
    dplyr::mutate(zhat_23 = model$summary.random$ID$mean[c((2*n)+(1:n))]) %>% 
    ggplot2::ggplot() +
    ggplot2::geom_sf(ggplot2::aes(fill = .data$zhat_23))+
    ggplot2::scale_fill_viridis_c(na.value = "white", direction = -1, limits = zbound) +
    ggplot2::theme_classic()
  
  gridExtra::grid.arrange(ggzhat21, ggzhat22, ggzhat23, nrow = 3, ncol = 1,
                          top = main)
  
}




## Spatial analysis ------------------------------------------------------------


if(!rlang::is_installed("INLAMSM")) devtools::install_github("becarioprecario/INLAMSM")

#' Besides calling the package, the present R code is frequently 
#' derived from INLAMSM source codes.

library(INLA)
library(INLAMSM)#' But before spatial analysis, let's do some NONspatial analysis:


#' Two ways of seeing this issue: 
#'   A) Panel - like analysis: covariate effects
#'      are constant in time
#'   B) Multivariate analysis: each year accounts
#'      for a different target variable, in other words
#'      covariate effects are time-varying ==>

cav_nosp_inla <- inla(
  N_ACC ~ 0 + Intercept +TEP_th + ELI + PGR + UIS + ELL + PDI + ER,
  offset = log(nn),
  family = rep("poisson", 3), data =dd_list,
  num.threads = 1, control.compute = list(internal.opt = F, cpo = T, waic = T, config = T), 
  verbose = T)

cav_nosp_inla_panel <- inla(
  N_ACC ~ 0 + Intercept +TEP_th + ELI + PGR + UIS + ELL + PDI + ER,
  offset = log(nn),
  family = "poisson", data =dd_long,
  num.threads = 1, control.compute = list(internal.opt = F, cpo = T, waic = T, config = T), 
  verbose = T)


#' The simplest way to take into account the three different years
#' is defining a multivariate model in which each year corresponds
#' to a different dependent variable.
#' 
#' We use the PCAR, as outlined in Gelfand and Vounatsu (2003):
#' Unique spatial autocorrelation parameter for the three 
#' target variables.
#' 


cav_IMCAR_inla <- inla(
  N_ACC ~ 0 + Intercept +TEP_th + ELI + PGR + UIS + ELL + PDI + ER+ 
    f(ID, model = inla.IMCAR.model(k = 3, W = W_con), extraconstr = list(
      A = A_constr, e = c(0,0,0))),
  offset = log(nn),
  family = rep("poisson", 3), data =dd_list,
  #inla.mode = "classic", control.inla = list(strategy = "laplace", int.strategy = "grid"),
  num.threads = 1, control.compute = list(internal.opt = F, cpo = T, waic = T, config = T), 
  verbose = T)

#' Warning: only works with unique intercept for
#' all years.

cav_IMCAR_inla_panel <- inla(
  N_ACC ~ 0 + Intercept +TEP_th + ELI + PGR + UIS + ELL + PDI + ER+ 
    f(ID_im, model = inla.IMCAR.model(k = 3, W = W_con), extraconstr = list(
      A = A_constr, e = c(0,0,0))),
  offset = log(nn),
  family = "poisson", data =dd_long,
  #inla.mode = "classic", control.inla = list(strategy = "laplace", int.strategy = "grid"),
  num.threads = 1, control.compute = list(internal.opt = F, cpo = T, waic = T, config = T), 
  verbose = T)


cav_INDPMCAR_inla <- inla(
  N_ACC ~ 0 + 1 +TEP_th + ELI + PGR + UIS + ELL + PDI + ER+ 
    f(ID, model = inla.INDMCAR.model(k = 3, W = W_con,  alpha.min = 0,alpha.max = 1)),
  offset = log(nn),
  family = rep("poisson", 3), data =dd_list,
  #control.fixed = list(prec = list(Intercept1 = 0, Intercept2 = 0, Intercept3 = 0)),
  #inla.mode = "classic", control.inla = list(strategy = "laplace", int.strategy = "grid"),
  num.threads = 1, control.compute = list(internal.opt = F, cpo = T, waic = T, config = T), 
  verbose = T)


cav_PMCAR_inla <- inla(
  N_ACC ~ 1 +TEP_th + ELI + PGR + UIS + ELL + PDI + ER+ 
    f(ID, model = inla.MCAR.model(k = 3, W = W_con,  alpha.min = 0,alpha.max = 1)),
  offset = log(nn),
  family = rep("poisson", 3), data =dd_list,
  #control.fixed = list(prec = list(Intercept1 = 0, Intercept2 = 0, Intercept3 = 0)),
  #inla.mode = "classic", control.inla = list(strategy = "laplace", int.strategy = "grid"),
  num.threads = 1, control.compute = list(internal.opt = F, cpo = T,  waic = T, config = T), 
  verbose = T)




#' Apparently seems nice.
#' Apparently.
#' 
#' The problem is that CPO is utterly messed up, as in 
#' most INLA applications to this dataset.

cav_PMCAR_inla_tHyper <- inla.MCAR.transform(cav_PMCAR_inla, k=3,
                                            model = "PMCAR", alpha.min = 0, alpha.max = 1)

#' Autocorrelation seems high. Still, remind the CPO problem:
#' may it be a red flag for INLA not doing well?

inla.zmarginal(inla.tmarginal(
  fun = function(X) 1/(1 + exp(-X)),
  marginal = cav_PMCAR_inla$marginals.hyperpar[[1]]))

#' Weird result: employment rate has negative association, and 
#' even stronger in absolute value than for 2022.


#' Leroux model, manual definition: --------------------------------------------

inla.rgeneric.MLCAR.model <- 
  function (cmd = c("graph", "Q", "mu", "initial", "log.norm.const", 
                    "log.prior", "quit"), theta = NULL) {
    interpret.theta <- function() {
      alpha <-  1/(1 + exp(-theta[1L]))
      mprec <- sapply(theta[as.integer(2:(k + 1))], function(x) {
        exp(x)
      })
      corre <- sapply(theta[as.integer(-(1:(k + 1)))], function(x) {
        (2 * exp(x))/(1 + exp(x)) - 1
      })
      param <- c(alpha, mprec, corre)
      n <- (k - 1) * k/2
      M <- diag(1, k)
      M[lower.tri(M)] <- param[k + 2:(n + 1)]
      M[upper.tri(M)] <- t(M)[upper.tri(M)]
      st.dev <- 1/sqrt(param[2:(k + 1)])
      st.dev.mat <- matrix(st.dev, ncol = 1) %*% matrix(st.dev, 
                                                        nrow = 1)
      M <- M * st.dev.mat
      PREC <- solve(M)
      return(list(alpha = alpha, param = param, VACOV = M, 
                  PREC = PREC))
    }
    graph <- function() {
      PREC <- matrix(1, ncol = k, nrow = k)
      G <- kronecker(PREC, Matrix::Diagonal(nrow(W), 1) + 
                       W)
      return(G)
    }
    Q <- function() {
      param <- interpret.theta()
      Lapl <- Matrix::Diagonal(nrow(W), apply(W, 1, sum)) -  W
      #Sigma.u <- MASS::ginv(as.matrix(Lapl))
      #Sigma <- param$alpha * Sigma.u + (1-param$alpha)*diag(1, nrow(W))
      R <- param$alpha*Lapl + (1-param$alpha)*diag(1, nrow(W))
      Q <- kronecker(param$PREC, R)
      return(Q)
    }
    mu <- function() {
      return(numeric(0))
    }
    log.norm.const <- function() {
      val <- numeric(0)
      return(val)
    }
    log.prior <- function() {
      param <- interpret.theta()
      # Uniform prior on \lambda
      # val <- -theta[1L] - 2 * log(1 + exp(-theta[1L]))
      # Normal prior on \lambda, in analogy with the univariate default case
      val <- log(dnorm(theta[1L], mean = 0, sd = sqrt(1/0.45))) 
      val <- val + log(MCMCpack::dwish(
        W = param$PREC, v = k, S = diag(rep(1, k)))) +
        sum(theta[as.integer(2:(k + 1))]) + 
        sum(log(2) + theta[-as.integer(1:(k + 1))] - 
              2 * log(1 + exp(theta[-as.integer(1:(k + 1))])))
      return(val)
    }
    initial <- function(){
      if(is.null(init)){
        return(c(0, rep(0, k), rep(0, (k * (k - 1)/2))))
      } else return(init)
    } 
      
    quit <- function() {
      return(invisible())
    }
    if (as.integer(R.version$major) > 3) {
      if (!length(theta)) 
        theta = initial()
    }
    else {
      if (is.null(theta)) {
        theta <- initial()
      }
    }
    val <- do.call(match.arg(cmd), args = list())
    return(val)
  }

inla.MLCAR.model <- function(...) INLA::inla.rgeneric.define(inla.rgeneric.MLCAR.model, ...)

cav_MLCAR_inla <- inla(
  N_ACC ~ 1 +TEP_th + ELI + PGR + UIS + ELL + PDI + ER+ 
    f(ID, model = inla.MLCAR.model(k = 3, W = W_con, init = NULL)),
  offset = log(nn),
  family = rep("poisson", 3), data =dd_list,
  #control.fixed = list(prec = list(Intercept1 = 0, Intercept2 = 0, Intercept3 = 0)),
  #inla.mode = "classic", control.inla = list(strategy = "laplace", int.strategy = "grid"),
  num.threads = 1, control.compute = list(internal.opt = F, cpo = T, waic = T, config = T), 
  verbose = T)

#' Allow to set initial values
cav_MLCAR_inla_init <- inla(
  N_ACC ~ 1 +TEP_th + ELI + PGR + UIS + ELL + PDI + ER+ 
    f(ID, model = inla.MLCAR.model(k = 3, W = W_con, init = c(-3, rep(0,6)))),
  offset = log(nn),
  family = rep("poisson", 3), data =dd_list,
  #control.fixed = list(prec = list(Intercept1 = 0, Intercept2 = 0, Intercept3 = 0)),
  #inla.mode = "classic", control.inla = list(strategy = "laplace", int.strategy = "grid"),
  num.threads = 1, control.compute = list(internal.opt = F, cpo = T, waic = T, config = T), 
  verbose = T)

#' Mixing parameter
#' (hard to interpret etcetera)

inla.zmarginal(inla.tmarginal(
  fun = function(X) 1/(1 + exp(-X)),
  marginal = cav_MLCAR_inla$marginals.hyperpar[[1]]))

#' Covariances
inla.MCAR.transform(cav_MLCAR_inla, model = "PMCAR", k = 3, alpha.min = 0, alpha.max = 1)$summary.hyperpar
inla.MCAR.transform(cav_PMCAR_inla, model = "PMCAR", k = 3, alpha.min = 0, alpha.max = 1)$summary.hyperpar

#' Manual transformation of ALL the covariance matrix 

inla.zmarginal(inla.tmarginal(fun = function(x) exp(-x), 
                              marginal = cav_MLCAR_inla$marginals.hyperpar[[2]]))

inla.zmarginal(inla.tmarginal(fun = function(x) exp(-x), 
                              marginal = cav_MLCAR_inla$marginals.hyperpar[[3]]))

inla.zmarginal(inla.tmarginal(fun = function(x) exp(-x), 
                              marginal = cav_MLCAR_inla$marginals.hyperpar[[4]]))


inla.zmarginal(inla.tmarginal(fun = function(x) (2 * exp(x))/(1 + exp(x)) - 1, 
                              marginal = cav_MLCAR_inla$marginals.hyperpar[[5]]))

inla.zmarginal(inla.tmarginal(fun = function(x) (2 * exp(x))/(1 + exp(x)) - 1, 
                              marginal = cav_MLCAR_inla$marginals.hyperpar[[6]]))

inla.zmarginal(inla.tmarginal(fun = function(x) (2 * exp(x))/(1 + exp(x)) - 1, 
                              marginal = cav_MLCAR_inla$marginals.hyperpar[[7]]))


#' Median of the scale parameter.
#' Yes, this is too much code for a 3x3 matrix.
#' 
Sigma_diagonal <- sapply(c(2,3,4), function(i){
  inla.zmarginal(inla.tmarginal(fun = function(x) exp(-x), 
                                marginal = 
                                  cav_MLCAR_inla$marginals.hyperpar[[i]]),
                 silent = T)$quant0.5
})

Sigma_offdiag <- sapply(c(5, 6, 7), function(i){
  inla.zmarginal(inla.tmarginal(fun = function(x) (2 * exp(x))/(1 + exp(x)) - 1, 
                                marginal = 
                                  cav_MLCAR_inla$marginals.hyperpar[[i]]),
                 silent = T)$quant0.5
})

Sigma <- Matrix::Diagonal(x = Sigma_diagonal)
Sigma[lower.tri(Sigma)] <- Sigma_offdiag
Sigma[upper.tri(Sigma)] <- t(Sigma[lower.tri(Sigma)])
Sigma




## Multivariate BYM ATTEMPT - Warning: slow ------------------------------------


#scaleQ <- INLA:::inla.scale.model.internal(
#  L_block, constr = list(A = A_constr, e = rep(0, nrow(A_constr))))
#' Eigenvalues of ICAR precision.
#' caveat: I used the SCALED precision, don't know if is what
#' inla.pc.bym.phi() needs. It should
#L_eigen_scaled <- pmax(eigen( scaleQ$Q)$values, 0)

#' PC prior on the mixing parameter
#' 
#dpc.log.0 <- INLA:::inla.pc.bym.phi(Q =  scaleQ$Q,
#                                  u = 0.5, alpha = 2/3)

#log.dpc.phi.bym <- INLA:::inla.pc.bym.phi(eigenvalues = L_eigen_scaled, 
#                                  marginal.variances = scaleQ$var,
#                                  rankdef = nrow(A_constr),
#                                  u = 0.5, alpha = 2/3)

#' WAIC: 2921.905, summaries for phi:
#' Mean            0.902401 
#' Stdev           0.0659761 
#' Quantile  0.025 0.724788 
#' Quantile  0.25  0.875523 
#' Quantile  0.5   0.920359 
#' Quantile  0.75  0.948348 
#' Quantile  0.975 0.976785

inla.rgeneric.MBYM.dense <- 
  function (cmd = c("graph", "Q", "mu", "initial", "log.norm.const", 
                    "log.prior", "quit"), theta = NULL ) {
    envir <- parent.env(environment())
    if(!exists("cache.done", envir=envir)){
      starttime.scale <- Sys.time()
      #' Unscaled Laplacian matrix (marginal precision of u_1, u_2 ... u_k)
      L_unscaled <- Matrix::Diagonal(nrow(W), rowSums(W)) -  W
      L_unscaled_block <- kronecker(diag(1,k), L_unscaled)
      A_constr <- t(pracma::nullspace(as.matrix(L_unscaled_block)))
      scaleQ <- INLA:::inla.scale.model.internal(
        L_unscaled_block, constr = list(A = A_constr, e = rep(0, nrow(A_constr))))
      #' Block Laplacian, i.e. precision of U = I_k \otimes L
      n <- nrow(W)
      L <- scaleQ$Q[c(1:n), c(1:n)]
      Sigma.u <- MASS::ginv(as.matrix(L))
      if(PC == TRUE){
        #' Eigenvalues of the SCALED Laplacian, sufficient for trace and determinant entering the KLD
        L_eigen_scaled <- eigen(scaleQ$Q)$values
        #' PC prior on mixing parameter - definition should be fine.
        log.dpc.phi.bym <- INLA:::inla.pc.bym.phi(eigenvalues = L_eigen_scaled, 
                                                  marginal.variances = scaleQ$var,
                                                  rankdef = nrow(A_constr),
                                                  u = 0.5, alpha = 2/3)
        assign("log.dpc.phi.bym", log.dpc.phi.bym, envir = envir)
        
      }
      endtime.scale <- Sys.time()
      cat("Time needed for scaling Laplacian matrix: ",
          round(difftime(endtime.scale, starttime.scale), 3), " seconds \n")
      assign("L", L, envir = envir)
      assign("Sigma.u", Sigma.u, envir = envir)
      assign("cache.done", TRUE, envir = envir)
    }
    interpret.theta <- function() {
      alpha <-  1/(1 + exp(-theta[1L]))
      mprec <- sapply(theta[as.integer(2:(k + 1))], function(x) {
        exp(x)
      })
      corre <- sapply(theta[as.integer(-(1:(k + 1)))], function(x) {
        (2 * exp(x))/(1 + exp(x)) - 1
      })
      param <- c(alpha, mprec, corre)
      n <- (k - 1) * k/2
      M <- diag(1, k)
      M[lower.tri(M)] <- param[k + 2:(n + 1)]
      M[upper.tri(M)] <- t(M)[upper.tri(M)]
      st.dev <- 1/sqrt(param[2:(k + 1)])
      st.dev.mat <- matrix(st.dev, ncol = 1) %*% matrix(st.dev, 
                                                        nrow = 1)
      M <- M * st.dev.mat
      PREC <- solve(M)
      return(list(alpha = alpha, param = param, VACOV = M, 
                  PREC = PREC))
    }
    graph <- function() {
      PREC <- matrix(1, ncol = k, nrow = k)
      G <- kronecker(PREC, Matrix::Diagonal(nrow(W), 1) + 
                       W)
      return(G)
    }
    Q <- function() {
      param <- interpret.theta()
      #' Weighted average of ICAR and IID variables: variance is the sum of variances.
      #' Precision here defined as inverse variance. Not
      #' the best way to do it; still using sparse
      #' parametrisation requires a latent effect of length 2*np
      Sigma <- param$alpha * Sigma.u + (1-param$alpha)*diag(1, nrow(W))
      Q <- kronecker(param$PREC, solve(Sigma))
      return(Q)
    }
    mu <- function() {
      return(numeric(0))
    }
    log.norm.const <- function() {
      val <- numeric(0)
      return(val)
    }
    log.prior <- function() {
      param <- interpret.theta()
      if(PC == TRUE){
        #' PC prior implementation
        val <- log.dpc.phi.bym(param$phi)- theta[1L] - 2 * log(1 + exp(-theta[1L]))
      } else {
        #' Uniform prior
        val <- -theta[1L] - 2 * log(1 + exp(-theta[1L]))
      }
      #' Whishart prior on precision (inverse scale)
      val <- val + log(MCMCpack::dwish(W = param$PREC, v = k,
                                       S = diag(rep(1, k)))) +
        #' This for the change of variable
        #' (code from INLAMSM)
        sum(theta[as.integer(2:(k +  1))]) +
        sum(log(2) + theta[-as.integer(1:(k + 1))] - 2 * log(1 + exp(theta[-as.integer(1:(k + 1))])))
      return(val)
    }
    initial <- function() {
      if(!exists("init", envir = envir)){
        return(c(0, rep(0, k), rep(0, (k * (k - 1)/2))))
      } else{
        return(init)
      }
    }
    quit <- function() {
      return(invisible())
    }
    if (as.integer(R.version$major) > 3) {
      if (!length(theta)) 
        theta = initial()
    }
    else {
      if (is.null(theta)) {
        theta <- initial()
      }
    }
    val <- do.call(match.arg(cmd), args = list())
    return(val)
  }

#' TBD
inla.rgeneric.MBYM.sparse <- 
  function (cmd = c("graph", "Q", "mu", "initial", "log.norm.const", 
                    "log.prior", "quit"), theta = NULL) {
    envir <- parent.env(environment())
    if(!exists("cache.done", envir=envir)){
      starttime.scale <- Sys.time()
      #' Unscaled Laplacian matrix (marginal precision of u_1, u_2 ... u_k)
      L_unscaled <- Matrix::Diagonal(nrow(W), rowSums(W)) -  W
      L_unscaled_block <- kronecker(diag(1,k), L_unscaled)
      A_constr <- t(pracma::nullspace(as.matrix(L_unscaled_block)))
      scaleQ <- INLA:::inla.scale.model.internal(
        L_unscaled_block, constr = list(A = A_constr, e = rep(0, nrow(A_constr))))
      #' Block Laplacian, i.e. precision of U = I_k \otimes L
      n <- nrow(W)
      L <- scaleQ$Q[c(1:n), c(1:n)]
      if(PC == TRUE){
        #' Eigenvalues of the SCALED Laplacian, sufficient for trace and determinant entering the KLD
        L_eigen_scaled <- eigen(scaleQ$Q)$values
        #' PC prior on mixing parameter - definition should be fine.
        log.dpc.phi.bym <- INLA:::inla.pc.bym.phi(eigenvalues = L_eigen_scaled, 
                                                  marginal.variances = scaleQ$var,
                                                  rankdef = nrow(A_constr),
                                                  u = 0.5, alpha = 2/3)
        assign("log.dpc.phi.bym", log.dpc.phi.bym, envir = envir)
        
      }
      endtime.scale <- Sys.time()
      cat("Time needed for scaling Laplacian matrix: ",
          round(difftime(endtime.scale, starttime.scale), 3), " seconds \n")
      assign("L", L, envir = envir)
      assign("cache.done", TRUE, envir = envir)
    }
    interpret.theta <- function() {
      phi <-  1/(1 + exp(-theta[1L]))
      mprec <- sapply(theta[as.integer(2:(k + 1))], function(x) {
        exp(x)
      })
      corre <- sapply(theta[as.integer(-(1:(k + 1)))], function(x) {
        (2 * exp(x))/(1 + exp(x)) - 1
      })
      param <- c(phi, mprec, corre)
      n <- (k - 1) * k/2
      M <- diag(1, k)
      M[lower.tri(M)] <- param[k + 2:(n + 1)]
      M[upper.tri(M)] <- t(M)[upper.tri(M)]
      st.dev <- 1/sqrt(param[2:(k + 1)])
      st.dev.mat <- matrix(st.dev, ncol = 1) %*% matrix(st.dev, 
                                                        nrow = 1)
      M <- M * st.dev.mat
      PREC <- solve(M)
      return(list(phi = phi, param = param, VACOV = M, 
                  PREC = PREC))
    }
    graph <- function() {
      BPrec <- matrix(1, ncol = 2*k, nrow = 2*k)
      G <- kronecker(BPrec, Matrix::Diagonal(nrow(W), 1) + 
                       W)
      return(G)
    }
    Q <- function() {
      param <- interpret.theta()
      Q11 <- 1/(1 - param$phi) * kronecker(param$PREC, Matrix::Diagonal(n = nrow(W), x  = 1))
      Q12 <- Q21 <- -sqrt(param$phi)/(1 - param$phi) * kronecker(param$PREC, Matrix::Diagonal(n = nrow(W), x  = 1))
      Q22 <- kronecker(param$PREC, 
                       ((param$phi/(1-param$phi))* Matrix::Diagonal(n = nrow(W), x = 1) + L))
      Q <- rbind(cbind(Q11, Q12), cbind(Q21, Q22))
      return(Q)
    }
    mu <- function() {
      return(numeric(0))
    }
    log.norm.const <- function() {
      val <- numeric(0)
      return(val)
    }
    
    log.prior <- function() {
      param <- interpret.theta()
      if(PC == TRUE){
        #' PC prior implementation
        val <-  log.dpc.phi.bym(param$phi) - theta[1L] - 2 * log(1 + exp(-theta[1L]))
      }else {
        #' Uniform prior
        val <- -theta[1L] - 2 * log(1 + exp(-theta[1L]))
      }
      #' Whishart prior on precision (inverse scale)
      val <- val + log(MCMCpack::dwish(W = param$PREC, v = k,
                                       S = diag(rep(1, k)))) +
        #' This for the change of variable
        #' (code from INLAMSM)
        sum(theta[as.integer(2:(k +  1))]) +
        sum(log(2) + theta[-as.integer(1:(k + 1))] - 2 * log(1 + exp(theta[-as.integer(1:(k + 1))])))
      return(val)
    }
    initial <- function() {
      if(!exists("init", envir = envir)){
        return(c(0, rep(0, k), rep(0, (k * (k - 1)/2))))
      } else{
        return(init)
      }
    }
    quit <- function() {
      return(invisible())
    }
    if (as.integer(R.version$major) > 3) {
      if (!length(theta)) 
        theta <- initial()
    }
    else {
      if (is.null(theta)) {
        theta <- initial()
      }
    }
    val <- do.call(match.arg(cmd), args = list())
    return(val)
  }

inla.MBYM.dense <- function(...) INLA::inla.rgeneric.define(inla.rgeneric.MBYM.dense, ...)
inla.MBYM.sparse <- function(...) INLA::inla.rgeneric.define(inla.rgeneric.MBYM.sparse, ...)


#' Warning: using a 'prudential' initial value for logit(phi)
#' can make this already slow model even slower; still, we want to rule out overestimation.

cav_MBYM_inla <- inla(
  N_ACC ~ 0 + Intercept +TEP_th + ELI + PGR + UIS + ELL + PDI + ER+ 
    f(ID, model = inla.MBYM.dense(k = 3, W = W_con, 
                                  PC = FALSE),
      extraconstr = list(A = A_constr, e = rep(0, 3))) ,
  offset = log(nn),
  family = rep("poisson", 3), data =dd_list,
  #control.fixed = list(prec = list(Intercept1 = 0, Intercept2 = 0, Intercept3 = 0)),
  #inla.mode = "classic", control.inla = list(strategy = "laplace", int.strategy = "grid"),
  num.threads = 1, control.compute = list(internal.opt = F, cpo = T, waic = T, config = T), 
  verbose = T)

#' Weird results, needs to be checked. 
#' Still, it's fast!
cav_MBYM_inla_sparse <- inla(
  N_ACC ~ 0 + Intercept +TEP_th + ELI + PGR + UIS + ELL + PDI + ER+ 
    f(ID, model = inla.MBYM.sparse(k = 3, W = W_con, 
                                   init = c(0, rep(0,3), rep(0,3)),
                                   PC = FALSE),
      extraconstr = list(A = kronecker(diag(1,2), A_constr), e = rep(0, 6)),
      values = dd_list$ID2 ) ,
  offset = log(nn),
  family = rep("poisson", 3), data =dd_list,
  #control.fixed = list(prec = list(Intercept1 = 0, Intercept2 = 0, Intercept3 = 0)),
  #inla.mode = "classic", control.inla = list(strategy = "laplace", int.strategy = "grid"),
  num.threads = 1, control.compute = list(internal.opt = F, cpo = T, waic = T, config = T), 
  verbose = T)


#' More 'standard' initial values, but not in line
#' with INLAMSM models
cav_MBYM_inla_I1 <- inla(
  N_ACC ~ 0 + Intercept +TEP_th + ELI + PGR + UIS + ELL + PDI + ER+ 
    f(ID, model = inla.MBYM.dense(k = 3, W = W_con, 
                                  init = c(-3, rep(4,3), rep(0,3)),
                                  PC = FALSE),
      extraconstr = list(A = A_constr, e = rep(0, 3))
      #values = dd_list$ID2
    ) ,
  offset = log(nn),
  family = rep("poisson", 3), data =dd_list,
  #control.fixed = list(prec = list(Intercept1 = 0, Intercept2 = 0, Intercept3 = 0)),
  #inla.mode = "classic", control.inla = list(strategy = "laplace", int.strategy = "grid"),
  num.threads = 1, control.compute = list(internal.opt = F, cpo = T, waic = T, config = T), 
  verbose = T)

#' Ungodly crashing
cav_MBYM_inla_I1_pc <- inla(
  N_ACC ~ 0 + Intercept +TEP_th + ELI + PGR + UIS + ELL + PDI + ER+ 
    f(ID, model = inla.MBYM.dense(k = 3, W = W_con, 
                                  init = c(-3, rep(0,3), rep(0,3)),
                                  PC = TRUE),
      extraconstr = list(A = A_constr, e = rep(0, 3))
      #values = dd_list$ID2
    ) ,
  offset = log(nn),
  family = rep("poisson", 3), data =dd_list,
  #control.fixed = list(prec = list(Intercept1 = 0, Intercept2 = 0, Intercept3 = 0)),
  #inla.mode = "classic", control.inla = list(strategy = "laplace", int.strategy = "grid"),
  num.threads = 1, control.compute = list(internal.opt = F, cpo = T, waic = T, config = T), 
  verbose = T)



#' No, too high.
inla.zmarginal(inla.tmarginal(
  fun = function(X) 1/(1 + exp(-X)),
  marginal = cav_MBYM_inla$marginals.hyperpar[[1]]))

#' Nasty:
#' 
alphalimX <-range(do.call(c, lapply(cav_MBYM_inla$marginals.fixed[-c(1:3)], FUN = function(x) x[,1])))
alphalimY <-range(do.call(c, lapply(cav_MBYM_inla$marginals.fixed[-c(1:3)], FUN = function(x) x[,2])))

alphamarg <- as.data.frame(do.call(cbind, cav_MBYM_inla$marginals.fixed[-c(1:3)]))
names(alphamarg) <- paste0(rep(names(cav_MBYM_inla$marginals.fixed[-c(1:3)]),each=2), c("__X", "__Y"))
names(alphamarg) <- 
  gsub("1__", "_._2021__", gsub("2__", "_._2022__", gsub("3__", "_._2023__",
                                                       names(alphamarg))))

alphamarg_long <- tidyr::pivot_longer(alphamarg, cols = c(1:42),
                                       names_to = c("Effect", ".value"),
                                       names_sep = "__") %>% 
  tidyr::separate(.data$Effect, into = c("Effect", "Year"), sep = "_._")

#' If we do not indulge into pogit regression, let us just keep calling effects \beta:

ggplot2::ggplot(alphamarg_long, ggplot2::aes(x = .data$X, y = .data$Y, color = .data$Effect)) +
  ggplot2::geom_line(size = 0.7) +
  ggplot2::geom_vline(xintercept = 0) +
  ggplot2::coord_cartesian(xlim = alphalimX, ylim = alphalimY) +
  ggplot2::labs(
    #title = "Posterior Marginals of Covariates Effects",
    x = expression(beta),
    y = expression(pi(beta ~ "|" ~ y)),
    color = "Effect") +
  ggplot2::theme_classic() +
  ggplot2::facet_wrap(~.data$Year, scales = "free_y") 


##' Comparison using LGOCV -----------------------------------------------------


#' List of models
mm <- list(ICAR = cav_IMCAR_inla, PCAR = cav_PMCAR_inla,
           LCAR = cav_MLCAR_inla, BYM = cav_MBYM_inla)

#' Wrapper function to summarise the output of inla.group.cv
lpml.lgo <- function(models, num.level.sets){
  res <- lapply(models, function(x){
    obj <- inla.group.cv(x, num.level.sets = num.level.sets)
    cv <- obj$cv
    groups <- obj$groups
    lpml <- -sum(log(obj$cv))
    return(list(cv = cv, lpml = lpml, groups = groups))
  })
  if(!is.null(names(models))) names(res) <- names(models)
  return(res)
}

#' Summary with the scores of various numbers of level sets
LPMLs <- function(models, num.level.sets){
  num.level.sets<- as.list(num.level.sets)
  LPMLs_list <- lapply(lapply(num.level.sets, function(x){
    lpml.lgo(models, x)
  }), function(x){
    return(lapply(x, function(x){
      return(x$lpml)
    }))
  })
  names(LPMLs_list) <- paste0("Out_", as.character(unlist(num.level.sets)), "_sets")
  df <- as.data.frame(do.call(rbind, lapply(LPMLs_list, unlist)))
  return(df)
}
LPMLs_df <- LPMLs(models = mm, c(1:6, 8, 10, 12, 15, 20, 25))

## Spatiotemporal model: INLA versus MCMC --------------------------------------



cav_BYM_st_bmstdrCARBayes <- 
  bmstdr::Bcartime(N_ACC ~ 1 + offset(log(nn)) + TEP_th + ELI + PGR + UIS + ELL + PDI + ER,
                 data=dd_long, package = "CARBayesST", model = "ar",
                 scol="ID_m", tcol = "Year",  family="poisson", W=W_con,
                 N=50000, burn.in=10000, thin=5)

## Multivariate Pogit analysis ATTEMPT ---------------------------------
 

#' Let's try a single-likelihood approach:
#' FUnction to build a bigger data object, nothing exciting
ls2df <- function(lst=dd_list){
  ls_M <- lapply(lst[-which(names(lst) %in% c("ID2", "N_ACC"))],
                 function(X) X <- as.matrix(X))
  df <- as.data.frame(do.call(cbind, ls_M))
  torep <- sum(sapply(ls_M, function(X) ncol(X))==3)
  names(df) <- c(paste0(
    rep(names(ls_M)[1:torep], sapply(ls_M, function(X) ncol(X))[1:torep]),
    c(rep(c("_2021", "_2022", "_2023"), torep)) ), 
    names(ls_M)[-c(1:torep)])
  df$N_ACC <- lst$N_ACC[which(!is.na(lst$N_ACC))]
  df$Year <- rep(c("2021", "2022", "2023"), each = n)
  df <- dplyr::relocate(df, .data$Year, .before = 1) %>%
    dplyr::relocate(.data$N_ACC, .before = .data$Year)
  return(df)
}
dd_extended <- ls2df()
dd_extended[is.na(dd_extended)] <- 0


#' This is the same as cav_IMCAR_inla, only a longer formula for,
#' however, the same result.
cav_IMCAR_inla_repl <- inla(formula = as.formula(
  paste0("N_ACC ~ 0 + ", 
         paste(names(dd_extended)[c(3:26)], collapse = " + "),
         " + f(ID, model = inla.IMCAR.model(k = 3, W = W_con),",
         "extraconstr = list(A = A_constr, e = c(0,0,0)))")),
  offset = log(nn),
  family = "poisson", data = dd_extended,
  #inla.mode = "classic", control.inla = list(strategy = "laplace", int.strategy = "grid"),
  num.threads = 1, control.compute = list(internal.opt = F, cpo = T, waic = T, config = T), 
  verbose = T)



library(inlabru)
bru_options_set(bru_max_iter = 30,
                control.inla = list(tolerance = 1e-10))

logexpit <- function(x, beta){
  pred <- beta[[1]] +
    rowSums(do.call(cbind, lapply(c(1:length(x)), function(n){
      beta[[n+1]]*x[[n]]
      })))
  return(-log(1+exp(-pred)))
}

cmp_spatial <- function(spatial_expr, st_expr = NULL, t_expr = NULL) {
  
  spatial <- function(id, ...) INLA::f(id, ...)
  spatiotemporal <- function(id, ...) INLA::f(id, ...)
  temporal <- function(id, ...) INLA::f(id, ...)
  
  f2 <- substitute(spatial_expr)
  f3 <- substitute(t_expr)
  f4 <- substitute(st_expr)
  f1 <- bquote(
    ~ 0 +
      alpha_0_2021(Intercept_2021) + 
      alpha_0_2022(Intercept_2022) +  
      alpha_0_2023(Intercept_2023) +  
      beta_0_2021(main = Intercept_2021,  model = "linear",
                  mean.linear = -2.2,
                  prec.linear = 1e+2) +
      beta_0_2022(main = Intercept_2022,  model = "linear",
                  mean.linear = -2.2,
                  prec.linear = 1e+2) +
      beta_0_2023(main = Intercept_2023,  model = "linear",
                  mean.linear = -2.2,
                  prec.linear = 1e+2) +
      myoffset(log(nn), model = "offset") +
      alpha_ELI_2021(ELI_2021) + alpha_ELI_2022(ELI_2022) + alpha_ELI_2023(ELI_2023) + 
      alpha_PGR_2021(PGR_2021) + alpha_PGR_2022(PGR_2022) + alpha_PGR_2023(PGR_2023) + 
      alpha_UIS_2021(UIS_2021) + alpha_UIS_2022(UIS_2022) + alpha_UIS_2023(UIS_2023) + 
      alpha_PDI_2021(PDI_2021) + alpha_PDI_2022(PDI_2022) + alpha_PDI_2023(PDI_2023) + 
      alpha_ER_2021(ER_2021) + alpha_ER_2022(ER_2022) + alpha_ER_2023(ER_2023) + 
      beta_TEP_2021(main = Intercept_2021, model = "linear", 
                    mean.linear = 0,
                    prec.linear = 1e-3) + 
      beta_TEP_2022(main = Intercept_2022, model = "linear", 
                    mean.linear = 0,
                    prec.linear = 1e-3)  +
      beta_TEP_2023(main = Intercept_2023, model = "linear", 
                    mean.linear = 0,
                    prec.linear = 1e-3) +
      beta_ELL_2021(main = Intercept_2021, model = "linear", 
                    mean.linear = 0,
                    prec.linear = 1e-3)  +
      beta_ELL_2022(main = Intercept_2022, model = "linear", 
                    mean.linear = 0,
                    prec.linear = 1e-3)  +
      beta_ELL_2023(main = Intercept_2023, model = "linear", 
                    mean.linear = 0,
                    prec.linear = 1e-3) )

  #ff <- as.call(c(quote(`+`), f1[[2]], f2, f3, f4))
  ff <- f1[[2]]
  if (!is.null(f2)) ff <- as.call(c(quote(`+`), ff, f2))
  if (!is.null(f3)) ff <- as.call(c(quote(`+`), ff, f3))
  if (!is.null(f4)) ff <- as.call(c(quote(`+`), ff, f4))
  final_formula <- as.formula(bquote(~ .(ff)))
  return(final_formula)
}

cmp_ICAR <- cmp_spatial(spatial_expr = spatial( 
  ID, model = inla.IMCAR.model(k = 3, W = W_con), 
  extraconstr = list(A = A_constr, e = c(0,0,0))))

cmp_PCAR <- cmp_spatial(spatial(
  ID, model = inla.MCAR.model(
    k = 3, W = W_con,  alpha.min = 0,alpha.max = 1)))

cmp_MLCAR <- cmp_spatial(spatial(
  ID, model = inla.MLCAR.model(k = 3, W = W_con, init = NULL)))

cmp_BYM <- cmp_spatial(spatial(
  ID, model = inla.MBYM.dense(k = 3, W = W_con, 
                              PC = FALSE),
  extraconstr = list(A = A_constr, e = rep(0, 3))))

cav_bru <- function(model_cmp, data = dd_extended){

  terms <- c("0", "alpha_0_2021", "alpha_0_2022", "alpha_0_2023",
             "myoffset", 
             "alpha_ELI_2021", "alpha_ELI_2022", "alpha_ELI_2023",
             "alpha_PGR_2021", "alpha_PGR_2022", "alpha_PGR_2023",
             "alpha_UIS_2021", "alpha_UIS_2022", "alpha_UIS_2023",
             "alpha_PDI_2021", "alpha_PDI_2022", "alpha_PDI_2023",
             "alpha_ER_2021", "alpha_ER_2022", "alpha_ER_2023",
             paste0("logexpit(x = list(TEP_th_2021, ELL_2021),",
                    "beta = list(beta_0_2021, beta_TEP_2021, beta_ELL_2021))"),
             paste0("logexpit(x = list(TEP_th_2022, ELL_2022),",
                    "beta = list(beta_0_2022, beta_TEP_2022, beta_ELL_2022))"),
             paste0("logexpit(x = list(TEP_th_2023, ELL_2023),",
                    "beta = list(beta_0_2023, beta_TEP_2023, beta_ELL_2023))"))
  
  if (any(grepl("spatial", as.character(model_cmp)))) terms <- c(terms, "spatial")
  if (any(grepl("time", as.character(model_cmp)))) terms <- c(terms, "time")
  if (any(grepl("spatiotemporal", as.character(model_cmp)))) terms <- c(terms, "spatiotemporal")
  
  
  formula <- as.formula(paste("N_ACC ~", paste(terms, collapse = " + ")))
  
  res <- inlabru::bru(
    components = model_cmp,
    lik = inlabru::like(family = "poisson", formula = formula, data = data),
    options = list(verbose = T, num.threads = 1,
                   control.compute = list(
                     waic = T, cpo = T, dic = T, internal.opt = F),
                   control.predictor = list(compute =  T)))
  return(res)
}

#' Nonspatial
cav_nosp_inlabru <- cav_bru(model_cmp=cmp_spatial(NULL))
#' Simple spatial models: ICAR...
cav_IMCAR_inlabru <- cav_bru(cmp_ICAR)
#' ...LCAR... --> Warning: very low mixing parameter
cav_MLCAR_inlabru <- cav_bru(cmp_MLCAR)
#'...and PCAR
cav_PMCAR_inlabru <- cav_bru(cmp_PCAR)
#' Warning: BYM is slow.
#' Easier one with Uniform prior on phi
cav_MBYM_inlabru <- cav_bru(cmp_BYM)

#' PC-prior on phi -->  WARNING!! Goes crash!!!!
#cav_MBYM_pc_inlabru <- cav_bru(
#  model_cmp =  cmp_spatial(spatial(
#    ID, model = inla.MBYM.dense(k = 3, W = W_con, PC = TRUE),
#    extraconstr = list(A = A_constr, e = rep(0, 3)))))



# Spatiotemporal model attempt:
dd_extented_st <- dd_extended %>% 
  dplyr::mutate(Year = as.numeric(as.factor(.data$Year))) %>% 
  dplyr::mutate(Area = rep(.data$ID[c(1:n)], 3))

cmp_ST_noint <- cmp_spatial(spatial_expr = NULL,
  st_expr = spatiotemporal(Year, model="iid", group=Area,
                           control.group=list(model="besag", graph=W_con, scale.model =T), constr = T,
                           hyper = list(prec = list(prior = "pc.prec", param = c(1.5, 0.01)))))


cmp_ST_3 <- cmp_spatial(
  spatial_expr=spatial(Area, model = "bym2", graph = W_con,  
                       scale.model = T, constr = T, 
                       hyper = list(prec = list(prior = "pc.prec", param = c(1.5, 0.01)))),
  st_expr = spatiotemporal(Year, model="iid", group=Area,
               control.group=list(model="besag", graph=W_con, scale.model =T), constr = T,
               hyper = list(prec = list(prior = "pc.prec", param = c(1.5, 0.01)))))

cav_ST_0_inlabru <- cav_bru(cmp_ST_noint, data = dd_extented_st)

cav_INDPMCAR_inlabru <- cav_bru(model_cmp = cmp_spatial(spatial_expr = spatial( 
  ID, model = inla.INDIMCAR.model(k = 3, W = W_con), 
  extraconstr = list(A = A_constr, e = c(0,0,0)))) )

cav_ST_inlabru <- cav_bru(cmp_ST_3, data = dd_extented_st)


#' The INLA linear model is 
#' not particularly fast, but one minute and half is still 
#' a reasonable time.


cav_STbym_i3_INLA <- inla(formula = as.formula(
  paste0("N_ACC ~ 0 + ", 
         paste(names(dd_extended)[c(3:26)], collapse = " + "),
         " + f(Area, model = 'bym2', graph = W_con,  scale.model = T,",
         " hyper = list(prec = list(prior = 'pc.prec', param = c(1.5, 0.01))))+",
         "f(Year, model='iid', group=Area,",
         "control.group=list(model='besag', graph=W_con, scale.model =T),",
         "hyper = list(prec = list(prior = 'pc.prec', param = c(1.5, 0.01))))")),
                          family = "poisson", offset = log(nn), data =dd_extented_st,
                          num.threads = 1, control.compute = 
                            list(internal.opt = F, cpo = T, waic = T), 
                          control.predictor = list(compute = T),
                          verbose = T)
cav_STbym_i3_INLA$cpu.used


X.bru <- dd_extended_alt %>% 
  dplyr::mutate(alpha_0_2021 = Intercept_2021) %>% 
  dplyr::mutate(alpha_0_2022 = Intercept_2022) %>% 
  dplyr::mutate(alpha_0_2023 = Intercept_2023) %>% 
  dplyr::mutate(beta_0_2021 = Intercept_2021) %>% 
  dplyr::mutate(beta_0_2022 = Intercept_2022) %>% 
  dplyr::mutate(beta_0_2023 = Intercept_2023) %>% 
  dplyr::rename(alpha_ELI_2021 = .data$ELI_2021, alpha_ELI_2022 = .data$ELI_2022, alpha_ELI_2023 = .data$ELI_2023) %>% 
  dplyr::rename(alpha_PGR_2021 = .data$PGR_2021, alpha_PGR_2022 = .data$PGR_2022, alpha_PGR_2023 = .data$PGR_2023) %>% 
  dplyr::rename(alpha_UIS_2021 = .data$UIS_2021, alpha_UIS_2022 = .data$UIS_2022, alpha_UIS_2023 = .data$UIS_2023) %>%  
  dplyr::rename(alpha_PDI_2021 = .data$PDI_2021, alpha_PDI_2022 = .data$PDI_2022, alpha_PDI_2023 = .data$PDI_2023) %>% 
  dplyr::rename(alpha_ER_2021 = .data$ER_2021, alpha_ER_2022 = .data$ER_2022, alpha_ER_2023 = .data$ER_2023) %>% 
  dplyr::rename(beta_TEP_2021 = .data$TEP_th_2021, beta_TEP_2022 = .data$TEP_th_2022, beta_TEP_2023 = .data$TEP_th_2023) %>% 
  dplyr::rename(beta_ELL_2021 = .data$ELL_2021, beta_ELL_2022 = .data$ELL_2022, beta_ELL_2023 = .data$ELL_2023) %>% 
  dplyr::select(rownames(cav_nosp_inlabru$summary.fixed))
X.bru <- as.matrix(X.bru)          

## M-Model PCAR attempt --------------------------------------------------------


#' Edited version of INLAMSM:::inla.rgeneric.Mmodel.model
#' matching latest R versions, in which dependencies must always be recalled through the namespace


inla.Mmodel.man <- function(...) INLA::inla.rgeneric.define(inla.rgeneric.Mmodel.man, ...)


inla.rgeneric.Mmodel.man <- function (cmd = c("graph", "Q", "mu", "initial", "log.norm.const", 
                                              "log.prior", "quit"), theta = NULL) {
  interpret.theta <- function() {
    alpha <- alpha.min + (alpha.max - alpha.min)/(1 + exp(-theta[as.integer(1:k)]))
    M <- matrix(theta[-as.integer(1:k)], ncol = k)
    return(list(alpha = alpha, M = M))
  }
  graph <- function() {
    MI <- kronecker(Matrix::Matrix(1, ncol = k, nrow = k), 
                    Matrix::Diagonal(nrow(W), 1))
    IW <- Matrix::Diagonal(nrow(W), 1) + W
    BlockIW <- Matrix::bdiag(replicate(k, IW, simplify = FALSE))
    G <- (MI %*% BlockIW) %*% MI
    return(G)
  }
  Q <- function() {
    param <- interpret.theta()
    M.inv <- solve(param$M)
    MI <- kronecker(M.inv, Matrix::Diagonal(nrow(W), 1))
    D <- as.vector(apply(W, 1, sum))
    BlockIW <- Matrix::bdiag(lapply(1:k, function(i) {
      Matrix::Diagonal(x = D) - param$alpha[i] * W
    }))
    Q <- (MI %*% BlockIW) %*% kronecker(t(M.inv), Matrix::Diagonal(nrow(W), 
                                                                   1))
    return(Q)
  }
  mu <- function() {
    return(numeric(0))
  }
  log.norm.const <- function() {
    val <- numeric(0)
    return(val)
  }
  log.prior <- function() {
    param <- interpret.theta()
    val <- sum(-theta[as.integer(1:k)] - 2 * log(1 + exp(-theta[as.integer(1:k)])))
    sigma2 <- 1000
    val = val + log(MCMCpack::dwish(W = crossprod(param$M), 
                                    v = k, S = diag(rep(sigma2, k))))
    return(val)
  }
  initial <- function() {
    return(c(rep(0, k), as.vector(diag(rep(1, k)))))
  }
  quit <- function() {
    return(invisible())
  }
  if (as.integer(R.version$major) > 3) {
    if (!length(theta)) 
      theta = initial()
  }
  else {
    if (is.null(theta)) {
      theta <- initial()
    }
  }
  val <- do.call(match.arg(cmd), args = list())
  return(val)
}

cav_PMMCAR_inla <- inla(
  N_ACC ~ 1 +TEP_th + ELI + PGR + UIS + ELL + PDI + ER+ 
    f(ID, model = inla.Mmodel.man(k = 3, W = W_con,  alpha.min = 0,alpha.max = 1)),
  offset = log(nn),
  family = rep("poisson", 3), data =dd_list,
  #control.fixed = list(prec = list(Intercept1 = 0, Intercept2 = 0, Intercept3 = 0)),
  #inla.mode = "classic", control.inla = list(strategy = "laplace", int.strategy = "grid"),
  num.threads = 1, control.compute = list(internal.opt = F, cpo = T, waic = T, config = T), 
  verbose = T)

inla.zmarginal(inla.tmarginal(fun = function(x) (1+exp(-x))^-1,
                              cav_PMMCAR_inla$marginals.hyperpar[[1]]))

#' More general version covering also LCAR and sparse BYM:

inla.rgeneric.Mmodel <- 
  function (cmd = c("graph", "Q", "mu", "initial", "log.norm.const", 
                    "log.prior", "quit"), theta = NULL) {
    envir <- parent.env(environment())
    if(!exists("cache.done", envir=envir)){
      starttime.scale <- Sys.time()
      if(Qmod == "BYM"){
        #' Unscaled Laplacian matrix (marginal precision of u_1, u_2 ... u_k)
        L_unscaled <- Matrix::Diagonal(nrow(W), rowSums(W)) -  W
        L_unscaled_block <- kronecker(diag(1,k), L_unscaled)
        A_constr <- t(pracma::nullspace(as.matrix(L_unscaled_block)))
        scaleQ <- INLA:::inla.scale.model.internal(
          L_unscaled_block, constr = list(A = A_constr, e = rep(0, nrow(A_constr))))
        #' Block Laplacian, i.e. precision of U = I_k \otimes L
        n <- nrow(W)
        L <- scaleQ$Q[c(1:n), c(1:n)]
        Sigma.u <- MASS::ginv(as.matrix(L))
        endtime.scale <- Sys.time()
        cat("Time needed for scaling Laplacian matrix: ",
            round(difftime(endtime.scale, starttime.scale), 3), " seconds \n")
        assign("Sigma.u", Sigma.u, envir = envir)
      }
      assign("cache.done", TRUE, envir = envir)
    }
    interpret.theta <- function() {
      alpha <- alpha.min + (alpha.max - alpha.min)/(1 + exp(-theta[as.integer(1:k)]))
      M <- matrix(theta[-as.integer(1:k)], ncol = k)
      return(list(alpha = alpha, M = M))
    }
    graph <- function() {
      MI <- kronecker(Matrix::Matrix(1, ncol = k, nrow = k), 
                      Matrix::Diagonal(nrow(W), 1))
      IW <- Matrix::Diagonal(nrow(W), 1) + W
      BlockIW <- Matrix::bdiag(replicate(k, IW, simplify = FALSE))
      G <- (MI %*% BlockIW) %*% MI
      return(G)
    }
    Q <- function() {
      param <- interpret.theta()
      M.inv <- solve(param$M)
      MI <- kronecker(M.inv, Matrix::Diagonal(nrow(W), 1))
      D <- as.vector(apply(W, 1, sum))
      if(Qmod == "LCAR"){
        BlockIW <- Matrix::bdiag(lapply(1:k, function(i) {
          param$alpha[i]*(Matrix::Diagonal(x = D) - W) + 
            (1 - param$alpha[i]) * Matrix::Diagonal(nrow(W), 1)
        }))
      } else if (Qmod == "BYM"){
        BlockIW <- Matrix::bdiag(lapply(1:k, function(i) {
          solve(
            sqrt(param$alpha[i])*Sigma.u+
              sqrt(1-param$alpha[i]*Matrix::Diagonal(nrow(W), 1))
          )
        }))
      } else if(Qmod == "PCAR"){
        BlockIW <- Matrix::bdiag(lapply(1:k, function(i) {
          Matrix::Diagonal(x = D) - param$alpha[i] * W
        }))
      }
      
      Q <- (MI %*% BlockIW) %*% kronecker(t(M.inv), Matrix::Diagonal(nrow(W),  1))
      
      
      
      return(Q)
    }
    mu <- function() {
      return(numeric(0))
    }
    log.norm.const <- function() {
      val <- numeric(0)
      return(val)
    }
    log.prior <- function() {
      param <- interpret.theta()
      val <- sum(-theta[as.integer(1:k)] - 2 * log(1 + exp(-theta[as.integer(1:k)])))
      sigma2 <- 1000
      val = val + log(MCMCpack::dwish(W = crossprod(param$M), 
                                      v = k, S = diag(rep(sigma2, k))))
      return(val)
    }
    initial <- function() {
      return(c(rep(0, k), as.vector(diag(rep(1, k)))))
    }
    quit <- function() {
      return(invisible())
    }
    if (as.integer(R.version$major) > 3) {
      if (!length(theta)) 
        theta = initial()
    }
    else {
      if (is.null(theta)) {
        theta <- initial()
      }
    }
    val <- do.call(match.arg(cmd), args = list())
    return(val)
  }
inla.Mmodel <- function (...)    INLA::inla.rgeneric.define(inla.rgeneric.Mmodel, ...)


cav_MmodPCAR_inla <- inla(
  N_ACC ~ 1 +TEP_th + ELI + PGR + UIS + ELL + PDI + ER+ 
    f(ID, model = inla.Mmodel(k = 3, W = W_con, 
                              alpha.min = 0, alpha.max = 1, Qmod = "PCAR")),
  offset = log(nn),
  family = rep("poisson", 3), data =dd_list,
  #control.fixed = list(prec = list(Intercept1 = 0, Intercept2 = 0, Intercept3 = 0)),
  #inla.mode = "classic", control.inla = list(strategy = "laplace", int.strategy = "grid"),
  num.threads = 1, control.compute = list(internal.opt = F, cpo = T,  waic = T, config = T), 
  verbose = T)


## TBD: Spatiotemporal attempt -------------------------------------------------

dd_list_st <- dd_list

names(dd_list_st)[which(names(dd_list_st)=="ID")] <- "Area"

dd_list_st$Area <- (dd_list_st$Area - 1)%%nrow(dd_con) + 1
dd_list_st$Year <- rep(c(1,2,3), each = nrow(dd_con))

dd_list_st$ID_bis <- dd_list_st$Area
dd_list_st$Year_bis <- dd_list_st$Year
dd_list_st$ID <- dd_list_st$Area + (dd_list_st$Year-1)*nrow(dd_con)

#' Null model: no interaction, only a temporal effect
cav_STbym_null_INLA <- inla(N_ACC ~ 1 +TEP_th + ELI + PGR + UIS + ELL + PDI + ER +
                       f(Area, model = "bym2", graph = W_con,  scale.model = T, 
                         hyper = list(prec = list(prior = "pc.prec", param = c(1.5, 0.01))))+
                         f(Year, model = "iid"),
                     family = c("poisson", "poisson", "poisson"), offset = log(nn), data =dd_list_st,
                     num.threads = 1, control.compute = 
                       list(internal.opt = F, cpo = T, waic = T), 
                     #inla.mode = "classic", control.inla = list(strategy = "laplace", int.strategy = "grid"),
                     control.predictor = list(compute = T),
                     verbose = T)

#' Works bad, but this makes sense since a unique spatial field is used for all years

#' Next: type-1 interaction:

cav_STbym_i1_INLA <- inla(N_ACC ~ 1 +TEP_th + ELI + PGR + UIS + ELL + PDI + ER +
                            f(Area, model = "bym2", graph = W_con,  scale.model = T, 
                              hyper = list(prec = list(prior = "pc.prec", param = c(1.5, 0.01))))+
                            #f(Year, model = "iid") + 
                            f(ID, model = "iid",
                              hyper = list(prec = list(prior = "pc.prec", param = c(1.5, 0.01)))),
                            family = c("poisson", "poisson", "poisson"), offset = log(nn), data =dd_list_st,
                            num.threads = 1, control.compute = 
                              list(internal.opt = F, cpo = T, waic = T), 
                            #inla.mode = "classic", control.inla = list(strategy = "laplace", int.strategy = "grid"),
                            control.predictor = list(compute = T),
                            verbose = T)

cav_STbym_i3_INLA <- inla(N_ACC ~ 1 +TEP_th + ELI + PGR + UIS + ELL + PDI + ER +
                            f(Area, model = "bym2", graph = W_con,  scale.model = T, 
                              hyper = list(prec = list(prior = "pc.prec", param = c(1.5, 0.01))))+
                            #f(Year, model = "iid", hyper = list(prec = list(prior = "pc.prec", param = c(1.5, 0.01)))) + 
                            f(Year_bis, model="iid", group=Area,
                              control.group=list(model="besag", graph=W_con),
                              hyper = list(prec = list(prior = "pc.prec", param = c(1.5, 0.01)))),
                          family = c("poisson", "poisson", "poisson"), offset = log(nn), data =dd_list_st,
                          num.threads = 1, control.compute = 
                            list(internal.opt = F, cpo = T, waic = T), 
                          #inla.mode = "classic", control.inla = list(strategy = "laplace", int.strategy = "grid"),
                          control.predictor = list(compute = T),
                          verbose = T)




## Some diagnostics (taken from markdown) --------------------------------------

WAICS <- tibble::tibble(
  Model = c("Null", "ICAR", "PCAR", "Leroux", "ST_I"),
  WAIC = round(c(
    cav_nosp_inla$waic$waic,  cav_IMCAR_inla$waic$waic,  cav_PMCAR_inla$waic$waic, 
    cav_MLCAR_inla$waic$waic, cav_STbym_i1_INLA$waic$waic),3),
  Eff_params = round(c(
    cav_nosp_inla$waic$p.eff,   cav_IMCAR_inla$waic$p.eff,  cav_PMCAR_inla$waic$p.eff,
    cav_MLCAR_inla$waic$p.eff, cav_STbym_i1_INLA$waic$p.eff),3))

WAICS




zhat_plot(cav_MLCAR_inla, main = "Leroux model")


## Obsolete: covariates choice -------------------------------------------------


# Plot of covariates correlations:
ggplot2::ggplot(data = reshape2::melt(cor(X[,-1]))) +
  ggplot2::geom_tile(ggplot2::aes(
    x = .data$Var2, y = .data$Var1, fill = .data$value), color = "black") +
  ggplot2::geom_text(ggplot2::aes(
    x = .data$Var2, y = .data$Var1, label = round(.data$value, 2))) +
  ggplot2::scale_fill_gradient2(
    low = "blue", mid = "white", high = "red", midpoint = 0) +
  ggplot2::theme_minimal()


# Forward selection attempt: USING THE BIC AS SELECTION CRITERION,
# two covariates are necessary
covariates <- colnames(X)[-c(1,2, which(colnames(X)=="AES"))]
covariates[1] <- "TEP_th"

cov_selector <- function(year){
  
  if(year == 2021){
    dd_con <- dd_con %>% 
      dplyr::rename(nn = .data$nn21, N_ACC = .data$N_ACC_21, TEP_th = .data$TEP_th_22)
  } 
  
  if(year == 2022){
    dd_con <- dd_con %>% 
      dplyr::rename(nn = .data$nn22, N_ACC = .data$N_ACC_22, TEP_th = .data$TEP_th_22)
  }
  
  if(year == 2023){
    dd_con <- dd_con %>% 
      dplyr::rename(nn = .data$nn23, N_ACC = .data$N_ACC_23, TEP_th = .data$TEP_th_23)
  }
  
  covs.in <- c()
  BIC.min <- c()
  
  while(length(covs.in) < length(covariates)){
    covs.out <- covariates[which(!covariates %in% covs.in)]
    
    BICs <- c()
    for(j in c(1:length(covs.out))) {
      formula.temp <- paste0("N_ACC ~ 1 + offset(log(nn)) +", 
                             paste(covs.in, collapse = "+"),
                             "+", covs.out[j])
      mod.tmp <- glm(formula.temp, data = dd_con, family = "poisson")
      BICs[j] <- stats::BIC(mod.tmp)
    }
    BIC.min <- c(BIC.min, min(BICs))
    if(length(BIC.min)>1 && BIC.min[length(BIC.min)] >= BIC.min[length(BIC.min)-1]){
      BIC.min <- BIC.min[c(1:which.min(BIC.min))]
      cat("Adding covariates is not necessary anymore \n")
      break
    } else{
      covs.in <- c(covs.in, covs.out[which.min(BICs)])
    }
  }
  
  return(list(BIC = BIC.min, covs = covs.in))
}

#' Three different sets.
cov_selector(2021)
cov_selector(2022)
cov_selector(2023)


#T_dist_ls <- compindexR::calc_compindex(
#as.matrix(sf::st_drop_geometry(dd_con)[,c("TEP_th", "AES")]))

#'    -------------------------------------------------------------------------#
#'    
#'    Problem: for each year, the optimal covariate changes.
#'    How to interpret this? 
#'    How to solve this?
#'  
#'    -------------------------------------------------------------------------#
