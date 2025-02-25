#' ---------  Analisi CAV Puglia 2022 -----------------------------------------#
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
  nn = c(dd_con$nn21, dd_con$nn22, dd_con$nn23)) 

#'  May be useful?
#'  dataframe input obejct ==> same regression coefficients
#'  for all years (not so trustable indeed)

dd_long <- do.call(rbind, rep(list(
  dplyr::select(sf::st_drop_geometry(dd_con), 
                colnames(X)[-c(1:3)])),3) )  %>% 
  dplyr::mutate(N_ACC = c(dd_con$N_ACC_21, dd_con$N_ACC_22, dd_con$N_ACC_23),
                TEP_th = c(dd_con$TEP_th_22, dd_con$TEP_th_22, dd_con$TEP_th_23),
                nn = c(dd_con$nn21, dd_con$nn22, dd_con$nn23),
                Intercept = rep(1, nrow(.)),
                ID = rep(dd_con$ID, 3), 
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

cav_nosp_inla <- inla(
  N_ACC ~ 0 + Intercept +TEP_th + ELI + PGR + UIS + ELL + PDI + ER,
  offset = log(nn),
  family = rep("poisson", 3), data =dd_list,
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
      A = kronecker(diag(1,3), matrix(1, nrow = 1, ncol = n)), e = c(0,0,0))),
  offset = log(nn),
  family = rep("poisson", 3), data =dd_list,
  #inla.mode = "classic", control.inla = list(strategy = "laplace", int.strategy = "grid"),
  num.threads = 1, control.compute = list(internal.opt = F, cpo = T, waic = T, config = T), 
  verbose = T)

#' Warning: only works with unique intercept for
#' all years.



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
  num.threads = 1, control.compute = list(internal.opt = F, cpo = T, waic = T, config = T), 
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


#' Leroux model, tentative manual definition:

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


inla.rgeneric.MBYM.dense <- 
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
      L.unscaled <- Matrix::Diagonal(nrow(W), apply(W, 1, sum)) -  W
      #' Constraint on the Laplacian matrix
      A.mat <- t(pracma::nullspace(as.matrix(L.unscaled)))
      #' Scaled Laplacian matrix, the actual precision of the ICAR field
      L <- INLA::inla.scale.model(L.unscaled, constr = list(A = A.mat, e = rep(0, nrow(A.mat))))
      Sigma.u <- MASS::ginv(as.matrix(L))
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
      #' PC prior implementation
      dpc <- INLA:::inla.pc.bym.phi(graph = W, u = 0.5, alpha = 2/3)
      val <- dpc(param$phi) - theta[1L] - 2 * log(1 + exp(-theta[1L]))
      #' Uniform prior
      #val <- -theta[1L] - 2 * log(1 + exp(-theta[1L]))
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
      if(!is.null(init)){
        return(init)
      } else{
        return(c(0, rep(0, k), rep(0, (k * (k - 1)/2))))
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

inla.MBYM.dense <- function(...) INLA::inla.rgeneric.define(inla.rgeneric.MBYM.dense, ...)

#' Warning: using a 'prudential' initial value for logit(phi)
#' can make this already slow model even slower; still, we want to rule out overestimation.

cav_MBYM_inla_phiInit <- inla(
  N_ACC ~ 1 +TEP_th + ELI + PGR + UIS + ELL + PDI + ER+ 
    f(ID, model = inla.MBYM.dense(k = 3, W = W_con, init = c(-3, rep(0, 3), c(0.1, 0, 0)))),
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




## M-Model PCAR attempt --------------------------------------------------------


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
