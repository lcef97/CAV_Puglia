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

load("input/CAV_input_mun_2022.RData")

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
Popolazione_Puglia_2022 <- readr::read_csv("input/Popolazione_Puglia_2022.csv") %>% 
  dplyr::select(.data$ITTER107, .data$Territorio, .data$SEXISTAT1, .data$ETA1, .data$Value) %>% 
  dplyr::filter(.data$ETA1 != "TOTAL") %>% 
  dplyr::mutate(ETA1 = nn_extract(ETA1)) %>% 
  dplyr::mutate(ITTER107 = as.numeric(ITTER107))
names(Popolazione_Puglia_2022) <- c("PRO_COM", "Comune", "Sesso", "Eta", "Popolazione")

# Filter and aggregate:
Pop_f_2022 <- Popolazione_Puglia_2022 %>% dplyr::filter(.data$Sesso == 2) %>% 
  dplyr::filter(.data$Eta > 14) %>% 
  dplyr::group_by(.data$PRO_COM, .data$Comune) %>% 
  dplyr::summarise(nn = sum(.data$Popolazione)) %>% dplyr::ungroup()

# Complete dataset:
dd <- Shp %>% dplyr::left_join(Pop_f_2022[,c(1,3)],
                               by = "PRO_COM") %>% 
  dplyr::left_join(dplyr::select(CAV_mun_22, -.data$comune),by = "PRO_COM") %>% 
  dplyr::left_join(dplyr::select(Indicators, -.data$Comune), by = "PRO_COM")

# Municipalities from which no woman reported violence --> count == 0
dd$N_ACC[is.na(dd$N_ACC)] <- 0 
# "access ratio"
dd$F_ACC <- dd$N_ACC/dd$nn



## Mapping municipalities to support centers ---------------------------------



# Tremiti Islands are a singleton --> need to remove them to perform spatial analysis
suppressWarnings({
  singletons <- which(unlist(lapply(spdep::poly2nb(dd), function(x) x[1L] == 0)))
})



#'   Do not delete: code to find minimum distances ----------------------------#
#'                                                
#'   Since the loop takes some minutes to work, we save the
#'   output object dists_th outside:   
#'                                           
#'   load("input/dist_short.RData") 
#'   
#'   Municipalities hosting a support center:
#'   
#'   Includes Capurso, not Triggiano.
#'   
#'   munWcav_22 <- c (71020,71024,71051,72004,72006,72011,72014,
#'                    72019,72021,72029,72031,72033,72035,73013,
#'                    73027,74001,74009,75018,75029,75035,75059,
#'                    110001,110002,110009)                                              
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
#'  And this is how file dists_th_22 has been created.
#'
#' -----------------------------------------------------------------------------

load("input/dists_th_22.RData")

# This is the dataset we will concretely work on.
# Covariates are all scaled to zero mean and unit variance
dd_con <- dd[-singletons, ] %>% 
  dplyr::left_join(dists_th_22, by = "PRO_COM") %>% 
  dplyr::mutate(TEP_th_22 = as.vector(scale(.data$TEP_th_22))) %>% 
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
n <- nrow(W_con)
#' Laplacian matrix:
#' May be useful for spectral analysis. Or may be not.
Lapl_con <- diag(rowSums(W_con)) - W_con
V_con <- eigen(Lapl_con)$vectors


# row ID - needed for spatial models
dd_con$ID <- c(1:nrow(dd_con))

# Full GLM --> for model matrix
glm_all_X <- glm(N_ACC ~ 1 + TEP_th_22 + MFI + AES + PDI + ELL + ER +
                   PGR + UIS + ELI + offset(log(nn)),
                 data = dd_con, family = "poisson")
# model matrix
X <- model.matrix(glm_all_X)

## nonspatial regression -------------------------------------------------------


# Plot of covariates correlations:
ggplot2::ggplot(data = reshape2::melt(cor(X[,-1]))) +
  ggplot2::geom_tile(ggplot2::aes(
    x = .data$Var2, y = .data$Var1, fill = .data$value), color = "black") +
  ggplot2::geom_text(ggplot2::aes(
    x = .data$Var2, y = .data$Var1, label = round(.data$value, 2))) +
  ggplot2::scale_fill_gradient2(
    low = "blue", mid = "white", high = "red", midpoint = 0) +
  ggplot2::theme_minimal()


#'  Forward selection attempt: USING THE BIC AS SELECTION CRITERION,
#'  four covariates are necessary.
#'  
#'  We drop AES, which is highly correlated with TEP_th_22, and MFI, 
#'  which is (the quantile of) a synthetic indicator
#'  summing up all other variables other than TEP_th_22.  

covariates <- colnames(X)[-c(1, which(colnames(X) %in% c("AES", "MFI")))]
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
  #if(length(BIC.min)>1 && BIC.min[length(BIC.min)] >= BIC.min[length(BIC.min)-1]){
  #  break
  #} else{
  covs.in <- c(covs.in, covs.out[which.min(BICs)])
  #}
}
#'  Only two, yet quite correlated.
covs.in[c(1:which.min(BIC.min))]
#'  We only consider the distance from closest CAV
#'  Also because AES is another distance indicator. 
#'  
#'  ---------------------------------------------------------------------------#  
#'                                  GLM here:                                  #
#'  
#'  
#' 


cav_glm <- glm(N_ACC ~ 1 +TEP_th_22 + ELI + PGR + 
                 UIS + ELL + PDI + ER, family = "poisson",
               offset = log(nn), data = dd_con)

#' Interaction between economic variables
cav_glm_EI <- glm(N_ACC ~ 1 +TEP_th_22 + ELI + PGR + 
                 UIS + ELL + PDI + ER + ELL*ER, family = "poisson",
               offset = log(nn), data = dd_con)
#' Not particularly interesting


#' For the ZIP regression we are going to need
#' a dedicated package, e.g. `pscl`:

if(!rlang::is_installed("pscl")) install.packages("pscl")

#' Simplest way: no explanatory variable for $\pi_0$:
#' 
cav_zip <- pscl::zeroinfl(N_ACC ~ 1 +TEP_th_22 + ELI + PGR +
                            UIS + ELL + PDI + ER | 1, dist = "poisson",
               link = "log", offset = log(nn), data = dd_con)

summary(cav_glm)

summary(cav_zip)

#' Crude, additive MSE. Don't even know if it's correct.
#' No noticeable changes.
#' 
var(dd_con$N_ACC - cav_glm$fitted.values) # Slightly lowerr
var(dd_con$N_ACC - cav_zip$fitted.values) # Slightly higher


## Spatial frequentist Poisson regression: spaMM -------------------------------

cav_car <- spaMM::fitme(N_ACC ~ 1 +TEP_th_22 + ELI + PGR + UIS + ELL + PDI + ER+
                          adjacency(1|PRO_COM) +  offset(log(nn)), 
                 adjMatrix = W_con,
                 data = dd_con, family = 'poisson')
summary(cav_car)
# rho close to zero - likely iid residuals


## Spatial frequentist Poisson regression: thin plate splines -----------------#

#' Extract centroids from areal geometries
dd_ctr <- dd_con
sf::st_agr(dd_ctr) <- "constant"
dd_ctr <- sf::st_point_on_surface(dd_ctr)
dd_ctr$lat <- sf::st_coordinates(dd_ctr)[,2]
dd_ctr$long <- sf::st_coordinates(dd_ctr)[,1]


cav_gam_TPS <- mgcv::gam(N_ACC ~ 1 +TEP_th_22 + ELI + PGR + UIS + ELL + PDI + ER + 
                           s(long, lat, bs="tp", m=2),
                         family = "poisson", offset = log(nn),
                         data = dd_ctr)

summary(cav_gam_TPS)
mgcv::gam.check(cav_gam_TPS)

# Thin plate spline model --> too complex?
stats::BIC(cav_gam_TPS)
stats::BIC(cav_glm)

#' TPS regression shrinks covariates effects estimation.
#' May this be the case for spatial+ application? Let's find out.

TEP_TPS <- mgcv::gam(TEP_th_22 ~ 1 + s(long, lat, bs="tp", m=2),
                     data = dd_ctr)$fitted.values
PGR_TPS <- mgcv::gam(PGR ~ 1 + s(long, lat, bs="tp", m=2),
                     data = dd_ctr)$fitted.values
UIS_TPS <- mgcv::gam(UIS ~ 1 + s(long, lat, bs="tp", m=2),
                     data = dd_ctr)$fitted.values
ELI_TPS <- mgcv::gam(PGR ~ 1 + s(long, lat, bs="tp", m=2),
                     data = dd_ctr)$fitted.values

dd_ctr_nosp <- dd_ctr %>% 
  dplyr::mutate(TEP_th_22 = dd_ctr$TEP_th_22 - TEP_TPS) %>% 
  dplyr::mutate(PGR = dd_ctr$PGR - PGR_TPS) %>% 
  dplyr::mutate(UIS = dd_ctr$UIS - UIS_TPS) %>% 
  dplyr::mutate(ELI = dd_ctr$ELI - ELI_TPS)
  
cav_gam_TPS_spatplus <- mgcv::gam(N_ACC ~ 1 + TEP_th_22 + UIS + ELI + PGR + 
                           s(long, lat, bs="tp", m=2),
                         family = "poisson", offset = log(nn),
                         data = dd_ctr_nosp)

summary(cav_gam_TPS_spatplus)
mgcv::gam.check(cav_gam_TPS_spatplus)

#' Barely changing

## Spatial regression: INLA ----------------------------------------------------

#' PCAR code taken from INLAMSM (https://github.com/becarioprecario/INLAMSM/tree/master/R)
#' since INLA has no built-in one - only ICAR, BYM, Leroux and BYM2

library(INLA)
inla.rgeneric.PCAR.model <- function (cmd = c("graph", "Q", "mu", "initial", "log.norm.const", 
                                              "log.prior", "quit"), theta = NULL) {
  interpret.theta <- function() {
    alpha <- 1/(1 + exp(-theta[1L]))
    mprec <- sapply(theta[2L], function(x) {
      exp(x)
    })
    PREC <- mprec
    return(list(alpha = alpha, mprec = mprec, PREC = PREC))
  }
  graph <- function() {
    G <- Matrix::Diagonal(nrow(W), 1) + W
    return(G)
  }
  Q <- function() {
    param <- interpret.theta()
    Q <- param$PREC * 
      (Matrix::Diagonal(nrow(W), apply(W, 1, sum)) - param$alpha * W)
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
    val <- -theta[1L] - 2 * log(1 + exp(-theta[1L]))
    # # PC prior
    val <- val + log(lambda/2) - theta[2L]/2 - (lambda * exp(-theta[2L]/2))
    # # Gamma(1, 5e-5), default prior:
    #val <- val + dgamma(exp(theta[2L]), shape = 1, rate = 5e-5, log = T) + theta[2L]
    # # Uniform prior on the standard deviation
    #val <- val - sum(theta[2L])/2 - k * log(2)
    return(val)
  }
  initial <- function() {
      return(init)
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
      cat("Init = ", init, "\n")
      cat("Actual initial values", initial(), "\n")
      theta <- initial()
    }
  }
  val <- do.call(match.arg(cmd), args = list())
  return(val)
}
PCAR.model <- function(...) INLA::inla.rgeneric.define(inla.rgeneric.PCAR.model, ...)

#' Comments and analyses can be found in the markdown file

# Replicates the glm quite well
m_0_INLA <- inla(N_ACC ~ 1 +TEP_th_22 + ELI + PGR + UIS + ELL + PDI + ER,
                 family = "poisson",  data =dd_con, offset = log(nn),
                 num.threads = 1, control.compute = 
                   list(internal.opt = F, cpo = T, waic = T), 
                 inla.mode = "classic", control.inla = list(strategy = "laplace", int.strategy = "grid"))

# ICAR model
cav_icar_INLA <- inla(N_ACC ~ 1 +TEP_th_22 + ELI + PGR + UIS + ELL + PDI + ER +
                        f(ID, model = "besag", graph = W_con, scale.model = T, 
                          hyper = list(prec = list(prior = "pc.prec", param = c(1.5, 0.01)))),
                         family = "poisson", offset = log(nn), data =dd_con,
                         num.threads = 1, control.compute = 
                           list(internal.opt = F, cpo = T, waic = T), 
                         #inla.mode = "classic", control.inla = list(strategy = "laplace", int.strategy ="grid"),
                         control.predictor = list(compute = T),
                         verbose = T) # better

# PCAR model
cav_pcar_INLA <- inla(N_ACC ~ 1 +TEP_th_22 + ELI + PGR + UIS + ELL + PDI + ER +
                        f(ID, model = PCAR.model(W = W_con, k = 1, lambda = log(100)/1.5, init = c(0, 4))),
                         family = "poisson", offset = log(nn), data =dd_con,
                         num.threads = 1, control.compute = 
                           list(internal.opt = F, cpo = T, waic = T), 
                         #inla.mode = "classic", control.inla = list(strategy = "laplace", int.strategy  = "grid"),
                         control.predictor = list(compute = T),
                         verbose = T) 

# BYM model
cav_bym_INLA <- inla(N_ACC ~ 1 +TEP_th_22 + ELI + PGR + UIS + ELL + PDI + ER +
                       f(ID, model = "bym2", graph = W_con,  scale.model = T, 
                         hyper = list(prec = list(prior = "pc.prec", param = c(1.5, 0.01)))),
                      family = "poisson", offset = log(nn), data =dd_con,
                      num.threads = 1, control.compute = 
                        list(internal.opt = F, cpo = T, waic = T), 
                      #inla.mode = "classic", control.inla = list(strategy = "laplace", int.strategy = "grid"),
                      control.predictor = list(compute = T),
                      verbose = T) # better

# Leroux model - sparse precision
cav_leroux_INLA <- inla(N_ACC ~ 1 +TEP_th_22 + ELI + PGR + UIS + ELL + PDI + ER +
                          f(ID, model = "besagproper2", graph = W_con, 
                            hyper = list(prec = list(prior = "pc.prec", param = c(1.5, 0.01)))),
                     family = "poisson", offset = log(nn), data =dd_con,
                     num.threads = 1, control.compute = 
                       list(internal.opt = F, cpo = T, waic = T), 
                     #inla.mode = "classic", control.inla = list(strategy = "laplace", int.strategy ="grid"),
                     control.predictor = list(compute = T),
                     verbose = T) # better

cav_icar_INLA$waic$waic # 962 --> poor fitting
cav_pcar_INLA$waic$waic # 955 --> better
cav_bym_INLA$waic$waic # 950 --> best one
cav_leroux_INLA$waic$waic # 951 --> second-best


summary(cav_glm)
summary(cav_bym_INLA)
summary(cav_leroux_INLA)

#' Expected values of regression coefficients
#' are quite similar. 
#' Variance increases.

#' Let us try removing spatial patterns:

deconfound <- function(X, Lapl = Lapl_con, n.eigen.out, rescale = T){
  if(!is.matrix(X)) {
    as.vectorX <- TRUE
    X <- as.matrix(X)
  } else {
      as.vectorX <- FALSE
      }
  V <- eigen(Lapl)$vectors
  rk <- Matrix::rankMatrix(Lapl)
  coef <- solve(V, X)
  eigen.in <- c(1:(rk-n.eigen.out), c((rk+1):ncol(V)))
  X_nosp <- V[, eigen.in] %*% coef[eigen.in, ]
  if(rescale) X_nosp <- scale(X_nosp)
  if(as.vectorX) X_nosp <- as.vector(X_nosp)
  return(X_nosp)
}


dd_con_nosp <- dd_con %>%
  dplyr::mutate(ELI  = deconfound(.data$ELI, n.eigen.out=13)) %>% 
  dplyr::mutate(PGR  = deconfound(.data$PGR, n.eigen.out=13)) %>% 
  dplyr::mutate(UIS  = deconfound(.data$UIS, n.eigen.out=13)) %>% 
  dplyr::mutate(ELL  = deconfound(.data$ELL, n.eigen.out=13)) %>% 
  dplyr::mutate(PDI  = deconfound(.data$PDI, n.eigen.out=13)) %>% 
  dplyr::mutate(ER  = deconfound(.data$ER, n.eigen.out=13))
  

cav_bym_INLA_spatplus <- inla(N_ACC ~ 1 +TEP_th_22 + ELI + PGR + UIS + ELL + PDI + ER +
                       f(ID, model = "bym2", graph = W_con,  scale.model = T, 
                         hyper = list(prec = list(prior = "pc.prec", param = c(1.5, 0.01)))),
                     family = "poisson", offset = log(nn), data = dd_con_nosp,
                     num.threads = 1, control.compute = 
                       list(internal.opt = F, cpo = T, waic = T), 
                     #inla.mode = "classic", control.inla = list(strategy = "laplace", strategy = "grid"),
                     control.predictor = list(compute = T),
                     verbose = T)



summary(cav_bym_INLA_spatplus) # Quite close



cav_icar_INLA_spatplus <- inla(N_ACC ~ 1 +TEP_th_22 + ELI + PGR + UIS + ELL + PDI + ER +
                                f(ID, model = "besag", graph = W_con,  scale.model = T, 
                                  hyper = list(prec = list(prior = "pc.prec", param = c(1.5, 0.01)))),
                              family = "poisson", offset = log(nn), data = dd_con_nosp,
                              num.threads = 1, control.compute = 
                                list(internal.opt = F, cpo = T, waic = T), 
                              #inla.mode = "classic", control.inla = list(strategy = "laplace"),
                              control.predictor = list(compute = T),
                              verbose = T)


#' Finally, let us try with a different likelihood, i.e. the ZIP

cav_bym_zip_INLA <- inla(N_ACC ~ 1 +TEP_th_22 + ELI + PGR + UIS + ELL + PDI + ER+
                       f(ID, model = "bym2", graph = W_con,  scale.model = T, 
                         hyper = list(prec = list(prior = "pc.prec", param = c(1.5, 0.01)))),
                     family = "zeroinflatedpoisson1", offset = log(nn), data =dd_con,
                     num.threads = 1, control.compute = 
                       list(internal.opt = F, cpo = T, waic = T), 
                     #inla.mode = "classic", control.inla = list(strategy = "laplace"),
                     control.predictor = list(compute = T),
                     verbose = T) 

summary(cav_bym_zip_INLA)
#' Interesting result. The zero-probability
#' is very low, yet compliance to regularity conditions to approximate
#' the CPOs improves greatly.


## Spatial regression, BYM: INLA  ----------------------------------------------

#' -----------------------------------------------------------------------------
#' 
#' Sparse parametrisation of the BYM, should replicate the ready-made INLA function.

inla.rgeneric.BYM.sparse <-   function (cmd = c("graph", "Q", "mu", "initial", "log.norm.const", 
                                                "log.prior", "quit"), theta = NULL) {
  
  envir <- parent.env(environment())
  if(!exists("cache.done", envir=envir)){
    starttime.scale <- Sys.time()
    #' Unscaled Laplacian matrix (marginal precision of u_1, u_2 ... u_k)
    L_unscaled <- Matrix::Diagonal(nrow(W), rowSums(W)) -  W
    A_constr <- t(pracma::nullspace(as.matrix(L_unscaled)))
    scaleQ <- INLA:::inla.scale.model.internal(
      L_unscaled, constr = list(A = A_constr, e = rep(0, nrow(A_constr))))
    #n <- nrow(W)
    L <- scaleQ$Q
    endtime.scale <- Sys.time()
    cat("Time needed for scaling Laplacian matrix: ",
        round(difftime(endtime.scale, starttime.scale), 3), " seconds \n")
    #' Eigenvalues of the SCALED Laplacian, sufficient for trace and determinant entering the KLD
    L_eigen_scaled <- eigen(L)$values
    #' PC prior on mixing parameter - definition should be fine.
    log.dpc.phi.bym <- INLA:::inla.pc.bym.phi(eigenvalues = L_eigen_scaled, 
                                              marginal.variances = scaleQ$var,
                                              rankdef = nrow(A_constr),
                                              u = 0.5, alpha = 2/3)
    assign("L", L, envir = envir)
    assign("log.dpc.phi.bym", log.dpc.phi.bym, envir = envir)
    assign("cache.done", TRUE, envir = envir)
  }
  
  #' Standard: mapping parameters from internal scale
  interpret.theta <- function() {
    #' Mixing parameter, defined on [0, 1]: expit transform
    phi <-  1/(1 + exp(-theta[1L]))
    #' Precision parameter, internally modelled on the log scale
    tau <- exp(theta[2L])
    return(list(phi = phi, tau = tau))
  }
  graph <- function() {
    G <- kronecker(matrix(1, nrow = 2, ncol = 2), Matrix::Diagonal(nrow(W), 1) + W)
    return(G)
  }
  #' Precision matrix. I use the BYM2 parametrisation
  #' to have sparse precision. 
  #' Problem: Riebler et al. define a random vector of size 2n,
  #' i.e. (z' u')', where z is the overall effect and u is the ICAR component
  Q <- function() {
    param <- interpret.theta()
    #' Blocks of the sparse precision matrix, as in Riebler et al. (2016)
    Q11 <-  param$tau/(1-param$phi) * Matrix::Diagonal(n=nrow(W), x=1)
    Q12 <- - sqrt(param$phi * param$tau)/(1 - param$phi) * Matrix::Diagonal(n = nrow(W), x = 1)
    Q21 <- Q12
    Q22 <- L + (param$phi/(1-param$phi))*Matrix::Diagonal(n=nrow(W), x=1)
    #' Structure matrix. Problem: it is of size 2n * 2n
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
    val <- log.dpc.phi.bym(param$phi) - theta[1L] - 2 * log(1 + exp(-theta[1L]))
    #val <- dnorm(theta[1L], mean = 0.5, sd = sqrt(2), log = TRUE)
    #' PC prior on precision; term +theta[2L] needed for change of variable
    val <- val + INLA::inla.pc.dprec(prec = param$tau, lambda = 1, log = TRUE) + theta[2L]
    return(val)
  }
  #' As in the built-in model
  initial <- function() {
    return(c(-3, 4))
  }
  quit <- function() {
    return(invisible())
  }
  #' Initialise hyperparameters
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
BYM.sparse <- function(...) INLA::inla.rgeneric.define(inla.rgeneric.BYM.sparse, ...)

#' built-in version
cav_bym_INLA <- inla(N_ACC ~ 1 +TEP_th_22 + ELI + PGR + UIS + ELL + PDI + ER +
                       f(ID, model = "bym2", graph = W_con,  scale.model = T, 
                         hyper = list(prec = list(prior = "pc.prec", param = c(1.5, 0.01)))),
                     family = "poisson", offset = log(nn), data =dd_con,
                     num.threads = 1, control.compute = 
                       list(internal.opt = F, cpo = T, waic = T), 
                     #inla.mode = "classic", control.inla = list(strategy = "laplace", int.strategy = "grid"),
                     control.predictor = list(compute = T),
                     verbose = T) 


##' manually made BYM model

constr <- INLA:::inla.bym.constr.internal(Lapl_con, adjust.for.con.comp = TRUE)
constr.BYM <- list(A = cbind(Matrix::Matrix(0, nrow = nrow(constr$constr$A), ncol(constr$constr$A)),
                             constr$constr$A),
                   e = constr$constr$e)

cav_bym_INLA_man <- inla(N_ACC ~ 1 +TEP_th_22 + ELI + PGR + UIS + ELL + PDI + ER +
                        f(ID, model = BYM.sparse(W = W_con),
                          extraconstr = constr.BYM
                          #, A.local = cbind(Matrix::Diagonal(n, 1), matrix(0, nrow = n, ncol = n))
                          ),
                      family = "poisson", offset = log(nn), data =dd_con,
                      num.threads = 1, control.compute = 
                        list(internal.opt = F, cpo = T, waic = T), 
                      #inla.mode = "classic", control.inla = list(strategy = "laplace", int.strategy  = "grid"),
                      control.predictor = list(compute = T),
                      verbose = T) 
#' mixing: quite low
inla.zmarginal(inla.tmarginal(function(X) {exp(X)/(1+exp(X))},
                              marginal = cav_bym_INLA_man$marginals.hyperpar[[1]]))


#' precision (perhaps the variance is more interesting?)
#' ... too high standard deviation, perhaps something went wrong.
inla.zmarginal(inla.tmarginal(function(X) {exp(X) },
                              marginal = cav_bym_INLA_man$marginals.hyperpar[[1]]))




#plot( as.vector(cav_bym_INLA_man$summary.linear.predictor$mean) - 
#  as.vector(cav_bym_INLA_man$model.matrix %*% cav_bym_INLA_man$summary.fixed$mean + 
#            cav_bym_INLA_man$summary.random$ID$mean[c(1:n)] +
#            cav_bym_INLA_man$offset.linear.predictor),
#  as.vector(cav_bym_INLA_man$summary.random$ID$mean[-c(1:n)]) )


## Spatial pogit regression: INLABRU -------------------------------------------

#' Coding strictly follows the work of Wøllo (2022)
#' See: https://github.com/saraew/Prosjektoppgave

#' For later; regarding under-reporting, try 
#' browsing this webpage: https://www.istat.it/comunicato-stampa/le-case-rifugio-e-le-strutture-residenziali-non-specializzate-per-le-vittime-di-violenza-anno-2023/
library(inlabru)
logexpit <- function(v1, v2, beta0, beta1, beta2){
  pred = beta0 + beta1 * v1 + beta2 * v2
  return(-log(1+exp(-pred)))
}

bru_options_set(bru_max_iter = 30, bru_verbose = TRUE,
                control.inla = list(tolerance = 1e-10))

cmp_spatial <- function(spatial_expr) {
  f2 <- substitute(spatial_expr)
  f1 <- bquote(
    ~ alpha_0(1) +  
      beta_0(main = 1, model = "linear",
             mean.linear = -2.2,
             prec.linear = 1e+2) +
      myoffset(log(nn), model = "offset") +
      alpha_ELI(ELI) + alpha_PGR(PGR) + alpha_UIS(UIS) + 
      alpha_PDI(PDI) + alpha_ER(ER) +
      beta_TEP(main = 1, model = "linear",
               mean.linear = 0,
               prec.linear = 1e-3) + 
      beta_ELL(main = 1, model = "linear",
               mean.linear = 0,
               prec.linear = 1e-3))
  
  ff <- as.call(c(quote(`+`), f1[[2]], f2))
  final_formula <- as.formula(bquote(~ .(ff)))
  return(final_formula)
}


cav_bru_basic <- function(model_cmp, verbose = FALSE) {
  terms <- c("alpha_0", "myoffset", "alpha_ELI", "alpha_PGR", "alpha_UIS",
             "alpha_PDI", "alpha_ER",
             paste0("logexpit(v1 = TEP_th_22, v2 = ELL,",
                    "beta0 = beta_0, beta1 = beta_TEP, beta2 = beta_ELL)"))
  
  if (any(grepl("spatial", as.character(model_cmp)))) terms <- c(terms, "spatial")
  
  formula <- as.formula(paste("N_ACC ~", paste(terms, collapse = " + ")))
  
  res <- inlabru::bru(
    components = model_cmp,
    lik = like("poisson", formula = formula,data = dd_con),
    options = list(verbose = verbose, num.threads = 1,
                   control.compute = list(
                     waic = T, cpo = T, dic = T, internal.opt = F)))
  
  return(res)
}


cmp_icar <- cmp_spatial(spatial(ID, model = "besag", graph = W_con, scale.model = TRUE, 
                                hyper = list(prec = list(prior = "pc.prec", param = c(1.5, 0.01)))))

cmp_pcar <- cmp_spatial(spatial(ID, model = PCAR.model(W = W_con, k = 1,
                                                       lambda = log(100)/1.5, init = c(0, 4))))

cmp_lcar <- cmp_spatial(spatial(ID, model = "besagproper", graph = W_con, 
                               hyper = list(prec = list(prior = "pc.prec", param = c(1.5, 0.01)))))

cmp_bym <- cmp_spatial(spatial(ID, model="bym2", graph = W_con, scale.model = TRUE, 
                               hyper = list(prec = list(prior = "pc.prec", param = c(1.5, 0.01)))))




#' Null model
cav_nosp_inlabru <- cav_bru_basic(cmp_spatial(NULL))

#' icar --> simplest, yet not satisfying
cav_icar_inlabru <- cav_bru_basic(cmp_icar)

#' lcar --> better
cav_lcar_inlabru <- cav_bru_basic(cmp_lcar)

#' pcar --> This is going to be obnoxious since verbose = T is mandatory not to make INLA crash :) 
cav_pcar_inlabru <- cav_bru_basic(cmp_pcar, verbose = T)

#' bym --> as for plain Poisson models: the best one.
cav_bym_inlabru <- cav_bru_basic(cmp_bym)


#' Overall comparison:
bru_models <- list(cav_nosp_inlabru, cav_icar_inlabru, 
                   cav_lcar_inlabru, cav_pcar_inlabru, 
                   cav_bym_inlabru)

WAICS_inlabru <- do.call(dplyr::bind_rows, 
                         lapply(bru_models, function(x) data.frame(cbind(x$waic$waic, x$waic$p.eff)))) %>% 
  dplyr::mutate(Model = c("Null", "ICAR", "LCAR", "PCAR", "BYM")) %>% 
  dplyr::relocate(.data$Model, .before = 1)
names(WAICS_inlabru)[c(2,3)] <- c("WAIC", "P_eff")
rownames(WAICS_inlabru) <- NULL

#' This should be the result for those who don't wanna
#' wait all models to run:
#'   Model      WAIC    P_eff
#' 1  Null 1098.0333 20.33822
#' 2  ICAR  951.7224 75.43719
#' 3  LCAR  947.4287 75.34904
#' 4  PCAR  947.8590 76.96200
#' 5   BYM  943.1022 75.43401
#'


#' Preliminary finding: interpreting the pogit model,
#' at least insofar as it is approximated with INLABRU,
#' is substantially analogous to interpreting the Poisson model.


## Spatial regression: MCMC using CARBayes -------------------------------------


library(CARBayes)

#' CARBAyes variable encoding:
#' phi: ICAR component with marginal variance tau^2
#' theta: IID component with marginal variance sigma^2
#' 

cav_bym0_INLA <- INLA::inla( N_ACC ~ 1 +TEP_th_22 + ELI + PGR + UIS + ELL + PDI + ER + 
                               f(ID, model = "bym", graph = W_con, scale.model = F,
                                 hyper = list(theta1 = list(param = c(1e-3, 1e-3)),
                                              theta2 = list(param = c(1e-3, 1e-3)))),
                      family = "poisson", offset = log(nn), data =dd_con,
                      num.threads = 1, control.compute = 
                        list(internal.opt = F, cpo = T, waic = T), 
                      #inla.mode = "classic", control.inla = list(strategy = "laplace"),
                      control.predictor = list(compute = T),
                      verbose = T)  



cav_bym0_CARBayes <- CARBayes::S.CARbym(N_ACC ~ 1 +TEP_th_22 + ELI + PGR + UIS + ELL + PDI + ER+ offset(log(nn)),
                      family = "poisson", data =dd_con,
                      prior.tau2 = c(1e-3, 1e-3), prior.sigma2 = c(1e-3, 1e-3),
                      W = W_con, prior.var.beta = rep(1e3, 8),
                      burnin = 10000, n.sample = 60000, 
                      n.chains = 3,
                      verbose = T) 

cav_bym0_CARBayes$summary.results
cav_bym0_CARBayes$modelfit




## Spatial regression: MCMC using NIMBLE ---------------------------------------


library(nimble)

#' Model constants and known values: ------------------------------------------#
#' 
LconvConsts <- list(
  N = nrow(dd_con), offset = dd_con$nn,
  adj_con = unlist(nb_con),
  #' Alternatively, num_con <- sapply(nb_con, length)
  num_con = rowSums(W_con),
      #L = sum(rowSums(W_con)),
  nsumN = sum(rowSums(W_con)),
  sigma0 = 1.5,
  typVar = exp(mean(log(diag(
    inla.qinv(Q = Lapl_con + Matrix::Diagonal(nrow(W_con)) * 
                max(diag(Lapl_con)) *sqrt(.Machine$double.eps),
              constr = list(
                A = matrix(1, nrow = 1, ncol = nrow(W_con)), e = 0))))))
)

#' input data to be arranged into a list
LconvData <- list(
  y = dd_con$N_ACC, TEP = dd_con$TEP_th_22, ELI = dd_con$ELI,
  PGR = dd_con$PGR, UIS = dd_con$UIS, ELL = dd_con$ELL, PDI = dd_con$PDI, 
  ER = dd_con$ER
)

#' Explicit model definition using the JAGS language --------------------------#
#' 
BYM_code <- nimbleCode({
  phi ~ dunif(0,1)
  sigma ~ dexp(log(100)/sigma0)
  uc[1:N]~dcar_normal(adj_con[1:nsumN],
                      wei_con[1:nsumN],
                      num_con[1:N],1,zero_mean=1)
  for(i in 1:N){
    vc[i] ~ dnorm(0,1)
    Corr[i]  <- sigma*uc[i]*sqrt(phi/sqrt(typVar))
    UCorr[i] <- sigma*vc[i]*sqrt((1-phi))
    z[i] <- Corr[i] + UCorr[i]
    log(eta[i])<- Intercept + 
      alpha_TEP * TEP[i] + alpha_ELI * ELI[i] + 
      alpha_PGR * PGR[i] + alpha_UIS * UIS[i] +
      alpha_ELL * ELL[i] + alpha_PDI * PDI[i] +
      alpha_ER * ER[i] + z[i]
    lambda[i] <- eta[i]*offset[i]
    y[i] ~ dpois(lambda[i])
  }
  for(k in 1:nsumN){wei_con[k]<-1}
  Intercept ~ dnorm(0, 1e-5)
  alpha_TEP ~ dnorm(0, 1e-3)
  alpha_ELI ~ dnorm(0, 1e-3)  
  alpha_PGR ~ dnorm(0, 1e-3)
  alpha_UIS ~ dnorm(0, 1e-3)
  alpha_ELL ~ dnorm(0, 1e-3)
  alpha_PDI ~ dnorm(0, 1e-3)
  alpha_ER  ~ dnorm(0, 1e-3)
})


Inits<-
  list(Intercept = - 6,
       alpha_TEP = 0, alpha_ELI = 0, alpha_PGR = 0, alpha_UIS = 0,
       alpha_ELL = 0, alpha_PDI = 0, alpha_ER = 0,
       uc=rep(0.0, LconvConsts$N),
       vc=rep(0.0, LconvConsts$N),
       sigma = exp(-2),
       phi = exp(-4))

#Lconv<-nimbleModel(code=LconvCode,
#                   name="Lconv",constants=LconvConsts,
#                   data=LconvData,inits=LconvInits)


#LconvConf<-configureMCMC(Lconv,print=TRUE,
#                         enableWAIC=nimbleOptions(`enableWAIC`=TRUE))
#LconvConf$addMonitors(c("a0",
#                        "alpha_TEP", "alpha_ELI", "alpha_PGR", 
#                        "alpha_UIS", "alpha_ELL", "alpha_PDI", "alpha_ER", #"tau0",
#                        "uc","vc","sigma", "phi"))


cav_BYM_nimble <-nimbleMCMC(code=BYM_code,
                     constants=LconvConsts,
                     data=LconvData, inits=Inits,
                     nchains=3, nburnin=5000, niter=30000,
                     summary=TRUE, WAIC=TRUE,
                     monitors = c("Intercept","alpha_TEP", "alpha_ELI", "alpha_PGR",  
                                  "alpha_UIS", "alpha_ELL", "alpha_PDI", "alpha_ER", 
                                  "uc","vc","sigma", "phi"))
cav_BYM_nimble$summary$all.chains
 









cav_bym_INLA <- inla(N_ACC ~ 1 +
                       f(ID, model = "bym2", graph = W_con,  scale.model = T, 
                         hyper = list(prec = list(prior = "pc.prec", param = c(1.5, 0.01)))),
                     family = "poisson", offset = log(nn), data =dd_con,
                     num.threads = 1, control.compute = 
                       list(internal.opt = F, cpo = T, waic = T), 
                     #inla.mode = "classic", control.inla = list(strategy = "laplace", int.strategy = "grid"),
                     control.predictor = list(compute = T),
                     verbose = T) # better

## TBD: model fitting with BRMS --> warning: slow ------------------------------
library(brms)
cav_icar_brms <- brm(N_ACC ~ 1 +TEP_th_22 + ELI + PGR + UIS + ELL + PDI + ER + offset(log(nn)) +
                       car(W, gr = PRO_COM, type = "icar"),
                     data = dd_con, data2 = list(W = W_con),
                     prior = c( prior(pc.prec(u = 1.5, alpha = 0.01), 
                                      class = "car", group = PRO_COM)),
                     family = poisson())




