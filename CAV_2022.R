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



## Mapping municipalities from support centers ---------------------------------



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
    Q <- param$PREC * (Matrix::Diagonal(nrow(W), apply(W, 1, sum)) - param$alpha * W)
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
    return(c(0, 4))
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
                         inla.mode = "classic", control.inla = list(strategy = "laplace",
                                                                    int.strategy ="grid"),
                         control.predictor = list(compute = T),
                         verbose = T) # better

# PCAR model
cav_pcar_INLA <- inla(N_ACC ~ 1 +TEP_th_22 + ELI + PGR + UIS + ELL + PDI + ER +
                        f(ID, model = PCAR.model(W = W_con, k = 1, lambda = 1.5)),
                         family = "poisson", offset = log(nn), data =dd_con,
                         num.threads = 1, control.compute = 
                           list(internal.opt = F, cpo = T, waic = T), 
                         inla.mode = "classic", control.inla = list(strategy = "laplace", int.strategy  = "grid"),
                         control.predictor = list(compute = T),
                         verbose = T) 

# BYM model
cav_bym_INLA <- inla(N_ACC ~ 1 +TEP_th_22 + ELI + PGR + UIS + ELL + PDI + ER +
                       f(ID, model = "bym2", graph = W_con,  scale.model = T, 
                         hyper = list(prec = list(prior = "pc.prec", param = c(1.5, 0.01)))),
                      family = "poisson", offset = log(nn), data =dd_con,
                      num.threads = 1, control.compute = 
                        list(internal.opt = F, cpo = T, waic = T), 
                      inla.mode = "classic", control.inla = list(strategy = "laplace", int.strategy = "grid"),
                      control.predictor = list(compute = T),
                      verbose = T) # better

# Leroux model - sparse precision
cav_leroux_INLA <- inla(N_ACC ~ 1 +TEP_th_22 + ELI + PGR + UIS + ELL + PDI + ER +
                          f(ID, model = "besagproper2", graph = W_con, 
                            hyper = list(prec = list(prior = "pc.prec", param = c(1.5, 0.01)))),
                     family = "poisson", offset = log(nn), data =dd_con,
                     num.threads = 1, control.compute = 
                       list(internal.opt = F, cpo = T, waic = T), 
                     inla.mode = "classic", control.inla = list(strategy = "laplace", int.strategy ="grid"),
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
                     inla.mode = "classic", control.inla = list(strategy = "laplace", strategy = "grid"),
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


## TBD: model fitting with BRMS --> warning: slow ------------------------------
library(brms)
cav_icar_brms <- brm(N_ACC ~ 1 +TEP_th_22 + ELI + PGR + UIS + ELL + PDI + ER + offset(log(nn)) +
                       car(W, gr = PRO_COM, type = "icar"),
                     data = dd_con, data2 = list(W = W_con),
                     prior = c( prior(pc.prec(u = 1.5, alpha = 0.01), 
                                      class = "car", group = PRO_COM)),
                     family = poisson())




