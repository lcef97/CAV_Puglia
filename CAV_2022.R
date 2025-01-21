#' ---------  Analisi CAV Puglia 2022 -----------------------------------------#
## Input -----------------------------------------------------------------------
library(magrittr)

#'  ---------------------------------------------------------------------------#
#' 
#'  Do not delete!! Code used to build input data from excel files
#'  Excel inputs include sensible information which we prefer not to share;
#'  we only leave track of how we aggregate data from individual level
#'  to municipality level
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
#'  ---------------------------------------------------------------------------#   



# Aggregated from individual-level data on accesses to support centers:
load("input/CAV_input_mun.RData")

#' 2022 municipalities shapefiles; easily obtainable by scraping with the following 
#' commented code:
#' Mun22_shp <- SchoolDataIT::Get_Shapefile(2022)
#' Shp <- Mun22_shp %>% dplyr::filter(.data$COD_REG == 16) %>% 
#'  dplyr::select(.data$COD_PROV, .data$PRO_COM, .data$COMUNE)
#'
#' Still, we leave the static shapefile in order NOT to need internet connection:
load("input/Shp.RData")

# Function to extract numeric digits from a strings vector (needed to filter age):
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
Pop_f_15 <- Popolazione_Puglia_2022 %>% dplyr::filter(.data$Sesso == 2) %>% 
  dplyr::filter(.data$Eta > 14) %>% 
  dplyr::group_by(.data$PRO_COM, .data$Comune) %>% 
  dplyr::summarise(nn = sum(.data$Popolazione)) %>% dplyr::ungroup()

# Complete dataset:
dd <- Shp %>% dplyr::left_join(Pop_f_15[,c(1,3)],
                               by = "PRO_COM") %>% 
  dplyr::left_join(dplyr::select(CAV_mun_22, -.data$comune),by = "PRO_COM") %>% 
  dplyr::left_join(dplyr::select(Indicators, -.data$Comune), by = "PRO_COM")

# Municipalities from which no woman reported violence --> count == 0
dd$N_ACC[is.na(dd$N_ACC)] <- 0 
# "access ratio"
dd$F_ACC <- dd$N_ACC/dd$nn



## Mapping municipalities from support centers ---------------------------------

# Municipalities hosting a support center:
munWcav <- munWcav <- c (71020,71024,71051,72004,72006,72011,72014,
                         72019,72021,72029,72031,72033,72035,73013,
                         73027,74001,74009,75018,75029,75035,75059,
                         110001,110002,110009)

# Tremiti Islands are a singleton --> need to remove them
# in order to perform spatial analysis
suppressWarnings({
  singletons <- which(unlist(lapply(spdep::poly2nb(dd), function(x) x[1L] == 0)))
})


# Filter out singletons
dd_con <- dd[-singletons, ]


#' Do not delete: code to find minimum distances ------------------------------#
#'                                                
#' Since the loop takes some minutes to work, we save the
#' output object dists_th outside:                                             
#'                                                
#'
#'dists_th <- NULL
#'for (i in c(1:nrow(dd_con))){
#'  X <- dd_con$PRO_COM[i]
#'  dists <- numeric(length(munWcav))
#'  IDs <- numeric(length(munWcav))
#'  for(j in c(1:length(munWcav))){
#'    c.out <- munWcav[j]
#'    id <- paste0(min(X, c.out),
#'                     " - ", max(X, c.out))
#'   nn <- which(dist_short$OR_DEST == id)
#'    dists[j] <- dist_short$TEP_TOT[nn]
#'    IDs[j] <- dist_short$OR_DEST[nn]
#'  }
#'  m <- which.min(dists)
#'  ret <- c(X, dists[m])
#'  dists_th <- data.frame(rbind(dists_th, ret))
#'}
#'names(dists_th) <- c("PRO_COM", "TEP_th")
#'dists_th$TEP_th <- as.numeric(dists_th$TEP_th)
#'
#' -----------------------------------------------------------------------------

load("input/dists_th.RData")

# This is the dataset we will concretely work on:
dd_con <- dd_con %>% 
  dplyr::left_join(dists_th, by = "PRO_COM") %>% 
  dplyr::mutate(TEP_th = as.vector(scale(.data$TEP_th)))

# sd of travel time: almost 16 minutes
attr(scale(dists_th$TEP_th), "scaled:scale")

# neighbours list
nb_con <- spdep::poly2nb(dd_con)
# neighbouring/adjacency matrix
W_con <- spdep::nb2mat(nb_con, style = "B")
rownames(W_con) <- colnames(W_con) <- dd_con$PRO_COM

# row ID - needed for spatial models
dd_con$ID <- c(1:nrow(dd_con))

# Full GLM --> for model matrix
glm_all_X <- glm(N_ACC ~ 1 + TEP_th + MFI + AES + PDI + ELL + ER +
                   PGR + UIS + ELI + offset(log(nn)),
                 data = dd_con, family = "poisson")
# model matrix
X <- model.matrix(glm_all_X)

## nonspatial regression -------------------------------------------------------

glm_0 <- glm(N_ACC ~ 1 + offset(log(nn)), data = dd_con, family = "poisson")


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
covariates <- colnames(X)[-1]
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
    cat("Adding covariates is not necessary anymore")
    break
  } else{
    covs.in <- c(covs.in, covs.out[which.min(BICs)])
  }
}

# Only two, yet quite correlated.
covs.in 
# We only consider the distance from closest CAV
# Also because AES is another distance indicator. 


                             ## USQUE ADEO
                             ## 2025 January 21

## Spatial Poisson regression: spaMM -------------------------------------------

cav_car <- spaMM::fitme(N_ACC ~ 1 + TEP_th + adjacency(1|PRO_COM) +  
                   offset(log(nn)), 
                 adjMatrix = W_con,
                 data = dd_con, family = 'poisson')
summary(cav_car)
# rho close to zero - likely iid residuals

# Code taken from INLAMSM (https://github.com/becarioprecario/INLAMSM/tree/master/R)
# since INLA has no built-in model for the PCAR - only ICAR, BYM, Leroux and BYM2

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


## Spatial Poisson regression: INLA (TBD) --------------------------------------




library(INLA)


m_0_INLA <- inla(N_ACC ~ 1 ,
                 family = "poisson",  data =dd_con, offset = log(nn),
                 num.threads = 1, control.compute = 
                   list(internal.opt = F, cpo = T, waic = T), 
                 inla.mode = "classic", control.inla = list(strategy = "laplace"),
                 verbose = T)

# Replicates the glm quite well
m_0_INLA <- inla(N_ACC ~ 1 + TEP_th,
                 family = "poisson",  data =dd_con, offset = log(nn),
                 num.threads = 1, control.compute = 
                   list(internal.opt = F, cpo = T, waic = T), 
                 inla.mode = "classic", control.inla = list(strategy = "laplace"))



cav_icar_INLA <- inla(N_ACC ~ 1 + TEP_th + f(ID, model = "besag", graph = W_con,
                                                scale.model = T, prior = "pc.prec"),
                         family = "poisson", offset = log(nn), data =dd_con,
                         num.threads = 1, control.compute = 
                           list(internal.opt = F, cpo = T, waic = T), 
                         inla.mode = "classic", control.inla = list(strategy = "laplace"),
                         control.predictor = list(compute = T),
                         verbose = T) # better






## usque adeo

dd_con %>%
  dplyr::mutate(zcol = cav_icar_INLA$summary.fitted.values$mean) %>% 
  mapview::mapview(zcol = "zcol")


plot(dd_con$F_ACC, cav_icar_INLA$summary.fitted.values$mean/dd_con$nn)


cav_pcar_INLA <- inla(N_ACC ~ 1 + TEP_th + f(ID, model = PCAR.model(W = W_con, k = 1, lambda = 1.5)),
                         family = "poisson", offset = log(nn), data =dd_con,
                         num.threads = 1, control.compute = 
                           list(internal.opt = F, cpo = T, waic = T), 
                         inla.mode = "classic", control.inla = list(strategy = "laplace"),
                         control.predictor = list(compute = T),
                         verbose = T) 

## TBD: model fitting with BRMS ------------------------------------------------
library(brms)
cav_icar_brms <- brm(N_ACC ~ 1 + TEP_th + offset(log(nn)) +
                       car(W, gr = PRO_COM, type = "icar"),
                     data = dd_con, data2 = list(W = W_con),
                     family = poisson())




