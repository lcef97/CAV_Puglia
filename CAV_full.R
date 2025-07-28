#' ---------  Analisi CAV Puglia 2021-24 --------------------------------------#
## Input -----------------------------------------------------------------------

#' NEVER forget calling magrittr and sf. Never --------------------------------#
#' 
library(magrittr)
library(sf)
#'  ---------------------------------------------------------------------------#
#' 
#'  Do not delete below ==> Code used to build input data from excel files
#'  Excel inputs include sensible information which we prefer NOT to share;
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
#'  Same thing for other years  -------........--------------------------------#
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
#'  CAV_mun_24 <- cav_2024 %>%
#'      dplyr::filter(.data$ESITO_ACCESSO == "presa in carico") %>%
#'      dplyr::filter(!is.na(.data$PRO_COM)) %>% 
#'      dplyr::group_by(.data$PRO_COM) %>% 
#'      dplyr::summarise(N_ACC = dplyr::n(),
#'      comune = stringr::str_to_title(
#'      .data$LUOGO_RESIDENZA_DOMICILIO[1L]))%>%   dplyr::ungroup()
#'
#'      
#'     
#'  ---------------------------------------------------------------------------#   
#'  
#'  Then we download municipality-level data on accesses to support centers,
#'  aggregated from individual-level ones:  -----------------------------------#

load("input/CAV_input_mun_2021.RData")
load("input/CAV_input_mun_2022.RData")
load("input/CAV_input_mun_2023.RData")
load("input/CAV_input_mun_2024.RData")

CAV_mun_21 <- CAV_mun_21 %>% dplyr::rename(N_ACC_2021 = .data$N_ACC)
CAV_mun_22 <- CAV_mun_22 %>% dplyr::rename(N_ACC_2022 = .data$N_ACC)
CAV_mun_23 <- CAV_mun_23 %>% dplyr::rename(N_ACC_2023 = .data$N_ACC)
CAV_mun_24 <- CAV_mun_24 %>% dplyr::rename(N_ACC_2024 = .data$N_ACC)


#'  2022 municipalities shapefiles; easily obtainable 
#'  by scraping with the following (commented) code:
#'    Mun22_shp <- SchoolDataIT::Get_Shapefile(2022)
#'    Shp <- Mun22_shp %>% dplyr::filter(.data$COD_REG == 16) %>% 
#'        dplyr::select(.data$COD_PROV, .data$PRO_COM, .data$COMUNE)
#'  
#'  In theory, a shapefile should be used for each year.
#'  Still, administrative units boundaries are unchanged at least
#'  from 2021 to 2023 ---------------------------------------------------------#
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

#'  Female population aged >= 15 years. Source data: 
#'  http://dati.istat.it/Index.aspx?DataSetCode=DCIS_POPRES1#  ----------------#

Popolazione_Puglia_2021 <- readr::read_csv("input/Popolazione_Puglia_2021.csv") %>% 
  dplyr::select(.data$ITTER107, .data$Territorio, .data$SEXISTAT1, .data$ETA1, .data$Value) %>% 
  dplyr::filter(.data$ETA1 != "TOTAL") %>% 
  dplyr::mutate(ETA1 = nn_extract(.data$ETA1)) %>% 
  dplyr::mutate(ITTER107 = as.numeric(.data$ITTER107))

Popolazione_Puglia_2022 <- readr::read_csv("input/Popolazione_Puglia_2022.csv") %>% 
  dplyr::select(.data$ITTER107, .data$Territorio, .data$SEXISTAT1, .data$ETA1, .data$Value) %>% 
  dplyr::filter(.data$ETA1 != "TOTAL") %>% 
  dplyr::mutate(ETA1 = nn_extract(.data$ETA1)) %>% 
  dplyr::mutate(ITTER107 = as.numeric(.data$ITTER107))

Popolazione_Puglia_2023 <- readr::read_csv("input/Popolazione_Puglia_2023.csv") %>% 
  dplyr::select(.data$ITTER107, .data$Territorio, .data$SEXISTAT1, .data$ETA1, .data$Value) %>% 
  dplyr::filter(.data$ETA1 != "TOTAL") %>% 
  dplyr::mutate(ETA1 = nn_extract(.data$ETA1)) %>% 
  dplyr::mutate(ITTER107 = as.numeric(.data$ITTER107))

Popolazione_Puglia_2024 <- readr::read_csv("input/Popolazione_Puglia_2024.csv",
                                           quote = "")  %>% 
  dplyr::filter(.data$AGE != "TOTAL") %>% 
  dplyr::mutate(ETA1 = nn_extract(.data$AGE)) %>% 
  dplyr::mutate(ITTER107 = as.numeric(.data$REF_AREA)) %>% 
  dplyr::rename(SEXISTAT1 = .data$SEX, Value = .data$Osservazione)%>% 
  dplyr::select(.data$ITTER107, .data$SEXISTAT1, .data$ETA1, .data$Value)

#' Filter and aggregate female population -------------------------------------#

Pop_f_15 <- Popolazione_Puglia_2021 %>% 
  dplyr::left_join(Popolazione_Puglia_2022,
                   by = c("ITTER107", "Territorio", "SEXISTAT1", "ETA1")) %>% 
  dplyr::left_join(Popolazione_Puglia_2023,
                   by = c("ITTER107", "Territorio", "SEXISTAT1", "ETA1")) %>% 
  dplyr::left_join(Popolazione_Puglia_2024,
                   by =  c("ITTER107",  "SEXISTAT1", "ETA1"))

names(Pop_f_15) <-  c("PRO_COM", "Comune", "Sesso", "Eta", 
                      "Popolazione_21", "Popolazione_22", "Popolazione_23", "Popolazione_24")

Pop_f_15 <- Pop_f_15 %>% dplyr::filter(.data$Sesso == 2) %>% 
  dplyr::filter(.data$Eta > 14) %>% 
  dplyr::group_by(.data$PRO_COM, .data$Comune) %>% 
  dplyr::summarise(nn_2021 = sum(.data$Popolazione_21),
                   nn_2022 = sum(.data$Popolazione_22),
                   nn_2023 = sum(.data$Popolazione_23),
                   nn_2024 = sum(.data$Popolazione_24)) %>%
  dplyr::ungroup()


#'  Build the dataset ---------------------------------------------------------#

dd <- Shp %>% dplyr::left_join(Pop_f_15[, -2], by = "PRO_COM") %>% 
  dplyr::left_join(dplyr::select(CAV_mun_21, -.data$comune),by = "PRO_COM") %>% 
  dplyr::left_join(dplyr::select(CAV_mun_22, -.data$comune),by = "PRO_COM") %>% 
  dplyr::left_join(dplyr::select(CAV_mun_23, -.data$comune),by = "PRO_COM") %>% 
  dplyr::left_join(dplyr::select(CAV_mun_24, -.data$comune),by = "PRO_COM") %>% 
  tidyr::pivot_longer(cols = tidyselect::starts_with("nn_") | 
                        tidyselect::starts_with("N_ACC_"),
                      names_to = c(".value", "Year"),
                      names_pattern = "(nn|N_ACC)_(\\d+)" ) %>% 
  dplyr::left_join(dplyr::select(Indicators, -.data$Comune), by = "PRO_COM") %>% 
  dplyr::arrange(.data$Year, .data$PRO_COM)

#'  Municipalities from which no woman reported violence --> count == 0  ------#
dd$N_ACC[is.na(dd$N_ACC)] <- 0

#' "access ratio" -------------------------------------------------------------#
dd$LN_ACC <- log(dd$N_ACC/dd$nn)

ggy21 <- ggplot2::ggplot() +
  ggplot2::geom_sf(data = dplyr::filter(dd, .data$Year == "2021"), 
                   ggplot2::aes(fill = .data$LN_ACC))+
  ggplot2::scale_fill_viridis_c(na.value = "white",
                                limits = c(-9.5, -4.6))+
  ggplot2::ggtitle("Year: 2021") +
  ggplot2::theme_classic()

ggy22 <- ggplot2::ggplot() +
  ggplot2::geom_sf(data = dplyr::filter(dd, .data$Year == "2022"), 
                   ggplot2::aes(fill = .data$LN_ACC))+
  ggplot2::scale_fill_viridis_c(na.value = "white",
                                limits = c(-9.5, -4.6))+
  ggplot2::ggtitle("Year: 2022") +
  ggplot2::theme_classic()

ggy23 <- ggplot2::ggplot() +
  ggplot2::geom_sf(data = dplyr::filter(dd, .data$Year == "2023"), 
                   ggplot2::aes(fill = .data$LN_ACC))+
  ggplot2::scale_fill_viridis_c(na.value = "white",
                                limits = c(-9.5, -4.6))+
  ggplot2::ggtitle("Year: 2023") +
  ggplot2::theme_classic()

ggy24 <- ggplot2::ggplot() +
  ggplot2::geom_sf(data = dplyr::filter(dd, .data$Year == "2024"), 
                   ggplot2::aes(fill = .data$LN_ACC))+
  ggplot2::scale_fill_viridis_c(na.value = "white",
                                limits = c(-9.5, -4.6))+
  ggplot2::ggtitle("Year: 2024")+
  ggplot2::theme_classic()


gridExtra::grid.arrange(ggy21, ggy22, ggy23, ggy24, nrow = 2, ncol = 2)

source("Auxiliary/Functions.R")

#' Tremiti Islands are a singleton --> 
#'   need to remove them to perform spatial analysis --------------------------#
suppressWarnings({
  singletons <- Shp$PRO_COM[which(
    unlist(lapply(spdep::poly2nb(Shp),
                  function(x) x[1L] == 0)))]
})


#'  Mapping municipalities from support centers   ----------------------------#
#'  
#'  Loop to compute minimal distances
#'
#'
#'  Municipalities hosting a support center:
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
#' Sportelli_CAV <- readxl::read_excel("input_excel/Sportelli_CAV.xlsx", sheet = "CAV")
#' munWcav_24 <- unique(sort(as.numeric(Sportelli_CAV$PRO_COM))) 
#' munWcav_24 == c(71020, 71024, 71029, 71051,
#'                  72004, 72006, 72011, 72014, 72019, 72021, 72029, 72031, 72033, 72035,
#'                  73013, 73027,
#'                  74001, 74009, 74019,
#'                  75029, 75035, 75059,
#'                  110001, 110002, 110009)
#'
#'  Routine to compute distances from closest municipality
#'  with an AVC ---------------------------------------------------------------#
#'  
#'  Can be replicated for other years of course -------------------------------#
#'  
#'  dists_th_24 <- NULL
#'  dd24 <- dd %>% dplyr::filter(.data$Year == "2024") %>% 
#'  dplyr::filter(!.data$PRO_COM %in% singletons)
#'  for (i in c(1:nrow(dd24))){
#'    X <- dd24$PRO_COM[i]
#'    dists <- numeric(length(munWcav_24))
#'    IDs <- numeric(length(munWcav_24))
#'    for(j in c(1:length(munWcav_24))){
#'      c.out <- munWcav_24[j]
#'      id <- paste0(min(X, c.out),
#'                   " - ", max(X, c.out))
#'      nn <- which(dist_short$OR_DEST == id)
#'      dists[j] <- as.numeric(dist_short$TEP_TOT[nn])
#'      IDs[j] <- dist_short$OR_DEST[nn]
#'    }
#'    m <- which.min(dists)
#'    ret <- c(X, dists[m])
#'    dists_th_24 <- data.frame(rbind(dists_th_24, ret))
#'  }
#'  names(dists_th_24) <- c("PRO_COM", "TEP_th_24")


load("input/dists_th_22.RData")
load("input/dists_th_23.RData")
load("input/dists_th_24.RData")
names(dists_th_22)[2] <- names(dists_th_23)[2] <- names(dists_th_24)[2] <- "TEP_th"
distances <- rbind(dists_th_22, dists_th_22, dists_th_23, dists_th_24) %>% 
  dplyr::mutate(Year = c(rep("2021", nrow(dists_th_22)),
                         rep("2022", nrow(dists_th_22)),
                         rep("2023", nrow(dists_th_23)),
                         rep("2024", nrow(dists_th_24))) )



#' This is the dataset we will concretely work on.
#' Covariates are all scaled to zero mean and unit variance -------------------#
dd_con <- dd %>% dplyr::filter(!.data$PRO_COM %in% singletons) %>% 
  dplyr::left_join(distances, by = c("PRO_COM", "Year")) %>% 
  dplyr::mutate(CAV = as.numeric(.data$TEP_th == 0)) %>% 
  dplyr::mutate(Y_2021 = as.numeric(.data$Year == "2021")) %>% 
  dplyr::mutate(Y_2022 = as.numeric(.data$Year == "2022")) %>% 
  dplyr::mutate(Y_2023 = as.numeric(.data$Year == "2023")) %>% 
  dplyr::mutate(Y_2024 = as.numeric(.data$Year == "2024")) %>% 
  dplyr::mutate(TEP_th = as.vector(scale(.data$TEP_th))) %>% 
  dplyr::mutate(AES = as.vector(scale(.data$AES))) %>% 
  dplyr::mutate(MFI = as.vector(scale(.data$MFI)))  %>% 
  dplyr::mutate(PDI = as.vector(scale(.data$PDI)))  %>% 
  dplyr::mutate(ELL = as.vector(scale(.data$ELL)))  %>% 
  dplyr::mutate(ER = as.vector(scale(.data$ER)))  %>% 
  dplyr::mutate(PGR = as.vector(scale(.data$PGR)))  %>% 
  dplyr::mutate(UIS = as.vector(scale(.data$UIS)))  %>% 
  dplyr::mutate(ELI = as.vector(scale(.data$ELI)))  %>% 
  dplyr::select(-.data$Year) %>% 
  dplyr::mutate(ID = c(1:nrow(.))) %>% 
  dplyr::mutate(Area = as.numeric(as.factor(.data$PRO_COM)))



#' Store the dataset in tensor format -----------------------------------------#
 
df2ls <- function(df, names_from, id_cols){
  df <- sf::st_drop_geometry(df)
  names_from.m <- as.matrix(dplyr::select(df, names_from))
  id_cols.df <- dplyr::select(df, id_cols)
  names_from.m[ names_from.m==0  ] <- NA
  obj <- df %>% 
    dplyr::select(-names_from) %>% 
    dplyr::select(-id_cols) %>% 
    dplyr::select_if(is.numeric) 
  res <- lapply(obj, function(x){
    return(names_from.m * x)
  })
  res <- append(as.list(id_cols.df), res)
  res$Intercept <- names_from.df
  return(res)
}
dd_list <- df2ls(dd_con, id_cols = c("ID", "COMUNE"), names_from = c("Y_2021", "Y_2022", "Y_2023", "Y_2024"))


## Model input setup -----------------------------------------------------------
#' 
#' Input data are defined; now we need some auxiliary objects -----------------#
#' 
#' Number of sites saved here -------------------------------------------------#
n <- length(unique(dd_con$PRO_COM))

#' sd of travel time: around 14 minutes ---------------------------------------#
attr(scale(dists_th_22$TEP_th), "scaled:scale")

#' neighbours list ------------------------------------------------------------#
nb_con <- spdep::poly2nb(dd_con[c(1:n), ])

#' neighbouring/adjacency matrix ----------------------------------------------#
W_con <- spdep::nb2mat(nb_con, style = "B")
rownames(W_con) <- colnames(W_con) <- dd_con$PRO_COM[c(1:n)]

#' Laplacian matrix -----------------------------------------------------------#
D_con <- Matrix::Diagonal(x=apply(W_con, MARGIN=1, FUN=function(x) sum(x)), n=n)
Lapl_con <- D_con - W_con
V_con <- eigen(Lapl_con)$vectors

constr <- INLA:::inla.bym.constr.internal(
  Q = Lapl_con, adjust.for.con.comp = T)
A_constr <- kronecker(Matrix::Diagonal(n=4,x=1), constr$constr$A)

#'  Necessary to constrain the BYM model 
constr.BYM <- list(
  A = cbind(Matrix::Matrix(0, nrow = nrow(A_constr), ncol = ncol(A_constr)), A_constr),
  e=c(0,0,0,0) )





#' Full GLM --> for model matrix
glm_all_X <- glm(N_ACC ~ 1 + TEP_th + MFI + AES + PDI + ELL + ER +
                   PGR + UIS + ELI + offset(log(nn)),
                 data = dd_con, family = "poisson")
#' model matrix
X <- model.matrix(glm_all_X)

#' Besides calling the package, the present R code is frequently 
#' derived from INLAMSM source codes.
if(!rlang::is_installed("INLAMSM")) devtools::install_github("becarioprecario/INLAMSM")

library(INLA)
library(INLAMSM)

#' Deconfounding with Spatial+ 
#' First approach, more radical: fitting an ICAR model on covariates

ICAR_ELL <- inla(ELL ~ 1 + f(Area, model = "besag", graph = W_con,
                             hyper = list(prec=list(prior = "PC.prec"))),
                 data = dd_con[c(1:n), ],
                 family = "gaussian", 
                 control.compute = list(internal.opt = F),
                 num.threads = 1, verbose = T)

ICAR_ER <- inla(ER ~ 1 + f(Area, model = "besag", graph = W_con,
                           hyper = list(prec=list(prior = "PC.prec"))),
                data = dd_con[c(1:n), ],
                family = "gaussian", 
                control.compute = list(internal.opt = F),
                num.threads = 1, verbose = T)

dd_con[c(1:n), ]%>% 
  dplyr::mutate(xhat = ICAR_ELL$summary.random$Area$mean) %>% 
  ggplot2::ggplot() +
  ggplot2::geom_sf(ggplot2::aes(fill = .data$xhat))+
  ggplot2::labs( fill = "xhat") +
  ggplot2::scale_fill_viridis_c(na.value = "white", direction = -1) +
  ggplot2::theme_classic()

ELL_nosp_resid <- as.vector(scale(dd_con$ELL - ICAR_ELL$summary.random$Area$mean))
ER_nosp_resid <- as.vector(scale(dd_con$ER - ICAR_ER$summary.random$Area$mean))


#' Here some internal functions that may or may not be useful -----------------#
zhat_plot <- function(model, main = NULL){
  
  zbound <- range(model$summary.random$ID$mean)
  
  if(is.null(main)) main <- deparse(substitute(model))
  
  ggzhat21 <- dd_con %>%
    dplyr::filter(.data$Y_2021 == 1) %>% 
    dplyr::mutate(zhat_21 = model$summary.random$ID$mean[c(1:n)]) %>% 
    ggplot2::ggplot() +
    ggplot2::geom_sf(ggplot2::aes(fill = .data$zhat_21))+
    ggplot2::scale_fill_viridis_c(na.value = "white", direction = -1, limits = zbound) +
    ggplot2::theme_classic()
  
  ggzhat22 <- dd_con %>% 
    dplyr::filter(.data$Y_2022 == 1) %>% 
    dplyr::mutate(zhat_22 = model$summary.random$ID$mean[c(n+(1:n))]) %>% 
    ggplot2::ggplot() +
    ggplot2::geom_sf(ggplot2::aes(fill = .data$zhat_22))+
    ggplot2::scale_fill_viridis_c(na.value = "white", direction = -1, limits = zbound) +
    ggplot2::theme_classic()
  
  ggzhat23 <- dd_con %>% 
    dplyr::filter(.data$Y_2023 == 1) %>% 
    dplyr::mutate(zhat_23 = model$summary.random$ID$mean[c((2*n)+(1:n))]) %>% 
    ggplot2::ggplot() +
    ggplot2::geom_sf(ggplot2::aes(fill = .data$zhat_23))+
    ggplot2::scale_fill_viridis_c(na.value = "white", direction = -1, limits = zbound) +
    ggplot2::theme_classic()
  
  ggzhat24 <- dd_con %>% 
    dplyr::filter(.data$Y_2024 == 1) %>% 
    dplyr::mutate(zhat_24 = model$summary.random$ID$mean[c((2*n)+(1:n))]) %>% 
    ggplot2::ggplot() +
    ggplot2::geom_sf(ggplot2::aes(fill = .data$zhat_24))+
    ggplot2::scale_fill_viridis_c(na.value = "white", direction = -1, limits = zbound) +
    ggplot2::theme_classic()
  
  gridExtra::grid.arrange(ggzhat21, ggzhat22, ggzhat23, ggzhat24, nrow = 2, ncol = 2,
                          top = main)
  
}

#' Expected accesses, may turn out to be useful -------------------------------#
Exp_Acc <- dd_con$nn * mean(dd_con$N_ACC/dd_con$nn)

#' Candidate scale parameter for Wishart priors on the marginal variance
V.prior <- matrix(1/10, nrow= 4, ncol = 4)
for(i in c(1:4)) V.prior[i,i] <- 1
scale.fac <- t(chol(V.prior))

#' ----------------------------------------------------------------------------#

## Exploratory analysis --------------------------------------------------------
#'
#' Covariates correlation plot. It has now become 
#' a standard -----------------------------------------------------------------#
ggplot2::ggplot(data = reshape2::melt(cor(X[,-1]))) +
  ggplot2::geom_tile(ggplot2::aes(
    x = .data$Var2, y = .data$Var1, fill = .data$value), color = "black") +
  ggplot2::geom_text(ggplot2::aes(
    x = .data$Var2, y = .data$Var1, label = round(.data$value, 2))) +
  ggplot2::scale_fill_gradient2(
    low = "blue", mid = "white", high = "red", midpoint = 0) +
  ggplot2::theme_minimal()


#' Before more involved analysis let us consider the nonspatial model ---------#
cav_nosp_glm <- glm(
  N_ACC ~ 0 + Y_2021 + Y_2022 + Y_2023 + Y_2024 +TEP_th + ELI + PGR + UIS + ELL + PDI + ER, 
  family = "poisson", offset = log(nn), data = dd_con)

#' Model with offset ----------------------------------------------------------#
cav_nosp_inla <- inla(
  N_ACC ~ 0 + Y_2021 + Y_2022 + Y_2023 + Y_2024 + TEP_th + ELI + PGR + UIS + ELL + PDI + ER,
  offset = log(nn),
  family = "poisson", data =dd_con,
  num.threads = 1, 
  control.compute = list(internal.opt = F, cpo = T, waic = T, config = T), 
  verbose = T)

#' Model with expected accesses - equivalent in theory ------------------------#
cav_nosp_inla_E <- inla(
  N_ACC ~ 0 + Y_2021 + Y_2022 + Y_2023 + Y_2024+TEP_th + ELI + PGR + UIS + ELL + PDI + ER,
  E = Exp_Acc,
  family = "poisson", data = dplyr::mutate(dd_con, Exp_Acc = Exp_Acc),
  num.threads = 1, 
  control.compute = list(internal.opt = F, cpo = T, waic = T, config = T), 
  verbose = T)

cav_nosp_inla.alt <- inla(
  N_ACC ~ 0 + Y_2021 + Y_2022 + Y_2023 + CAV + ELI + PGR + UIS + ELL + PDI + ER,
  offset = log(nn),
  family = "poisson", data =dd_con,
  num.threads = 1, 
  control.compute = list(internal.opt = F, cpo = T, waic = T, config = T), 
  verbose = T)

#' Univariate models: ICAR
#' 

cav_icar_INLA_2021 <- inla(
  N_ACC ~ 1 + TEP_th + ELI + PGR + UIS + ELL + PDI + ER+ 
    f(c(1:n), model = "besag", graph = W_con, scale.model = T, constr = T),
  offset = log(nn), 
  family = "poisson", data =dd_con[c(1:n),],
  num.threads = 1, control.compute = list(internal.opt = F, cpo = T, waic = T, config = T), 
  verbose = T)

cav_icar_INLA_2022 <- inla(
  N_ACC ~ 1 + TEP_th + ELI + PGR + UIS + ELL + PDI + ER+ 
    f(c(1:n), model = "besag", graph = W_con, scale.model = T, constr = T),
  offset = log(nn), 
  family = "poisson", data =dd_con[n+c(1:n),],
  num.threads = 1, control.compute = list(internal.opt = F, cpo = T, waic = T, config = T), 
  verbose = T)

cav_icar_INLA_2023 <- inla(
  N_ACC ~ 1 + TEP_th + ELI + PGR + UIS + ELL + PDI + ER+ 
    f(c(1:n), model = "besag", graph = W_con, scale.model = T, constr = T),
  offset = log(nn), 
  family = "poisson", data =dd_con[n+n+c(1:n),],
  num.threads = 1, control.compute = list(internal.opt = F, cpo = T, waic = T, config = T), 
  verbose = T)

cav_icar_INLA_2024 <- inla(
  N_ACC ~ 1 + TEP_th + ELI + PGR + UIS + ELL + PDI + ER+ 
    f(c(1:n), model = "besag", graph = W_con, scale.model = T, constr = T),
  offset = log(nn), 
  family = "poisson", data =dd_con[n+n+n+c(1:n),],
  num.threads = 1, control.compute = list(internal.opt = F, cpo = T, waic = T, config = T), 
  verbose = T)


#' LCAR ------------------------------------------------------------------------
cav_lcar_INLA_2021 <- inla(
  N_ACC ~ 1 + TEP_th + ELI + PGR + UIS + ELL + PDI + ER+ 
    f(c(1:n), model = "besagproper2", graph = W_con, constr = T),
  offset = log(nn), 
  family = "poisson", data =dd_con[c(1:n),],
  num.threads = 1, control.compute = list(internal.opt = F, cpo = T, waic = T, config = T), 
  verbose = T)

cav_lcar_INLA_2022 <- inla(
  N_ACC ~ 1 + TEP_th + ELI + PGR + UIS + ELL + PDI + ER+ 
    f(c(1:n), model = "besagproper2", graph = W_con, constr = T),
  offset = log(nn), 
  family = "poisson", data =dd_con[n+c(1:n),],
  num.threads = 1, control.compute = list(internal.opt = F, cpo = T, waic = T, config = T), 
  verbose = T)

cav_lcar_INLA_2023 <- inla(
  N_ACC ~ 1 + TEP_th + ELI + PGR + UIS + ELL + PDI + ER+ 
    f(c(1:n), model = "besagproper2", graph = W_con, constr = T),
  offset = log(nn), 
  family = "poisson", data =dd_con[n+n+c(1:n),],
  num.threads = 1, control.compute = list(internal.opt = F, cpo = T, waic = T, config = T), 
  verbose = T)

cav_lcar_INLA_2024 <- inla(
  N_ACC ~ 1 + TEP_th + ELI + PGR + UIS + ELL + PDI + ER+ 
    f(c(1:n), model = "besagproper2", graph = W_con, constr = T),
  offset = log(nn), 
  family = "poisson", data =dd_con[n+n+n+c(1:n),],
  num.threads = 1, control.compute = list(internal.opt = F, cpo = T, waic = T, config = T), 
  verbose = T)


#' BYM -------------------------------------------------------------------------

cav_bym_INLA_2021 <- inla(
  N_ACC ~ 1 + TEP_th + ELI + PGR + UIS + ELL + PDI + ER+ 
    f(c(1:n), model = "bym2", graph = W_con, scale.model = T),
  offset = log(nn), 
  family = "poisson", data =dd_con[c(1:n),],
  num.threads = 1, control.compute = list(internal.opt = F, cpo = T, waic = T, config = T), 
  verbose = T)


cav_bym_INLA_2022 <- inla(
  N_ACC ~ 1 + TEP_th + ELI + PGR + UIS + ELL + PDI + ER+ 
    f(c(1:n), model = "bym2", graph = W_con, scale.model = T),
  offset = log(nn), 
  family = "poisson", data =dd_con[n+c(1:n),],
  num.threads = 1, control.compute = list(internal.opt = F, cpo = T, waic = T, config = T), 
  verbose = T)

cav_bym_INLA_2023 <- inla(
  N_ACC ~ 1 + TEP_th + ELI + PGR + UIS + ELL + PDI + ER+ 
    f(c(1:n), model = "bym2", graph = W_con, scale.model = T),
  offset = log(nn), 
  family = "poisson", data =dd_con[n+n+c(1:n),],
  num.threads = 1, control.compute = list(internal.opt = F, cpo = T, waic = T, config = T), 
  verbose = T)

cav_bym_INLA_2024 <- inla(
  N_ACC ~ 1 + TEP_th + ELI + PGR + UIS + ELL + PDI + ER+ 
    f(c(1:n), model = "bym2", graph = W_con, scale.model = T),
  offset = log(nn), 
  family = "poisson", data =dd_con[n+n+n+c(1:n),],
  num.threads = 1, control.compute = list(internal.opt = F, cpo = T, waic = T, config = T), 
  verbose = T)

## Spatial analysis: Block-factorisable models ---------------------

#' Simplest spatial model: ICAR -----------------------------------------------#


cav_IMCAR_inla <- inla(
  N_ACC ~ 0 + Y_2021 + Y_2022 + Y_2023 + Y_2024 + TEP_th + ELI + PGR + UIS + ELL + PDI + ER+ 
    f(ID, model = inla.IMCAR.Bartlett(k = 4, W = W_con, df=6), 
      extraconstr = list(A = A_constr, e = c(0,0,0,0))),
  offset = log(nn), family = "poisson", data =dd_con,
  #inla.mode = "classic", control.inla = list(strategy = "laplace", int.strategy = "grid"),
  num.threads = 1, control.compute = list(internal.opt = F, cpo = T, waic = T, config = T), 
  verbose = T)


cav_IMCAR_inla.scale.fac <- inla(
  N_ACC ~ 0 + Y_2021 + Y_2022 + Y_2023 + Y_2024 + TEP_th + ELI + PGR + UIS + ELL + PDI + ER+ 
    f(ID, model = inla.IMCAR.Bartlett(k = 4, W = W_con, df=6, scale.fac = scale.fac/sqrt(6) ), 
      extraconstr = list(A = A_constr, e = c(0,0,0,0))),
  offset = log(nn), family = "poisson", data =dd_con,
  #inla.mode = "classic", control.inla = list(strategy = "laplace", int.strategy = "grid"),
  num.threads = 1, control.compute = list(internal.opt = F, cpo = T, waic = T, config = T), 
  verbose = T)


cav_IMCAR_inla_ls <- inla(
  N_ACC ~ 0 + dd_list$intercept + dd_list$TEP_th + dd_list$ELI + dd_list$PGR + dd_list$UIS + dd_list$ELL + dd_list$PDI + dd_list$ER+ 
    f(ID, model = inla.IMCAR.Bartlett(k = 4, W = W_con, df=6), 
      extraconstr = list(A = A_constr, e = c(0,0,0,0))),
  offset = log(nn), family = rep("poisson",4), data =dd_list,
  #inla.mode = "classic", control.inla = list(strategy = "laplace", int.strategy = "grid"),
  num.threads = 1, control.compute = list(internal.opt = F, cpo = T, waic = T, config = T), 
  verbose = T)



Mmodel_compute_cor_bigDM(cav_IMCAR_inla.W, J=4)[c(1,3)]

#' ----------------------------------------------------------------------------#
#' The simplest way to take into account the three different years
#' is defining a multivariate model in which each year corresponds
#' to a different dependent variable.
#' We start with the PCAR, as outlined in Gelfand and Vounatsu (2003)
#' and the LCAR by Leroux et al. (2000)
#' Unique spatial autocorrelation/mixing parameter for the three 
#' target variables -----------------------------------------------------------#


#' PMCAR model ----------------------------------------------------------------# 


#' CRASH 

 

cav_PMCAR_inla.W <- inla(
  N_ACC ~ 0 + Y_2021 + Y_2022 + Y_2023 + Y_2024 + 
    ELI + PGR + UIS + ELL + PDI + ER+ 
    f(ID, model = inla.PMCAR.Bartlett(W = W_con, k = 4,  df = 6, Wishart.on.scale = T)),
  offset = log(nn), family = "poisson", data = dd_con,
  #inla.mode = "classic", control.inla = list(strategy = "laplace", int.strategy = "grid"),
  num.threads = 1, control.compute = list(internal.opt = F, cpo = T, waic = T, config = T, dic = T), 
  verbose = T)


cav_PMCAR_inla.IW <- inla(
  N_ACC ~ 0 + Y_2021 + Y_2022 + Y_2023 + Y_2024 + 
    ELI + PGR + UIS + ELL + PDI + ER+ 
    f(ID, model = inla.PMCAR.Bartlett(W = W_con, k = 4,  df = 6, Wishart.on.scale = F)),
  offset = log(nn), family = "poisson", data = dd_con,
  #inla.mode = "classic", control.inla = list(strategy = "laplace", int.strategy = "grid"),
  num.threads = 1, control.compute = list(internal.opt = F, cpo = T, waic = T, config = T, dic = T), 
  verbose = T)


#' Autocorrelation seems high. Still, remind the CPO problem:
#' may it be a red flag for INLA not doing well?
Mmodel_compute_cor_bigDM(cav_PMCAR_inla.W,  J=4)[c(1,3)]
Mmodel_compute_cor_bigDM(cav_PMCAR_inla.IW, J=4)[c(1,3)]

inla.zmarginal(inla.tmarginal(
  fun = function(X) 1/(1 + exp(-X)),
  marginal = cav_PMCAR_inla.W$marginals.hyperpar[[1]]))



cav_LMCAR_inla.W <- inla(
  N_ACC ~ 0 + Y_2021 + Y_2022 + Y_2023 + Y_2024 + 
    TEP_th + ELI + PGR + UIS + ELL + PDI + ER+ 
    f(ID, model = inla.LMCAR.Bartlett(k = 4, W = W_con, Wishart.on.scale = T, df = 6)),
  offset = log(nn),
  family = "poisson", data =dd_con,
  #inla.mode = "classic", control.inla = list(strategy = "laplace", int.strategy = "grid"),
  num.threads = 1, control.compute = list(internal.opt = F, cpo = T, waic = T, config = T, dic = T), 
  verbose = T)

cav_LMCAR_inla.IW <- inla(
  N_ACC ~ 0 + Y_2021 + Y_2022 + Y_2023 + Y_2024 + 
    TEP_th + ELI + PGR + UIS + ELL + PDI + ER+ 
    f(ID, model = inla.LMCAR.Bartlett(k = 4, W = W_con, Wishart.on.scale = F, df = 6)),
  offset = log(nn),
  family = "poisson", data =dd_con,
  #inla.mode = "classic", control.inla = list(strategy = "laplace", int.strategy = "grid"),
  num.threads = 1, control.compute = list(internal.opt = F, cpo = T, waic = T, config = T, dic = T), 
  verbose = T)


#' Mixing parameter (hard to interpret etcetera) ------------------------------#

inla.zmarginal(inla.tmarginal(
  fun = function(X) 1/(1 + exp(-X)),
  marginal = cav_LMCAR_inla.IW$marginals.hyperpar[[1]]))

   
 


## ----------------------------------------------------------------------------#
#
#' Multivariate BYM ATTEMPT - Warning: slow -----------------------------------#



#' Warning: using a 'prudential' initial value for logit(phi)
#' can make this already slow model even slower; still, we want to rule out overestimation.

cav_MBYM_inla.dense <- inla(
  N_ACC ~ 0 + Y_2021 + Y_2022 + Y_2023 + Y_2024 +
    TEP_th + ELI + PGR + UIS + ELL + PDI + ER+ 
    f(ID, model = inla.MBYM.Bartlett(k = 4, W = W_con, PC = FALSE, sparse =FALSE, df = 6,
                                     Wishart.on.scale = T)) ,
  offset = log(nn),
  family = "poisson", data =dd_con,
  #control.fixed = list(prec = list(Intercept1 = 0, Intercept2 = 0, Intercept3 = 0)),
  #inla.mode = "classic", control.inla = list(strategy = "laplace", int.strategy = "grid"),
  num.threads = 1, control.compute = list(internal.opt = F, cpo = T, waic = T, config = T), 
  verbose = T)


cav_MBYM_inla.W <- inla(
  N_ACC ~ 0 + Y_2021 + Y_2022 + Y_2023 + Y_2024 +
    TEP_th + ELI + PGR + UIS + ELL + PDI + ER+ 
    f(ID, model = inla.MBYM.Bartlett(k = 4, W = W_con, PC = FALSE, df = 6,
                                     sparse = TRUE, Wishart.on.scale = T),
      extraconstr = constr.BYM) ,
  offset = log(nn),
  family = "poisson", data =dd_con,
  num.threads = 1, control.compute = list(internal.opt = F, cpo = T, waic = T, config = T), 
  verbose = T)

cav_MBYM_inla.IW <- inla(
  N_ACC ~ 0 + Y_2021 + Y_2022 + Y_2023 + Y_2024 +
    TEP_th + ELI + PGR + UIS + ELL + PDI + ER+ 
    f(ID, model = inla.MBYM.Bartlett(k = 4, W = W_con, PC = FALSE, df = 6,
                                     sparse = TRUE, Wishart.on.scale = F),
      extraconstr = constr.BYM) ,
  offset = log(nn),
  family = "poisson", data =dd_con,
  num.threads = 1, control.compute = list(internal.opt = F, cpo = T, waic = T, config = T), 
  verbose = T)


inla.zmarginal(inla.tmarginal(
  fun = function(X) 1/(1 + exp(-X)),
  marginal = cav_MBYM_inla.W$marginals.hyperpar[[1]]))

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


## Comparison using LGOCV -----------------------------------------------------


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

## TBD TBD TBD Multivariate Pogit analysis ATTEMPT ---------------------------------
 

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

## M-Model attempt --------------------------------------------------------


#' M-model defined using the bigDM package
inla.IMCAR.bigDM  <- function(...) INLA::inla.rgeneric.define(bigDM::Mmodel_icar, ...)
inla.PMMCAR.bigDM <- function(...) INLA::inla.rgeneric.define(bigDM::Mmodel_pcar, ... )
inla.LMMCAR.bigDM <- function(...) INLA::inla.rgeneric.define(bigDM::Mmodel_lcar, ... )
 




cav_IMMCAR_inla <- inla(
  N_ACC ~ 0+ Y_2021 + Y_2022 + Y_2023 + Y_2024 + TEP_th + ELI + PGR + UIS + ELL + PDI + ER + 
    f(ID, model = inla.IMCAR.bigDM(J = 4, W = W_con, initial.values = rep(0.1, 10)),
      extraconstr = list(A = A_constr, e = rep(0, nrow(A_constr)))  ),
  offset = log(nn),
  family = "poisson", data =dd_con,
  num.threads = 1, control.compute = list(internal.opt = F, cpo = T, waic = T, config = T), 
  verbose = T)

##' PCAR model 

#' 
cav_PMMCAR_bigDM <- inla(
  N_ACC ~ 0+ Y_2021 + Y_2022 + Y_2023 + Y_2024 +TEP_th + ELI + PGR + UIS + ELL + PDI + ER+ 
    f(ID, model = inla.PMMCAR.bigDM(J = 4, W = W_con,  alpha.min = 0,alpha.max = 1,
                                    initial.values = rep(0, 10))),
  offset = log(nn),
  family = "poisson", data =dd_con,
  #control.fixed = list(prec = list(Intercept1 = 0, Intercept2 = 0, Intercept3 = 0)),
  #inla.mode = "classic", control.inla = list(strategy = "laplace", int.strategy = "grid"),
  num.threads = 1, control.compute = list(internal.opt = F, cpo = T, waic = T, config = T), 
  verbose = T)

cav_PMMCAR_unif.W <- inla(
  N_ACC ~  0+ Y_2021 + Y_2022 + Y_2023 + Y_2024 +
    TEP_th + ELI + PGR + UIS + ELL + PDI + ER+ 
    f(ID, model = inla.PMMCAR.model(k = 4, W = W_con, df = 4, PC = F, 
                                    Wishart.on.scale = T,
                                    initial.values = c(0, 0, 0, 0, cav_PMCAR_inla.IW$summary.hyperpar$mode[-1]))),
  offset = log(nn),
  family = "poisson", data =dd_con,
  num.threads = 1, control.compute = list(internal.opt = F, cpo = T, waic = T, config = T), 
  verbose = T)


cav_PMMCAR_unif.W.pc <- inla(
  N_ACC ~  0+ Y_2021 + Y_2022 + Y_2023 + Y_2024 +
    TEP_th + ELI + PGR + UIS + ELL + PDI + ER+ 
    f(ID, model = inla.PMMCAR.model(k = 4, W = W_con, df = 6, PC = T, 
                                    initial.values = c(0, 0, 0, 0, cav_IMCAR_inla$summary.hyperpar$mode)  )),
  offset = log(nn),
  family = "poisson", data =dd_con,
  num.threads = 1, control.compute = list(internal.opt = F, cpo = T, waic = T, config = T), 
  verbose = T)


 
#' Interpret hyperparameters --------------------------------------------------#

data.frame(do.call(rbind, lapply(
  lapply(cav_PMMCAR_bigDM$marginals.hyperpar[c(1,2,3,4)], function(f){
    inla.tmarginal(fun = function(X) 1/(1 + exp(-X)), marginal = f)
  }), function(x) unlist(inla.zmarginal(x, silent = TRUE))))) %>% 
  dplyr::select(1,2,3,5,7)

Mmodel_compute_cor_bigDM(cav_PMMCAR_bigDM, J=4)[c(1,3)]




cav_LMMCAR_bigDM <- inla(
  N_ACC ~ 0+ Y_2021 + Y_2022 + Y_2023 +Y_2024 +
    +TEP_th + ELI + PGR + UIS + ELL + PDI + ER+ 
    f(ID, model = inla.LMMCAR.bigDM(J = 4, W = W_con,  alpha.min = 0,alpha.max = 1,
                                       initial.values = rep(0, 10))),
  offset = log(nn),
  family = "poisson", data =dd_con,
  num.threads = 1, control.compute = list(internal.opt = F, cpo = T, waic = T, config = T), 
  verbose = T)


cav_LMMCAR_unif <- inla(
  N_ACC ~ 0+ Y_2021 + Y_2022 + Y_2023 + Y_2024 + 
    TEP_th + ELI + PGR + UIS + ELL + PDI + ER+ 
    f(ID, model = inla.LMMCAR.model(k = 4, W = W_con, 
                                    df = 6, PC = F )),
  offset = log(nn),
  family = "poisson", data =dd_con, 
  num.threads = 1, control.compute = list(internal.opt = F, cpo = T, waic = T, config = T), 
  verbose = T)


cav_LMMCAR_pc  <- inla(
  N_ACC ~ 0+ Y_2021 + Y_2022 + Y_2023 + Y_2024+
    TEP_th + ELI + PGR + UIS + ELL + PDI + ER+ 
    f(ID, model = inla.LMMCAR.model(k = 4, W = W_con,  df = 6, PC = T,
                                    initial.values = c(0, 0, 0, 0, cav_IMCAR_inla$summary.hyperpar$mode))),
  offset = log(nn),
  family = "poisson", data =dd_con, 
  num.threads = 1, control.compute = list(internal.opt = F, cpo = T, waic = T, config = T), 
  verbose = T)


cav_LMMCAR_pc_strict <- inla(
  N_ACC ~ 0+ Y_2021 + Y_2022 + Y_2023 +TEP_th + ELI + PGR + UIS + ELL + PDI + ER+ 
    f(ID, model = inla.LMMCAR.model(k = 3, W = W_con, Bartlett =T, df = 5, PC = T,
                                    alpha  = 9/10, U = 2/3)),
  offset = log(nn),
  family = "poisson", data =dd_con, 
  num.threads = 1, control.compute = list(internal.opt = F, cpo = T, waic = T, config = T), 
  verbose = T)



data.frame(do.call(rbind, lapply(
  lapply(cav_LMMCAR_bigDM$marginals.hyperpar[c(1,2,3)], function(f){
    inla.tmarginal(fun = function(X) 1/(1 + exp(-X)), marginal = f)
  }), function(x) unlist(inla.zmarginal(x, silent = TRUE))))) %>% 
  dplyr::select(1,2,3,5,7)

Mmodel_compute_cor_bigDM(cav_LMMCAR_bigDM, J=3)[c(1,3)]

#' More general version covering also LCAR and sparse BYM:
#cav_MmodPCAR_inla_bigDM_copy <- inla(
#  N_ACC ~ 1 +TEP_th + ELI + PGR + UIS + ELL + PDI + ER+ 
#    f(ID_ym, model = inla.Mmodel(k = 3, W = W_con, Qmod = "PCAR", Bartlett = TRUE)),
#  offset = log(nn),
#  family = "poisson", data =dd_long,
#  #control.fixed = list(prec = list(Intercept1 = 0, Intercept2 = 0, Intercept3 = 0)),
#  #inla.mode = "classic", control.inla = list(strategy = "laplace", int.strategy = "grid"),
#  num.threads = 1, control.compute = list(internal.opt = F, cpo = T,  waic = T, config = T), 
#  verbose = T)


#cav_MmodPCAR_inla_spatplus <- inla(
#  N_ACC ~ 1 +TEP_th + ELI+ PGR + UIS + ELL_nosp_resid  + PDI + ER_nosp_resid+ 
#    f(ID, model = inla.Mmodel(k = 3, W = W_con, Qmod = "PCAR")),
#  offset = log(nn),
#  family = rep("poisson", 3), data =dd_list,
#  #control.fixed = list(prec = list(Intercept1 = 0, Intercept2 = 0, Intercept3 = 0)),
#  #inla.mode = "classic", control.inla = list(strategy = "laplace", int.strategy = "grid"),
#  num.threads = 1, control.compute = list(internal.opt = F, cpo = T,  waic = T, config = T), 
#  verbose = T)

#cav_MmodLCAR_inla <- inla(
#  N_ACC ~ 1 +TEP_th + ELI + PGR + UIS + ELL + PDI + ER+ 
#    f(ID, model = inla.Mmodel(k = 3, W = W_con,  Qmod = "LCAR")),
#  offset = log(nn),
#  family = rep("poisson", 3), data =dd_list,
#  #control.fixed = list(prec = list(Intercept1 = 0, Intercept2 = 0, Intercept3 = 0)),
#  #inla.mode = "classic", control.inla = list(strategy = "laplace", int.strategy = "grid"),
#  num.threads = 1, control.compute = list(internal.opt = F, cpo = T,  waic = T, config = T), 
#  verbose = T)

#cav_MmodBYM_inla <- inla(
#  N_ACC ~ 0 + Intercept + TEP_th + ELI + PGR + UIS + ELL + PDI + ER+ 
#    f(ID, model = inla.Mmodel(k = 3, W = W_con, Qmod = "BYM"),
#      extraconstr = list(A = A_constr, e = rep(0, 3))),
#  offset = log(nn),
#  family = rep("poisson", 3), data =dd_list,
#  #control.fixed = list(prec = list(Intercept1 = 0, Intercept2 = 0, Intercept3 = 0)),
#  #inla.mode = "classic", control.inla = list(strategy = "laplace", int.strategy = "grid"),
#  num.threads = 1, control.compute = list(internal.opt = F, cpo = T,  waic = T, config = T), 
#  verbose = T)
##' crashes and results have terrible metrics - waic 2914.96

#' Extremely slow but at least it seems to work:
#cav_MmodBYM_inla_panel <- inla(
#  N_ACC ~ 0 +TEP_th + ELI + PGR + UIS + ELL + PDI + ER+ 
#    f(Year, model = "iid", hyper = list(prec = list(initial = 1e-3, fixed = TRUE))) +
#    f(ID_ym, model = inla.Mmodel(k = 3, W = W_con, Qmod = "BYM"),
#      extraconstr = list(A = A_constr, e = rep(0, 3))),
#  offset = log(nn),
#  family = "poisson", data =dd_long,
#  #control.fixed = list(prec = list(Intercept1 = 0, Intercept2 = 0, Intercept3 = 0)),
#  #inla.mode = "classic", control.inla = list(strategy = "laplace", int.strategy = "grid"),
#  num.threads = 1, control.compute = list(internal.opt = F, cpo = T, waic = T, config = T), 
#  verbose = T)



source("Auxiliary/Functions.R")



#' 'Dense' parametrisation, i.e. only the convolution process is modelled -----#
#'                !!! Enable early stop, e.g.:
#'   > Enable early_stop ff < f0: 1612.813420 < 1612.814101 (diff 0.000681048)
#'   > Early stop. Mode not found sufficiently accurate f0=[1612.814101] f_best=[1612.813420] local.value=[1612.813420]
#'  The marginal prior of the latent effect is proper -------------------------#
#'  
#'  NOT RUn - extremely slow --------------------------------------------------#
cav_MmodBYM_inla_dense <- inla(
  N_ACC ~ 0 + Y_2021 + Y_2022 + Y_2023 +TEP_th + ELI + PGR + UIS + ELL + PDI + ER+ 
    f(ID, model = inla.MMBYM.model(k = 3, W = W_con, sparse =F, PC= F, df = 5) ),
  offset = log(nn),
  family = "poisson", data =dd_con,
  #control.fixed = list(prec = list(Intercept1 = 0, Intercept2 = 0, Intercept3 = 0)),
  #inla.mode = "classic", control.inla = list(strategy = "laplace", int.strategy = "grid"),
  num.threads = 1, control.compute = list(internal.opt = F, cpo = T, waic = T, config = T), 
  verbose = T)

#'  > summary(cav_MmodBYM_inla_dense)
#' Time used:
#'  Pre = 1.38, Running = 914, Post = 4.36, Total = 920 
#' Fixed effects:
#'  mean    sd 0.025quant 0.5quant 0.975quant   mode kld
#' Y_2021 -7.399 0.049     -7.496   -7.399     -7.305 -7.399   0
#' Y_2022 -7.598 0.058     -7.714   -7.597     -7.485 -7.598   0
#' Y_2023 -7.181 0.048     -7.276   -7.181     -7.088 -7.181   0
#' TEP_th -0.246 0.036     -0.317   -0.246     -0.176 -0.246   0
#' ELI    -0.024 0.033     -0.087   -0.024      0.040 -0.024   0
#' PGR     0.094 0.037      0.022    0.094      0.167  0.094   0
#'  UIS     0.020 0.033     -0.045    0.020      0.085  0.020   0
#'  ELL    -0.227 0.043     -0.310   -0.226     -0.143 -0.226   0
#'  PDI    -0.013 0.040     -0.093   -0.013      0.066 -0.013   0
#' ER     -0.278 0.049     -0.374   -0.278     -0.182 -0.278   0

#' Random effects:
#'  Name	  Model
#'  ID RGeneric2

#'  Model hyperparameters:
#'    mean    sd 0.025quant 0.5quant 0.975quant   mode
#' Theta1 for ID  0.593 1.599     -2.975    0.734      3.217  1.470
#' Theta2 for ID -0.416 1.334     -3.296   -0.347      1.927  0.064
#' Theta3 for ID -0.039 0.666     -1.266   -0.065      1.352 -0.189
#' Theta4 for ID  0.560 0.418     -0.249    0.555      1.398  0.534
#' Theta5 for ID  0.383 0.467     -0.542    0.386      1.296  0.395
#' Theta6 for ID -0.643 0.122     -0.876   -0.645     -0.396 -0.655
#' Theta7 for ID -0.986 0.804     -2.532   -0.999      0.635 -1.054
#' Theta8 for ID  1.282 1.219     -1.036    1.256      3.762  1.136
#' Theta9 for ID  1.152 0.601     -0.032    1.152      2.333  1.155

#'Watanabe-Akaike information criterion (WAIC) ...: 2921.93
#'Effective number of parameters .................: 202.48

#Marginal log-Likelihood:  -1611.61 


#' Mixing posterior: ok -------------------------------------------------------#
#'                    mean        sd quant0.025  quant0.5 quant0.975
#' Theta1 for ID 0.6136884 0.2774067 0.05037687 0.6865006  0.9607055
#' Theta2 for ID 0.4302582 0.2448343 0.03687192 0.4242366  0.8707938
#' Theta3 for ID 0.4899396 0.1511219 0.22151810 0.4813382  0.7918091

#' Variance posterior - weird!! -----------------------------------------------#
#'  > Mmodel_compute_cor_bigDM(cav_MmodBYM_inla_dense, J=3)[c(1,3)]
#' $summary.cor
#' mean        sd 0.025quant   0.5quant 0.975quant
#' rho12 -0.4424220 0.3571116 -0.8858624 -0.5284553  0.5409819
#' rho13  0.5363203 0.4356559 -0.5232013  0.6879010  0.9849486
#' rho23  0.2830047 0.4708118 -0.6866583  0.3410534  0.9587893

#'   $summary.var
#' mean       sd 0.025quant 0.5quant 0.975quant
#' var1 4.365016 4.479759  0.5452476 3.013357   16.18146
#' var2 4.934897 4.616448  0.4598091 3.623166   17.01168
#' var3 5.088716 3.655035  0.9224199 4.152399   14.91578





#'  Sparse parametrisation attempt

#' Uniform prior --------------------------------------------------------------#

cav_MMBYM_INLA.unif <- inla(
  N_ACC ~ 0 + Y_2021 + Y_2022 + Y_2023 + Y_2024 +
    TEP_th + ELI + PGR + UIS + ELL + PDI + ER+ 
    f(ID, model = inla.MMBYM.model(k = 4, W = W_con,  PC= F, df = 6 ,
                                   initial.values = c(0, 0, 0, 0, cav_IMCAR_inla$summary.hyperpar$mode)), 
      extraconstr = constr.BYM),
  offset = log(nn),
  family = "poisson", data =dd_con,
  num.threads = 1, control.compute = list(internal.opt = F, cpo = T, waic = T, config = T), 
  verbose = T)

cav_MMBYM_INLA.scale.fac <- inla(
  N_ACC ~ 0 + Y_2021 + Y_2022 + Y_2023 + Y_2024 +
    TEP_th + ELI + PGR + UIS + ELL + PDI + ER+ 
    f(ID, model = inla.MMBYM.model(k = 4, W = W_con,  PC= F, df = 6 ,
                                   initial.values = c(1, 1, 1, 1, cav_IMCAR_inla$summary.hyperpar$mode),
                                   scale.fac = scale.fac ), 
      extraconstr = constr.BYM),
  offset = log(nn), control.inla = list(h = 1e-5),
  family = "poisson", data =dd_con,
  num.threads = 1, control.compute = list(internal.opt = F, cpo = T, waic = T, config = T), 
  verbose = T)

cav_MMBYM_INLA.scale.fac.PC <- inla(
  N_ACC ~ 0 + Y_2021 + Y_2022 + Y_2023 + Y_2024 +
    TEP_th + ELI + PGR + UIS + ELL + PDI + ER+ 
    f(ID, model = inla.MMBYM.model(k = 4, W = W_con,  PC= T, df = 6 ,
                                   initial.values = c(1, 1, 1, 1, cav_IMCAR_inla$summary.hyperpar$mode),
                                   scale.fac = scale.fac ), 
      extraconstr = constr.BYM),
  offset = log(nn), control.inla = list(h = 1e-5),
  family = "poisson", data =dd_con,
  num.threads = 1, control.compute = list(internal.opt = F, cpo = T, waic = T, config = T), 
  verbose = T)

cav_MMBYM_INLA.scale.fac.div <- inla(
  N_ACC ~ 0 + Y_2021 + Y_2022 + Y_2023 + Y_2024 +
    TEP_th + ELI + PGR + UIS + ELL + PDI + ER+ 
    f(ID, model = inla.MMBYM.model(k = 4, W = W_con,  PC= F, df = 6 ,
                                   initial.values = c(0, 0, 0, 0, cav_IMCAR_inla$summary.hyperpar$mode),
                                   scale.fac = scale.fac/sqrt(6) ), 
      extraconstr = constr.BYM),
  offset = log(nn), control.inla = list(h = 5e-5),
  family = "poisson", data =dd_con,
  num.threads = 1, control.compute = list(internal.opt = F, cpo = T, waic = T, config = T), 
  verbose = T)






data.frame(do.call(rbind, lapply(
  lapply(cav_MMBYM_inla_sparse$marginals.hyperpar[c(1,2,3)], function(f){
    inla.tmarginal(fun = function(X) 1/(1 + exp(-X)), marginal = f)
  }), function(x) unlist(inla.zmarginal(x, silent = TRUE))))) %>% 
  dplyr::select(1,2,3,5,7)

Mmodel_compute_cor_bigDM(cav_MMBYM_inla.unif, J=4)[c(1,3)]
#init <- c(-0.8146, 1.0785, 4.3659, -0.7363, -0.8948, -0.5576, 0.3971, 0.1238, -0.0215) 


#' PC - prior -----------------------------------------------------------------#
cav_MMBYM_inla_sparse.pc <- inla(
  N_ACC ~ 0 + Y_2021 + Y_2022 + Y_2023 + Y_2024 + TEP_th + ELI + PGR + UIS + ELL + PDI + ER+ 
    f(ID, model = inla.MMBYM.model(k = 4, W = W_con, Bartlett = T, PC= T,
                                   df = 6, alpha = 2/3, U = 1/2), 
      extraconstr = constr.BYM),
  offset = log(nn),
  family = "poisson", data =dd_con,
  num.threads = 1, control.compute = list(internal.opt = F, cpo = T, waic = T, config = T), 
  verbose = T)

cav_MMBYM_inla_sparse.pc.rest <- inla(
  N_ACC ~ 0 + Y_2021 + Y_2022 + Y_2023 + TEP_th + ELI + PGR + UIS + ELL + PDI + ER+ 
    f(ID, model = inla.MMBYM.model(k = 3, W = W_con, Bartlett = T, PC= T,
                                   df = 5, alpha = 2/3, U = 1/2), 
      extraconstr = constr.BYM),
  offset = log(nn),
  family = "poisson", data =dd_con,
  num.threads = 1, control.compute = list(internal.opt = F, cpo = T, waic = T, config = T), 
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



## misc ------------------------------------------------------------------------

 #' simulate PCAR
 
alpha.true <- c(0.7, 0.9, 0.5, 0.8)
sigma.true <- c(1.5, 1.8, 1.4, 1.5)
corr.true <- c(0.9, 0.7, 0.6, 0.6, 0.8, 0.6)

R.true <- diag(4)
R.true[lower.tri(R.true)] <- corr.true
R.true[upper.tri(R.true)] <- t(R.true[lower.tri(R.true)])
Scale.true <- diag(sigma.true) %*% R.true %*% diag(sigma.true)
M.true <- diag(sqrt(eigen(Scale.true)$values)) %*% t(eigen(Scale.true)$vectors)

MI.true <- kronecker(solve(M.true), Matrix::Diagonal(n=n, x=1))
blockIW.true <- kronecker(diag(4), D_con) - kronecker(diag(alpha.true), W_con)
var.true <- solve(MI.true %*% blockIW.true %*% Matrix::t(MI.true))
fac <- t(chol(Scale.true))


Z.true <- MASS::mvrnorm(1, mu=rep(0, 4*n), Sigma = var.true)

dd_con %>% dplyr::mutate(Z = Z.true) %>% 
  dplyr::filter(.data$Y_2021==1) %>% ggplot2::ggplot()+
  ggplot2::geom_sf(ggplot2::aes(fill = .data$LN_ACC))+
  ggplot2::scale_fill_viridis_c(na.value = "white")+
  ggplot2::theme_classic()


y.sim <- numeric(length(Z.true))
for(i in seq(length(y.sim))) y.sim[i] <- rpois(n=1, lambda = exp (1+Z.true[i]) )

cav_PMCAR_sim.W<- inla(
  y.sim ~ 1+ 
    f(ID, model = inla.PMCAR.Bartlett(k = 4, W = W_con, df=6, Wishart.on.scale = T )),
  family = "poisson", data = data.frame(y.sim = y.sim, ID = seq(length(y.sim))),
  num.threads = 1, control.compute = list(internal.opt = F, cpo = T, waic = T, config = T), 
  verbose = T)


cav_PMCAR_sim.IW<- inla(
  y.sim ~ 1+ 
    f(ID, model = inla.PMCAR.Bartlett(k = 4, W = W_con, df=6, Wishart.on.scale = F )),
  family = "poisson", data = data.frame(y.sim = y.sim, ID = seq(length(y.sim))),
  num.threads = 1, control.compute = list(internal.opt = F, cpo = T, waic = T, config = T), 
  verbose = T)

Mmodel_compute_cor_bigDM(cav_PMCAR_sim.IW, J=4)[c(1,3)]

cav_PMMCAR_sim.W.0 <- inla(
  y.sim ~ 1+ 
    f(ID, model = inla.PMMCAR.model(k = 4, W = W_con, df=6)),
  family = "poisson", data = data.frame(y.sim = y.sim, ID = seq(length(y.sim))),
  num.threads = 1, control.compute = list(internal.opt = F, cpo = T, waic = T, config = T), 
  verbose = T)

V.prior + diag(x=9/10, ncol = 4)
for(i in c(1:4)) V.prior[i,i] <- 1
scale.fac.prior <- t(chol(V.prior))

cav_PMMCAR_sim.W.1 <- inla(
  y.sim ~ 1+ 
    f(ID, model = inla.PMMCAR.model(k = 4, W = W_con, df=6, scale.fac = scale.fac.prior)),
  family = "poisson", data = data.frame(y.sim = y.sim, ID = seq(length(y.sim))),
  num.threads = 1, control.compute = list(internal.opt = F, cpo = T, waic = T, config = T), 
  verbose = T)

cav_PMMCAR_sim.W.2 <- inla(
  y.sim ~ 1+ 
    f(ID, model = inla.PMMCAR.model(k = 4, W = W_con, df=6, scale.fac = sqrt(1000)*scale.fac.prior)),
  family = "poisson", data = data.frame(y.sim = y.sim, ID = seq(length(y.sim))),
  num.threads = 1, control.compute = list(internal.opt = F, cpo = T, waic = T, config = T), 
  verbose = T)

cav_PMMCAR_sim.IW_init <- inla(
  y.sim ~ 1+ 
    f(ID, model = inla.PMMCAR.model(k = 4, W = W_con, df=6, Wishart.on.scale=F,
                                    initial.values = c(0, 0, 0, 0, cav_PMCAR_sim.IW$summary.hyperpar$mode[-1]))),
  family = "poisson", data = data.frame(y.sim = y.sim, ID = seq(length(y.sim))),
  num.threads = 1, control.compute = list(internal.opt = F, cpo = T, waic = T, config = T), 
  verbose = T)


cav_PMMCAR_sim.W <- inla(
  y.sim ~ 1+ 
    f(ID, model = inla.PMMCAR.bigDM(J = 4, W = W_con,  alpha.min = 0,alpha.max = 1,
                                    initial.values = cav_PMCAR_sim.W$summary.hyperpar$mode[-1])),
  family = "poisson", data = data.frame(y.sim = y.sim, ID = seq(length(y.sim))),
  num.threads = 1, control.compute = list(internal.opt = F, cpo = T, waic = T, config = T), 
  verbose = T)


