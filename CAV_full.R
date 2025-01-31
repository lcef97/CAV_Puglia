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


gridExtra::grid.arrange({
  ggplot2::ggplot() +
    ggplot2::geom_sf(data = dd, 
                     ggplot2::aes(fill = .data$LN_ACC_21))+
    ggplot2::scale_fill_viridis_c(na.value = "white")+
    ggplot2::theme_classic(),
  
  ggplot2::ggplot() +
    ggplot2::geom_sf(data = dd, 
                     ggplot2::aes(fill = .data$LN_ACC_22))+
    ggplot2::scale_fill_viridis_c(na.value = "white")+
    ggplot2::theme_classic(),
  
  ggplot2::ggplot() +
    ggplot2::geom_sf(data = dd, 
                     ggplot2::aes(fill = .data$LN_ACC_23))+
    ggplot2::scale_fill_viridis_c(na.value = "white")+
    ggplot2::theme_classic(),
  nrow = 3, ncol = 1
  
})


## Mapping municipalities from support centers ---------------------------------

# Municipalities hosting a support center:
munWcav <- munWcav <- c (71020,71024,71051,72004,72006,72011,72014,
                         72019,72021,72029,72031,72033,72035,73013,
                         73027,74001,74009,75018,75029,75035,75059,
                         110001,110002,110009)

# Tremiti Islands are a singleton --> need to remove them to perform spatial analysis
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

# This is the dataset we will concretely work on.
# Covariates are all scaled to zero mean and unit variance
dd_con <- dd_con %>% 
  dplyr::left_join(dists_th, by = "PRO_COM") %>% 
  dplyr::mutate(TEP_th = as.vector(scale(.data$TEP_th))) %>% 
  dplyr::mutate(AES = as.vector(scale(.data$AES))) %>% 
  dplyr::mutate(MFI = as.vector(scale(.data$MFI)))  %>% 
  dplyr::mutate(PDI = as.vector(scale(.data$PDI)))  %>% 
  dplyr::mutate(ELL = as.vector(scale(.data$ELL)))  %>% 
  dplyr::mutate(ER = as.vector(scale(.data$ER)))  %>% 
  dplyr::mutate(PGR = as.vector(scale(.data$PGR)))  %>% 
  dplyr::mutate(UIS = as.vector(scale(.data$UIS)))  %>% 
  dplyr::mutate(ELI = as.vector(scale(.data$ELI))) 

# sd of travel time: almost 16 minutes
attr(scale(dists_th$TEP_th), "scaled:scale")

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
glm_all_X <- glm(N_ACC_21 ~ 1 + TEP_th + MFI + AES + PDI + ELL + ER +
                   PGR + UIS + ELI + offset(log(nn21)),
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


# Forward selection attempt: USING THE BIC AS SELECTION CRITERION,
# two covariates are necessary
covariates <- colnames(X)[-1]

cov_selector <- function(year){
  
  if(year == 2021){
    dd_con <- dd_con %>% 
      dplyr::rename(nn = .data$nn21, N_ACC = .data$N_ACC_21)
  } 
  
  if(year == 2022){
    dd_con <- dd_con %>% 
      dplyr::rename(nn = .data$nn22, N_ACC = .data$N_ACC_22)
  }
  
  if(year == 2023){
    dd_con <- dd_con %>% 
      dplyr::rename(nn = .data$nn23, N_ACC = .data$N_ACC_23)
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




