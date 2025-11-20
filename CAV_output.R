#' ---------  Analisi CAV Puglia 2021-24 --------------------------------------#
##  Input ----------------------------------------------------------------------

#' NEVER forget calling magrittr and sf. Never --------------------------------#
#' 
library(magrittr)
library(sf)
#' Also INLA is needful here: better to call it -------------------------------#
library(INLA)
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
#'  Same thing for other years  -----------------------------------------------#
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
#'  ---------------------------------------------------------------------------#   
#'  
#'  Then we download municipality-level data on accesses to support centers,
#'  aggregated from individual-level ones:  -----------------------------------#

load(url("https://github.com/lcef97/CAV_Puglia/raw/main/input/CAV_input_mun_2021.RData"))
load(url("https://github.com/lcef97/CAV_Puglia/raw/main/input/CAV_input_mun_2022.RData"))
load(url("https://github.com/lcef97/CAV_Puglia/raw/main/input/CAV_input_mun_2023.RData"))
load(url("https://github.com/lcef97/CAV_Puglia/raw/main/input/CAV_input_mun_2024.RData"))


CAV_mun_21 <- CAV_mun_21 %>% dplyr::rename(N_ACC_2021 = .data$N_ACC)
CAV_mun_22 <- CAV_mun_22 %>% dplyr::rename(N_ACC_2022 = .data$N_ACC)
CAV_mun_23 <- CAV_mun_23 %>% dplyr::rename(N_ACC_2023 = .data$N_ACC)
CAV_mun_24 <- CAV_mun_24 %>% dplyr::rename(N_ACC_2024 = .data$N_ACC)

#'  2022 municipalities shapefiles, provided by ISTAT.
#'  In theory, a shapefile should be used for each year.
#'  Still, administrative units boundaries are unchanged at least
#'  from 2021 to 2024 ---------------------------------------------------------#

load(url("https://github.com/lcef97/CAV_Puglia/raw/main/input/Shp.RData"))
Centroids <- Shp
sf::st_agr(Centroids) <- "constant"
Centroids <- sf::st_point_on_surface(Centroids)

#'  Function to extract numeric digits from a strings vector (needed to filter age):
nn_extract <- function(string){
  nn <- gregexpr("([0-9])", string)
  ls.out <- regmatches(as.list(string), nn)
  res <- unlist(lapply(ls.out, function(x) as.numeric(paste(x, collapse = ""))))  
  return(res)
}

#'  Female population aged >= 15 years. Source data: 
#'  http://dati.istat.it/Index.aspx?DataSetCode=DCIS_POPRES1#  ----------------#

Popolazione_Puglia_2021 <- readr::read_csv(
  "https://github.com/lcef97/CAV_Puglia/raw/main/input/Popolazione_Puglia_2021.csv") %>% 
  dplyr::select(.data$ITTER107, .data$Territorio, .data$SEXISTAT1, .data$ETA1, .data$Value) %>% 
  dplyr::filter(.data$ETA1 != "TOTAL") %>% 
  dplyr::mutate(ETA1 = nn_extract(.data$ETA1)) %>% 
  dplyr::mutate(ITTER107 = as.numeric(.data$ITTER107))

Popolazione_Puglia_2022 <- readr::read_csv(
  "https://github.com/lcef97/CAV_Puglia/raw/main/input/Popolazione_Puglia_2022.csv") %>% 
  dplyr::select(.data$ITTER107, .data$Territorio, .data$SEXISTAT1, .data$ETA1, .data$Value) %>% 
  dplyr::filter(.data$ETA1 != "TOTAL") %>% 
  dplyr::mutate(ETA1 = nn_extract(.data$ETA1)) %>% 
  dplyr::mutate(ITTER107 = as.numeric(.data$ITTER107))

Popolazione_Puglia_2023 <- readr::read_csv(
  "https://github.com/lcef97/CAV_Puglia/raw/main/input/Popolazione_Puglia_2023.csv") %>% 
  dplyr::select(.data$ITTER107, .data$Territorio, .data$SEXISTAT1, .data$ETA1, .data$Value) %>% 
  dplyr::filter(.data$ETA1 != "TOTAL") %>% 
  dplyr::mutate(ETA1 = nn_extract(.data$ETA1)) %>% 
  dplyr::mutate(ITTER107 = as.numeric(.data$ITTER107))

Popolazione_Puglia_2024 <- readr::read_csv(
  "https://github.com/lcef97/CAV_Puglia/raw/main/input/Popolazione_Puglia_2024.csv",quote = "")  %>% 
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
                      "Popolazione_21", "Popolazione_22", 
                      "Popolazione_23", "Popolazione_24")

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
dd$Log_accesses <- log(dd$N_ACC/dd$nn)
dd$F_accesses <- dd$N_ACC/dd$nn


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
#'
#'
#'
#'  Municipalities hosting a support center:
#'  
#'  #' Dubious status: Castrignano de'Greci (Dafne ==> active until 2022 I guess) 
#'  #' and Cerignola (why missing??)
  munWcav_21 <- c(71024,	71051,
                  72004,	72006,	72011,	72014,	72019,	72021,	72029,	72031,	72035,	
                  73013,	73027,	
                  74001,	74009,	
                  75029,	75035,  75059,
                  110001,	110002,	110009)
  
  
  munWcav_22 <- c(71020, 71024, 71051, 
                  72004, 72006, 72011, 72014, 72019, 72021, 72029, 72031, 72033, 72035, 
                  73013, 73027, 
                  74001, 74009, 
                  75018, 75029, 75035, 75059,
                  110001, 110002, 110009)
  
  munWcav_23 <- c(71020, 71024, 71029, 71051, 
                  72004, 72006, 72011, 72014, 72019, 72021, 72029, 72031, 72033, 72035,
                  73013, 73027, 
                  74001, 74009, 
                  75029, 75035, 75059,
                  110001, 110002, 110009)
  
  munWcav_24 <- c(71020, 71024, 71029, 71051,
                  72004, 72006, 72011, 72014, 72019, 72021, 72029, 72031, 72033, 72035,
                  73013, 73027,
                  74001, 74009, 74019,
                  75029, 75035, 75059,
                  110001, 110002, 110009)
  
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


munWdesk <- c(
  71004,  71006,  71008,  71010,  71012,  71020,  71021,  71022,  71024,  
  71025,  71027,  71028,  71035,  71036,  71038,  71041,  71043,  71046,  
  71047,  71049,  71050,  71051,  71053,  71054,  71055,  71056,  71058,  
  71059,  71060,  71063,  72003,  72006,  72010,  72011,  72012,  72016,  
  72020,  72022,  72023,  72024,  72025,  72027,  72033,  72038,  72039,  
  72041,  72043,  73003,  73007,  73008,  73015,  73019,  73020,  73021,  
  73024,  73027,  73029,  74002,  74003,  74005,  74006,  74007,  74008, 
  74011,  74012,  74014,  74018,  74020,  75002,  75003,  75010,  75012, 
  75016,  75021,  75022,  75024,  75028,  75030,  75031,  75035,  75037,
  75039,  75040,  75043,  75044,  75050,  75051,  75052,  75057,  75063, 
  75064,  75073,  75077,  75081,  75085,  75086,  75088,  75089,  75093, 
  75097, 110003, 110004, 110005, 110007, 110010)

#'dists_desk <- NULL
#'dd24 <- dd %>% dplyr::filter(.data$Year == "2024") %>% 
#'  dplyr::filter(!.data$PRO_COM %in% singletons)
#'for (i in c(1:nrow(dd24))){
#'  X <- dd24$PRO_COM[i]
#'  dists <- numeric(length(munWdesk))
#'  IDs <- numeric(length(munWdesk))
#'  for(j in c(1:length(munWdesk))){
#'    c.out <- munWdesk[j]
#'    id <- paste0(min(X, c.out),
#'                 " - ", max(X, c.out))
#'    nn <- which(dist_short$OR_DEST == id)
#'    dists[j] <- as.numeric(dist_short$TEP_TOT[nn])
#'    IDs[j] <- dist_short$OR_DEST[nn]
#'  }
#'  m <- which.min(dists)
#'  ret <- c(X, dists[m])
#'  dists_desk <- data.frame(rbind(dists_desk, ret))
#'}

load(url("https://github.com/lcef97/CAV_Puglia/raw/main/input/dists_th_21.RData"))
load(url("https://github.com/lcef97/CAV_Puglia/raw/main/input/dists_th_22.RData"))
load(url("https://github.com/lcef97/CAV_Puglia/raw/main/input/dists_th_23.RData"))
load(url("https://github.com/lcef97/CAV_Puglia/raw/main/input/dists_th_24.RData"))
load(url("https://github.com/lcef97/CAV_Puglia/raw/main/input/dists_desk.RData"))

names(dists_th_21)[2] <- names(dists_th_22)[2] <- 
  names(dists_th_23)[2] <- names(dists_th_24)[2] <- "AVC_dist"
distances <- rbind(dists_th_21, dists_th_22, dists_th_23, dists_th_24) %>% 
  dplyr::mutate(Year = c(rep("2021", nrow(dists_th_22)),
                         rep("2022", nrow(dists_th_22)),
                         rep("2023", nrow(dists_th_23)),
                         rep("2024", nrow(dists_th_24))) )

#' Centroids of municipalities with an AVC ------------------------------------#
Centroids_21 <- dplyr::filter(Centroids, PRO_COM %in% munWcav_21)
Centroids_22 <- dplyr::filter(Centroids, PRO_COM %in% munWcav_22)
Centroids_23 <- dplyr::filter(Centroids, PRO_COM %in% munWcav_23)
Centroids_24 <- dplyr::filter(Centroids, PRO_COM %in% munWcav_24)


ggy21 <- ggplot2::ggplot() +
  ggplot2::geom_sf(data = dplyr::filter(dd, .data$Year == "2021"), 
                   ggplot2::aes(fill = .data$Log_accesses))+
  ggplot2::scale_fill_viridis_c(na.value = "white",
                                limits = c(-9.5, -4.6))+
  ggplot2::geom_sf(data = Centroids_21, color = "#00ffff",size  = 1.2) +
  ggplot2::ggtitle("Year: 2021") +
  ggplot2::theme_classic()

ggy22 <- ggplot2::ggplot() +
  ggplot2::geom_sf(data = dplyr::filter(dd, .data$Year == "2022"), 
                   ggplot2::aes(fill = .data$Log_accesses))+
  ggplot2::scale_fill_viridis_c(na.value = "white",
                                limits = c(-9.5, -4.6))+
  ggplot2::geom_sf(data = Centroids_22, color = "#00ffff",size  = 1.2)+
  ggplot2::ggtitle("Year: 2022") +
  ggplot2::theme_classic()

ggy23 <- ggplot2::ggplot() +
  ggplot2::geom_sf(data = dplyr::filter(dd, .data$Year == "2023"), 
                   ggplot2::aes(fill = .data$Log_accesses))+
  ggplot2::scale_fill_viridis_c(na.value = "white",
                                limits = c(-9.5, -4.6))+
  ggplot2::geom_sf(data = Centroids_23, color = "#00ffff",size  = 1.2)+
  ggplot2::ggtitle("Year: 2023") +
  ggplot2::theme_classic()

ggy24 <- ggplot2::ggplot() +
  ggplot2::geom_sf(data = dplyr::filter(dd, .data$Year == "2024"), 
                   ggplot2::aes(fill = .data$Log_accesses))+
  ggplot2::scale_fill_viridis_c(na.value = "white",
                                limits = c(-9.5, -4.6))+
  ggplot2::geom_sf(data = Centroids_24, color = "#00ffff",size  = 1.2)+
  ggplot2::ggtitle("Year: 2024")+
  ggplot2::theme_classic()


gridExtra::grid.arrange(ggy21, ggy22, ggy23, ggy24, nrow = 2, ncol = 2)




#' This is the dataset we will concretely work on.
#' Covariates are all scaled to zero mean and unit variance -------------------#
dd_con <- dd %>% dplyr::filter(!.data$PRO_COM %in% singletons) %>% 
  dplyr::left_join(distances, by = c("PRO_COM", "Year")) %>% 
  dplyr::left_join(dists_desk, by = "PRO_COM") %>% 
  dplyr::mutate(CAV = as.numeric(.data$AVC_dist == 0)) %>% 
  dplyr::mutate(Y_2021 = as.numeric(.data$Year == "2021")) %>% 
  dplyr::mutate(Y_2022 = as.numeric(.data$Year == "2022")) %>% 
  dplyr::mutate(Y_2023 = as.numeric(.data$Year == "2023")) %>% 
  dplyr::mutate(Y_2024 = as.numeric(.data$Year == "2024")) %>% 
  dplyr::mutate(AVC_dist = as.vector(scale(.data$AVC_dist))) %>% 
  dplyr::mutate(AES = as.vector(scale(.data$AES))) %>% 
  dplyr::mutate(Desk_dist = as.vector(scale(.data$Desk_dist))) %>% 
  dplyr::mutate(MFI = as.vector(scale(.data$MFI)))  %>% 
  dplyr::mutate(PDI = as.vector(scale(.data$PDI)))  %>% 
  dplyr::mutate(ELL = as.vector(scale(.data$ELL)))  %>% 
  dplyr::mutate(ER = as.vector(scale(.data$ER)))  %>% 
  dplyr::mutate(PGR = as.vector(scale(.data$PGR)))  %>% 
  dplyr::mutate(UIS = as.vector(scale(.data$UIS)))  %>% 
  dplyr::mutate(ELI = as.vector(scale(.data$ELI)))  %>% 
  dplyr::mutate(Exp = .data$nn * mean(.data$N_ACC/.data$nn)) %>% 
  dplyr::mutate(Year = as.numeric(as.factor(.data$Year))) %>% 
  dplyr::mutate(ID = c(1:nrow(.))) %>% 
  dplyr::mutate(Area = as.numeric(as.factor(.data$PRO_COM)))


##  Model input setup -----------------------------------------------------------
#' 
#' Input data are defined; now we need some auxiliary objects -----------------#
#' 
#' Number of sites saved here -------------------------------------------------#
n <- length(unique(dd_con$PRO_COM))

#' sd of travel time: around 14 minutes ---------------------------------------#
attr(scale(dists_th_22$AVC_dist), "scaled:scale")

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

#'  Necessary to constrain the BYM model --------------------------------------#
constr.BYM <- list(
  A = cbind(Matrix::Matrix(0, nrow = nrow(A_constr), ncol = ncol(A_constr)), A_constr),
  e=c(0,0,0,0) )


#' Full GLM --> for model matrix
glm_all_X <- glm(N_ACC ~ 1 + Desk_dist + AVC_dist + MFI + AES + PDI + ELL + ER +
                   PGR + UIS + ELI + offset(log(nn)),
                 data = dd_con, family = "poisson")
#' model matrix
X <- model.matrix(glm_all_X)


#' Spatial filtering of Laplacian eigenvectors --------------------------------#

#' Speed-up initialising:
#covars <- c("MFI", "HMI", "MWR", "PA", "LRA", "LC", "AES", "PDI",
#            "ELL", "ER", "PGR", "UIS", "ELI","AVC_dist")

#n.eigen <- rep(10, length(covars))
VV <- eigen(Lapl_con)$vectors


#' R function for filtering here ----------------------------------------------#
filter.spatplus <- function(data, covars = c(), V, n.eigen = c(), rescale = T, center = T){
  if(length(n.eigen) != length(covars)) n.eigen <- rep(n.eigen, length(covars))
  trends <- list()
  res <- data
  for(j in c(1:length(covars))){
    xx <- data[c(1:nrow(V)), ] %>% dplyr::select(covars[j])
    m <- which(names(data) == covars[j])
    xx <- as.matrix(sf::st_drop_geometry(xx))
    s <- var(xx)
    coef <- solve(a=V, b=xx)
    eigen.in <- c( 1:(nrow(V) - n.eigen[j]))
    eigen.out <- setdiff(c(1:nrow(V)), eigen.in)#c((nrow(V) - n.eigen[j]+1) : nrow(V) )
    nrep <- nrow(data)/nrow(xx)
    xx.nosp <- rep(V[, eigen.in] %*% coef[eigen.in], nrep)
    xx.nosp <- as.vector(scale(xx.nosp, scale = rescale, center = center))
    xx.sp <-  as.vector(V[, eigen.out] %*% coef[eigen.out])
    trends[[j]] <- xx.sp
    names(trends)[[j]] <- names(data)[m]
    res[,m] <- xx.nosp
  }
  trends.df <- as.data.frame(do.call(cbind, trends))
  return(list(data = res, trends = trends.df))
}

dd_filtered_10 <- filter.spatplus(dd_con, covars=c("ELI", "ELL"), V = VV,
                                  n.eigen = c(10, 10))
dd_filtered_15 <- filter.spatplus(dd_con, covars=c("ELI", "ELL"), V = VV,
                                  n.eigen = c(15, 15))
dd_filtered_20 <- filter.spatplus(dd_con, covars=c("ELI", "ELL"), V = VV,
                                  n.eigen = c(20, 20))
dd_filtered_25 <- filter.spatplus(dd_con, covars=c("ELI", "ELL"), V = VV,
                                  n.eigen = c(25, 25))


#' R function for forest plots ------------------------------------------------#

inla.forestplot.beta <- function(models, covar.in = NULL, model_names = NULL,           
                                 xlab = expression(beta), 
                                 main = "") {
  
  if (is.null(model_names)) model_names <- paste0("Model ", seq_along(models))
  stopifnot(length(model_names) == length(models))
  
  beta.df <- purrr::map2_dfr(models, model_names, ~{
    sf <- .x$summary.fixed %>% as.data.frame() %>%
      tibble::rownames_to_column("Var") %>% dplyr::transmute(
        Var=.data$Var, mean = .data$mean,
        LB  = .data$`0.025quant`, UB  = .data$`0.975quant`, 
      model = .y )  
  })
  
  if(is.null(covar.in)) covar.in <- unique(beta.df$Var)
  beta.df <- beta.df %>% dplyr::filter(Var %in% covar.in)  %>% 
    dplyr::mutate(Var = factor(.data$Var, levels = rev(covar.in)))  
  
  
  n_vars   <- length(unique(beta.df$Var))
  n_models <- length(unique(beta.df$model))
  
  # automatic scaling
  line_scale  <- 2.5 / sqrt(max(length(unique(beta.df$model)), 1))
  point_scale <- 4.5 / sqrt(max(length(unique(beta.df$model)), 1))
  
  dodge <- ggplot2::position_dodge(width = 0.6)
  p <- ggplot2::ggplot(beta.df, ggplot2::aes(
    x = .data$mean, y = .data$Var, color = .data$model)) +
    ggplot2::geom_vline(xintercept = 0, linetype = "dashed") +
    ggplot2::geom_errorbarh(
      ggplot2::aes(xmin = .data$LB, xmax = .data$UB),
      height = 0, position = dodge, linewidth = line_scale) +
    ggplot2::geom_point(position = dodge, size = point_scale) +
    ggplot2::labs(x = xlab, y = NULL, color = "Model", title = main) +
    ggplot2::theme_bw(base_size = 12) +
    ggplot2::scale_y_discrete(expand = ggplot2::expansion(mult = c(0.05, 0.05))) +
    ggplot2::theme(
      axis.text.y = ggplot2::element_text(margin = ggplot2::margin(r = 6, b=10, t=10),
                                          lineheight = 1.8),
      panel.spacing.y = ggplot2::unit(1.2, "lines"),
      panel.grid.minor = ggplot2::element_blank(),
      legend.position = "bottom")+
    ggplot2::guides(color = ggplot2::guide_legend(ncol = 8))

  
  return(list(data = beta.df, plot = p))
}


#' ----------------------------------------------------------------------------#

##  INLA code for factorisable ST autoregressive models ------------------------
#'
#' First of all, recall INLA. Otherwise, what is the whole thing about? -------#
library(INLA)
#' Temporal structure is AR(1)-like -------------------------------------------#

##'    IMCAR model ------------------------------------------------------------#

inla.IMCAR.AR1  <- function(...){
  INLA::inla.rgeneric.define(inla.rgeneric.IMCAR.AR1,
                             scale.model = TRUE, heteroskedastic = F,
                             alpha.sd = 1/100, U.sd = 1,
                             PC.ar1 = T, alpha.corr = 0.8, U.corr = 0.4,
                             chisq = FALSE, ...)
} 

inla.rgeneric.IMCAR.AR1  <- 
  function (cmd = c("graph", "Q", "mu", "initial", "log.norm.const", 
                    "log.prior", "quit"), theta = NULL) {
    envir <- parent.env(environment())
    if(!exists("cache.done", envir = envir)){
      L_unscaled <- Matrix::Diagonal(n=nrow(W), x=apply(W, 1, sum)) -  W
      if(scale.model){
        constr <- INLA:::inla.bym.constr.internal(L_unscaled, adjust.for.con.comp = T)
        scaleQ <- INLA:::inla.scale.model.internal(
          L_unscaled, constr = list(A = constr$constr$A, e = constr$constr$e))
        L <- scaleQ$Q
      } else L <- L_unscaled
      assign("L", L, envir=envir)
      if(PC.ar1){
        dpc.corr.ar1 <- function(x, t=k, n=1000, alpha=0.8, U=0.4, log=F){
          x <- pmax(abs(x), 1e-12) * ifelse(x < 0, -1, 1)
          KLD <- function(x, t, n){
            return(n/2 * ( log(1-x^2) + t*x^2/(1-x^2) ))
          }
          deriv.KLD <- function(x, t, n){
            return(n*x*( -1/(1-x^2) + t/((1-x^2)^2)  ) )
          }
          dist.zero <- sqrt(2*KLD(U, t=t, n=n))
          rate <- -log(1-alpha)/dist.zero
          ff <- stats::dexp(x=sqrt(2*KLD(x=x, t=t, n=n)), rate = rate)*
            1/sqrt(2*KLD(x=x, t=t, n=n)) * abs(deriv.KLD(x=x, t=t, n=n)) / 2
          
          return(ifelse(log, log(ff), ff))
        }
        assign("dpc.corr.ar1", dpc.corr.ar1, envir = envir)
      }
      n.prec <- ifelse(heteroskedastic, k, 1)
      assign("n.prec", n.prec, envir = envir)
      assign("cache.done", TRUE, envir=envir)
    }
    interpret.theta <- function() {
      prec <- exp(theta[1:n.prec]) 
      corr  <- 2/(1 + exp(-theta[n.prec+1])) - 1
      invR <- Matrix::bandSparse(
        n=k, m=k, k = c(-1, 0, 1),
        diagonals = list(rep(-corr, k-1), rep(1+corr^2, k), rep(-corr, k-1)))
      #' Not so elegant but works when k==1; otherwise, c(..., rep(., k-2)) would crash
      invR[1,1] <- invR[k,k] <- 1 
      PREC <-  diag(sqrt(prec), nrow = k) %*% invR %*% diag(sqrt(prec), nrow=k)
      return(list(corr=corr, PREC=PREC))
    }
    graph <- function() {
      PREC <- Matrix::bandSparse(
        n=k, m=k, k = c(-1, 0, 1),
        diagonals = list(rep(1, k-1), rep(1, k), rep(1, k-1))
      )
      return(kronecker(PREC, L))
    }
    Q <- function() {
      param <- interpret.theta()
      Q <- kronecker(param$PREC, L)
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
      #' Exponential prior on the sd
      val <- theta[1:n.prec] +
        sum(INLA:::inla.pc.dprec(exp(theta[1:n.prec]), alpha = alpha.sd, u = U.sd))
      #val <- theta[1]/2 - log(2) + dexp(exp(log.sd/2), rate=sd.rate, log=T) 
      if(PC.ar1){
        val <- val + log(2) - theta[n.prec+1] - 2*log(1+exp(-theta[n.prec+1])) +
          dpc.corr.ar1(x=param$corr, n=nrow(W), t = k, alpha = alpha.corr, U = U.corr, log=T)
      } else {
        val <- val+ # log(2) - theta[k+1] - log(1+exp(-theta[k+1]))  +
          dnorm(theta[n.prec+1], sd = sqrt(1/0.15), log=T)
      }
      return(val)
    }
    initial <- function(){
      if(!exists("initial.values", envir= envir )){
        return(c(rep(4, n.prec), -4))
      } else {
        return(initial.values)
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



##' PMCAR model ---------------------------------------------------------------#

inla.rgeneric.PMCAR.AR1 <- 
  function (cmd = c("graph", "Q", "mu", "initial", "log.norm.const", 
                    "log.prior", "quit"), theta = NULL) {
    envir <- parent.env(environment())
    if(!exists("cache.done", envir=envir)){
      D <- Matrix::Diagonal(n=nrow(W), x=rowSums(as.matrix(W))) 
      assign("D", D, envir = envir)
      eigenvalues <- Re(eigen(solve(D) %*% W)$values)
      inla.pc.pmmcar.rho <- function(rho, eigenvalues, alpha = 2/3, U = 1/2){
        n <- length(eigenvalues)
        In <- Matrix::Diagonal(n = n, x = 1)
        log.p <- numeric(length(rho))
        KLD <- function(rho, eigenvalues){
          res <-  1/2 * sum(log(1 - rho*eigenvalues)) +
            1/2 * sum(1/(1 - rho * eigenvalues) - 1)
          return(res)
        }
        for(j in c(1:length(rho))){
          KLD_j <- KLD(rho = rho[j], eigenvalues = eigenvalues)
          if(KLD_j < 0){
            message("!!! PROBLEM !!!! \n!!! NEGATIVE OR NULL KLD - MUST FIX MODEL !!!")
            return(NULL)
          }
          derivative <- 1/2 * sum( (rho[j]*eigenvalues^2)/(1 - rho[j]*eigenvalues)^2 )
          rate <- -1/sqrt(2*KLD(U, eigenvalues = eigenvalues)) * log(1 - alpha)
          log.p[j] <- dexp(x=sqrt(2*KLD_j), rate = rate, log = T) +
            log(1/sqrt(2*KLD_j)) + log(abs(derivative))
        }
        return(sum(log.p))
      }
      assign("inla.pc.pmmcar.rho", inla.pc.pmmcar.rho, envir = envir)
      assign("eigenvalues", eigenvalues, envir = envir)
      
      
      dpc.corr.ar1 <- function(x, t=k, n=1000, alpha=0.8, U=0.4, log=F){
        x <- pmax(abs(x), 1e-12) * ifelse(x < 0, -1, 1)
        KLD <- function(x, t, n){
          return(n/2 * ( log(1-x^2) + t*x^2/(1-x^2) ))
        }
        deriv.KLD <- function(x, t, n){
          return(n*x*( -1/(1-x^2) + t/((1-x^2)^2)  ) )
        }
        dist.zero <- sqrt(2*KLD(U, t=t, n=n))
        rate <- -log(1-alpha)/dist.zero
        ff <- stats::dexp(x=sqrt(2*KLD(x=x, t=t, n=n)), rate = rate)*
          1/sqrt(2*KLD(x=x, t=t, n=n)) * abs(deriv.KLD(x=x, t=t, n=n)) / 2
        
        return(ifelse(log, log(ff), ff))
      }
      assign("dpc.corr.ar1", dpc.corr.ar1, envir = envir)
      
      assign("cache.done", TRUE, envir = envir)
    }
    interpret.theta <- function() {
      rho <- 1/(1 + exp(-theta[1]))
      prec <- exp(theta[2])
      corr  <-   2/(1 + exp(-theta[3])) - 1
      invR <- Matrix::bandSparse(
        n=k, m=k, k = c(-1, 0, 1),
        diagonals = list(rep(-corr, k-1), rep(1+corr^2, k), rep(-corr, k-1)))
      invR[1,1] <- invR[k,k] <- 1 
      PREC <-  prec * invR 
      return(list(corr = corr, rho = rho, PREC = PREC))
    }
    graph <- function() {
      G <- Q()
      return(G)
    }
    Q <- function() {
      param <- interpret.theta()
      Q <- kronecker(param$PREC, D - param$rho*W)
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
      if(!PC.spat.cor){#' Uniform prior on the autoregressive parameter
        val <- -theta[1] - 2 * log(1 + exp(-theta[1]))
      } else{
        val <- inla.pc.pmmcar.rho(eigenvalues = eigenvalues, rho = param$rho, 
                                  alpha = alpha.spat.cor, U = U.spat.cor) -
          theta[1] - 2 * log(1 + exp(-theta[1]))
      }
      val <- val + theta[2] + 
        INLA:::inla.pc.dprec(exp(theta[2]), alpha = alpha.sd, u = U.sd)
      if(PC.ar1){
        val <- val + log(2) - theta[3] - 2*log(1+exp(-theta[3])) +
          dpc.corr.ar1(x=param$corr, n=nrow(W), t = k, alpha = alpha.corr, U = U.corr, log=T)
      } else {
        val <- val+ 
          dnorm(theta[3], sd = sqrt(1/0.15), log=T)
      }
      return(val)
    }
    initial <- function(){
      if(!exists("initial.values", envir= envir )){
        return(c(-3, 4, -4))
      } else {
        return(initial.values)
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

inla.PMCAR.AR1 <- function(...) {
  INLA::inla.rgeneric.define(inla.rgeneric.PMCAR.AR1,
                             PC.spat.cor = T, alpha.spat.cor = 2/3, U.spat.cor = 1/2,
                             alpha.sd = 1/100, U.sd = 1,
                             PC.ar1 = T, alpha.corr = 0.8, U.corr = 0.4, 
                             ...)}


##' LMCAR model ---------------------------------------------------------------#

inla.rgeneric.LMCAR.AR1 <- 
  function (cmd = c("graph", "Q", "mu", "initial", "log.norm.const", 
                    "log.prior", "quit"), theta = NULL) {
    envir <- parent.env(environment())
    if(!exists("cache.done", envir=envir)){
      L <- Matrix::Diagonal(n=nrow(W), x=rowSums(as.matrix(W))) -  W
      In <- Matrix::Diagonal(n = nrow(W), x = 1)
      if(PC.lambda) {
        n <- nrow(W)
        eigenvalues <- eigen(L - In, symmetric = T)$values
        rankdef <- n - Matrix::rankMatrix(L, method = "qr")
        eigenvalues[n - c(0:rankdef)] <- -1
        inla.pc.lmmcar.lambda <- function(lambda, eigenvalues, alpha = 2/3, U = 1/2){
          n <- length(eigenvalues)
          In <- Matrix::Diagonal(n = n, x = 1)
          #gammas <- list()
          log.p <- numeric(length(lambda))
          KLD <- function(lambda, eigenvalues){
            res <-  1/2 * sum(log(1 + lambda*eigenvalues)) +
              1/2 * sum(1/(1 + lambda * eigenvalues) - 1)
            return(res)
          }
          for(j in c(1:length(lambda))){
            KLD_j <- KLD(lambda = lambda[j], eigenvalues = eigenvalues)
            if(KLD_j < 0){
              message("!!! PROBLEM !!!! \n!!! NEGATIVE KLD - MUST FIX MODEL !!!")
              return(NULL)
            }
            derivative <- 1/2 * sum( (lambda[j]*eigenvalues^2)/(1 + lambda[j]*eigenvalues)^2 )
            rate <- -1/sqrt(2*KLD(U, eigenvalues = eigenvalues)) * log(1 - alpha)
            log.p[j] <- dexp(x=sqrt(2*KLD_j), rate = rate, log = T) +
              log(1/sqrt(2*KLD_j)) + log(abs(derivative))
          }
          return(sum(log.p))
        }
        assign("inla.pc.lmmcar.lambda", inla.pc.lmmcar.lambda, envir = envir)
        assign("eigenvalues", eigenvalues, envir = envir)
      }
      if(PC.ar1){
        dpc.corr.ar1 <- function(x, t=10, n=1000, alpha=0.8, U=0.4, log=F){
          x <- pmax(abs(x), 1e-12) * ifelse(x < 0, -1, 1)
          KLD <- function(x, t, n){
            return(n/2 * ( log(1-x^2) + t*x^2/(1-x^2) ))
          }
          deriv.KLD <- function(x, t, n){
            return(n*x*( -1/(1-x^2) + t/((1-x^2)^2)  ) )
          }
          dist.zero <- sqrt(2*KLD(U, t=t, n=n))
          rate <- -log(1-alpha)/dist.zero
          ff <- stats::dexp(x=sqrt(2*KLD(x=x, t=t, n=n)), rate = rate)*
            1/sqrt(2*KLD(x=x, t=t, n=n)) * abs(deriv.KLD(x=x, t=t, n=n)) / 2
          
          return(ifelse(log, log(ff), ff))
        }
        assign("dpc.corr.ar1", dpc.corr.ar1, envir = envir)
      }
      assign("L", L, envir = envir)
      assign("cache.done", TRUE, envir = envir)
    }
    interpret.theta <- function() {
      lambda <- 1/(1 + exp(-theta[1L]))
      prec <- exp( theta[2] )
      corr  <-   2/(1 + exp(-theta[3])) - 1
      invR <- Matrix::bandSparse(
        n=k, m=k, k = c(-1, 0, 1),
        diagonals = list(rep(-corr, k-1), rep(1+corr^2, k), rep(-corr, k-1)))
      invR[1,1] <- invR[k,k] <- 1 
      PREC <- prec* invR
      return(list(corr = corr, lambda = lambda, PREC = PREC))
    }
    graph <- function() {
      G <- Q()
      return(G)
    }
    Q <- function() {
      param <- interpret.theta()
      blockIW <- param$lambda * L +
        (1-param$lambda)*Matrix::Diagonal(n=nrow(W), x=1)
      Q <- kronecker(param$PREC, blockIW)
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
      #' Uniform prior on the autoregressive parameter
      if(!PC.lambda){
        val <- -theta[1L] - 2 * log(1 + exp(-theta[1L]))
      } else {
        val <- inla.pc.lmmcar.lambda(eigenvalues = eigenvalues,
                                     lambda = param$lambda,
                                     alpha = alpha.lambda, U = U.lambda) -
          theta[1L] - 2 * log(1 + exp(-theta[1L]))
      }
      val <- val + theta[2] + INLA:::inla.pc.dprec(exp(theta[2]), alpha = alpha.sd,
                                                   u = U.sd)
      if(PC.ar1){
        val <- val + log(2) - theta[3] - 2*log(1+exp(-theta[3])) +
          dpc.corr.ar1(x=param$corr, n=nrow(W), t = k, alpha = alpha.corr, U = U.corr, log=T)
      } else {
        val <- val+ 
          dnorm(theta[3], sd = sqrt(1/0.15), log=T)
      }
      return(val)
    }
    initial <- function(){
      if(!exists("initial.values", envir= envir )){
        return(c(-3, 4, -4))
      } else {
        return(initial.values)
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

inla.LMCAR.AR1 <- function(...) {
  INLA::inla.rgeneric.define(inla.rgeneric.LMCAR.AR1,
                             PC.lambda = T, alpha.lambda= 2/3, U.lambda = 1/2,
                             alpha.sd = 1/100, U.sd = 1,
                             PC.ar1 = T, alpha.corr = 0.8, U.corr = 0.4,
                             ...)}

##' BYM -----------------------------------------------------------------------#

#' No option to scale model or not. Model is scaled, mandatory ----------------#

#' Same is true for sparse parameterisation: both the BYM and ICAR fields 
#' are modelled, no choice between dense or sparse precision. -----------------#

inla.rgeneric.MBYM.AR1 <- function(
    cmd = c("graph", "Q", "mu", "initial", "log.norm.const",
            "log.prior", "quit"), theta = NULL){
  
  envir <- parent.env(environment())
  if(!exists("cache.done", envir=envir)){
    L_unscaled <- Matrix::Diagonal(n=nrow(W), x=rowSums(as.matrix(W))) -  W
    constr <- INLA:::inla.bym.constr.internal(L_unscaled, adjust.for.con.comp = T)
    scaleQ <- INLA:::inla.scale.model.internal(
      L_unscaled, constr = list(A = constr$constr$A, e = constr$constr$e))
    L <- scaleQ$Q
    invL <- INLA:::inla.ginv(L)
    In <- Matrix::Diagonal(n = nrow(W), x = 1)
    rankdef <- constr$rankdef
    eigenvalues <- eigen(invL, symmetric = T)$values - 1
    eigenvalues[(length(eigenvalues)-rankdef+1):length(eigenvalues)] <- 1
    inla.pc.mbym.phi <- function(phi, eigenvalues, alpha, U, force.const=1){
      n <- length(eigenvalues)
      In <- Matrix::Diagonal(n = n, x = 1)
      log.p <- numeric(length(phi))
      KLD <- function(phi, eigenvalues){
        res <- -1/2 * sum(log(1 + phi*eigenvalues)) +
          1/2 * phi*sum(eigenvalues)
        return(res)
      }
      deriv.KLD <- function(phi, eigenvalues){
        res <- 1/2 * sum(-eigenvalues/(1+phi*eigenvalues) + eigenvalues)
        return(res)
      }
      for(j in c(1:length(phi))){
        KLD_j <- KLD(phi = phi[j], eigenvalues = eigenvalues)
        if(KLD_j < 0){
          message("!!! PROBLEM !!!! \n!!! NEGATIVE KLD - MUST FIX MODEL !!!")
          return(NULL)
        }
        derivative <- 1/2 * sum(eigenvalues - eigenvalues / (1 + phi[j] * eigenvalues))
        rate <- -1/sqrt(2*KLD(U, eigenvalues = eigenvalues)) * log(1 - alpha)
        log.p[j] <- dexp(x=sqrt(2*KLD_j), rate = rate, log = T) +
          log(1/sqrt(2*KLD_j)) + log(abs(derivative)) - log(force.const)
      }
      return(sum(log.p))
    }
    ff <- Vectorize(function(x) {
      exp(inla.pc.mbym.phi(phi=x, eigenvalues=eigenvalues, alpha=alpha.phi, U=U.phi,force.const=1))
    })
    force.const <- stats::integrate(ff,0,1)$value
    if(PC.ar1){
      dpc.corr.ar1 <- function(x, t=10, n=1000, alpha=0.6, U=0.3, log=F){
        x <- pmax(abs(x), 1e-12) * ifelse(x < 0, -1, 1)
        KLD <- function(x, t, n){
          return(n/2 * ( log(1-x^2) + t*x^2/(1-x^2) ))
        }
        deriv.KLD <- function(x, t, n){
          return(n*x*( -1/(1-x^2) + t/((1-x^2)^2)  ) )
        }
        dist.zero <- sqrt(2*KLD(U, t=t, n=n))
        rate <- -log(1-alpha)/dist.zero
        ff <- stats::dexp(x=sqrt(2*KLD(x=x, t=t, n=n)), rate = rate)*
          1/sqrt(2*KLD(x=x, t=t, n=n)) * abs(deriv.KLD(x=x, t=t, n=n)) / 2
        
        return(ifelse(log, log(ff), ff))
      }
      assign("dpc.corr.ar1", dpc.corr.ar1, envir = envir)
    }
    
    assign("L", L, envir = envir)
    assign("invL", invL, envir = envir)
    assign("inla.pc.mbym.phi", inla.pc.mbym.phi, envir = envir)
    assign("eigenvalues", eigenvalues, envir = envir)
    assign("force.const", force.const, envir=envir)
    #assign("eigenvectors", eigenvectors, envir = envir)
    assign("cache.done", TRUE, envir = envir)
  }
  interpret.theta <- function() {
    phi <- 1/(1 + exp(-theta[1L]))
    prec <- exp(  theta[2] )
    corr  <-   2/(1 + exp(-theta[3])) - 1
    tCholPrec <-  Matrix::bandSparse(
      n=k, m=k, k = c(0, 1),
      diagonals = list(c(sqrt(1-corr^2), rep(1,k-1)), rep(-corr, k-1) ))
    invM <- sqrt(prec) * tCholPrec 
    PREC <- invM %*% Matrix::t(invM)
    return(list(corr = corr, phi = phi, PREC = PREC, invM=invM))
  }
  graph <- function() {
    QQ <- Q()
    G <- (QQ != 0) * 1
    return(G)
  }
  Q <- function() {
    param <- interpret.theta()
    In <- Matrix::Diagonal(nrow(W), 1)
    
    q11 <- 1/(1-param$phi) * param$PREC
    q12 <- sqrt(param$phi)/(1-param$phi) * param$invM
    q22 <- param$phi/(1-param$phi) * Matrix::Diagonal(x=1, n=k)
    
    Q11 <- kronecker(q11, In)
    Q12 <- - kronecker(q12, In)
    Q21 <- Matrix::t(Q12)
    Q22 <- kronecker(q22, In) + kronecker(Matrix::Diagonal(x=1, n=k), L)
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
    if(! PC.phi) {
      val <- -theta[1L] - 2 * log(1 + exp(-theta[1L]))
    } else {
      val <- inla.pc.mbym.phi(eigenvalues = eigenvalues, phi = param$phi,
                              alpha = alpha.phi, U = U.phi, force.const = force.const) -
        theta[1L] - 2 * log(1 + exp(-theta[1L]))
    }
    val <- val + theta[2] + INLA:::inla.pc.dprec(exp(theta[2]), alpha = alpha.sd,
                                                 u = U.sd)
    if(PC.ar1){
      val <- val + log(2) - theta[3] - 2*log(1+exp(-theta[3])) +
        dpc.corr.ar1(x=param$corr, n=nrow(W), t = k, alpha = alpha.corr, U = U.corr, log=T)
    } else {
      val <- val+ 
        dnorm(theta[3], sd = sqrt(1/0.15), log=T)
    }
    return(val)
  }
  initial <- function(){
    if(!exists("initial.values", envir= envir )){
      return(  c(-3, 4, -4) )
    } else {
      return(initial.values)
    }
  }
  quit <- function() {
    return(invisible())
  }
  if (as.integer(R.version$major) > 3) {
    if (!length(theta))  theta <- initial()
  } else {
    if (is.null(theta))  theta <- initial()
  }
  val <- do.call(match.arg(cmd), args = list())
  return(val)
}

inla.MBYM.AR1 <- function(...) {
  INLA::inla.rgeneric.define(inla.rgeneric.MBYM.AR1,
                             sparse = TRUE, 
                             PC.phi = T, alpha.phi = 2/3, U.phi = 1/2,
                             alpha.sd = 1/100, U.sd = 1,
                             PC.ar1 = T, alpha.corr = 0.8, U.corr = 0.4, 
                             ...)}





##  Exploratory analysis --------------------------------------------------------
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
  N_ACC ~ 0 + Y_2021 + Y_2022 + Y_2023 + Y_2024 +
    Desk_dist + AVC_dist + ELI + PGR + UIS + ELL + PDI + ER, 
  family = "poisson", offset = log(nn), data = dd_con)

#' Model with offset ----------------------------------------------------------#
cav_nosp_inla <- inla(
  N_ACC ~ 0 + Y_2021 + Y_2022 + Y_2023 + Y_2024 +
  AVC_dist + Desk_dist + ELI + PGR + UIS + ELL + PDI + ER,
  offset = log(nn),
  family = "poisson", data =dd_con,
  num.threads = 1, 
  control.compute = list(internal.opt = F, cpo = T, waic = T, dic = T), 
  verbose = T)

#' Model with expected accesses - equivalent in theory ------------------------#
#cav_nosp_inla_E <- inla(
#  N_ACC ~ 0 + Y_2021 + Y_2022 + Y_2023 + Y_2024 +
#    AVC_dist + Desk_dist + ELI + PGR + UIS + ELL + PDI + ER,
#  E = Exp,
#  family = "poisson", data = dd_con,
#  num.threads = 1, 
#  control.compute = list(internal.opt = F, cpo = T, waic = T, config = T), 
#  verbose = T)

##' Plot distance

Shp %>% dplyr::select(PRO_COM) %>% 
  dplyr::left_join(dists_th_24) %>% 
  ggplot2::ggplot()+
  ggplot2::geom_sf(ggplot2::aes(fill = .data$AVC_dist))+
  ggplot2::scale_fill_viridis_c(direction = -1)+
  ggplot2::ggtitle("Distance from closest municipality hosting AVCs, 2024")+
  ggplot2::theme_classic()

dd  %>% dplyr::filter(.data$Year == "2024") %>%
  dplyr::select(PRO_COM) %>% 
  dplyr::left_join(dists_desk) %>% 
  dplyr::mutate(DDist = ifelse(.data$Desk_dist>0, .data$Desk_dist, NA)) %>% 
  ggplot2::ggplot()+
  ggplot2::geom_sf(ggplot2::aes(fill = .data$DDist))+
  ggplot2::scale_fill_viridis_c(direction = -1, na.value  = "white")+
  ggplot2::ggtitle("Distance (minutes) from closest help desk")+
  ggplot2::theme_classic()

##' Plot covariates -----------------------------------------------------------#

dd %>% dplyr::filter(.data$Year == "2024") %>% 
  ggplot2::ggplot()+
  ggplot2::geom_sf(ggplot2::aes( fill = .data$ELL ))+
  ggplot2::scale_fill_viridis_c()+
  ggplot2::ggtitle("Incidence of low education levels")+
  ggplot2::theme_classic()
  


##' Tuning a priori -----------------------------------------------------------#

dpc.corr.ar1 <- function(x, t=k, n=1000, alpha=0.8, U=0.4, log=F){
  x <- pmax(abs(x), 1e-12) * ifelse(x < 0, -1, 1)
  KLD <- function(x, t, n){
    return(n/2 * ( log(1-x^2) + t*x^2/(1-x^2) ))
  }
  deriv.KLD <- function(x, t, n){
    return(n*x*( -1/(1-x^2) + t/((1-x^2)^2)  ) )
  }
  dist.zero <- sqrt(2*KLD(U, t=t, n=n))
  rate <- -log(1-alpha)/dist.zero
  ff <- stats::dexp(x=sqrt(2*KLD(x=x, t=t, n=n)), rate = rate)*
    1/sqrt(2*KLD(x=x, t=t, n=n)) * abs(deriv.KLD(x=x, t=t, n=n)) / 2
  
  return(ifelse(log, log(ff), ff))
}


u <- 0.5; alpha = 0.9
rhos <- c(-499:-1, 1:499)/500
ff.r <- Vectorize(function(x) dpc.corr.ar1(x=x, t=4, U=u, alpha = alpha))
plot(rhos, ff.r(rhos), type = 'l', main = paste0("u = ",u, " alpha = ", alpha))
abline(v=u)
integrate(ff.r, 0, u)

u <- 0.4; alpha = 0.8
rhos <- c(-499:-1, 1:499)/500
ff.r <- Vectorize(function(x) dpc.corr.ar1(x=x, t=4, U=u, alpha = alpha))
plot(rhos, ff.r(rhos), type = 'l', main = paste0("u = ",u, " alpha = ", alpha))
abline(v=u)
integrate(ff.r, 0, u)



##  Spatiotemporal models: AR(1) - ICAR -----------------------------------------

 



cav_IMCAR_inla.AR1 <- inla(
  N_ACC ~ 0 + Y_2021 + Y_2022 + Y_2023 + Y_2024 + 
    Desk_dist + AVC_dist + ELI + PGR + UIS + ELL + PDI + ER+ 
    f(ID, model = inla.IMCAR.AR1(k = 4, W = W_con, PC.ar1 = T, 
                                 alpha.sd = 0.1, U.sd = sqrt(1/2)), 
      extraconstr = list(A = A_constr, e = c(0,0,0,0))),
  offset = log(nn), family = "poisson", data =dd_con,
  num.threads = 1, control.compute = list(internal.opt = F, cpo = T, waic = T, dic = T), 
  verbose = T)
cav_IMCAR_inla.AR1$waic$waic


cav_IMCAR_inla.AR1_s15 <- inla(
  N_ACC ~ 0 + Y_2021 + Y_2022 + Y_2023 + Y_2024 + 
    Desk_dist + AVC_dist + ELI + PGR + UIS + ELL + PDI + ER+ 
    f(ID, model = inla.IMCAR.AR1(k = 4, W = W_con, PC.ar1 = T, 
                                 alpha.sd = 0.1, U.sd = sqrt(1/2)), 
      extraconstr = list(A = A_constr, e = c(0,0,0,0))),
  offset = log(nn), family = "poisson", data =dd_filtered_15$data,
  num.threads = 1, control.compute = list(internal.opt = F, cpo = T, waic = T, dic = T), 
  verbose = T)

cav_IMCAR_inla.AR1_s20 <- inla(
    N_ACC ~ 0 + Y_2021 + Y_2022 + Y_2023 + Y_2024 + 
      Desk_dist + AVC_dist + ELI + PGR + UIS + ELL + PDI + ER+ 
      f(ID, model = inla.IMCAR.AR1(k = 4, W = W_con, PC.ar1 = T, 
                                   alpha.sd = 0.1, U.sd = sqrt(1/2)), 
        extraconstr = list(A = A_constr, e = c(0,0,0,0))),
    offset = log(nn), family = "poisson", data =dd_filtered_20$data,
    num.threads = 1, control.compute = list(internal.opt = F, cpo = T, waic = T, dic = T), 
    verbose = T)

cav_IMCAR_inla.AR1_s25 <- inla(
      N_ACC ~ 0 + Y_2021 + Y_2022 + Y_2023 + Y_2024 + 
        Desk_dist + AVC_dist + ELI + PGR + UIS + ELL + PDI + ER+ 
        f(ID, model = inla.IMCAR.AR1(k = 4, W = W_con, PC.ar1 = T, 
                                     alpha.sd = 0.1, U.sd = sqrt(1/2)), 
          extraconstr = list(A = A_constr, e = c(0,0,0,0))),
      offset = log(nn), family = "poisson", data =dd_filtered_25$data,
      num.threads = 1, control.compute = list(internal.opt = F, cpo = T, waic = T, dic = T), 
      verbose = T)


##  Spatiotemporal models: AR(1) - PCAR -----------------------------------------


#' Simple input ---------------------------------------------------------------#

##' Uniform -------------------------------------------------------------------#
cav_PMCAR_inla.AR1.Unif <- inla(
  N_ACC ~ 0 + Y_2021 + Y_2022 + Y_2023 + Y_2024 +
    Desk_dist + AVC_dist + ELI + PGR + UIS + ELL + PDI + ER+ 
    f(ID, model = inla.PMCAR.AR1(k = 4, W = W_con, PC.spat.cor = F, alpha.sd = 0.1, U.sd = sqrt(1/2)), 
      extraconstr = list(A = A_constr, e = c(0,0,0,0))
    ),
  offset = log(nn), family = "poisson", data =dd_con,
  num.threads = 1, control.compute = list(internal.opt = F, cpo = T, waic = T, dic = T), 
  verbose = T)

##' Default PC-prior ----------------------------------------------------------#
cav_PMCAR_inla.AR1.PC <- inla(
  N_ACC ~ 0 + Y_2021 + Y_2022 + Y_2023 + Y_2024 + 
    Desk_dist + AVC_dist + ELI + PGR + UIS + ELL + PDI + ER+ 
    f(ID, model = inla.PMCAR.AR1(k = 4, W = W_con, PC.ar1 = T, alpha.sd = 0.1, U.sd = sqrt(1/2)),
      extraconstr = list(A = A_constr, e = c(0,0,0,0))
    ),
  offset = log(nn), family = "poisson", data =dd_con,
  num.threads = 1, control.compute = list(internal.opt = F, cpo = T, waic = T, dic = T), 
  verbose = T)

##' Strict PC-prior -----------------------------------------------------------#
#' Better
cav_PMCAR_inla.AR1.PC.strict <- inla(
  N_ACC ~ 0 + Y_2021 + Y_2022 + Y_2023 + Y_2024 +
    Desk_dist + AVC_dist + ELI + PGR + UIS + ELL + PDI + ER+ 
    f(ID, model = inla.PMCAR.AR1(k = 4, W = W_con, PC = T, PC.ar1 = T,
                                 alpha.spat.cor = 0.9, U.spat.cor = 0.6,
                                 alpha.sd = 0.1, U.sd = sqrt(1/2)),
      extraconstr = list(A = A_constr, e = c(0,0,0,0)) ),
  offset = log(nn), family = "poisson", data =dd_con,
  num.threads = 1, control.compute = list(internal.opt = F, cpo = T, waic = T, dic = T), 
  verbose = T)




##' FIltered removing 15 eigenvectors -----------------------------------------#
##' Uniform prior -------------------------------------------------------------#
cav_PMCAR_inla.AR1.Unif_s15 <- inla(
  N_ACC ~ 0 + Y_2021 + Y_2022 + Y_2023 + Y_2024 +
    Desk_dist + AVC_dist + ELI + PGR + UIS + ELL + PDI + ER+ 
    f(ID, model = inla.PMCAR.AR1(k = 4, W = W_con, PC.spat.cor = F, alpha.sd = 0.1, U.sd = sqrt(1/2)), 
      extraconstr = list(A = A_constr, e = c(0,0,0,0))
    ),
  offset = log(nn), family = "poisson", data =dd_filtered_15$data,
  num.threads = 1, control.compute = list(internal.opt = F, cpo = T, waic = T, dic = T), 
  verbose = T)

##' Standard pC-prior ---------------------------------------------------------#
cav_PMCAR_inla.AR1.PC_s15 <- inla(
  N_ACC ~ 0 + Y_2021 + Y_2022 + Y_2023 + Y_2024 + 
    Desk_dist + AVC_dist + ELI + PGR + UIS + ELL + PDI + ER+ 
    f(ID, model = inla.PMCAR.AR1(k = 4, W = W_con, PC.ar1 = T, alpha.sd = 0.1, U.sd = sqrt(1/2)),
      extraconstr = list(A = A_constr, e = c(0,0,0,0))
    ),
  offset = log(nn), family = "poisson", data =dd_filtered_15$data,
  num.threads = 1, control.compute = list(internal.opt = F, cpo = T, waic = T, dic = T), 
  verbose = T)

#' Better
cav_PMCAR_inla.AR1.PC.strict_s15 <- inla(
  N_ACC ~ 0 + Y_2021 + Y_2022 + Y_2023 + Y_2024 +
    Desk_dist + AVC_dist + ELI + PGR + UIS + ELL + PDI + ER+ 
    f(ID, model = inla.PMCAR.AR1(k = 4, W = W_con, PC = T, PC.ar1 = T,
                                 alpha.spat.cor = 0.9, U.spat.cor = 0.6,
                                 alpha.sd = 0.1, U.sd = sqrt(1/2)),
      extraconstr = list(A = A_constr, e = c(0,0,0,0))
    ),
  offset = log(nn), family = "poisson", data =dd_filtered_15$data,
  num.threads = 1, control.compute = list(internal.opt = F, cpo = T, waic = T, dic = T), 
  verbose = T)

#' Spatial filter - removed 20 eigenvectors -----------------------------------#

cav_PMCAR_inla.AR1.Unif_s20 <- inla(
  N_ACC ~ 0 + Y_2021 + Y_2022 + Y_2023 + Y_2024 +
    Desk_dist + AVC_dist + ELI + PGR + UIS + ELL + PDI + ER+ 
    f(ID, model = inla.PMCAR.AR1(k = 4, W = W_con, PC.spat.cor = F, alpha.sd = 0.1, U.sd = sqrt(1/2)), 
      extraconstr = list(A = A_constr, e = c(0,0,0,0))
    ),
  offset = log(nn), family = "poisson", data =dd_filtered_20$data,
  num.threads = 1, control.compute = list(internal.opt = F, cpo = T, waic = T, dic = T), 
  verbose = T)

cav_PMCAR_inla.AR1.PC_s20 <- inla(
  N_ACC ~ 0 + Y_2021 + Y_2022 + Y_2023 + Y_2024 + 
    Desk_dist + AVC_dist + ELI + PGR + UIS + ELL + PDI + ER+ 
    f(ID, model = inla.PMCAR.AR1(k = 4, W = W_con, PC.ar1 = T, alpha.sd = 0.1, U.sd = sqrt(1/2)),
      extraconstr = list(A = A_constr, e = c(0,0,0,0))
    ),
  offset = log(nn), family = "poisson", data =dd_filtered_20$data,
  num.threads = 1, control.compute = list(internal.opt = F, cpo = T, waic = T, dic = T), 
  verbose = T)

#' Better
cav_PMCAR_inla.AR1.PC.strict_s20 <- inla(
  N_ACC ~ 0 + Y_2021 + Y_2022 + Y_2023 + Y_2024 +
    Desk_dist + AVC_dist + ELI + PGR + UIS + ELL + PDI + ER+ 
    f(ID, model = inla.PMCAR.AR1(k = 4, W = W_con, PC = T, PC.ar1 = T,
                                 alpha.spat.cor = 0.9, U.spat.cor = 0.6,
                                 alpha.sd = 0.1, U.sd = sqrt(1/2)),
      extraconstr = list(A = A_constr, e = c(0,0,0,0))
    ),
  offset = log(nn), family = "poisson", data =dd_filtered_20$data,
  num.threads = 1, control.compute = list(internal.opt = F, cpo = T, waic = T, dic = T), 
  verbose = T)

#' Spatial filter - 25 eigenvectors -------------------------------------------#
cav_PMCAR_inla.AR1.Unif_s25 <- inla(
  N_ACC ~ 0 + Y_2021 + Y_2022 + Y_2023 + Y_2024 +
    Desk_dist + AVC_dist + ELI + PGR + UIS + ELL + PDI + ER+ 
    f(ID, model = inla.PMCAR.AR1(k = 4, W = W_con, PC.spat.cor = F, alpha.sd = 0.1, U.sd = sqrt(1/2)), 
      extraconstr = list(A = A_constr, e = c(0,0,0,0))
    ),
  offset = log(nn), family = "poisson", data =dd_filtered_25$data,
  num.threads = 1, control.compute = list(internal.opt = F, cpo = T, waic = T, dic = T), 
  verbose = T)

cav_PMCAR_inla.AR1.PC_s25 <- inla(
  N_ACC ~ 0 + Y_2021 + Y_2022 + Y_2023 + Y_2024 + 
    Desk_dist + AVC_dist + ELI + PGR + UIS + ELL + PDI + ER+ 
    f(ID, model = inla.PMCAR.AR1(k = 4, W = W_con, PC.ar1 = T, alpha.sd = 0.1, U.sd = sqrt(1/2)),
      extraconstr = list(A = A_constr, e = c(0,0,0,0))
    ),
  offset = log(nn), family = "poisson", data =dd_filtered_25$data,
  num.threads = 1, control.compute = list(internal.opt = F, cpo = T, waic = T, dic = T), 
  verbose = T)

#' Better
cav_PMCAR_inla.AR1.PC.strict_s25 <- inla(
  N_ACC ~ 0 + Y_2021 + Y_2022 + Y_2023 + Y_2024 +
    Desk_dist + AVC_dist + ELI + PGR + UIS + ELL + PDI + ER+ 
    f(ID, model = inla.PMCAR.AR1(k = 4, W = W_con, PC = T, PC.ar1 = T,
                                 alpha.spat.cor = 0.9, U.spat.cor = 0.6,
                                 alpha.sd = 0.1, U.sd = sqrt(1/2)),
      extraconstr = list(A = A_constr, e = c(0,0,0,0))
    ),
  offset = log(nn), family = "poisson", data =dd_filtered_25$data,
  num.threads = 1, control.compute = list(internal.opt = F, cpo = T, waic = T, dic = T), 
  verbose = T)


##  Spatiotemporal models: AR(1) - LCAR -----------------------------------------
 
#' Yes, this model features sum-to-zero constraints.
#' Why? It is not singular.
#' No, it is not. Yet absence of constraints inflates variance.
#' Better to avoid confounding with intercepts --------------------------------#

#' Input data -----------------------------------------------------------------#
##' Uniform prior -------------------------------------------------------------#
cav_LMCAR_inla.AR1.Unif <- inla(
  N_ACC ~ 0 + Y_2021 + Y_2022 + Y_2023 + Y_2024 + 
    Desk_dist + AVC_dist + ELI + PGR + UIS + ELL + PDI + ER+ 
    f(ID, model = inla.LMCAR.AR1(k = 4, W = W_con, PC.lambda = F, alpha.sd = 0.1, U.sd = sqrt(1/2)), 
      extraconstr = list(A = A_constr, e = c(0,0,0,0)) ),
  offset = log(nn), family = "poisson", data =dd_con,
  num.threads = 1, control.compute = list(internal.opt = F, cpo = T, waic = T, dic = T), 
  verbose = T)

##' Standard PC-prior ----------------------------------------------------------#
cav_LMCAR_inla.AR1.PC <- inla(
  N_ACC ~ 0 + Y_2021 + Y_2022 + Y_2023 + Y_2024 + 
    Desk_dist + AVC_dist + ELI + PGR + UIS + ELL + PDI + ER+ 
    f(ID, model = inla.LMCAR.AR1(k = 4, W = W_con, alpha.sd = 0.1, U.sd = sqrt(1/2)), 
      extraconstr = list(A = A_constr, e = c(0,0,0,0))),
  offset = log(nn), family = "poisson", data =dd_con,
  num.threads = 1, control.compute = list(internal.opt = F, cpo = T, waic = T, dic = T), 
  verbose = T)
##' strict PC-prior -----------------------------------------------------------#
#' Better, though...
cav_LMCAR_inla.AR1.PC.strict <- inla(
  N_ACC ~ 0 + Y_2021 + Y_2022 + Y_2023 + Y_2024 +
    Desk_dist + AVC_dist + ELI + PGR + UIS + ELL + PDI + ER+ 
    f(ID, model = inla.LMCAR.AR1(k = 4, W = W_con, alpha.lambda =0.9, U.lambda = 0.6,
                                 alpha.sd = 0.1, U.sd = sqrt(1/2)), 
      extraconstr = list(A = A_constr, e = c(0,0,0,0)) ),
  offset = log(nn), family = "poisson", data =dd_con,
  num.threads = 1, control.compute = list(internal.opt = F, cpo = T, waic = T, dic = T), 
  verbose = T)

#' Spatial filter - remove 15 eigenvectors ------------------------------------#
##' Uniform prior -------------------------------------------------------------#

cav_LMCAR_inla.AR1.Unif_s15 <- inla(
  N_ACC ~ 0 + Y_2021 + Y_2022 + Y_2023 + Y_2024 + 
    Desk_dist + AVC_dist + ELI + PGR + UIS + ELL + PDI + ER+ 
    f(ID, model = inla.LMCAR.AR1(k = 4, W = W_con, PC.lambda = F, alpha.sd = 0.1, U.sd = sqrt(1/2)), 
      extraconstr = list(A = A_constr, e = c(0,0,0,0)) ),
  offset = log(nn), family = "poisson", data =dd_filtered_15$data,
  num.threads = 1, control.compute = list(internal.opt = F, cpo = T, waic = T, dic = T), 
  verbose = T)

##' Standard PC-prior ----------------------------------------------------------#
cav_LMCAR_inla.AR1.PC_s15 <- inla(
  N_ACC ~ 0 + Y_2021 + Y_2022 + Y_2023 + Y_2024 + 
    Desk_dist + AVC_dist + ELI + PGR + UIS + ELL + PDI + ER+ 
    f(ID, model = inla.LMCAR.AR1(k = 4, W = W_con, alpha.sd = 0.1, U.sd = sqrt(1/2)), 
      extraconstr = list(A = A_constr, e = c(0,0,0,0))),
  offset = log(nn), family = "poisson", data =dd_filtered_15$data,
  num.threads = 1, control.compute = list(internal.opt = F, cpo = T, waic = T, dic = T), 
  verbose = T)
##' strict PC-prior -----------------------------------------------------------#
#' Better, though...
cav_LMCAR_inla.AR1.PC.strict_s15 <- inla(
  N_ACC ~ 0 + Y_2021 + Y_2022 + Y_2023 + Y_2024 +
    Desk_dist + AVC_dist + ELI + PGR + UIS + ELL + PDI + ER+ 
    f(ID, model = inla.LMCAR.AR1(k = 4, W = W_con, alpha.lambda =0.9, U.lambda = 0.6,
                                 alpha.sd = 0.1, U.sd = sqrt(1/2)), 
      extraconstr = list(A = A_constr, e = c(0,0,0,0)) ),
  offset = log(nn), family = "poisson", data =dd_filtered_15$data,
  num.threads = 1, control.compute = list(internal.opt = F, cpo = T, waic = T, dic = T), 
  verbose = T)

#' Spatial filter - remove 20 eigenvectors ------------------------------------#
##' Uniform prior -------------------------------------------------------------#

cav_LMCAR_inla.AR1.Unif_s20 <- inla(
  N_ACC ~ 0 + Y_2021 + Y_2022 + Y_2023 + Y_2024 + 
    Desk_dist + AVC_dist + ELI + PGR + UIS + ELL + PDI + ER+ 
    f(ID, model = inla.LMCAR.AR1(k = 4, W = W_con, PC.lambda = F, alpha.sd = 0.1, U.sd = sqrt(1/2)), 
      extraconstr = list(A = A_constr, e = c(0,0,0,0)) ),
  offset = log(nn), family = "poisson", data =dd_filtered_20$data,
  num.threads = 1, control.compute = list(internal.opt = F, cpo = T, waic = T, dic = T), 
  verbose = T)

##' Standard PC-prior ----------------------------------------------------------#
cav_LMCAR_inla.AR1.PC_s20 <- inla(
  N_ACC ~ 0 + Y_2021 + Y_2022 + Y_2023 + Y_2024 + 
    Desk_dist + AVC_dist + ELI + PGR + UIS + ELL + PDI + ER+ 
    f(ID, model = inla.LMCAR.AR1(k = 4, W = W_con, alpha.sd = 0.1, U.sd = sqrt(1/2)), 
      extraconstr = list(A = A_constr, e = c(0,0,0,0))),
  offset = log(nn), family = "poisson", data =dd_filtered_20$data,
  num.threads = 1, control.compute = list(internal.opt = F, cpo = T, waic = T, dic = T), 
  verbose = T)
##' strict PC-prior -----------------------------------------------------------#
#' Better, though...
cav_LMCAR_inla.AR1.PC.strict_s20 <- inla(
  N_ACC ~ 0 + Y_2021 + Y_2022 + Y_2023 + Y_2024 +
    Desk_dist + AVC_dist + ELI + PGR + UIS + ELL + PDI + ER+ 
    f(ID, model = inla.LMCAR.AR1(k = 4, W = W_con, alpha.lambda =0.9, U.lambda = 0.6,
                                 alpha.sd = 0.1, U.sd = sqrt(1/2)), 
      extraconstr = list(A = A_constr, e = c(0,0,0,0)) ),
  offset = log(nn), family = "poisson", data =dd_filtered_20$data,
  num.threads = 1, control.compute = list(internal.opt = F, cpo = T, waic = T, dic = T), 
  verbose = T)


#' Spatial filter - remove 25 eigenvectors ------------------------------------#
##' Uniform prior -------------------------------------------------------------#

cav_LMCAR_inla.AR1.Unif_s25<- inla(
  N_ACC ~ 0 + Y_2021 + Y_2022 + Y_2023 + Y_2024 + 
    Desk_dist + AVC_dist + ELI + PGR + UIS + ELL + PDI + ER+ 
    f(ID, model = inla.LMCAR.AR1(k = 4, W = W_con, PC.lambda = F, alpha.sd = 0.1, U.sd = sqrt(1/2)), 
      extraconstr = list(A = A_constr, e = c(0,0,0,0)) ),
  offset = log(nn), family = "poisson", data =dd_filtered_25$data,
  num.threads = 1, control.compute = list(internal.opt = F, cpo = T, waic = T, dic = T), 
  verbose = T)

##' Standard PC-prior ----------------------------------------------------------#
cav_LMCAR_inla.AR1.PC_s25 <- inla(
  N_ACC ~ 0 + Y_2021 + Y_2022 + Y_2023 + Y_2024 + 
    Desk_dist + AVC_dist + ELI + PGR + UIS + ELL + PDI + ER+ 
    f(ID, model = inla.LMCAR.AR1(k = 4, W = W_con, alpha.sd = 0.1, U.sd = sqrt(1/2)), 
      extraconstr = list(A = A_constr, e = c(0,0,0,0))),
  offset = log(nn), family = "poisson", data =dd_filtered_25$data,
  num.threads = 1, control.compute = list(internal.opt = F, cpo = T, waic = T, dic = T), 
  verbose = T)
##' strict PC-prior -----------------------------------------------------------#
#' Better, though...
cav_LMCAR_inla.AR1.PC.strict_s25 <- inla(
  N_ACC ~ 0 + Y_2021 + Y_2022 + Y_2023 + Y_2024 +
    Desk_dist + AVC_dist + ELI + PGR + UIS + ELL + PDI + ER+ 
    f(ID, model = inla.LMCAR.AR1(k = 4, W = W_con, alpha.lambda =0.9, U.lambda = 0.6,
                                 alpha.sd = 0.1, U.sd = sqrt(1/2)), 
      extraconstr = list(A = A_constr, e = c(0,0,0,0)) ),
  offset = log(nn), family = "poisson", data =dd_filtered_25$data,
  num.threads = 1, control.compute = list(internal.opt = F, cpo = T, waic = T, dic = T), 
  verbose = T)




##  Spatiotemporal models: AR(1) - BYM  ----------------------------------------

#' Input data -----------------------------------------------------------------#

##' Uniform prior -------------------------------------------------------------#

cav_MBYM_inla.AR1.Unif <- inla(
  N_ACC ~ 0 + Y_2021 + Y_2022 + Y_2023 + Y_2024 + AVC_dist + Desk_dist + ELI + PGR + UIS + ELL + PDI + ER+ 
    f(ID, model = inla.MBYM.AR1(k = 4, W = W_con, PC.ar1 = T, PC.phi = F, alpha.sd = 0.1, U.sd=sqrt(1/2)), 
      extraconstr = constr.BYM),
  offset = log(nn), family = "poisson", data =dd_con,
  num.threads = 1, control.compute = list(internal.opt = F, cpo = T, waic = T, dic = T), 
  verbose = T)

##' Standard PC-prior ---------------------------------------------------------#
cav_MBYM_inla.AR1.PC <- inla(
  N_ACC ~ 0 + Y_2021 + Y_2022 + Y_2023 + Y_2024 + AVC_dist + Desk_dist + ELI + PGR + UIS + ELL + PDI + ER+ 
    f(ID, model = inla.MBYM.AR1(k = 4, W = W_con, PC.ar1 = T, PC.phi = T, alpha.sd =0.1, U.sd = sqrt(1/2)), 
      extraconstr = constr.BYM),
  offset = log(nn), family = "poisson", data =dd_con,
  num.threads = 1, control.compute = list(internal.opt = F, cpo = T, waic = T, dic = T), 
  verbose = T)
 
##' Strict PC-prior -----------------------------------------------------------#
cav_MBYM_inla.AR1.PC.strict <- inla(
  N_ACC ~ 0 + Y_2021 + Y_2022 + Y_2023 + Y_2024 + AVC_dist + Desk_dist+ ELI + PGR + UIS + ELL + PDI + ER+ 
    f(ID, model = inla.MBYM.AR1(k = 4, W = W_con, PC.ar1 = T, alpha.phi =0.9, U.phi=0.6,
                                alpha.sd =0.1, U.sd = sqrt(1/2)), 
      extraconstr = constr.BYM),
  offset = log(nn), family = "poisson", data =dd_con,
  num.threads = 1, control.compute = list(internal.opt = F, cpo = T, waic = T, dic = T), 
  verbose = T)


#' Spatial filter - remove 15 eigenvectors ------------------------------------#

##' Uniform prior -------------------------------------------------------------#

cav_MBYM_inla.AR1.Unif_s15 <- inla(
  N_ACC ~ 0 + Y_2021 + Y_2022 + Y_2023 + Y_2024 + AVC_dist + Desk_dist + ELI + PGR + UIS + ELL + PDI + ER+ 
    f(ID, model = inla.MBYM.AR1(k = 4, W = W_con, PC.ar1 = T, PC.phi = F, alpha.sd = 0.1, U.sd=sqrt(1/2)), 
      extraconstr = constr.BYM),
  offset = log(nn), family = "poisson", data =dd_filtered_15$data,
  num.threads = 1, control.compute = list(internal.opt = F, cpo = T, waic = T, dic = T), 
  verbose = T)

##' Standard PC-prior ---------------------------------------------------------#
cav_MBYM_inla.AR1.PC_s15 <- inla(
  N_ACC ~ 0 + Y_2021 + Y_2022 + Y_2023 + Y_2024 + AVC_dist + Desk_dist + ELI + PGR + UIS + ELL + PDI + ER+ 
    f(ID, model = inla.MBYM.AR1(k = 4, W = W_con, PC.ar1 = T, PC.phi = T, alpha.sd =0.1, U.sd = sqrt(1/2)), 
      extraconstr = constr.BYM),
  offset = log(nn), family = "poisson", data =dd_filtered_15$data,
  num.threads = 1, control.compute = list(internal.opt = F, cpo = T, waic = T, dic = T), 
  verbose = T)

##' Strict PC-prior -----------------------------------------------------------#
cav_MBYM_inla.AR1.PC.strict_s15 <- inla(
  N_ACC ~ 0 + Y_2021 + Y_2022 + Y_2023 + Y_2024 + AVC_dist + Desk_dist+ ELI + PGR + UIS + ELL + PDI + ER+ 
    f(ID, model = inla.MBYM.AR1(k = 4, W = W_con, PC.ar1 = T, alpha.phi =0.9, U.phi=0.6,
                                alpha.sd =0.1, U.sd = sqrt(1/2)), 
      extraconstr = constr.BYM),
  offset = log(nn), family = "poisson", data =dd_filtered_15$data,
  num.threads = 1, control.compute = list(internal.opt = F, cpo = T, waic = T, dic = T), 
  verbose = T)
 



#' Spatial filter - remove 20 eigenvectors ------------------------------------#

##' Uniform prior -------------------------------------------------------------#

cav_MBYM_inla.AR1.Unif_s20 <- inla(
  N_ACC ~ 0 + Y_2021 + Y_2022 + Y_2023 + Y_2024 + AVC_dist +
    Desk_dist + ELI + PGR + UIS + ELL + PDI + ER+ 
    f(ID, model = inla.MBYM.AR1(k = 4, W = W_con, PC.ar1 = T, PC.phi = F, alpha.sd = 0.1, U.sd=sqrt(1/2)), 
      extraconstr = constr.BYM),
  offset = log(nn), family = "poisson", data =dd_filtered_20$data,
  num.threads = 1, control.compute = list(internal.opt = F, cpo = T, waic = T, dic = T), 
  verbose = T)

##' Standard PC-prior ---------------------------------------------------------#
cav_MBYM_inla.AR1.PC_s20 <- inla(
  N_ACC ~ 0 + Y_2021 + Y_2022 + Y_2023 + Y_2024 + AVC_dist +
    Desk_dist + ELI + PGR + UIS + ELL + PDI + ER+ 
    f(ID, model = inla.MBYM.AR1(k = 4, W = W_con, PC.ar1 = T, PC.phi = T, alpha.sd =0.1, U.sd = sqrt(1/2)), 
      extraconstr = constr.BYM),
  offset = log(nn), family = "poisson", data =dd_filtered_20$data,
  num.threads = 1, control.compute = list(internal.opt = F, cpo = T, waic = T, dic = T), 
  verbose = T)

##' Strict PC-prior -----------------------------------------------------------#
cav_MBYM_inla.AR1.PC.strict_s20 <- inla(
  N_ACC ~ 0 + Y_2021 + Y_2022 + Y_2023 + Y_2024 + AVC_dist + Desk_dist+ ELI + PGR + UIS + ELL + PDI + ER+ 
    f(ID, model = inla.MBYM.AR1(k = 4, W = W_con, PC.ar1 = T, alpha.phi =0.9, U.phi=0.6,
                                alpha.sd =0.1, U.sd = sqrt(1/2)), 
      extraconstr = constr.BYM),
  offset = log(nn), family = "poisson", data =dd_filtered_20$data,
  num.threads = 1, control.compute = list(internal.opt = F, cpo = T, waic = T, dic = T), 
  verbose = T)

#' Spatial filter - remove 25 eigenvectors ------------------------------------#

##' Uniform prior -------------------------------------------------------------#

cav_MBYM_inla.AR1.Unif_s25 <- inla(
  N_ACC ~ 0 + Y_2021 + Y_2022 + Y_2023 + Y_2024 + 
    AVC_dist + Desk_dist + ELI + PGR + UIS + ELL + PDI + ER+ 
    f(ID, model = inla.MBYM.AR1(k = 4, W = W_con, PC.ar1 = T, 
                                PC.phi = F, alpha.sd = 0.1, U.sd=sqrt(1/2)), 
      extraconstr = constr.BYM),
  offset = log(nn), family = "poisson", data =dd_filtered_25$data,
  num.threads = 1, control.compute = list(internal.opt = F, cpo = T, waic = T, dic = T), 
  verbose = T)

##' Standard PC-prior ---------------------------------------------------------#
cav_MBYM_inla.AR1.PC_s25 <- inla(
  N_ACC ~ 0 + Y_2021 + Y_2022 + Y_2023 + Y_2024 +
    AVC_dist + Desk_dist + ELI + PGR + UIS + ELL + PDI + ER+ 
    f(ID, model = inla.MBYM.AR1(k = 4, W = W_con, PC.ar1 = T, 
                                PC.phi = T, alpha.sd =0.1, U.sd = sqrt(1/2)), 
      extraconstr = constr.BYM),
  offset = log(nn), family = "poisson", data =dd_filtered_25$data,
  num.threads = 1, control.compute = list(internal.opt = F, cpo = T, waic = T, dic = T), 
  verbose = T)

##' Strict PC-prior -----------------------------------------------------------#
cav_MBYM_inla.AR1.PC.strict_s25 <- inla(
  N_ACC ~ 0 + Y_2021 + Y_2022 + Y_2023 + Y_2024 + 
    AVC_dist + Desk_dist+ ELI + PGR + UIS + ELL + PDI + ER+ 
    f(ID, model = inla.MBYM.AR1(k = 4, W = W_con, PC.ar1 = T, alpha.phi =0.9, U.phi=0.6,
                                alpha.sd =0.1, U.sd = sqrt(1/2)), 
      extraconstr = constr.BYM),
  offset = log(nn), family = "poisson", data =dd_filtered_25$data,
  num.threads = 1, control.compute = list(internal.opt = F, cpo = T, waic = T, dic = T), 
  verbose = T)


#' Posterior summaries - can done more elegantly than this --------------------#

#' Transformed marginals ------------------------------------------------------#
phi.marg <- inla.tmarginal(
  function(x) 1/(1+exp(-x)),
  marginal = cav_MBYM_inla.AR1.Unif_s20$marginals.hyperpar[[1]])

sigma.marg <- inla.tmarginal(
  function(x) exp(-x/2),
  marginal = cav_MBYM_inla.AR1.Unif_s20$marginals.hyperpar[[2]])

sigmasq.marg <- inla.tmarginal(
  function(x) exp(-x),
  marginal = cav_MBYM_inla.AR1.Unif_s20$marginals.hyperpar[[2]])


rho.marg <- inla.tmarginal(
  function(x) 2/(1+exp(-x))-1,
  marginal = cav_MBYM_inla.AR1.Unif_s20$marginals.hyperpar[[3]])


hyper.post <- as.data.frame(round(do.call(rbind, lapply(
  list(phi.marg, sigma.marg, sigmasq.marg, rho.marg), function(f) {
    unlist(inla.zmarginal(f, silent=T))})), 3)) %>% 
  dplyr::select(c(1,2,3,5,7)) %>% 
  dplyr::mutate(param = c("phi", "sigma", "sigmaSq", "rho")) %>% 
  dplyr::relocate(param, .before=1)
print(xtable::xtable(hyper.post, digits=3), include.rowname = F)





##  Model comparison -----------------------------------------------------------


#' Forest plot for covariate effects, compare spatial and nonspatial models ---#


.mods <- list(cav_nosp_inla, cav_IMCAR_inla.AR1, cav_PMCAR_inla.AR1.PC.strict,
              cav_LMCAR_inla.AR1.PC.strict, cav_MBYM_inla.AR1.Unif,
              cav_IMCAR_inla.AR1_s20, cav_PMCAR_inla.AR1.PC.strict_s20,
              cav_LMCAR_inla.AR1.PC.strict_s20, cav_MBYM_inla.AR1.Unif_s20)

covar.in <- c("Desk_dist","AVC_dist","ELI","PGR","UIS","ELL","PDI","ER")

forestplot. <- inla.forestplot.beta(
  models = .mods,  covar.in = covar.in,
  model_names = c(".Nonspatial", "base ICAR", "base PCAR", "base LCAR", "base BYM",
                  "S+ ICAR", "S+ PCAR", "S+ LCAR", "S+ BYM"),
  main = "Credible intervals of covariate effects")

#' !!!!!!!!!!!!!! Warning!! currently saved with dim 9.64 x 6.5 !!!!!!!!!!!!!!!!
#' 

#' WAIC and DIC, no spatial confounding treatment -----------------------------#

mm.tot <- list(nosp= cav_nosp_inla, 
               ICAR = cav_IMCAR_inla.AR1, 
               PCAR_Un = cav_PMCAR_inla.AR1.Unif,
               PCAR_PC_d= cav_PMCAR_inla.AR1.PC,
               PCAR_PC_s=cav_PMCAR_inla.AR1.PC.strict,
               LCAR_Un = cav_LMCAR_inla.AR1.Unif, 
               LCAR_PC_d=cav_LMCAR_inla.AR1.PC,
               LCAR_PC_s=cav_LMCAR_inla.AR1.PC.strict,
               BYM_Un=cav_MBYM_inla.AR1.Unif,
               BYM_PC_d = cav_MBYM_inla.AR1.PC,
               BYM_PC__s = cav_MBYM_inla.AR1.PC.strict)

#' WAIC comparison with spatial+ treatment.
#' Ensure all these models have been run, with these eigenvector
#' removal patterns -----------------------------------------------------------#
mm.tot_s15 <- list(
  ICAR = cav_IMCAR_inla.AR1_s15, 
  PCAR_Un = cav_PMCAR_inla.AR1.Unif_s15,
  PCAR_PC_d= cav_PMCAR_inla.AR1.PC_s15,
  PCAR_PC_s=cav_PMCAR_inla.AR1.PC.strict_s15,
  LCAR_Un = cav_LMCAR_inla.AR1.Unif_s15, 
  LCAR_PC_d=cav_LMCAR_inla.AR1.PC_s15,
  LCAR_PC_s=cav_LMCAR_inla.AR1.PC.strict_s15,
  BYM_Un=cav_MBYM_inla.AR1.Unif_s15,
  BYM_PC_d = cav_MBYM_inla.AR1.PC_s15,
  BYM_PC__s = cav_MBYM_inla.AR1.PC.strict_s15)


mm.tot_s20 <- list(
  ICAR = cav_IMCAR_inla.AR1_s20, 
  PCAR_Un = cav_PMCAR_inla.AR1.Unif_s20,
  PCAR_PC_d= cav_PMCAR_inla.AR1.PC_s20,
  PCAR_PC_s=cav_PMCAR_inla.AR1.PC.strict_s20,
  LCAR_Un = cav_LMCAR_inla.AR1.Unif_s20, 
  LCAR_PC_d=cav_LMCAR_inla.AR1.PC_s20,
  LCAR_PC_s=cav_LMCAR_inla.AR1.PC.strict_s20,
  BYM_Un=cav_MBYM_inla.AR1.Unif_s20,
  BYM_PC_d = cav_MBYM_inla.AR1.PC_s20,
  BYM_PC__s = cav_MBYM_inla.AR1.PC.strict_s20)

mm.tot_s25<- list(
  ICAR = cav_IMCAR_inla.AR1_s25, 
  PCAR_Un = cav_PMCAR_inla.AR1.Unif_s25,
  PCAR_PC_d= cav_PMCAR_inla.AR1.PC_s25,
  PCAR_PC_s=cav_PMCAR_inla.AR1.PC.strict_s25,
  LCAR_Un = cav_LMCAR_inla.AR1.Unif_s25, 
  LCAR_PC_d=cav_LMCAR_inla.AR1.PC_s25,
  LCAR_PC_s=cav_LMCAR_inla.AR1.PC.strict_s25,
  BYM_Un=cav_MBYM_inla.AR1.Unif_s25,
  BYM_PC_d = cav_MBYM_inla.AR1.PC_s25,
  BYM_PC__s = cav_MBYM_inla.AR1.PC.strict_s25)



WAICS <- as.data.frame(do.call(rbind, lapply(mm.tot, function(x) {
  cbind(x$waic$waic, x$waic$p.eff)} )))
names(WAICS) <- c("WAIC", "ow_P_eff")
WAICS$models <- names(mm.tot)
WAICS <- WAICS[,c(3,1,2)]

WAICS_s15 <- as.data.frame(do.call(rbind, lapply(mm.tot_s15, function(x) {
  cbind(x$waic$waic, x$waic$p.eff)} )))
names(WAICS_s15) <- c("WAIC_s15", "ow_P_eff_s15")
WAICS_s15$models <- names(mm.tot_s15)
WAICS_s15 <- WAICS_s15[,c(3,1,2)]

WAICS_s20 <- as.data.frame(do.call(rbind, lapply(mm.tot_s20, function(x) {
  cbind(x$waic$waic, x$waic$p.eff)} )))
names(WAICS_s20) <- c("WAIC_s20", "ow_P_eff_s20")
WAICS_s20$models <- names(mm.tot_s20)
WAICS_s20 <- WAICS_s20[,c(3,1,2)]

WAICS_s25 <- as.data.frame(do.call(rbind, lapply(mm.tot_s25, function(x) {
  cbind(x$waic$waic, x$waic$p.eff)} )))
names(WAICS_s25) <- c("WAIC_s25", "ow_P_eff_s25")
WAICS_s25$models <- names(mm.tot_s25)
WAICS_s25 <- WAICS_s25[,c(3,1,2)]

WAICS.all <- dplyr::left_join(WAICS, WAICS_s15, by = "models") %>% 
  dplyr::left_join(WAICS_s20, by = "models") %>% 
  dplyr::left_join(WAICS_s25, by = "models")

print(xtable::xtable(WAICS.all, n.digits = 3), include.rowname = F)





