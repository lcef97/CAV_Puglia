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


#'munWdesk <- c(
#'  71004,  71006,  71008,  71010,  71012,  71020,  71021,  71022,  71024,  
#'  71025,  71027,  71028,  71035,  71036,  71038,  71041,  71043,  71046,  
#'  71047,  71049,  71050,  71051,  71053,  71054,  71055,  71056,  71058,  
#'  71059,  71060,  71063,  72003,  72006,  72010,  72011,  72012,  72016,  
#'  72020,  72022,  72023,  72024,  72025,  72027,  72033,  72038,  72039,  
#'  72041,  72043,  73003,  73007,  73008,  73015,  73019,  73020,  73021,  
#'  73024,  73027,  73029,  74002,  74003,  74005,  74006,  74007,  74008, 
#'  74011,  74012,  74014,  74018,  74020,  75002,  75003,  75010,  75012, 
#'  75016,  75021,  75022,  75024,  75028,  75030,  75031,  75035,  75037,
#'  75039,  75040,  75043,  75044,  75050,  75051,  75052,  75057,  75063, 
#'  75064,  75073,  75077,  75081,  75085,  75086,  75088,  75089,  75093, 
#'  75097, 110003, 110004, 110005, 110007, 110010)
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


load("input/dists_th_22.RData")
load("input/dists_th_23.RData")
load("input/dists_th_24.RData")
load("input/dists_desk.RData")

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
  dplyr::left_join(dists_desk, by = "PRO_COM") %>% 
  dplyr::mutate(CAV = as.numeric(.data$TEP_th == 0)) %>% 
  dplyr::mutate(Y_2021 = as.numeric(.data$Year == "2021")) %>% 
  dplyr::mutate(Y_2022 = as.numeric(.data$Year == "2022")) %>% 
  dplyr::mutate(Y_2023 = as.numeric(.data$Year == "2023")) %>% 
  dplyr::mutate(Y_2024 = as.numeric(.data$Year == "2024")) %>% 
  dplyr::mutate(TEP_th = as.vector(scale(.data$TEP_th))) %>% 
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

#'  Necessary to constrain the BYM model --------------------------------------#
constr.BYM <- list(
  A = cbind(Matrix::Matrix(0, nrow = nrow(A_constr), ncol = ncol(A_constr)), A_constr),
  e=c(0,0,0,0) )


#' Full GLM --> for model matrix
glm_all_X <- glm(N_ACC ~ 1 + Desk_dist + TEP_th + MFI + AES + PDI + ELL + ER +
                   PGR + UIS + ELI + offset(log(nn)),
                 data = dd_con, family = "poisson")
#' model matrix
X <- model.matrix(glm_all_X)


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


#' Spatial filtering of Laplacian eigenvectors --------------------------------#

#' Speed-up initialising:
covars <- c("MFI", "HMI", "MWR", "PA", "LRA", "LC", "AES", "PDI",
            "ELL", "ER", "PGR", "UIS", "ELI","TEP_th")

n.eigen <- rep(10, length(covars))
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
    eigen.out <- c((nrow(V) - n.eigen[j]+1) : nrow(V) )
    nrep <- nrow(data)/nrow(xx)
    xx.nosp <- scale(V[, eigen.in] %*% coef[eigen.in], 
                     scale = rescale, center = center)
    xx.nosp <- rep(as.vector(xx.nosp), nrep)
    xx.sp <-  as.vector(V[, eigen.out] %*% coef[eigen.out])
    trends[[j]] <- xx.sp
    names(trends)[[j]] <- names(data)[m]
    res[,m] <- xx.nosp
  }
  trends.df <- as.data.frame(do.call(cbind, trends))
  return(list(data = res, trends = trends.df))
}

dd_filtered <- filter.spatplus(dd_con, covars=c("ER", "ELL", "UIS"), V = VV,
                               n.eigen = c(15, 15, 15))


#' ----------------------------------------------------------------------------#

##  INLA code for factorisable ST autoregressive models ------------------------
#'
#' First of all, recall INLA. Otherwise, what is the whole thing about? -------#
library(INLA)
#' Temporal structure is AR(1)-like -------------------------------------------#

##'    IMCAR model ------------------------------------------------------------#

inla.IMCAR.AR1  <- function(...){
  INLA::inla.rgeneric.define(inla.rgeneric.IMCAR.AR1,
                             scale.model = TRUE, 
                             alpha.sd = 1/100, U.sd = 1,
                             PC.ar1 = T, alpha.corr = 0.6, U.corr = 0.3,
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
        dpc.corr.ar1 <- function(x, t=k, n=1000, alpha=0.6, U=0.3, log=F){
          x <- pmax(abs(x), 1e-12) * ifelse(x < 0, -1, 1)
          KLD <- function(x, t, n){
            return(n/2 * ( log(1-x^2) + t*x^2/(1-x^2) ))
          }
          deriv.KLD <- function(x, t, n){
            return(n*x*( -1/(1-x^2) + t/((1-x^2)^2)  ) )
          }
          dist.zero <- sqrt(2*KLD(U, t=t, n=n))
          rate <- -log(alpha)/dist.zero
          ff <- stats::dexp(x=sqrt(2*KLD(x=x, t=t, n=n)), rate = rate)*
            1/sqrt(2*KLD(x=x, t=t, n=n)) * abs(deriv.KLD(x=x, t=t, n=n)) / 2
          
          return(ifelse(log, log(ff), ff))
        }
        assign("dpc.corr.ar1", dpc.corr.ar1, envir = envir)
      }
      assign("cache.done", TRUE, envir=envir)
    }
    interpret.theta <- function() {
      prec <- exp(theta[1]) 
      corr  <- 2/(1 + exp(-theta[2])) - 1
      invR <- Matrix::bandSparse(
        n=k, m=k, k = c(-1, 0, 1),
        diagonals = list(rep(-corr, k-1), rep(1+corr^2, k), rep(-corr, k-1)))
      #' Not so elegant but works when k==1; otherwise, c(..., rep(., k-2)) would crash
      invR[1,1] <- invR[k,k] <- 1 
      PREC <-  prec * invR 
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
      val <- theta[1] +
        INLA:::inla.pc.dprec(exp(theta[1]), alpha = alpha.sd, u = U.sd)
      #val <- theta[1]/2 - log(2) + dexp(exp(log.sd/2), rate=sd.rate, log=T) 
      if(PC.ar1){
        val <- val + log(2) - theta[2] - 2*log(1+exp(-theta[2])) +
          dpc.corr.ar1(x=param$corr, n=nrow(W), t = k, alpha = alpha.corr, U = U.corr, log=T)
      } else {
        val <- val+ # log(2) - theta[k+1] - log(1+exp(-theta[k+1]))  +
          dnorm(theta[2], sd = sqrt(1/0.15), log=T)
      }
      return(val)
    }
    initial <- function(){
      if(!exists("initial.values", envir= envir )){
        return(c(4, -4))
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
      
      
      dpc.corr.ar1 <- function(x, t=k, n=1000, alpha=0.6, U=0.3, log=F){
        x <- pmax(abs(x), 1e-12) * ifelse(x < 0, -1, 1)
        KLD <- function(x, t, n){
          return(n/2 * ( log(1-x^2) + t*x^2/(1-x^2) ))
        }
        deriv.KLD <- function(x, t, n){
          return(n*x*( -1/(1-x^2) + t/((1-x^2)^2)  ) )
        }
        dist.zero <- sqrt(2*KLD(U, t=t, n=n))
        rate <- -log(alpha)/dist.zero
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
                             PC.ar1 = T, alpha.corr = 0.6, U.corr = 0.3, 
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
        eigenvalues <- eigen(L - In)$values
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
        dpc.corr.ar1 <- function(x, t=10, n=1000, alpha=0.6, U=0.3, log=F){
          x <- pmax(abs(x), 1e-12) * ifelse(x < 0, -1, 1)
          KLD <- function(x, t, n){
            return(n/2 * ( log(1-x^2) + t*x^2/(1-x^2) ))
          }
          deriv.KLD <- function(x, t, n){
            return(n*x*( -1/(1-x^2) + t/((1-x^2)^2)  ) )
          }
          dist.zero <- sqrt(2*KLD(U, t=t, n=n))
          rate <- -log(alpha)/dist.zero
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
                             PC.ar1 = T, alpha.corr = 0.6, U.corr = 0.3,
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
#    eigenvectors <- eigen(invL)$vectors
    inla.pc.mbym.phi <- function(phi, eigenvalues, alpha = 2/3, U = 1/2, force.const=1){
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
    ff <- function(x) exp(inla.pc.mbym.phi(phi=x, eigenvalues=eigenvalues, alpha=alpha, U=U,force.const=1))
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
        rate <- -log(alpha)/dist.zero
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
                              alpha = alpha.phi, U = U.phi) -
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
                             PC.ar1 = T, alpha.corr = 0.6, U.corr = 0.3, 
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
    Desk_dist + TEP_th + ELI + PGR + UIS + ELL + PDI + ER, 
  family = "poisson", offset = log(nn), data = dd_con)

#' Model with offset ----------------------------------------------------------#
cav_nosp_inla <- inla(
  N_ACC ~ 0 + Y_2021 + Y_2022 + Y_2023 + Y_2024 +
  TEP_th + Desk_dist + ELI + PGR + UIS + ELL + PDI + ER,
  offset = log(nn),
  family = "poisson", data =dd_con,
  num.threads = 1, 
  control.compute = list(internal.opt = F, cpo = T, waic = T, dic = T), 
  verbose = T)

#' Model with expected accesses - equivalent in theory ------------------------#
cav_nosp_inla_E <- inla(
  N_ACC ~ 0 + Y_2021 + Y_2022 + Y_2023 + Y_2024 +
    TEP_th + Desk_dist + ELI + PGR + UIS + ELL + PDI + ER,
  E = Exp,
  family = "poisson", data = dd_con,
  num.threads = 1, 
  control.compute = list(internal.opt = F, cpo = T, waic = T, config = T), 
  verbose = T)




##  Spatiotemporal models: AR(1) - ICAR -----------------------------------------

 

##' Tuning a priori -----------------------------------------------------------#
u <- sqrt(1/2); alpha = 0.1
ff <- Vectorize(function(x) INLA:::inla.pc.dprec(prec=x, u=u, alpha = alpha))
plot(taus, ff(taus), type = 'l', main = paste0("u = ",u, " alpha = ", alpha))
abline(v=u)
abline(v=1.5, col="blue")
abline(v=1/u^2, col="red")
integrate(ff, 0, 1.5)
integrate(ff,0, 1/u^2)

u <- 1/3; alpha = 2/3
rhos <- c(-499:-1, 1:499)/500
ff.r <- Vectorize(function(x) dpc.corr.ar1(x=x, t=4, U=u, alpha = alpha))
plot(rhos, ff.r(rhos), type = 'l', main = paste0("u = ",u, " alpha = ", alpha))
abline(v=u)



cav_IMCAR_inla.AR1 <- inla(
  N_ACC ~ 0 + Y_2021 + Y_2022 + Y_2023 + Y_2024 + 
    Desk_dist + TEP_th + ELI + PGR + UIS + ELL + PDI + ER+ 
    f(ID, model = inla.IMCAR.AR1(k = 4, W = W_con, PC.ar1 = T, 
                                 alpha.sd = 0.1, U.sd = sqrt(1/2)), 
      extraconstr = list(A = A_constr, e = c(0,0,0,0))),
  offset = log(nn), family = "poisson", data =dd_con,
  num.threads = 1, control.compute = list(internal.opt = F, cpo = T, waic = T, dic = T), 
  verbose = T)
cav_IMCAR_inla.AR1$waic$waic



##  Spatiotemporal models: AR(1) - PCAR -----------------------------------------





cav_PMCAR_inla.AR1.Unif <- inla(
  N_ACC ~ 0 + Y_2021 + Y_2022 + Y_2023 + Y_2024 +
    Desk_dist + TEP_th + ELI + PGR + UIS + ELL + PDI + ER+ 
    f(ID, model = inla.PMCAR.AR1(k = 4, W = W_con, PC.spat.cor = F, alpha.sd = 0.1, U.sd = sqrt(1/2)), 
      extraconstr = list(A = A_constr, e = c(0,0,0,0))
    ),
  offset = log(nn), family = "poisson", data =dd_con,
  num.threads = 1, control.compute = list(internal.opt = F, cpo = T, waic = T, dic = T), 
  verbose = T)



cav_PMCAR_inla.AR1.PC <- inla(
  N_ACC ~ 0 + Y_2021 + Y_2022 + Y_2023 + Y_2024 + 
    Desk_dist + TEP_th + ELI + PGR + UIS + ELL + PDI + ER+ 
    f(ID, model = inla.PMCAR.AR1(k = 4, W = W_con, PC.ar1 = T, alpha.sd = 0.1, U.sd = sqrt(1/2)),
      extraconstr = list(A = A_constr, e = c(0,0,0,0))
    ),
  offset = log(nn), family = "poisson", data =dd_con,
  num.threads = 1, control.compute = list(internal.opt = F, cpo = T, waic = T, dic = T), 
  verbose = T)

#' Better
cav_PMCAR_inla.AR1.PC.strict <- inla(
  N_ACC ~ 0 + Y_2021 + Y_2022 + Y_2023 + Y_2024 +
    Desk_dist + TEP_th + ELI + PGR + UIS + ELL + PDI + ER+ 
    f(ID, model = inla.PMCAR.AR1(k = 4, W = W_con, PC = T, PC.ar1 = T,
                                 alpha.spat.cor = 0.9, U.spat.cor = 0.6,
                                 alpha.sd = 0.1, U.sd = sqrt(1/2)),
      extraconstr = list(A = A_constr, e = c(0,0,0,0))
    ),
  offset = log(nn), family = "poisson", data =dd_con,
  num.threads = 1, control.compute = list(internal.opt = F, cpo = T, waic = T, dic = T), 
  verbose = T)




## Spatiotemporal models: AR(1) - LCAR -----------------------------------------
 
#' Yes, this model features sum-to-zero constraints.
#' Why? It is not singular.
#' No, it is not. Yet absence of constraints inflates variance.
#' Better to avoid confounding with intercepts --------------------------------#


cav_LMCAR_inla.AR1.Unif <- inla(
  N_ACC ~ 0 + Y_2021 + Y_2022 + Y_2023 + Y_2024 + 
    Desk_dist + TEP_th + ELI + PGR + UIS + ELL + PDI + ER+ 
    f(ID, model = inla.LMCAR.AR1(k = 4, W = W_con, PC.lambda = F, alpha.sd = 0.1, U.sd = sqrt(1/2)), 
      extraconstr = list(A = A_constr, e = c(0,0,0,0)) ),
  offset = log(nn), family = "poisson", data =dd_con,
  num.threads = 1, control.compute = list(internal.opt = F, cpo = T, waic = T, dic = T), 
  verbose = T)

cav_LMCAR_inla.AR1.PC <- inla(
  N_ACC ~ 0 + Y_2021 + Y_2022 + Y_2023 + Y_2024 + 
    Desk_dist + TEP_th + ELI + PGR + UIS + ELL + PDI + ER+ 
    f(ID, model = inla.LMCAR.AR1(k = 4, W = W_con, alpha.sd = 0.1, U.sd = sqrt(1/2)), 
      extraconstr = list(A = A_constr, e = c(0,0,0,0))),
  offset = log(nn), family = "poisson", data =dd_con,
  num.threads = 1, control.compute = list(internal.opt = F, cpo = T, waic = T, dic = T), 
  verbose = T)

#' Better, though...
cav_LMCAR_inla.AR1.PC.strict <- inla(
  N_ACC ~ 0 + Y_2021 + Y_2022 + Y_2023 + Y_2024 +
    Desk_dist + TEP_th + ELI + PGR + UIS + ELL + PDI + ER+ 
    f(ID, model = inla.LMCAR.AR1(k = 4, W = W_con, alpha.lambda =0.9, U.lambda = 0.6,
                                 alpha.sd = 0.1, U.sd = sqrt(1/2)), 
      extraconstr = list(A = A_constr, e = c(0,0,0,0)) ),
  offset = log(nn), family = "poisson", data =dd_con,
  num.threads = 1, control.compute = list(internal.opt = F, cpo = T, waic = T, dic = T), 
  verbose = T)






## Spatiotemporal models: AR(1) - BYM  -----------------------------------------



cav_MBYM_inla.AR1.Unif <- inla(
  N_ACC ~ 0 + Y_2021 + Y_2022 + Y_2023 + Y_2024 + TEP_th + Desk_dist + ELI + PGR + UIS + ELL + PDI + ER+ 
    f(ID, model = inla.MBYM.AR1(k = 4, W = W_con, PC.ar1 = T, PC.phi = F, alpha.sd = 0.1, U.sd=sqrt(1/2)), 
      extraconstr = constr.BYM),
  offset = log(nn), family = "poisson", data =dd_con,
  num.threads = 1, control.compute = list(internal.opt = F, cpo = T, waic = T, dic = T), 
  verbose = T)
cav_MBYM_inla.AR1.Unif$waic$waic

cav_MBYM_inla.AR1.PC <- inla(
  N_ACC ~ 0 + Y_2021 + Y_2022 + Y_2023 + Y_2024 + TEP_th + Desk_dist + ELI + PGR + UIS + ELL + PDI + ER+ 
    f(ID, model = inla.MBYM.AR1(k = 4, W = W_con, PC.ar1 = T, PC.phi = T, alpha.sd =0.1, U.sd = sqrt(1/2)), 
      extraconstr = constr.BYM),
  offset = log(nn), family = "poisson", data =dd_con,
  num.threads = 1, control.compute = list(internal.opt = F, cpo = T, waic = T, dic = T), 
  verbose = T)
cav_MBYM_inla.AR1.PC$waic$waic



cav_MBYM_inla.AR1.PC.strict <- inla(
  N_ACC ~ 0 + Y_2021 + Y_2022 + Y_2023 + Y_2024 + TEP_th + Desk_dist+ ELI + PGR + UIS + ELL + PDI + ER+ 
    f(ID, model = inla.MBYM.AR1(k = 4, W = W_con, PC.ar1 = T, alpha.phi =0.9, U.phi=0.6,
                                alpha.sd =0.1, U.sd = sqrt(1/2)), 
      extraconstr = constr.BYM),
  offset = log(nn), family = "poisson", data =dd_con,
  num.threads = 1, control.compute = list(internal.opt = F, cpo = T, waic = T, dic = T), 
  verbose = T)






## TBD: model comparison draft -------------------------------------------------

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

ICS <- as.data.frame(do.call(rbind, lapply(mm.tot, function(x) {
  cbind(x$waic$waic, x$waic$p.eff, x$dic$dic, x$dic$p.eff)} )))
names(ICS) <- c("WAIC", "WAIC_P_eff", "DIC", "DIC_P_eff")
ICS$models <- names(mm.tot)
ICS <- ICS[,c(5,1,2,3,4)]

print(xtable::xtable(ICS, n.digits = 3), include.rowname = F)

#' LGOCV - to overcome problems with LOOCV in INLA ----------------------------#

#' List of models
mm <- mm.tot

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
#LPMLs_df <- LPMLs(models = mm, c(1:6, 8, 10, 12, 15, 20, 25))
LPMLs_short <- LPMLs(models = mm, c(1, 2, 5, 8, 10, 15))
print(xtable::xtable( t(LPMLs_short),
                     n.digits = 3), include.rowname = T)


##' MISC - to be deleted -------------------------------------------------------
#############'
inla.pc.mbym.phi <- function(phi, eigenvalues.invm1, alpha = 2/3, U = 1/2, force.const = 1){
  n <- length(eigenvalues.invm1)
  In <- Matrix::Diagonal(n = n, x = 1)
  log.p <- numeric(length(phi))
  KLD <- function(phi, eigenvalues.invm1){
    res <- -1/2 * sum(log(1 + phi*eigenvalues.invm1)) +
      1/2 * phi*sum(eigenvalues.invm1)
    return(res)
  }
  deriv.KLD <- function(phi, eigenvalues.invm1){
    res <- 1/2 * sum(-eigenvalues.invm1/(1+phi*eigenvalues.invm1) +
                       eigenvalues.invm1)
    return(res)
  }
  for(j in c(1:length(phi))){
    KLD_j <- KLD(phi = phi[j], eigenvalues.invm1 = eigenvalues.invm1)
    if(KLD_j < 0){
      message("!!! PROBLEM !!!! \n!!! NEGATIVE KLD - MUST FIX MODEL !!!")
      return(NULL)
    }
    derivative <- 1/2 * sum(eigenvalues.invm1 - 
                              eigenvalues.invm1 / (1 + phi[j] * eigenvalues.invm1))
    rate <- -1/sqrt(2*KLD(U, eigenvalues.invm1 = eigenvalues.invm1)) * log(1 - alpha)
    log.p[j] <- dexp(x=sqrt(2*KLD_j), rate = rate, log = T) +
      log(1/sqrt(2*KLD_j)) + log(abs(derivative)) - log(force.const)
  }
  return(sum(log.p))
}
W <- W_con
L_unscaled <- Matrix::Diagonal(n=nrow(W), x=rowSums(as.matrix(W))) -  W
constr <- INLA:::inla.bym.constr.internal(L_unscaled, adjust.for.con.comp = T)
scaleQ <- INLA:::inla.scale.model.internal(
  L_unscaled, constr = list(A = constr$constr$A, e = constr$constr$e))
L <- scaleQ$Q
invL <- INLA:::inla.ginv(L)
In <- Matrix::Diagonal(n = nrow(W), x = 1)
eigenvalues <- eigen(L, symmetric = T)$values
rankdef <- constr$rankdef
eigenvalues.invm1 <- sort(1/eigenvalues, decreasing = T) - 1
#eigenvalues <- eigen(invL - In, symmetric = T)$values 
eigenvalues.invm1[(length(eigenvalues.invm1)-rankdef+1):length(eigenvalues.invm1)] <- - 1


u <- 0.5; alpha <- 0.8
phis <- c( 1:500)/500
ff <- Vectorize(function(x) exp(inla.pc.mbym.phi(phi=x,eigenvalues.invm1 = eigenvalues.invm1,
                                                 U=u, alpha = alpha, force.const=1)))
marginal.variances <- diag(invL)
eigen.L <- eigen(L, symmetric = T)$values
inla.pc.bym.phi.auto <- 
  INLA:::inla.pc.bym.phi(eigenvalues = eigen.L,
                         rankdef = 1, alpha=alpha, u=u, 
                         marginal.variances = marginal.variances)

ff.auto <- Vectorize(function(x) exp(inla.pc.bym.phi.auto(phi=x )))

plot(phis, ff(phis), type = 'l', main = paste0("u = ",u, " alpha = ", alpha))
lines(ff.auto(phis))

abline(v=u)


integrate(ff, 0, u)
integrate(ff, 0 ,1)