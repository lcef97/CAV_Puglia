#' ---------  Analisi CAV Puglia 2021-24 --------------------------------------#
#' 
#' This is only a resume 
#' of \url{https://github.com/lcef97/CAV_Puglia/blob/main/CAV_panel.Rmd}
#' employed to generate workspaces, which speeds up the HTML compiler.
#' It is memory-intensive but space-saving
#'
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
}

library(knitr)
#' This is a great function to extract R code from a remote script -------------
knitr::purl("index.Rmd", output = "extracted_chunks.R", documentation = 0)
code_lines <- readLines("extracted_chunks.R")

start_source <- grep("StartRemoteSource", code_lines)
end_source <- grep("EndRemoteSource", code_lines)
remote_source <- code_lines[start_source:end_source]

eval(parse(text = remote_source), envir = globalenv())








chunks <- knitr:::knit_code$restore() 
knitr::read_chunk("docs/index.Rmd")
chunks_env <- knitr:::knit_code$get()

if (!grepl("InputMute", chunks_env)) {
  stop(paste("Chunk", chunk_label, "not found in", rmd_path))
}

# Extract the code from the named chunk
code <- knitr:::knit_code$get()[[chunk_label]]

# Evaluate it
eval(parse(text = code), envir = globalenv())


#' Load inputs, as in the HTML
#' 
## Input ----------------------------------------------------------------------#

load("input/CAV_input_mun_2021.RData")
load("input/CAV_input_mun_2022.RData")
load("input/CAV_input_mun_2023.RData")
load("input/CAV_input_mun_2024.RData")

source("Auxiliary/Functions.R")

CAV_mun_21 <- CAV_mun_21 %>% dplyr::rename(N_ACC_2021 = .data$N_ACC)
CAV_mun_22 <- CAV_mun_22 %>% dplyr::rename(N_ACC_2022 = .data$N_ACC)
CAV_mun_23 <- CAV_mun_23 %>% dplyr::rename(N_ACC_2023 = .data$N_ACC)
CAV_mun_24 <- CAV_mun_24 %>% dplyr::rename(N_ACC_2024 = .data$N_ACC)

load("input/Shp.RData")

#'  Function to extract numeric digits from a strings vector (needed to filter age):
nn_extract <- function(string){
  nn <- gregexpr("([0-9])", string)
  ls.out <- regmatches(as.list(string), nn)
  res <- unlist(lapply(ls.out, function(x) as.numeric(paste(x, collapse = ""))))  
  return(res)
}

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


# Tremiti Islands are a singleton --> need to remove them to perform spatial analysis
suppressWarnings({
  singletons <- Shp$PRO_COM[which(
    unlist(lapply(spdep::poly2nb(Shp),
                  function(x) x[1L] == 0)))]
})

load("input/dists_th_22.RData")
load("input/dists_th_23.RData")
load("input/dists_th_24.RData")
names(dists_th_22)[2] <- names(dists_th_23)[2] <- names(dists_th_24)[2] <- "TEP_th"
distances <- rbind(dists_th_22, dists_th_22, dists_th_23, dists_th_24) %>% 
  dplyr::mutate(Year = c(rep(2021, nrow(dists_th_22)),
                         rep(2022, nrow(dists_th_22)),
                         rep(2023, nrow(dists_th_23)),
                         rep(2024, nrow(dists_th_24))) )


Shp_con <- dplyr::filter(Shp, !.data$PRO_COM %in% singletons)
n <- nrow(Shp_con)


dd <- Shp_con %>% dplyr::left_join(Pop_f_15[, -2], by = "PRO_COM") %>% 
  dplyr::left_join(dplyr::select(CAV_mun_21, -.data$comune),by = "PRO_COM") %>% 
  dplyr::left_join(dplyr::select(CAV_mun_22, -.data$comune),by = "PRO_COM") %>% 
  dplyr::left_join(dplyr::select(CAV_mun_23, -.data$comune),by = "PRO_COM") %>% 
  dplyr::left_join(dplyr::select(CAV_mun_24, -.data$comune),by = "PRO_COM") %>% 
  tidyr::pivot_longer(cols = tidyselect::starts_with("nn_") | 
                        tidyselect::starts_with("N_ACC_"),
                      names_to = c(".value", "Year"),
                      names_pattern = "(nn|N_ACC)_(\\d+)" ) %>% 
  dplyr::mutate(Year=as.numeric(.data$Year)) %>% 
  dplyr::left_join(dplyr::select(Indicators, -.data$Comune), by = "PRO_COM") %>% 
  dplyr::arrange(.data$Year, .data$PRO_COM)  %>% 
  dplyr::left_join(distances, by = c("PRO_COM", "Year")) %>% 
  dplyr::mutate(CAV = as.numeric(.data$TEP_th == 0)) %>% 
  dplyr::mutate(Y_2021 = as.numeric(.data$Year == 2021)) %>% 
  dplyr::mutate(Y_2022 = as.numeric(.data$Year == 2022)) %>% 
  dplyr::mutate(Y_2023 = as.numeric(.data$Year == 2023)) %>% 
  dplyr::mutate(Y_2024 = as.numeric(.data$Year == 2024)) %>% 
  dplyr::mutate(TEP_th = as.vector(scale(.data$TEP_th))) %>% 
  dplyr::mutate(AES = as.vector(scale(.data$AES))) %>% 
  dplyr::mutate(MFI = as.vector(scale(.data$MFI)))  %>% 
  dplyr::mutate(PDI = as.vector(scale(.data$PDI)))  %>% 
  dplyr::mutate(ELL = as.vector(scale(.data$ELL)))  %>% 
  dplyr::mutate(ER = as.vector(scale(.data$ER)))  %>% 
  dplyr::mutate(PGR = as.vector(scale(.data$PGR)))  %>% 
  dplyr::mutate(UIS = as.vector(scale(.data$UIS)))  %>% 
  dplyr::mutate(ELI = as.vector(scale(.data$ELI)))  %>% 
  #dplyr::select(-.data$Year) %>% 
  dplyr::mutate(ID = c(1:nrow(.))) %>% 
  dplyr::mutate(Area = as.numeric(as.factor(.data$PRO_COM)))



# Municipalities from which no woman reported violence --> count == 0
dd$N_ACC[is.na(dd$N_ACC)] <- 0

# "access ratio"
dd$LN_ACC <- log(dd$N_ACC/dd$nn)

#' neighbours list ------------------------------------------------------------#
nb_con <- spdep::poly2nb(dd[c(1:n), ])

#' neighbouring/adjacency matrix ----------------------------------------------#
W_con <- spdep::nb2mat(nb_con, style = "B")
rownames(W_con) <- colnames(W_con) <- dd$PRO_COM[c(1:n)]

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

 
##' ------------------  ICAR model ---------------------------------------------
#'
#'
#'
cav_IMCAR_inla <- inla(
  N_ACC ~ 0 + Y_2021 + Y_2022 + Y_2023 + Y_2024 +
    TEP_th + ELI + PGR + UIS + ELL + PDI + ER+ 
    f(ID, model = inla.IMCAR.Bartlett(k = 4, W = W_con, df = 8), 
      extraconstr = list(A = A_constr, e = c(0,0,0,0))),
  offset = log(nn), family = "poisson", data =dd,
  num.threads = 1, control.compute = list(internal.opt = F, cpo = T, waic = T, config = T), 
  verbose = T)
